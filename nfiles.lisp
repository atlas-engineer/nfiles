;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nfiles)

;; TODO: Test on file-less content.
;; TODO: Reindent.

;; TODO: Make sure existing file is read before writing.

(defclass* profile ()
    ((name "default"
           :documentation "The name of the profile to refer it with."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer (class*:make-name-transformer name))
    (:documentation "This is the default profile.
`file' path expansion is specialized against "))

(defvar *profile-index* (tg:make-weak-hash-table :weakness :key :test 'equal)
  "Set of all `profile's objects.
It's a weak hash table to that garbage-collected nfiles are automatically
removed.")

(defmethod initialize-instance :after ((profile profile) &key)
  (setf (gethash profile *profile-index*) profile))

(defmethod find-profile ((name string))
  (loop for profile being the hash-key of *profile-index*
        when (string= name (name profile))
          return profile))

(defmethod find-profile ((p profile))
  (gethash p *profile-index*))

(export-always '*default-profile*)
(defvar *default-profile* (make-instance 'profile))

(class*:defclass* file ()
    ((path
      #p""
      :type pathname
      :export nil
      :initarg :path
      :documentation "")
     (profile
      *default-profile*
      :type profile
      :reader t
      :writer nil
      :initarg :profile
      :documentation "The `profile' of the `file'.
File expansion is performed against it.
The profile is only set at instantiation time.")
     (name
      ""
      :type string
      :documentation "Name used to identify the object in a human-readable manner.")
     (timeout                           ; Unexport? Make global option?
      0.1
      :type real
      :documentation "Time in seconds to wait for other write requests.")
     (writable
      t
      :type boolean
      :documentation "If T, the file is writable.
If NIL, then attempting to write to the file raises an error.")
     )
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer (class*:make-name-transformer name))
    (:documentation "TODO: complete me!"))

(defclass* lisp-file (file)
    ()
    (:documentation "Like regular `file' but assume a `.lisp' extension, even if
not provided."))

(defvar *index* (tg:make-weak-hash-table :weakness :key :test 'equal)
  "Set of all `file's objects.
It's a weak hash table to that garbage-collected files are automatically
removed.")

(defmethod initialize-instance :after ((file file) &key)
  (setf (path file) (uiop:ensure-pathname (path file)  ; TODO: Test that this disambiguate anything.
                                          :truenamize t))
  (setf (gethash file *index*) file))

;; TODO: Useless?
;; (defmethod find-file ((name string))
;;   (loop for profile being the hash-key of *profile-index*
;;         when (string= name (name profile))
;;           return profile))

(defmethod resolve ((profile profile) (file file))
  "Default path resolution."
  (path file))

(defmethod resolve ((profile profile) (file lisp-file))
  (make-pathname :defaults (call-next-method) :type "lisp"))

;; TODO: Make defgenerics with doc.

;; TODO: Does it make sense to serialize / deserialize over a profile?
;; Yes, because this is where we chose to not touch the filesystem, or to be read-only.

(export-always 'deserialize)
(defmethod deserialize ((profile profile) (file file))
  ;; TODO: Handle .gpg files.
  ;; TODO: Manage backups on error.
  ;; TODO: Handle OS locks.
  (when (uiop:file-exists-p (expand file)) ; TODO: We are checking twice.  Unnecessary?
    (alexandria:read-file-into-string (expand file))))

(defmethod deserialize ((profile profile) (file lisp-file))
  (read-from-string (call-next-method)))

;; TODO: Make sure serialization methods can be compounded.
(export-always 'serialize)
(defmethod serialize ((profile profile) (file file))
  ;; TODO: File creation / existence check.
  ;; TODO: Handle .gpg files.
  ;; TODO: Backup management.
  ;; TODO: Handle OS locks.
  (alexandria:write-string-into-file
   (content file)
   (expand file)
   :if-does-not-exist :create
   :if-exists :supersede))

(defmethod serialize ((profile profile) (file lisp-file))
  (alexandria:write-string-into-file
   (write-to-string
    (content file))
   (expand file)
   :if-does-not-exist :create
   :if-exists :supersede))


(export-always 'expand)
(-> expand (file) (values pathname &optional))
(defun expand (file)
  "Return the pathname corresponding to FILE and its `profile'."
  (the (values pathname &optional)
       (resolve (profile file) file)))


;; We need a cache system to avoid reading a file that's already in memory (from
;; another data file).

;; We also don't want to needlessly duplicate the content in memory.

;; TODO: How do we garbage collect cache entries?

(defclass* cache-entry ()               ; TODO: Rename?
    ((source-file
      (error "Source file must be given.")
      :type file
      :documentation "This is the `file' object that was used to instantiate the
entry's `cached-value'. ")
     (cached-value                      ; TODO: Rename to `content'?
      nil
      :type t)
     (worker
      nil
      :type (or null bt:thread))
     (worker-channel
      :type (or null calispel:channel)))
    (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod initialize-instance :after ((entry cache-entry) &key)
  (setf (cached-value entry)
        (when (uiop:file-exists-p (expand (source-file entry)))
          (deserialize (profile (source-file entry)) (source-file entry)))))

(defvar *cache* (sera:dict)
  "Internal `*cache*' associating expanded paths with a dedicated `cache-entry'.")

(defun cache-entry (file)
  ;; TODO: To support path-less content, maybe do (or (expand file) file)?
  (sera:synchronized (*cache*)
    (alexandria:ensure-gethash (expand file) *cache*
                               (make-instance 'cache-entry :source-file file))))

(export-always 'content)
(defmethod content ((file file))
  "Return the content of FILE."
  (let ((entry (cache-entry file)))
    (sera:synchronized (entry)
      (cached-value entry))))

(defun make-channel (&optional size)
  "Return a channel of capacity SIZE.
If SIZE is NIL, capicity is infinite."
  (cond
    ((null size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:unbounded-fifo-queue)))
    ((zerop size)
     (make-instance 'calispel:channel))
    ((plusp size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity size)))))

(defun drain-channel (channel &optional timeout)
  "Listen to CHANNEL until a value is available, then return all CHANNEL values
as a list.
TIMEOUT specifies how long to wait for a value after the first one.
This is a blocking operation."
  (labels ((fetch ()
             (multiple-value-bind (value received?)
                 (calispel:? channel timeout)
               (if received?
                   (cons value (fetch))
                   nil))))
    (nreverse (fetch))))

(defun make-worker (file cache-entry)
  (lambda ()
    (sera:nlet lp ((value (drain-channel (worker-channel cache-entry) (timeout file))))
      (if value
          (progn
            (serialize (profile file) file)
            (lp (drain-channel (worker-channel cache-entry) (timeout file)))))
      (sera:synchronized (cache-entry)
        (setf
         (worker-channel cache-entry) nil
         (worker cache-entry) nil)))))

;; TODO: Replace channel with semaphore?

(defmethod (setf content) (value (file file))
  (let ((entry (cache-entry file)))
    (sera:synchronized (entry)
      (unless (worker entry)
        (setf (worker-channel entry) (make-channel))
        (setf (worker entry)
              (bt:make-thread (make-worker file entry)
                              :name "NFiles worker")))
      (setf (cached-value entry) value)
      ;; Signal the worker with an arbitrary value:
      (calispel:! (worker-channel entry) t))))



(defmacro with-file-content ((content file) &body body)
  "Bind CONTENT to FILE's content in BODY.
The new value of CONTENT is saved to FILE on exit."
  `(let ((,content (content ,file)))
     (unwind-protect (progn ,@body)
       (setf (content file) ,content))))

;; TODO: Useless?
(defmacro with-existing-content ((content file) &body body) ; TODO: Add default keyword parameter?
  "Bind CONTENT to FILE's content in BODY.
If CONTENT is NIL, BODY is ignored.
Unlike with `with-file-content', the file is not written to."
  `(alex:when-let ((,content (content ,file)))
     ,@body))
