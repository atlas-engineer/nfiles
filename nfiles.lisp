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
  ;; TODO: Call uiop:ensure-pathname in `resolve' instead, `path' should be kept as is.
  ;; TODO: Make sure native pathnames like "[" are not problematic.
  (setf (path file) (uiop:ensure-pathname (path file)
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
     (worker-notifier
      nil
      :type (or null bt:semaphore)))
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

(defun drain-semaphore (semaphore &optional timeout)
  "Decrement the semaphore counter down to 0.
Return the number of decrements, or NIL if there was none."
  (let ((decrement-count nil))
    (labels ((drain ()
               (let ((result (bt:wait-on-semaphore semaphore :timeout timeout)))
                 (when result
                   (unless decrement-count
                     (setf decrement-count 0))
                   (incf decrement-count)
                   (drain)))))
      (drain))
    decrement-count))

(defun make-worker (file cache-entry)
  (lambda ()
    (labels ((maybe-write ()
               (let ((write-signaled? (drain-semaphore (worker-notifier cache-entry) (timeout file))))
                 (if write-signaled?
                     (progn
                       (serialize (profile file) file)
                       (maybe-write))
                     (sera:synchronized (cache-entry)
                       (setf
                        (worker-notifier cache-entry) nil
                        (worker cache-entry) nil))))))
      (maybe-write))))

(defmethod (setf content) (value (file file))
  (let ((entry (cache-entry file)))
    (sera:synchronized (entry)
      (setf (cached-value entry) value)
      (unless (worker-notifier entry)
        (setf (worker-notifier entry) (bt:make-semaphore :name "NFiles notifier")))
      (bt:signal-semaphore (worker-notifier entry))
      (unless (worker entry)
        (setf (worker entry)
              (bt:make-thread (make-worker file entry)
                              :name "NFiles worker"))))))

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
