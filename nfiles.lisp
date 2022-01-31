;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nfiles)

;; TODO: Handle write errors (e.g. when trying to write to /root).

(defclass* profile ()
  ((name "default"
         :type string
         :documentation "The name of the profile to refer it with."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "This is the default profile.
Subclass this profile to make your own, possibly carrying more data.

`file' path expansion is specialized against its `profile' slot throught the
`resolve' method."))

(defvar *profile-index* (tg:make-weak-hash-table :weakness :key :test 'equal)
  "Set of all `profile's objects.
It's a weak hash table to that garbage-collected nfiles are automatically
removed.")

(defmethod initialize-instance :after ((profile profile) &key)
  (setf (gethash profile *profile-index*) profile))

(defclass* read-only-profile (profile)
  ()
  (:export-class-name-p t)
  (:documentation "In this profile, files by default don't get written to."))

(defclass* virtual-profile (read-only-profile)
    ()
    (:export-class-name-p t)
    (:documentation "In this profile, files are neither read nor written to by default."))

(export-always 'find-profile)
(defgeneric find-profile (designator)
  (:documentation "Return a profile matching DESIGNATOR among all instantiated
`profile'."))

(defmethod find-profile ((name string))
  (loop for profile being the hash-key of *profile-index*
        when (string= name (name profile))
          return profile))

(defmethod find-profile ((p profile))
  (gethash p *profile-index*))

(export-always 'all-profiles)
(defun all-profiles (&rest packages)
  (flet ((ensure-package (designator)   ; TODO: Factor this.
           (if (packagep designator)
               designator
               (find-package designator))))
    (let ((result '())
          (packages (mapcar #'ensure-package packages)))
      (maphash (lambda (file value)
                 (declare (ignore value))
                 (when (or (not packages)
                           (find (sera:class-name-of file) packages))
                   (push file result)))
               *profile-index*)
      result)))

(export-always '*default-profile*)
(defvar *default-profile* (make-instance 'profile))

(defclass* file ()
  ((path                                ; TODO: Rename to `base-path'?
    #p""
    :type pathname
    :export t
    :reader t
    :writer nil
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
   (timeout                             ; Unexport? Make global option?
    0.1
    :type real
    :documentation "Time in seconds to wait for other write requests.")
   (writable                            ; TODO: Remove?
    t
    :type boolean
    :documentation "If T, the file is writable.
If NIL, then attempting to write to the file raises an error."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "The main object to manipulate and subclass.
The `profile' slot can be used to drive the specializations of multiple `file' methods.
See `resolve', `serialize', `etc'.

The `name' slot can be used to refer to `file' objects in a human-readable fashion."))

(defclass* lisp-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "Like regular `file' but assume a `.lisp' extension, even if
not provided."))

(defclass* config-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "Like regular `file' but set directory to `uiop:xdg-config-home'."))

(defclass* cache-file (file)
    ()
    (:export-class-name-p t)
    (:documentation "Like regular `file' but set directory to `uiop:xdg-cache-home'"))

(defclass* data-file (file)
    ()
    (:export-class-name-p t)
    (:documentation "Like regular `file' but set directory to `uiop:xdg-data-home'"))

(defclass* gpg-file (file)
    ()
    (:export-class-name-p t)
    (:documentation "The file is automatically crypted and decrypted using the
specified recipient key.
The '.gpg' extension is automatically added if missing."))

(defclass* read-only-file (file)
    ()
    (:export-class-name-p t)
    (:documentation "File that's not written to on change.
Note that the file's `content' can still be modified in-memory."))

(defclass* virtual-file (read-only-file)
    ()
    (:export-class-name-p t)
    (:documentation "File that's not read nor written to.  It's meant to handle data
in-memory.

Note that if multiple `virtual-file' expand to the same path, they share the
same content.

To disable content-sharing for specific `file', have their `resolve' method
return `uiop:*nil-pathname*'."))

(defvar *index* (tg:make-weak-hash-table :weakness :key :test 'equal)
  "Set of all `file's objects.
It's a weak hash table to that garbage-collected files are automatically
removed.")

(export-always 'all-files)
(defun all-files (&rest packages)
  (flet ((ensure-package (designator)
           (if (packagep designator)
               designator
               (find-package designator))))
    (let ((result '())
          (packages (mapcar #'ensure-package packages)))
      (maphash (lambda (file value)
                 (declare (ignore value))
                 (when (or (not packages)
                           (find (sera:class-name-of file) packages))
                   (push file result)))
               *index*)
      result)))

(defmethod initialize-instance :after ((file file) &key (path (error "Path required.")))
  (setf (slot-value file 'path) (uiop:ensure-pathname path))
  (setf (gethash file *index*) file))

;; TODO: Useless?
;; (defmethod find-file ((name string))
;;   (loop for profile being the hash-key of *profile-index*
;;         when (string= name (name profile))
;;           return profile))

(export-always 'resolve)
(defgeneric resolve (profile file)
  (:method ((profile profile) (file file))
    (path file))
  (:documentation "Return the final expanded path foe `file' depending on its `profile'.
This method is meant to be specialized against the user-defined `profile's and `file's.
See `expand' for a convenience wrapper."))

(defmethod resolve :around ((profile profile) (file file))
  "Clean up the result before returning it."
  ;; TODO: Make sure native pathnames like "[" are not problematic.
  (uiop:ensure-pathname (call-next-method)
                        :truenamize t))

(defmethod resolve ((profile profile) (file lisp-file))
  (make-pathname :defaults (call-next-method) :type "lisp"))

(defmethod resolve ((profile profile) (file gpg-file))
  (let ((path (call-next-method)))
    (if (string-equal (pathname-type* path) "gpg")
        file
        (make-pathname :defaults path :type "gpg"
                       :name (alex:if-let ((ext (pathname-type* path)))
                               (uiop:strcat (pathname-name path) "." ext)
                               (pathname-name path))))))

(defmethod resolve ((profile profile) (file config-file))
  (uiop:xdg-config-home (call-next-method)))

(defmethod resolve ((profile profile) (file cache-file))
  (uiop:xdg-cache-home (call-next-method)))

(defmethod resolve ((profile profile) (file data-file))
  (uiop:xdg-data-home (call-next-method)))

;; TODO: Does it make sense to serialize / deserialize over a profile?
;; Yes, because this is where we chose to not touch the filesystem, or to be read-only.
;; Now that we have `read' and `write', maybe not so much.
;; But who knows... Plus for consistency with other methods, we can keep the same parameters.

(export-always 'deserialize)
(defgeneric deserialize (profile file raw-content)
  (:method ((profile profile) (file file) raw-content)
    raw-content)
  (:documentation "Transform raw-content into a useful form ready to be
  manipulated on the Lisp side.
See `serialize' for the reverse action."))

(defmethod deserialize :around ((profile profile) (file file) content)
  "Don't try deserialize if there is no file."
  (unless (nil-pathname-p (expand file))
    (call-next-method)))

(defmethod deserialize ((profile profile) (file lisp-file) content)
  (read-from-string (call-next-method)))

(defmethod deserialize ((profile profile) (file virtual-file) content)
  nil)

(defmethod deserialize ((profile virtual-profile) (file file) content)
  nil)

;; TODO: Can serialization methods be compounded?
(export-always 'serialize)
(defgeneric serialize (profile file)
  (:method ((profile profile) (file file))
      (princ-to-string (content file)))
  (:documentation "Transform `file' content into a string meant to be persisted
to a file.

See `deserialize' for the reverse action."))

(defmethod serialize :around ((profile profile) (file file))
  (unless (nil-pathname-p (expand file))
    (call-next-method)))

(defmethod serialize ((profile profile) (file lisp-file))
  (write-to-string (content file)))

(defmethod serialize ((profile profile) (file read-only-file))
  nil)

(defmethod serialize ((profile read-only-profile) (file file))
  nil)

;; TODO: Add support for streams.
(export-always 'write-file)
(defgeneric write-file (profile file)
  (:documentation "Persist FILE to disk.
See `read-file' for the reverse action."))

(defmethod write-file :around ((profile profile) (file file))
  (unless (nil-pathname-p (expand file))
    (call-next-method)))

(defmethod write-file ((profile profile) (file file))
  "Write the result of `serialize' to the `file' path."
  ;; TODO: Handle .gpg files.
  (let ((destination (expand file)))
    ;; TODO: Preserve permissions.
    (uiop:with-staging-pathname (destination)
      (alex:write-string-into-file (serialize profile file) destination
                                   :if-exists :supersede))))

(defmethod write-file ((profile profile) (file gpg-file))
  "Crypt to FILE with GPG.
See `*gpg-default-recipient*'."
  ;; TODO: Use (with-gpg-file).
  (nfiles/gpg::call-with-gpg-file
   (expand file)
   '(:direction :output)
   (lambda (stream)
     (write-string
      (serialize profile file)
      stream))))

(defmethod write-file ((profile profile) (file read-only-file))
  "Don't write anything for `read-only-file'."
  nil)

(defmethod write-file ((profile read-only-profile) (file file))
  "Don't write anything when using the `read-only-profile'."
  nil)

(defun backup (path)
  (let* ((path (uiop:ensure-pathname path :truename t))
         (temp-path
           (uiop:with-temporary-file (:prefix (uiop:strcat (pathname-name path) "-backup-")
                                      :suffix ""
                                      :type (pathname-type path)
                                      :directory (uiop:pathname-directory-pathname path)
                                      :keep t
                                      :pathname temp-path)
             temp-path)))
    (uiop:rename-file-overwriting-target path temp-path)))

(export-always 'read-file)
(defgeneric read-file (profile file)
  (:documentation "Load FILE from disk.
See `write-file' for the reverse action."))

(defmethod read-file :around ((profile profile) (file file))
  "Don't try to load the file if its expanded path is nil."
  (unless (nil-pathname-p (expand file))
    (let ((path (expand file)))
      (when (uiop:file-exists-p path)
        (handler-case
            (deserialize profile file
                         (call-next-method))
          (t ()
            ;; TODO: Add (optional) restart?
            (backup path)
            nil))))))

(defmethod read-file ((profile profile) (file file))
  "Load FILE then return the result of the call to `deserialize' on it.
On failure, create a backup of the file."
  (alexandria:read-file-into-string (expand file)))

(defmethod read-file ((profile profile) (file gpg-file))
  "Decrypt FILE with GPG.
See `*gpg-default-recipient*'."
  ;; TODO: Use (with-gpg-file).
  (nfiles/gpg::call-with-gpg-file
   (expand file)
   '(:direction :input)
   (lambda (stream)
     (alex:read-stream-content-into-string stream))))

(defmethod read-file ((profile profile) (file virtual-file))
  "Don't load anything for virtual files."
  nil)

(defmethod read-file ((profile virtual-profile) (file file))
  "Don't load anything when using the `virtual-profile'."
  nil)

(export-always 'expand)
(-> expand (file) (values pathname &optional))
(defun expand (file)
  "Return the pathname corresponding to FILE and its `profile'.
It's a convenience wrapper around `resolve' (to avoid specifying the `profile')."
  (the (values pathname &optional)
       (resolve (profile file) file)))


;; We need a cache system to avoid reading a file that's already in memory (from
;; another data file).

;; We also don't want to needlessly duplicate the content in memory.

;; TODO: How do we garbage collect cache entries?  We can call `clear-cache'.

(defclass* cache-entry ()                ; TODO: Rename?
  ((source-file
    (error "Source file must be given.")
    :type file
    :documentation "This is the `file' object that was used to instantiate the
entry's `cached-value'. ")
   (cached-value                        ; TODO: Rename to `content'?
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
          (read-file (profile (source-file entry)) (source-file entry)))))

(defvar *cache* (sera:dict)
  "Internal `*cache*' associating expanded paths with a dedicated `cache-entry'.")

(defun clear-cache ()                  ; TODO: Export?
  (clrhash *cache*))

(defun cache-entry (file)
  "Files that expand to `uiop:*nil-pathname*' have their own cache entry."
  (sera:synchronized (*cache*)
    (alexandria:ensure-gethash (let ((path (expand file)))
                                 (if (nil-pathname-p path)
                                     file
                                     path))
                               *cache*
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
                       (write-file (profile file) file)
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
                              :initial-bindings `((*default-pathname-defaults* . ,*default-pathname-defaults*))
                              :name "NFiles worker"))))))

(export-always 'with-file-content)
(defmacro with-file-content ((content file) &body body)
  "Bind CONTENT to FILE's content in BODY.
The new value of CONTENT is saved to FILE on exit."
  `(let ((,content (content ,file)))
     (unwind-protect (progn ,@body)
       (setf (content ,file) ,content))))

;; TODO: Useless?
(defmacro with-existing-content ((content file) &body body) ; TODO: Add default keyword parameter?
  "Bind CONTENT to FILE's content in BODY.
If CONTENT is NIL, BODY is ignored.
Unlike with `with-file-content', the file is not written to."
  `(alex:when-let ((,content (content ,file)))
     ,@body))
