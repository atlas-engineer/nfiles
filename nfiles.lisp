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

(-> find-profile ((or null string)) (or null profile))
(export-always 'find-profile)
(defun find-profile (name)
  "Return first `profile' object matching NAME."
  (when name
    (loop for profile being the hash-key of *profile-index*
          when (string= name (name profile))
            return profile)))

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
  ((base-path
    #p""
    :type pathname
    :export t
    :initarg nil
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
    :documentation "Name used to identify the object in a human-readable manner."))
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

(defmethod initialize-instance :after ((file file) &key base-path)
  (when base-path
    (setf (slot-value file 'base-path) (uiop:ensure-pathname base-path)))
  (setf (gethash file *index*) file))

;; TODO: Useless?
;; (defmethod find-file ((name string))
;;   (loop for profile being the hash-key of *profile-index*
;;         when (string= name (name profile))
;;           return profile))

(export-always 'resolve)
(defgeneric resolve (profile file)
  (:method ((profile profile) (file file))
    (base-path file))
  (:documentation "Return the final expanded path for `file' depending on its `profile'.
This method is meant to be specialized against the user-defined `profile's and `file's.
See `expand' for a convenience wrapper."))

(defmethod resolve :around ((profile profile) (file file))
  "Clean up the result before returning it."
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

(defun maybe-xdg (xdg-fun path)
  (if (uiop:absolute-pathname-p path)
      path
      (funcall xdg-fun path)))

(defmethod resolve ((profile profile) (file config-file))
  (maybe-xdg #'uiop:xdg-config-home (call-next-method)))

(defmethod resolve ((profile profile) (file cache-file))
  (maybe-xdg #'uiop:xdg-cache-home (call-next-method)))

(defmethod resolve ((profile profile) (file data-file))
  (maybe-xdg #'uiop:xdg-data-home (call-next-method)))

(export-always 'deserialize)
(defgeneric deserialize (profile file raw-content &key &allow-other-keys)
  (:method ((profile profile) (file file) raw-content &key)
    raw-content)
  (:documentation "Transform RAW-CONTENT (a stream) into a useful form
ready to be manipulated on the Lisp side.
See `serialize' for the reverse action."))

(defmethod deserialize :around ((profile profile) (file file) content &key)
  "Don't try deserialize if there is no file."
  (declare (ignore content))
  (unless (nil-pathname-p (expand file))
    (let ((result (call-next-method)))
      (if (streamp result)
          (alex:read-stream-content-into-string result)
          result))))

(defmethod deserialize ((profile profile) (file lisp-file) content &key)
  (declare (ignore content))
  (read (call-next-method)))

(defmethod deserialize ((profile profile) (file virtual-file) content &key)
  (declare (ignore content))
  nil)

(defmethod deserialize ((profile virtual-profile) (file file) content &key)
  (declare (ignore content))
  nil)

;; TODO: Can serialization methods be compounded?
(export-always 'serialize)
(defgeneric serialize (profile file stream &key &allow-other-keys)
  (:method ((profile profile) (file file) stream &key)
    (princ (content file) stream))
  (:documentation "Transform `file' content meant to
be persisted to a file.

See `deserialize' for the reverse action."))

(defmethod serialize :around ((profile profile) (file file) stream &key)
  (declare (ignore stream))
  (unless (nil-pathname-p (expand file))
    (call-next-method)))

(defmethod serialize ((profile profile) (file lisp-file) stream &key)
  (write (content file) :stream stream))

(defmethod serialize ((profile profile) (file read-only-file) stream &key)
  (declare (ignore stream))
  nil)

(defmethod serialize ((profile read-only-profile) (file file) stream &key)
  (declare (ignore stream))
  nil)

(export-always 'write-file)
(defgeneric write-file (profile file &key &allow-other-keys)
  (:documentation "Persist FILE to disk.
See `read-file' for the reverse action."))

(defmethod write-file :around ((profile profile) (file file) &key)
  (unless (nil-pathname-p (expand file))
    (call-next-method)))

(defmethod write-file ((profile profile) (file file) &key)
  "Write the result of `serialize' to the `file' path."
  (let* ((path (expand file))
         (destination path)
         (exists? (uiop:file-exists-p path))
         (permissions nil)
         (user nil)
         (group nil))
    (when exists?
      (setf
       permissions (permissions path)
       user (file-user path)
       group (file-group path)))
    (uiop:with-staging-pathname (destination)
      (with-open-file (stream destination :direction :output :if-exists :supersede)
        (serialize profile file stream)))
    (when exists?
      (setf
       (permissions path) permissions
       (file-user path) user
       (file-group path) group))))

(defmethod write-file ((profile profile) (file gpg-file) &key)
  "Crypt to FILE with GPG.
See `*gpg-default-recipient*'."
  (nfiles/gpg:with-gpg-file (stream (expand file) :direction :output)
    (serialize profile file stream)))

(defmethod write-file ((profile profile) (file read-only-file) &key)
  "Don't write anything for `read-only-file'."
  nil)

(defmethod write-file ((profile read-only-profile) (file file) &key)
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
(defgeneric read-file (profile file &key &allow-other-keys)
  (:documentation "Load FILE by calling `deserialize' on a stream opened on the file.
See `write-file' for the reverse action."))

(defmethod read-file :around ((profile profile) (file file) &key)
  "Don't try to load the file if its expanded path is nil.
Close the FILE stream when `deserialize' is done.
On failure, create a backup of the file."
  (unless (nil-pathname-p (expand file))
    (let ((path (expand file)))
      (when (uiop:file-exists-p path)
        (handler-case (call-next-method)
          (t ()
            ;; TODO: Add (optional) restart?
            (backup path)
            nil))))))

(defmethod read-file ((profile profile) (file file) &key)
  "Open FILE and call `deserialize' on its content."
  (with-open-file (stream (expand file))
    (deserialize profile file stream)))

(defmethod read-file ((profile profile) (file gpg-file) &key)
  "Decrypt FILE with GPG and return resulting stream.
See `*gpg-default-recipient*'."
  (nfiles/gpg:with-gpg-file (stream (expand file))
    (deserialize profile file stream)))

(defmethod read-file ((profile profile) (file virtual-file) &key)
  "Don't load anything for virtual files."
  nil)

(defmethod read-file ((profile virtual-profile) (file file) &key)
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
   (last-update
    0
    :type integer
    :documentation "The write date of the file the last time the cache was updated.")
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
        (let ((path (expand (source-file entry))))
          (when (uiop:file-exists-p path)
            (prog1 (read-file (profile (source-file entry)) (source-file entry))
              (setf (last-update entry) (file-write-date path)))))))

(defvar *cache* (sera:dict)
  "Internal `*cache*' associating expanded paths with a dedicated `cache-entry'.")

(defun clear-cache ()                  ; TODO: Export?
  (clrhash *cache*))

(defun cache-entry (file &optional force-read)
  "Files that expand to `uiop:*nil-pathname*' have their own cache entry."
  (sera:synchronized (*cache*)
    (let* ((path (expand file))
           (key (if (nil-pathname-p path)
                    file
                    (uiop:native-namestring path))))
      (if (and (not (nil-pathname-p path))
               (or force-read
                   (alex:when-let ((entry (gethash key *cache*)))
                     (< (last-update entry) (or (uiop:safe-file-write-date path)
                                                0)))))
          (setf (gethash key *cache*) (make-instance 'cache-entry :source-file file))
          (alex:ensure-gethash key
                               *cache*
                               (make-instance 'cache-entry :source-file file))))))

(export-always 'content)
(defgeneric content (file &optional force-read)
  (:documentation "Get and set the content of the file.
This generic function is not meant to be specialized.
See `serialize', `deserialize', `write-file' and `read-file' instead."))

(defmethod content ((file file) &optional force-read)
  "Return the content of FILE.
When FORCE-READ is non-nil, the cache is skipped and the file is re-read."
  (let ((entry (cache-entry file force-read)))
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

(declaim (type real *timeout*))
(defvar *timeout* 0.1
  "Time in seconds to wait for other write requests.")

(defun make-worker (file cache-entry)
  (lambda ()
    (labels ((maybe-write ()
               (let ((write-signaled? (drain-semaphore (worker-notifier cache-entry) *timeout*)))
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
  "Set FILE content to VALUE and persist change to disk.
While the content of the FILE object is updated instantly, the file is persisted
in the background.

Return a `bt:thread' object.  Call `bt:join-thread' on it to know when it's done
writing the file."
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
                              :name "NFiles worker")))
      (worker entry))))

(export-always 'with-file-content)
(defmacro with-file-content ((content file &key default) &body body)
  "Bind CONTENT to FILE's content in BODY.
In case there's no content, bind CONTENT to DEFAULT.
The new value of CONTENT is saved to FILE on exit."
  `(let ((,content (or (content ,file) ,default)))
     (prog1 (progn ,@body)
       (setf (content ,file) ,content))))
