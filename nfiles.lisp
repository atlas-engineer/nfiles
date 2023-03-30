;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nfiles)

(define-class profile ()
  ((name "default"
         :type string
         :documentation "The name of the profile to refer it with."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "This is the default profile.
Subclass this profile to make your own, possibly carrying more data.

`file' path expansion is specialized against its `profile' slot through the
`resolve' method."))

(define-class read-only-profile (profile)
  ()
  (:export-class-name-p t)
  (:documentation "In this profile, files by default don't get written to."))

(define-class virtual-profile (read-only-profile)
    ()
    (:export-class-name-p t)
    (:documentation "In this profile, files are neither read nor written to by default.
Their path expands to #p\"\".
If you want to know where a `file' would have expanded in another profile, use `resolve'."))

(export-always '*default-profile*)
(defvar *default-profile* (make-instance 'profile)
  "The fallback profile for files not having overriden one.")

(define-class file ()
  ((base-path
    #p"." ; Current directory by default, so that path merging does the right thing.
    :type pathname
    :export t
    :initarg nil
    :reader t
    :writer nil
    :documentation "The pathname used by the `resolve' method to yield the final path.")
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
   (read-handler
    #'identity
    :type trivial-types:function-designator
    :documentation "Function of one argument, the condition that may be raised
in the reader thread.")
   (write-handler
    #'identity
    :type trivial-types:function-designator
    :documentation "Function of one argument, the condition that may be raised
in the writer thread.")
   (on-external-modification
    'ask
    :type symbol
    :documentation "Whether to reload or overwrite the file if it was modified
since it was last loaded.")
   (on-deserialization-error
    'ask
    :type symbol
    :documentation "What to do on deserialization error.
The offending file may be backed up with the `backup' function.
Or it may simply be `delete'd.
`ask' leaves the condition unhandled, so unless you handle it, it will prompt
the debugger with the other options.")
   (on-read-error
    'ask
    :type symbol
    :documentation "What to do on file read error.
See `on-deserialization-error' for the meaning of the different actions."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "The main object to manipulate and subclass.
The `profile' slot can be used to drive the specializations of multiple `file'
methods. See `resolve', `serialize', etc.

The `name' slot can be used to refer to `file' objects in a human-readable fashion."))

(defun try-restart (name &rest arguments)
  (alex:when-let ((restart (find-restart name)))
    (apply #'invoke-restart restart arguments)))

(macrolet ((define-restarter (name args)
             `(progn
                (export-always ',name)
                (defun ,name (,@args &optional condition)
                  (declare (ignore condition))
                  ,(format
                    nil
                    "Transfer control to a restart named ~a, or return NIL if none exists."
                    name)
                  (try-restart ',name ,@args)))))
  (define-restarter ask ())
  (define-restarter reload ())
  (define-restarter overwrite ())
  (define-restarter backup ())
  (define-restarter delete ())
  (define-restarter ignore-checksum ())
  (define-restarter discard ()))

(define-class lisp-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "Like regular `file' but assume a `.lisp' extension, even if
not provided."))

(define-class config-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "Like regular `file' but set directory to `uiop:xdg-config-home'."))

(define-class cache-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "Like regular `file' but set directory to `uiop:xdg-cache-home'"))

(define-class data-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "Like regular `file' but set directory to `uiop:xdg-data-home'"))

(define-class runtime-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "Like regular `file' but set directory to `uiop:xdg-runtime-dir'"))

(define-class gpg-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "If the resolved path has the GPG type (extension), the file is
automatically encrypted and decrypted using the specified recipient key.

See `nfiles/gpg:*gpg-default-recipient*'."))

(define-class gpg-lisp-file (gpg-file lisp-file)
  ()
  (:export-class-name-p t)
  (:documentation "If the resolved path has the GPG type (extension), the file is
automatically encrypted and decrypted using the specified recipient key.

The '.lisp' extension is automatically added if missing."))

(define-class read-only-file (file)
  ()
  (:export-class-name-p t)
  (:documentation "File that's not written to on change.
Note that the file's `content' can still be modified in-memory."))

(define-class virtual-file (read-only-file)
  ()
  (:export-class-name-p t)
  (:documentation "File that's not read nor written to.

Virtual files are useful when you only need dynamic path expansion, while the
read-and-write business is handled by a third party.

You can nonetheless store data in-memory if needed.

Note that if multiple `virtual-file's expand to the same path, they share the
same content.

To disable content-sharing for a specific `file', their `resolve' method should
return `uiop:*nil-pathname*'."))

(define-class remote-file (file)
  ((url
    (quri:uri "")
    :type quri:uri
    :documentation "URL from where to download the file.
If empty, then this behaves just like a regular `file'.")
   (url-content
    ""
    :type string
    :documentation "The content of the downloaded file.
This is useful if you want to maintain both the raw content and the deserialized
content.")
   (last-update
    (get-universal-time)                ; TODO: 0?
    :type unsigned-byte
    :reader t
    :documentation "Last time the file was downloaded.")
   (update-interval
    0
    :type unsigned-byte
    :documentation "Re-download the file after this amount of seconds has
elapsed since last update.
If 0, disable automatic re-download.")
   (checksum
    ""
    :type string
    :documentation "If non-empty, the `check' method is called on download against the resulting data.
If it does not match the `checksum', raise an error.
This probably only makes sense for immutable data, thus `update-interval' ought to be 0.")
   (on-invalid-checksum
    'ask
    :type symbol
    :documentation "What to do when the downloaded content does not match `checksum'.")
   (on-fetch-error
    'ask ; TODO: Can't put `retry' here, lest it would loop.  What else?
    :type symbol
    :documentation "What to do when the file download failed."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "File which can be found on a remote system or online.

If the local file is not found, then an attempt is made at downloading the file
using `fetch'."))

(defvar *index* (tg:make-weak-hash-table :weakness :key :test 'equal)
  "Set of all `file' objects.
It's a weak hash table so that garbage-collected files are automatically
removed.")

(export-always 'all-files)
(defun all-files (&rest packages)
  "Return all instantiated `file' objects belonging to PACKAGES.
If no package is specified, return all `file' objects."
  (flet ((ensure-package (designator)
           (if (packagep designator)
               designator
               (find-package designator))))
    (let ((result '())
          (packages (mapcar #'ensure-package packages)))
      (maphash (lambda (file value)
                 (declare (ignore value))
                 (when (or (not packages)
                           (find (symbol-package (sera:class-name-of file)) packages))
                   (push file result)))
               *index*)
      result)))

(defun expand-user-home (string)
  (let* ((path (if (pathnamep string) string
                   (uiop:parse-native-namestring string)))
         (dir (pathname-directory path)))
    (if (or (and (eq (first dir) :relative)
                 (string= (second dir) "~"))
            (and (eq (first dir) :absolute)
                 (eq (second dir) :home)))
        (make-pathname :directory (append (pathname-directory (user-homedir-pathname))
                                          (rest (rest dir)))
                       :name (pathname-name path)
                       :type (pathname-type path))
        path)))

(defmethod initialize-instance :after ((file file) &key base-path)
  (when base-path
    (setf (slot-value file 'base-path) (uiop:ensure-pathname (expand-user-home base-path))))
  (setf (gethash file *index*) file))

(export-always 'resolve)
(defgeneric resolve (profile file)
  (:method ((profile profile) (file file))
    (base-path file))
  (:documentation "Return the final expanded path for `file' depending on its `profile'.
This method is meant to be specialized against the user-defined `profile's and `file's.
See `expand' for a convenience wrapper."))

(defmethod resolve :around ((profile profile) (file file))
  "Clean up the result before returning it."
  (let ((path (call-next-method)))
    (if (nil-pathname-p path)
        path
        (uiop:ensure-pathname path
                              :truenamize t))))

(defmethod resolve ((profile profile) (file lisp-file))
  "Append the '.lisp' extension if not already present."
  (ensure-type (call-next-method) "lisp"))

(defmethod resolve ((profile profile) (file gpg-lisp-file))
  "Ensure the extension is .lisp or .lisp.gpg."
  (let ((path (base-path file)))
    (if (string-equal "gpg" (pathname-type path))
        (if (string-equal "lisp" (pathname-type (pathname-name path)))
            path
            (ensure-type (ensure-type (pathname-name path) "lisp") "gpg"))
        (call-next-method))))

(defmethod resolve ((profile virtual-profile) (file file))
  "Virtual files are in-memory only."
  ;; Necessary so that virtual files don't use the cache.
  #p"")

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

(defmethod resolve ((profile profile) (file runtime-file))
  (maybe-xdg #'uiop:xdg-runtime-dir (call-next-method)))

(defun auto-restarter (restarter)
  "Return a function that calls RESTARTER.
RESTARTER is a function that invokes a given restart if valid.
See `ask' for instance."
  (lambda (c)
    (declare (ignore c))
    (funcall restarter)))

(export-always 'deserialize)
(defgeneric deserialize (profile file stream &key &allow-other-keys)
  (:method ((profile profile) (file file) stream &key)
    stream)
  (:documentation "Transform STREAM into a useful form
ready to be manipulated on the Lisp side.
The STREAM is provided by `read-file'.

See `serialize' for the reverse action."))

(defmethod deserialize :around ((profile profile) (file file) stream &key)
  "Handle errors gracefully.  See `on-deserialization-error'."
  (declare (ignore stream))
  (handler-bind ((error (auto-restarter (on-deserialization-error file))))
    (let ((result (call-next-method)))
      (if (streamp result)
          (alex:read-stream-content-into-string result)
          result))))

(defmethod deserialize ((profile profile) (file lisp-file) stream &key)
  (declare (ignore stream))
  (read (call-next-method)))

(defmethod deserialize :around ((profile profile) (file virtual-file) stream &key)
  ;; Must be an `:around' method to overrule other possible `:around' specialization.
  (declare (ignore stream))
  nil)

(defmethod deserialize :around ((profile virtual-profile) (file file) stream &key)
  ;; Must be an `:around' method to overrule other possible `:around' specialization.
  (declare (ignore stream))
  nil)

(export-always 'serialize)
(defgeneric serialize (profile file stream &key &allow-other-keys)
  (:method ((profile profile) (file file) stream &key)
    (princ (content file) stream))
  (:documentation "Transform `file' content in a form meant to be persisted to a file.
For the actual writing to disk, see `write-file'.

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

(defvar *content-override* nil          ; FIXME: Hack, can we do better?
  "Implementation detail.
This is used by `remote-file' to write content to disk when the `cache-entry' is
initialized, without looping indefinitely.")
(defvar *cache-entry-override* nil          ; FIXME: Hack, can we do better?
  "Implementation detail.
This is used by `remote-file' to write content to disk when the `cache-entry' is
initialized, without looping indefinitely.")

(export-always 'write-file)
(defgeneric write-file (profile file &key destination &allow-other-keys)
  (:documentation "Persist FILE to disk.
DESTINATION is set by default to a staged pathname (using
`uiop:with-staging-pathname') which is renamed to the final name (the result of
`expand' on FILE) if everything went well.
This guarantees that on error the original file is left untouched.

The specialized method is in charge for calling `serialize'.

See `read-file' for the reverse action."))

(defmethod write-file :around ((profile profile) (file file) &key destination)
  "Perform some checks before performing the actual write:
- Preserve permissions.
- If FILE expands to `uiop:*nil-pathname*' or a directory, do nothing."
  (declare (ignore destination))
  (let ((path (expand file)))
    (unless (or (nil-pathname-p path)
                (directory-pathname-p path))
      (let ((entry (or *cache-entry-override* (cache-entry file))))
        ;; It's important to fetch the entry before we write to avoid a cache miss.
        (let* ((destination path)
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
            (call-next-method profile file :destination destination))
          (when exists?
            (setf
             (permissions path) permissions
             (file-user path) user
             (file-group path) group)))
        (sera:synchronized (entry)
          (setf (last-update entry) (get-universal-time)))))))

(defmethod write-file ((profile profile) (file file) &key destination)
  "Write the result of `serialize' to the `file' path."
  (with-open-file (stream destination :direction :output :if-exists :supersede)
    (serialize profile file stream)))

(defun gpg-path-p (file)
  (string-equal "gpg" (pathname-type (expand file))))

(defmethod write-file ((profile profile) (file gpg-file) &key destination)
  "Crypt to FILE with GPG.
See `*gpg-default-recipient*'."
  (if (gpg-path-p file)
      (nfiles/gpg:with-gpg-file (stream destination :direction :output)
        (serialize profile file stream))
      (call-next-method profile file :destination destination)))

(defmethod write-file ((profile profile) (file read-only-file) &key destination)
  "Don't write anything for `read-only-file'."
  (declare (ignore destination))
  nil)

(defmethod write-file ((profile read-only-profile) (file file) &key destination)
  "Don't write anything when using the `read-only-profile'."
  (declare (ignore destination))
  nil)

(defun back-up (path)
  "Rename PATH to a file of the same name with a unique suffix appended.
The file is guaranteed to not conflict with any existing file."
  (let* ((path (uiop:ensure-pathname path :truename t))
         (temp-path
           (uiop:with-temporary-file (:prefix (uiop:strcat (pathname-name path) "-backup-")
                                      :suffix ""
                                      :type (pathname-type path)
                                      :directory (uiop:pathname-directory-pathname path)
                                      :keep t
                                      :pathname temp-path)
             temp-path)))
    (uiop:rename-file-overwriting-target path temp-path)
    temp-path))

(export-always 'fetch)
(defgeneric fetch (profile file  &key &allow-other-keys)
  (:documentation "Download the FILE `url' and return the result as a string."))

(export-always 'check)
(defgeneric check (profile file content &key &allow-other-keys)
  (:documentation "Check CONTENT when FILE's `checksum' is non-empty.
This is meant to return a string which is then automatically compared to `checksum'."))

(defmethod check :around ((profile profile) (file file) content &key)
  (if (uiop:emptyp (checksum file))
      content
      (let ((wrong-checksum ""))
        (restart-case (handler-bind ((invalid-checksum (auto-restarter (on-invalid-checksum file))))
                        (let ((checksum (call-next-method profile file content)))
                          (setf wrong-checksum checksum)
                          (if (string= checksum (checksum file))
                              content
                              (error 'invalid-checksum :path (expand file)
                                                       :wanted-checksum (checksum file)
                                                       :wrong-checksum checksum))))
          (ignore-checksum ()
            :report "Proceed regardless of checksum."
            (warn "Bad checksum ~s, expected ~s"
                  wrong-checksum (checksum file))
            content)
          (discard ()
            :report "Abort reading the file."
            (error 'read-error))))))

(export-always 'read-file)
(defgeneric read-file (profile file &key &allow-other-keys)
  (:documentation "Load FILE by calling `deserialize' on a stream opened on the file.
The specialized method is in charge for calling `deserialize'.
See `write-file' for the reverse action."))

(defmethod read-file :around ((profile profile) (file file) &key)
  "Don't try to load the file if it does not exist."
  (let ((path (expand file)))
    (when (uiop:file-exists-p path)
      (restart-case (handler-bind ((error (auto-restarter (on-read-error file))))
                      (call-next-method))
        (backup ()
          :report "Backup the file contents to a newly generated file."
          (back-up path)
          ;; Return `nil' so that `content' also returns `nil' on error.
          nil)
        (delete ()
          :report "Delete the file."
          (uiop:delete-file-if-exists path)
          nil)))))

(-> url-empty-p ((or quri:uri string null)) boolean)
(defun url-empty-p (url)
  "Small convenience function to check whether the given URL is empty."
  (the (values boolean &optional)
       (uiop:emptyp (if (quri:uri-p url) (quri:render-uri url) url))))

(defun updatable-p (file)
  (when (typep file 'remote-file)
    (or
     (when (< 0 (update-interval file))
       (< (update-interval file)
          (- (get-universal-time) (last-update file))))
     (let ((path (expand file)))
       (or
        (not (uiop:file-exists-p path))
        (when (< 0 (update-interval file))
          (< (update-interval file)
             (- (get-universal-time) (uiop:safe-file-write-date path)))))))))

(defmethod read-file :around ((profile profile) (file remote-file) &key force-update skip-update)
  ;; Must be an `:around' method to overrule the `nfiles:file' specialization
  ;; which does file existence check.
  "Try to download the file from its `url' if it does not exist.

If file is already on disk and younger than `update-interval', call next
`read-file' method instead of fetching online."
  (when (and force-update skip-update)
    (warn "Both FORCE-UPDATE and SKIP-UPDATE specified, opting to force update."))
  (if (and (not skip-update)
           (not (url-empty-p (url file)))
           (or force-update
               (not (exists-p file))
               (updatable-p file)))
      ;; We bind the handler against `T' because some networking library raise non-error conditions.
      (restart-case (handler-bind ((t (auto-restarter (on-fetch-error file))))
                      (let ((content (fetch profile file)))
                        (unless (uiop:emptyp (checksum file))
                          (setf content (check profile file content)))
                        (unless (uiop:emptyp content)
                          (setf (url-content file) content)
                          (setf (slot-value file 'last-update) (get-universal-time))
                          (with-input-from-string (stream content)
                            (deserialize profile file stream)))))
        (retry ()                       ; TODO: Test!
          :report "Try reading the file again."
          (read-file profile file)))

      (call-next-method)))

(defmethod read-file ((profile profile) (file file) &key)
  "Open FILE and call `deserialize' on its content."
  (with-open-file (stream (expand file))
    (deserialize profile file stream)))

(defmethod read-file ((profile profile) (file gpg-file) &key)
  "Decrypt FILE with GPG and return resulting stream.
See `*gpg-default-recipient*'."
  (if (gpg-path-p file)
      (nfiles/gpg:with-gpg-file (stream (expand file))
        (deserialize profile file stream))
      (call-next-method profile file)))

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

(export-always 'exists-p)
(-> exists-p (file) boolean)
(defun exists-p (file)
  "Boolean-ensured `uiop:file-exists-p' wrapper for `file'."
  (the (values boolean &optional)
       (sera:true (uiop:file-exists-p (expand file)))))

;; We need a cache system to avoid reading a file that's already in memory (from
;; another data file).
;; We also don't want to needlessly duplicate the content in memory.
;; TODO: How do we garbage collect cache entries?  We can call `clear-cache'.

(define-class cache-entry ()                ; TODO: Rename?
  ((source-file
    (error "Source file must be given.")
    :type file
    :documentation "This is the `file' object that was used to instantiate the
entry's `cached-value'. ")
   (last-update
    (get-universal-time)
    :type unsigned-byte
    :documentation "The date at which the cache entry was last updated.")
   (cached-value
    nil
    :type t)
   (worker
    nil
    :type (or null bt:thread))
   (worker-notifier
    nil
    :type (or null bt:semaphore))))

(defmacro run-thread (name handler &body body)
  `(bt:make-thread (lambda ()
                     (restart-case (handler-bind ((error ,handler))
                                     ,@body)
                       (forward-condition (&optional c)
                         :report "Delegate handling this error to the code beyond NFiles."
                         c)))
                   :initial-bindings `((*default-pathname-defaults* . ,*default-pathname-defaults*))
                   :name ,name))

(defmethod initialize-instance :after ((entry cache-entry) &key force-update skip-update)
  (setf (cached-value entry)
        (prog1 (run-thread "NFiles reader" (read-handler (source-file entry))
                 (let ((value (read-file (profile (source-file entry)) (source-file entry)
                                         :force-update force-update
                                         :skip-update skip-update)))
                   (when (and (typep (source-file entry) 'remote-file)
                              (not (uiop:file-exists-p (expand (source-file entry)))))
                     ;; If the file content is never set, then the `write-file'
                     ;; method is not called; however we want to persist the
                     ;; file for `remote-file' regardless, so we force a write
                     ;; here.
                     (let ((*content-override* value)
                           (*cache-entry-override* entry))
                       (write-file (profile (source-file entry)) (source-file entry))))
                   value))
          (setf (last-update entry) (get-universal-time)))))

(defvar *cache* (sera:dict)
  "Internal `*cache*' associating expanded paths with a dedicated `cache-entry'.")

(defun clear-cache ()                  ; TODO: Export?
  (clrhash *cache*))

(defun file-key (file)
  (let ((path (expand file)))
    (if (nil-pathname-p path)
        file
        (uiop:native-namestring path))))

(defun cache-entry (file &optional force-read force-update skip-update)
  "Files that expand to `uiop:*nil-pathname*' have their own cache entry."
  (sera:synchronized (*cache*)
    (let* ((path (expand file))
           (key (file-key file))
           ;; This `force-update' settings saves us from calling `updatable-p'
           ;; again in `read-file'.
           (force-update (or force-update (and (not skip-update) (updatable-p file)))))
      (if (and (not (nil-pathname-p path))
               (or force-read
                   force-update
                   (alex:when-let ((entry (gethash key *cache*)))
                     (restart-case (handler-bind ((external-modification (auto-restarter (on-external-modification file))))
                                     (sera:synchronized (entry)
                                       (when (< (last-update entry) (or (uiop:safe-file-write-date path)
                                                                        0))
                                         (error 'external-modification
                                                :path path))))
                       (reload ()
                         t)
                       (overwrite ()
                         (sera:synchronized (entry)
                           (write-cache-entry file entry))
                         nil)))))
          (setf (gethash key *cache*) (make-instance 'cache-entry :source-file file
                                                                  :force-update force-update
                                                                  :skip-update skip-update))
          (alex:ensure-gethash key
                               *cache*
                               (make-instance 'cache-entry :source-file file
                                                           :force-update force-update
                                                           :skip-update skip-update))))))

(export-always 'content)
(-> content (file &key (:force-read boolean)
                  (:force-update boolean)
                  (:skip-update boolean)
                  (:wait-p boolean))
    t)
(defun content (file &key force-read force-update skip-update (wait-p t))
  "Return the content of FILE.
When FORCE-READ is non-nil, the cache is skipped and the file is re-read.
When FORCE-UPDATE is non-nil, the file is re-downloaded if it's a `remote-file'.
When SKIP-UPDATE is non-nil, the file is not re-downloaded if it's present locally.

The read is asynchronous.  By default, `content' waits for the read to finish
before returning the result.  But if `wait-p' is nil, it returns directly
with (VALUES NIL THREAD) if the reading THREAD is not done yet."
  (or *content-override*
      (let ((entry (cache-entry file force-read force-update skip-update)))
        ;; WARNING: We don't lock `entry' when joining thread to avoid a dead lock.
        (let ((value (sera:synchronized (entry)
                       (cached-value entry))))
          (if (and (bt:threadp value)
                   (or wait-p (not (bt:thread-alive-p value))))
              ;; Thread may be aborted, so we wrap with  `ignore-errors'.
              (multiple-value-bind (result error)
                  (ignore-errors (bt:join-thread value))
                (if (or error
                        (typep result 'condition))
                    (progn
                      (sera:synchronized (*cache*)
                        (remhash (file-key file) *cache*))
                      (values nil (or error result)))
                    (progn
                      (sera:synchronized (entry)
                        (setf (cached-value entry) result))
                      (values result nil))))

              (sera:synchronized (entry)
                (if (bt:threadp value)
                    (values nil value)
                    (values value nil))))))))

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

(defun worker-write (file cache-entry)
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
    (maybe-write)))

(defun write-cache-entry (file entry)
  (unless (worker-notifier entry)
    (setf (worker-notifier entry) (bt:make-semaphore :name "NFiles notifier")))
  (bt:signal-semaphore (worker-notifier entry))
  (unless (worker entry)
    (setf (worker entry)
          (run-thread "NFiles worker" (write-handler file)
            (worker-write file entry))))
  (worker entry))

(-> (setf content) (t file) t)
(defun (setf content) (value file)
  "Set FILE content to VALUE and persist change to disk.
While the content of the FILE object is updated instantly, the file is persisted
in the background.

This is a user-facing function, you should probably not try to call it from one
of the specialized methods as this would set the logic upside down.

Return a `bt:thread' object.  Call `bt:join-thread' on it to know when it's done
writing the file."
  (let ((entry (cache-entry file)))
    (sera:synchronized (entry)
      (setf (cached-value entry) value)
      (write-cache-entry file entry))))

(export-always 'with-paths)
(defmacro with-paths (bindings &body body)
  "Bind let-style BINDINGS to `file' path expansions, then run BODY if all these
paths are non-nil after `nil-pathname-p'."
  (alex:with-gensyms (path)
    `(sera:and-let* (,@(mapcar
                        (lambda (binding)
                          (let ((sym (first binding))
                                (file (second binding)))
                            (list sym `(let ((,path (nfiles:expand ,file)))
                                         (unless (nil-pathname-p ,path) ,path)) )))
                        bindings))
       ,@body)))

(export-always 'with-file-content)
(defmacro with-file-content ((content file &key default) &body body)
  "Bind CONTENT to FILE's content in BODY.
In case there's no content, bind CONTENT to DEFAULT.
The new value of CONTENT is saved to FILE on exit."
  `(let ((,content (or (content ,file) ,default)))
     (prog1 (progn ,@body)
       (setf (content ,file) ,content))))
