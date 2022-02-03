;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nfiles)

;; TODO: Make methods that take both pathnames and nfiles?
;; TODO: Define own `path-designator' type?

(export-always 'nil-pathname-p)
(-> nil-pathname-p (pathname-designator) boolean)
(defun nil-pathname-p (pathname)
  "Return non-nil if PATHNAME is `uiop:*nil-pathname*'."
  (the (values boolean &optional)
       (uiop:pathname-equal pathname uiop:*nil-pathname*)))

(export-always 'empty-pathname-p)
(-> empty-pathname-p (t) boolean)
(defun empty-pathname-p (p)
  "Return non-nil if PATHNAME is `uiop:*nil-pathname*', \"\" or NIL."
  (the (values boolean &optional)
       (or
        (null p)
        (equal p "")
        (uiop:pathname-equal p uiop:*nil-pathname*))))

(export-always 'pathname-type*)
(defun pathname-type* (pathname)
  "Like `pathname-type' but return NIL instead of \"\" or :UNSPECIFIC."
  (let ((type (pathname-type pathname)))
    (if (member type '(nil "" :unspecific) :test 'equal)
        nil
        type)))

(export-always 'parent)
(-> parent (pathname-designator) (or pathname-designator null))
(defun parent (path)
  "Return the parent directory of PATH."
  (let ((path (uiop:ensure-pathname path)))
    (the (values (or pathname null) &optional)
         (if (uiop:directory-pathname-p path)
             (uiop:pathname-parent-directory-pathname path)
             (uiop:pathname-directory-pathname path)))))

(alex:define-constant +permissions+
    '((:user-read . 256) (:user-write . 128) (:user-exec . 64) (:group-read . 32)
      (:group-write . 16) (:group-exec . 8) (:other-read . 4) (:other-write . 2)
      (:other-exec . 1) (:set-user-id . 2048) (:set-group-id . 1024) (:sticky . 512))
  :test #'equal)

(export-always 'permissions)
(defmethod permissions (path)
  "Return a list of permissions as per `+permissions+'."
  #+sbcl
  (let ((mode (sb-posix:stat-mode
               (sb-posix:lstat path))))
    (loop :for (name . value) :in +permissions+
          :when (plusp (logand mode value))
            :collect name))
  #-sbcl
  (iolib/os:file-permissions (uiop:native-namestring path)))

(defmethod (setf permissions) (permissions path)
  "Set the PERMISSIONS or PATH as per `+permissions+'."
  #+sbcl
  (sb-posix:chmod path
                  (reduce (lambda (a b)
                            (logior a (rest (assoc b +permissions+))))
                          permissions :initial-value 0))
  #-sbcl
  (setf (iolib/os:file-permissions (uiop:native-namestring path)) permissions))

(export-always 'file-user)
(defmethod file-user (path)
  "Return PATH owner name."
  (file-author path))

(defmethod (setf file-user) (new-user path)
  "Set PATH owner to NEW-USER (a string)."
  #+sbcl
  (sb-posix:chown path
                  (sb-posix:passwd-uid (sb-posix:getpwnam new-user))
                  (sb-posix:stat-gid (sb-posix:lstat path)))
  #-sbcl
  (let ((native-path (uiop:native-namestring path)))
    (iolib/syscalls:chown native-path
                          (nth-value 2 (iolib/syscalls:getpwnam new-user))
                          (iolib/syscalls:stat-gid (iolib/syscalls:lstat
                                                    native-path)))))

(export-always 'file-group)
(defmethod file-group (path)
  "Return PATH group name."
  #+sbcl
  (sb-posix:group-name
   (sb-posix:getgrgid (sb-posix:stat-gid
                       (sb-posix:lstat path))))
  #-sbcl
  (iolib/syscalls:getgrgid (iolib/syscalls:stat-gid
                            (iolib/syscalls:lstat
                             (uiop:native-namestring path)))))

(defmethod (setf file-group) (new-group path)
  "Set PATH group to NEW-GROUP (a string)."
  #+sbcl
  (sb-posix:chown path
                  (sb-posix:stat-uid (sb-posix:lstat path))
                  (sb-posix:group-gid (sb-posix:getgrnam new-group)))
  #-sbcl
  (let ((native-path (uiop:native-namestring path)))
    (iolib/syscalls:chown native-path
                          (iolib/syscalls:stat-uid (iolib/syscalls:lstat
                                                    native-path))
                          (nth-value 2 (iolib/syscalls:getgrnam new-group)))))
