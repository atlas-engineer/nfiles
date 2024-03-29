;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nfiles/pathname
  (:use #:common-lisp)
  (:import-from #:nclasses
                #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:import-from #:trivial-types
                #:pathname-designator)
  (:documentation "Some `cl:pathname' helpers."))
(in-package :nfiles/pathname)
(serapeum:eval-always
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

;; TODO: Make methods that take both pathnames and nfiles?
;; TODO: Define own `path-designator' type?

(export-always 'nil-pathname-p)
(-> nil-pathname-p ((or null pathname-designator)) boolean)
(defun nil-pathname-p (pathname)
  "Return non-nil if PATHNAME is `uiop:*nil-pathname*' or nil."
  (the (values boolean &optional)
       (or (null pathname)
           (uiop:pathname-equal pathname uiop:*nil-pathname*))))

(export-always 'pathname-type*)
(defun pathname-type* (pathname)
  "Like `pathname-type' but return NIL instead of \"\" or :UNSPECIFIC."
  (let ((type (pathname-type pathname)))
    (if (member type '(nil "" :unspecific) :test 'equal)
        nil
        type)))

(export-always 'directory-pathname-p)
(defun directory-pathname-p (pathname)
  "Like `uiop:directory-pathname-p' but also return T if `pathname-name' is \".\"
and check for existence."
  (or (uiop:directory-pathname-p pathname)
      (string= "." (pathname-name pathname))
      (uiop:directory-exists-p pathname)))

(export-always 'parent)
(-> parent (pathname-designator) (or pathname-designator null))
(defun parent (path)
  "Return the parent directory of PATH."
  (let ((path (uiop:ensure-pathname path)))
    (the (values (or pathname null) &optional)
         (if (uiop:directory-pathname-p path)
             (uiop:pathname-parent-directory-pathname path)
             (uiop:pathname-directory-pathname path)))))

(export-always 'basename)
(-> basename (pathname-designator) (or pathname-designator null))
(defun basename (pathname)
  "Return the basename, that is:
- if it's a directory, the name of the directory,
- if it's a file, the name of the file including its type (extension),
- nil if it's a nil-pathname (#p\"\")."
  (if (nil-pathname-p pathname)
      nil                               ; TODO: Shouldn't we return #p"" instead?
      (first (last (pathname-directory
                    ;; Ensure directory _after_ truenamizing, otherwise if
                    ;; non-directory file exists it may not yield a directory.
                    (uiop:ensure-directory-pathname
                     (uiop:ensure-pathname pathname :truenamize t)))))))

(defun special-pathname-namestring-p (pathname-namestring)
  "Return non-nil if pathname is special like \".\" or \"..\".
Note that this returns nil on #p\".\" and #p\"..\"."
  (and (stringp pathname-namestring)
       (member (namestring pathname-namestring) '("." "..") :test 'string=)))

(export-always 'join)
(-> join (&rest pathname-designator) (or pathname-designator null))
(defun join (&rest paths)
  "Concatenate PATHS."
  (if (< (length paths) 2)
      (the (values pathname &optional)
           (uiop:ensure-pathname (first paths)))
      (apply #'join
             (let ((path1 (first paths))
                   (path2 (second paths)))
               (if (or (null (pathname-name path1))
                       (pathname-directory path2))
                   (uiop:merge-pathnames* (uiop:relativize-pathname-directory (uiop:ensure-pathname path2))
                                          (uiop:ensure-pathname path1
                                                                :ensure-directory t))
                   (let ((new-base (uiop:strcat (basename path1)
                                                (if (special-pathname-namestring-p path2)
                                                    path2
                                                    (basename path2)))))
                     (make-pathname :defaults path1 :type (pathname-type new-base)
                                    :name (pathname-name new-base)))))
             (cddr paths))))

(export-always 'ensure-type)
(-> ensure-type (pathname-designator string) pathname-designator)
(defun ensure-type (path type)
  "Return PATH with type set to TYPE, if it's not already the case.
Case is ignored."
  (if (string-equal type (pathname-type path))
      path
      (the (values pathname &optional) (join path (uiop:strcat "." type)))))

(alex:define-constant +permissions+
    '((:user-read . 256) (:user-write . 128) (:user-exec . 64) (:group-read . 32)
      (:group-write . 16) (:group-exec . 8) (:other-read . 4) (:other-write . 2)
      (:other-exec . 1) (:set-user-id . 2048) (:set-group-id . 1024) (:sticky . 512))
  :test #'equal)

(export-always 'permissions)
(defun permissions (path)
  "Return a list of permissions as per `+permissions+'."
  #+(and sbcl (not android))
  (let ((mode (sb-posix:stat-mode
               (sb-posix:lstat path))))
    (loop :for (name . value) :in +permissions+
          :when (plusp (logand mode value))
            :collect name))
  #-(and sbcl (not android))
  (iolib/os:file-permissions (uiop:native-namestring path)))

(defun (setf permissions) (permissions path)
  "Set the PERMISSIONS or PATH as per `+permissions+'."
  #+(and sbcl (not android))
  (sb-posix:chmod path
                  (reduce (lambda (a b)
                            (logior a (rest (assoc b +permissions+))))
                          permissions :initial-value 0))
  #-(and sbcl (not android))
  (setf (iolib/os:file-permissions (uiop:native-namestring path)) permissions))

(export-always 'file-user)
(defun file-user (path)
  "Return PATH owner name."
  ;; `file-author' seems broken on many implementations.
  #+(or ccl (and sbcl (not android)))
  (file-author path)
  #-(or ccl (and sbcl (not android)))
  (iolib/syscalls:getpwuid (iolib/syscalls:stat-uid
                            (iolib/syscalls:lstat
                             (uiop:native-namestring path)))))

(defun (setf file-user) (new-user path)
  "Set PATH owner to NEW-USER (a string)."
  #+(and sbcl (not android))
  (sb-posix:chown path
                  (sb-posix:passwd-uid (sb-posix:getpwnam new-user))
                  (sb-posix:stat-gid (sb-posix:lstat path)))
  #-(and sbcl (not android))
  (let ((native-path (uiop:native-namestring path))
        (uid (nth-value 2 (iolib/syscalls:getpwnam new-user))))
    (if uid
        (iolib/syscalls:chown native-path
                              uid
                              (iolib/syscalls:stat-gid (iolib/syscalls:lstat
                                                        native-path)))
        (error "User ~a does not exist" new-user))))

(export-always 'file-group)
(defun file-group (path)
  "Return PATH group name."
  #+(and sbcl (not android))
  (sb-posix:group-name
   (sb-posix:getgrgid (sb-posix:stat-gid
                       (sb-posix:lstat path))))
  #-(and sbcl (not android))
  (iolib/syscalls:getgrgid (iolib/syscalls:stat-gid
                            (iolib/syscalls:lstat
                             (uiop:native-namestring path)))))

(defun (setf file-group) (new-group path)
  "Set PATH group to NEW-GROUP (a string)."
  #+(and sbcl (not android))
  (sb-posix:chown path
                  (sb-posix:stat-uid (sb-posix:lstat path))
                  (sb-posix:group-gid (sb-posix:getgrnam new-group)))
  #-(and sbcl (not android))
  (let ((native-path (uiop:native-namestring path))
        (gid (nth-value 2 (iolib/syscalls:getgrnam new-group))))
    (if gid
        (iolib/syscalls:chown native-path
                              (iolib/syscalls:stat-uid (iolib/syscalls:lstat
                                                        native-path))
                              gid)
        (error "Group ~s does not exist" new-group))))
