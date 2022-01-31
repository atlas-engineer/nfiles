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
