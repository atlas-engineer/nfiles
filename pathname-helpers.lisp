(in-package :nfiles)

;; TODO: Unused?  See if Nyxt needs anything.
;; TODO: Make methods that take both pathnames and nfiles.

(export-always 'parent)
(-> parent (pathname-designator) pathname-designator)
(defun parent (path)
  (if (uiop:directory-pathname-p path)
      (uiop:pathname-parent-directory-pathname path)
      (uiop:pathname-directory-pathname path)))

;; (export-always 'ensure-parent-exists)
;; (-> ensure-parent-exists (trivial-types:pathname-designator) trivial-types:pathname-designator)
;; (defun ensure-parent-exists (path)
;;   "Create parent directories of PATH if they don't exist and return coerced PATH."
;;   (let ((path (uiop:ensure-pathname path)))
;;     (ensure-directories-exist (directory-namestring path))
;;     path))
