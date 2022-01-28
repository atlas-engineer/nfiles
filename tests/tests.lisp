;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package nfiles/tests
  (:use #:common-lisp #:prove)
  (:import-from #:hu.dwim.defclass-star
                #:defclass*) )
(in-package :nfiles/tests)

(plan nil)

(defvar *test-dir* (uiop:ensure-pathname
                    (uiop:merge-pathnames* "nfiles/" (uiop:temporary-directory))))

(defmacro nfile-test (name &body body)
  `(subtest ,name
     (clrhash nfiles::*cache*)
     (unwind-protect
          (uiop:with-current-directory ((ensure-directories-exist *test-dir*))
            ,@body)
       (uiop:delete-directory-tree *test-dir* :validate t))))

(nfile-test "Simple path check"
  (let ((file (make-instance 'nfiles:file :path "foo" )))
    (is (nfiles:expand file)
        (uiop:merge-pathnames* "foo" *test-dir*)
        :test 'uiop:pathname-equal)))

(nfile-test "Current dir change"
  (let* ((file (make-instance 'nfiles:file :path "foo" ))
         (old-path (nfiles:expand file)))
    (uiop:with-current-directory ((uiop:temporary-directory))
      (isnt (nfiles:expand file) old-path))))

(defclass* myapp-file (nfiles:file)
    ())

(defmethod nfiles:resolve ((profile nfiles:profile) (file myapp-file))
  (let ((path (call-next-method)))
    (make-pathname :defaults path
                   :directory (pathname-directory (uiop:merge-pathnames* "myapp/" (uiop:pathname-directory-pathname path))))))

(defclass* myapp-config-file (myapp-file nfiles:lisp-file nfiles:config-file)
    ())

(nfile-test "Application config file"
  (let ((file (make-instance 'myapp-config-file :path "init")))
    (is (nfiles:expand file)
        (uiop:xdg-config-home "myapp/init.lisp"))))

(nfile-test "Simple write"
  (let ((file (make-instance 'nfiles:file :path "foo"))
        (test-content "Hello world!"))
    (setf (nfiles:content file) test-content)
    (sleep 1)                           ; Wait for file write.
    (is (alexandria:read-file-into-string (nfiles:expand file))
        test-content)
    (is (nfiles:content file)
        test-content)))

(nfile-test "Read non-existing file"
  (let ((file (make-instance 'nfiles:file :path "bar")))
    (is (nfiles:content file)
        nil)))

(nfile-test "Cache"
  (let ((file1 (make-instance 'nfiles:file :path "baz"))
        (file2 (make-instance 'nfiles:file :path "baz"))
        (test-content "Cache test"))
    (setf (nfiles:content file1) test-content)
    (is (nfiles:content file2) test-content)))

(nfile-test "Backup"
  (let ((corrupted-path "corrupt.lisp"))
    (alexandria:write-string-into-file "(" corrupted-path)
    (let ((corrupted-file (make-instance 'nfiles:lisp-file :path "corrupt")))
      (is (nfiles:content corrupted-file) nil)
      (ok (find-if (lambda (filename) (search "-backup" filename))
                   (mapcar #'pathname-name (uiop:directory-files *test-dir*)))))))

(defclass* slow-file (nfiles:file)
    ((write-count
      0
      :type integer)
     (nfiles:timeout
      1))
    (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:serialize ((profile nfiles:profile) (file slow-file))
  (incf (write-count file))
  (call-next-method))

(nfile-test "Skip useless writes"
  (let ((file (make-instance 'slow-file :path "qux"))
        (test-content "Skip test")
        (limit 5))
    (setf (nfiles:content file) test-content)
    ;; Wait for worker to be done writing the first file.
    (loop until (uiop:file-exists-p (nfiles:expand file))
          do (sleep 0.1))
    (dotimes (i limit)
      (setf (nfiles:content file) (format nil "~a: ~a" test-content i)) )
    ;; Wait for worker's notification (potentially as long as 1 timeout) + final
    ;; timeout.
    (sleep (+ 1 (* 2 (nfiles:timeout file))))
    (is (nfiles::worker (nfiles::cache-entry file)) nil)
    (is (write-count file) 2)
    (is (nfiles:content file) (format nil "~a: ~a" test-content (1- limit)))))

;; (subtest "Custom path expansion"
;;   (uiop:with-current-directory (*test-dir*)
;;     (let ((file (make-instance 'nfiles:file :path "foo" )))
;;       (is (nfiles:expand file)
;;           (uiop:merge-pathnames* "foo" *test-dir*)
;;           :test 'uiop:pathname-equal))))

(finalize)
