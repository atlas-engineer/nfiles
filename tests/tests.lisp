;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package nfiles/tests
  (:use #:common-lisp #:prove)
  (:import-from #:hu.dwim.defclass-star
                #:defclass*) )
(in-package :nfiles/tests)

(plan nil)

;; TODO: Clean up test folder on exit.

(defvar *test-dir* (ensure-directories-exist
                    (uiop:ensure-pathname
                     (uiop:merge-pathnames* "nfiles/" (uiop:temporary-directory)))))

(subtest "Simple path check"
  (uiop:with-current-directory (*test-dir*)
    (let ((file (make-instance 'nfiles:file :path "foo" )))
      (is (nfiles:expand file)
          (uiop:merge-pathnames* "foo" *test-dir*)
          :test 'uiop:pathname-equal))))

(subtest "Simple write"
  (uiop:with-current-directory (*test-dir*)
    (clrhash nfiles::*cache*)
    (let ((file (make-instance 'nfiles:file :path "foo"))
          (test-content "Hello world!"))
      (setf (nfiles:content file) test-content)
      (sleep 1)                         ; Wait for file write.
      (is (alexandria:read-file-into-string (nfiles:expand file))
          test-content)
      (is (nfiles:content file)
          test-content))))

(subtest "Read non-existing file"
  (uiop:with-current-directory (*test-dir*)
    (clrhash nfiles::*cache*)
    (let ((file (make-instance 'nfiles:file :path "bar")))
      (is (nfiles:content file)
          nil))))

(subtest "Cache"
  (uiop:with-current-directory (*test-dir*)
    (clrhash nfiles::*cache*)
    (let ((file1 (make-instance 'nfiles:file :path "baz"))
          (file2 (make-instance 'nfiles:file :path "baz"))
          (test-content "Cache test"))
      (setf (nfiles:content file1) test-content)
      (is (nfiles:content file2) test-content))))

(defclass* slow-file (nfiles:file)
    ((write-count
      0
      :type integer)
     (nfiles:timeout
      1))
    (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:serialize ((profile nfiles:profile) (file slow-file))
  (incf (write-count file))
  (sleep 2)
  (call-next-method))

(subtest "Skip useless writes"
  (uiop:with-current-directory (*test-dir*)
    (clrhash nfiles::*cache*)
    (let ((file (make-instance 'slow-file :path "qux"))
          (test-content "Skip test"))
      (dotimes (i 10)
        (setf (nfiles:content file) (format nil "~a: ~a" test-content i)))
      (sleep 3)
      ;; TODO:
      (is (write-count file) 1))))

;; (subtest "Custom path expansion"
;;   (uiop:with-current-directory (*test-dir*)
;;     (let ((file (make-instance 'nfiles:file :path "foo" )))
;;       (is (nfiles:expand file)
;;           (uiop:merge-pathnames* "foo" *test-dir*)
;;           :test 'uiop:pathname-equal))))

(finalize)
