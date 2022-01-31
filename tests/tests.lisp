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
     (nfiles::clear-cache)
     (unwind-protect
          (uiop:with-current-directory ((ensure-directories-exist *test-dir*))
            ,@body)
       (uiop:delete-directory-tree *test-dir* :validate t))))

(nfile-test "Simple path check"
  (let ((file (make-instance 'nfiles:file :base-path "foo" )))
    (is (nfiles:expand file)
        (uiop:merge-pathnames* "foo" *test-dir*)
        :test 'uiop:pathname-equal)))

(nfile-test "Current dir change"
  (let* ((file (make-instance 'nfiles:file :base-path "foo" ))
         (old-path (nfiles:expand file)))
    (uiop:with-current-directory ((uiop:temporary-directory))
      (isnt (nfiles:expand file) old-path))))

(defclass* myapp-file (nfiles:file)
    ())

(defmethod nfiles:resolve ((profile nfiles:profile) (file myapp-file))
  (let ((path (call-next-method)))
    (uiop:merge-pathnames* #p"myapp/" path)))

(defclass* myapp-config-file (myapp-file nfiles:lisp-file nfiles:config-file)
    ())

(nfile-test "Application config file"
  (let ((file (make-instance 'myapp-config-file :base-path "init")))
    (is (nfiles:expand file)
        (uiop:xdg-config-home "myapp/init.lisp"))))

(nfile-test "Read-only file"
  (let ((file (make-instance 'nfiles:read-only-file :base-path "should-not-exist")))
    (setf (nfiles:content file) "foo")
    (is (nfiles:expand file)
        (uiop:merge-pathnames* "should-not-exist" *test-dir*)
        :test #'uiop:pathname-equal)
    (is (find-if (lambda (filename) (search "should-not-exist" filename))
                 (mapcar #'pathname-name (uiop:directory-files *test-dir*)))
        nil)))

(nfile-test "Simple write"
  (let ((file (make-instance 'nfiles:file :base-path "foo"))
        (test-content "Hello world!"))
    (setf (nfiles:content file) test-content)
    (sleep 1)                           ; Wait for file write.
    (is (alexandria:read-file-into-string (nfiles:expand file))
        test-content)
    (is (nfiles:content file)
        test-content)))

(nfile-test "Read non-existing file"
  (let ((file (make-instance 'nfiles:file :base-path "bar")))
    (is (nfiles:content file)
        nil)))

(nfile-test "Cache"
  (let ((file1 (make-instance 'nfiles:file :base-path "baz"))
        (file2 (make-instance 'nfiles:file :base-path "baz"))
        (test-content "Cache test"))
    (setf (nfiles:content file1) test-content)
    (is (nfiles:content file2) test-content)))

(nfile-test "Backup"
  (let ((corrupted-path "corrupt.lisp"))
    (alexandria:write-string-into-file "(" corrupted-path)
    (let ((corrupted-file (make-instance 'nfiles:lisp-file :base-path "corrupt")))
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

(defmethod nfiles:serialize ((profile nfiles:profile) (file slow-file) &key)
  (incf (write-count file))
  (call-next-method))

(nfile-test "Skip useless writes"
  (let ((file (make-instance 'slow-file :base-path "qux"))
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

(nfile-test "GPG test"
  (let ((file (make-instance 'nfiles:gpg-file :base-path "fog"))
        (test-content "Cryptic world")
        (nfiles/gpg:*gpg-default-recipient* "mail@ambrevar.xyz"))
    (setf (nfiles:content file) test-content)
    (sleep 1)                           ; Wait for write.
    (ok (uiop:file-exists-p (nfiles:expand file)))
    (is-error (alexandria:read-file-into-string (nfiles:expand file))
              'error)
    (nfiles::clear-cache)
    (let ((synonym-file (make-instance 'nfiles:gpg-file :base-path "fog")))
      (is (nfiles:content synonym-file) test-content))))

(finalize)
