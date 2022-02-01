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
  (let ((file (make-instance 'nfiles:file :base-path "foo")))
    (is (nfiles:expand file)
        (uiop:merge-pathnames* "foo" *test-dir*)
        :test 'uiop:pathname-equal)))

(nfile-test "Special character support"
  (let ((file (make-instance 'nfiles:file :base-path "[")))
    (is (nfiles:expand file)
        (uiop:merge-pathnames* #p"\\[" *test-dir*)
        :test 'uiop:pathname-equal)))

(nfile-test "Current dir change"
  (let* ((file (make-instance 'nfiles:file :base-path "foo"))
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

(subtest "Symlinks"
  (uiop:with-current-directory ((uiop:merge-pathnames*
                                 "tests"
                                 (asdf:system-source-directory (asdf:find-system :nfiles))))

    (let ((file (make-instance 'nfiles:file :base-path "link-to-dummy")))
      (is (nfiles:expand file)
          (uiop:ensure-pathname "dummy" :truenamize t))
      (let ((content (nfiles:content file)))
        (setf (nfiles:content file) content)
        (is (nfiles:expand (make-instance 'nfiles:file :base-path "link-to-dummy"))
            (uiop:ensure-pathname "dummy" :truenamize t))))))

(nfile-test "Simple write"
  (let ((file (make-instance 'nfiles:file :base-path "foo"))
        (test-content "Hello world!"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (is (alexandria:read-file-into-string (nfiles:expand file))
        test-content)
    (is (nfiles:content file)
        test-content)))

(nfile-test "Preserve attributes"
  (let ((file (make-instance 'nfiles:file :base-path "private"))
        (test-content "Hello world!")
        (new-content "Hello new world!"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (let ((permissions (nfiles:permissions (nfiles:expand file))))
      (if (member :other-read permissions)
          (setf permissions (remove :other-read permissions))
          (push :other-read permissions))
      (setf (nfiles:permissions (nfiles:expand file))
            permissions)
      (bt:join-thread (setf (nfiles:content file) new-content))
      (is (nfiles:permissions (nfiles:expand file))
          permissions))))

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
      :type integer))
    (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:serialize ((profile nfiles:profile) (file slow-file) &key)
  (incf (write-count file))
  (call-next-method))

(nfile-test "Skip useless writes"
  (let ((file (make-instance 'slow-file :base-path "qux"))
        (test-content "Skip test")
        (limit 5)
        (nfiles::*timeout* 1))
    ;; Wait for worker to be done writing the file.
    (bt:join-thread (setf (nfiles:content file) test-content))
    (let ((last-thread nil))
      (dotimes (i limit)
        (setf last-thread
              (setf (nfiles:content file) (format nil "~a: ~a" test-content i))) )
      (bt:join-thread last-thread))
    (is (nfiles::worker (nfiles::cache-entry file)) nil)
    (is (write-count file) 2)
    (is (nfiles:content file) (format nil "~a: ~a" test-content (1- limit)))))

(nfile-test "GPG test"
  (let ((file (make-instance 'nfiles:gpg-file :base-path "fog"))
        (test-content "Cryptic world")
        (nfiles/gpg:*gpg-default-recipient* "mail@ambrevar.xyz"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (ok (uiop:file-exists-p (nfiles:expand file)))
    (is-error (alexandria:read-file-into-string (nfiles:expand file))
              'error)
    (nfiles::clear-cache)
    (let ((synonym-file (make-instance 'nfiles:gpg-file :base-path "fog")))
      (is (nfiles:content synonym-file) test-content))))

(finalize)
