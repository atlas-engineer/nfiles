;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nfiles/tests
  (:use #:common-lisp #:lisp-unit2)
  (:import-from #:hu.dwim.defclass-star
                #:defclass*))
(in-package :nfiles/tests)

(defvar *test-dir* (uiop:ensure-pathname
                    (uiop:merge-pathnames* "nfiles/" (uiop:temporary-directory))))

(defun with-nfiles-context (thunk)
  "A context that clears the cache and sets up a temporary directory."
  (nfiles::clear-cache)
  (unwind-protect
       (uiop:with-current-directory ((ensure-directories-exist *test-dir*))
         (funcall thunk))
    (uiop:delete-directory-tree *test-dir* :validate t)))

(defun with-nfiles-gpg-context (thunk)
  "A context that checks if `nfiles/gpg:*gpg-default-recipient*' is set and defer
to `with-nfiles-context'."
  (if nfiles/gpg:*gpg-default-recipient*
      (with-nfiles-context thunk)
      (warn "Skipping GPG tests, set the `nfiles/gpg:*gpg-default-recipient*' to enable.")))

(defmacro assert-pathname-equal (&whole whole expected form &rest extras)
  "Assert whether expected and form are equal according to test."
  `(lisp-unit2::expand-assert 'lisp-unit2::equal-result ,form ,form ,expected ,extras :test #'uiop:pathname-equal
    :full-form ',whole))

(define-test basename (:contexts '(with-nfiles-context))
  "Basename tests."
  (assert-false (nfiles:basename ""))
  (assert-equal "bar"
                (nfiles:basename "foo/bar"))
  (assert-false (nfiles:basename #p""))
  (assert-equal "baz"
                (nfiles:basename #p"/foo/bar/baz"))
  (assert-equal "baz"
                (nfiles:basename #p"/foo/bar/baz/"))
  (assert-equal "baz.ext"
                (nfiles:basename #p"/foo/bar/baz.ext"))
  (assert-equal "baz.ext"
                (nfiles:basename #p"foo/bar/baz.ext")))

(define-test join (:contexts '(with-nfiles-context))
  (assert-pathname-equal #p"foo"
                         (nfiles:join "foo"))
  (assert-pathname-equal #p"foobar"
                         (nfiles:join #p"foo" "bar"))
  (assert-pathname-equal #p"foobarbaz"
                         (nfiles:join #p"foo" "bar" #p"baz"))
  (assert-pathname-equal #p"foo/bar/baz"
                         (nfiles:join #p"foo" "bar/baz"))
  (assert-pathname-equal #p"foo/barbaz"
                         (nfiles:join #p"foo/bar" "baz"))
  (assert-pathname-equal #p"foo/bar/baz"
                         (nfiles:join #p"foo/bar/" "baz"))
  (assert-pathname-equal #p"foo/bar/bazqux"
                         (nfiles:join #p"foo/" "bar/" "baz" "qux"))
  (assert-pathname-equal #p"foo.txt/bar/baz"
                         (nfiles:join #p"foo.txt" "bar/baz"))
  (assert-pathname-equal #p"foo.txtbar.ext"
                         (nfiles:join #p"foo.txt" "bar.ext")))

(define-test simple-path-check (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'nfiles:file :base-path #p"foo")))
    (assert-pathname-equal (uiop:merge-pathnames* "foo" *test-dir*)
                           (nfiles:expand file))))

(define-test special-character-support (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'nfiles:file :base-path "["))) ; REVIEW: OK to use string here?
    (assert-pathname-equal (uiop:ensure-pathname (uiop:strcat (namestring *test-dir*) "["))
                           (nfiles:expand file))))

(define-test tilde-home-directory (:contexts '(with-nfiles-context))
  "Tilde '~' should expand to home directory."
  (let ((file1 (make-instance 'nfiles:file :base-path "~/[foo")) ; REVIEW: OK to use string here?
        (file2 (make-instance 'nfiles:file :base-path #p"~/\\[bar")))
    (assert-pathname-equal (uiop:ensure-pathname (uiop:strcat (namestring (user-homedir-pathname)) "[foo"))
                           (nfiles:expand file1))
    (assert-pathname-equal (uiop:merge-pathnames* #p"\\[bar" (user-homedir-pathname))
                           (nfiles:expand file2))))

(define-test current-dir-change (:contexts '(with-nfiles-context))
  (let* ((file (make-instance 'nfiles:file :base-path #p"foo"))
         (old-path (nfiles:expand file)))
    (uiop:with-current-directory ((uiop:temporary-directory))
      (assert-false (uiop:pathname-equal (nfiles:expand file) old-path)))))

(defclass* myapp-file (nfiles:file)
    ())

(defmethod nfiles:resolve ((profile nfiles:profile) (file myapp-file))
  (let ((path (call-next-method)))
    (uiop:merge-pathnames* #p"myapp/" path)))

(defclass* myapp-config-file (myapp-file nfiles:lisp-file nfiles:config-file)
    ())

(define-test application-config-file (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'myapp-config-file :base-path #p"init")))
    (assert-pathname-equal (uiop:xdg-config-home "myapp/init.lisp")
                           (nfiles:expand file))))

(define-test read-only-file (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'nfiles:read-only-file :base-path #p"should-not-exist")))
    (setf (nfiles:content file) "foo")
    (assert-pathname-equal (uiop:merge-pathnames* "should-not-exist" *test-dir*)
                           (nfiles:expand file))
    (assert-false (find-if (lambda (filename) (search "should-not-exist" filename))
                           (mapcar #'pathname-name (uiop:directory-files *test-dir*))))))

(define-test file-without-base-path (:contexts '(with-nfiles-context))
  "File without base path / directory as path."
  (let ((test-content "foo")
        (file (make-instance 'nfiles:file)))
    (setf (nfiles:content file) test-content)
    (assert-false (uiop:file-exists-p (nfiles:expand file)))
    (assert-equal test-content
                  (nfiles:content file))))

(define-test symlinks (:contexts '(with-nfiles-context))
  ;; WARNING: UIOP 3.3.1 (bundled with SBCL by default) requires a trailing / in
  ;; `uiop:with-current-directory'.
  (uiop:with-current-directory ((asdf:system-relative-pathname :nfiles "test-data/"))

    (let ((file (make-instance 'nfiles:file :base-path #p"link-to-dummy")))
      (assert-pathname-equal (uiop:ensure-pathname "dummy" :truenamize t)
                             (nfiles:expand file))
      (let ((content (nfiles:content file)))
        (setf (nfiles:content file) content)
        (assert-pathname-equal
         (uiop:ensure-pathname "dummy" :truenamize t)
         (nfiles:expand (make-instance 'nfiles:file :base-path #p"link-to-dummy")))))))

(define-test simple-write (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'nfiles:file :base-path #p"foo"))
        (test-content "Hello world!"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (assert-equal test-content
                  (alexandria:read-file-into-string (nfiles:expand file)))
    (assert-equal test-content
                  (nfiles:content file))))

(define-test parent-creation (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'nfiles:file :base-path #p"foo/bar"))
        (test-content "Hello world!"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (assert-equal test-content
                  (alexandria:read-file-into-string (nfiles:expand file)))))

(define-test preserve-attributes (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'nfiles:file :base-path #p"private"))
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
      (assert-equal permissions
                    (nfiles:permissions (nfiles:expand file))))))

(define-test read-non-existing-file (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'nfiles:file :base-path #p"bar")))
    (assert-false (nfiles:content file))))

(defclass* counter-file (nfiles:file)
    ((write-count
      0
      :type unsigned-byte)
     (read-count
      0
      :type unsigned-byte))
    (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:write-file ((profile nfiles:profile) (file counter-file) &key destination)
  (declare (ignore destination))
  (incf (write-count file))
  (call-next-method))

(defmethod nfiles:read-file ((profile nfiles:profile) (file counter-file) &key)
  (incf (read-count file))
  (call-next-method))

(define-test cache (:contexts '(with-nfiles-context))
  (let ((file1 (make-instance 'counter-file :base-path #p"foo"))
        (file2 (make-instance 'counter-file :base-path #p"bar"))
        (test-content "Hello world!")
        (test-content2 "Hello new world!"))

    (assert-false (nfiles:content file1))
    (assert-eql 0 (read-count file1))
    (assert-eql 0 (write-count file1))
    (bt:join-thread (setf (nfiles:content file1) test-content))
    (assert-eql 0 (read-count file1))
    (assert-eql 1 (write-count file1))

    (assert-equal test-content
                  (nfiles:content file1))
    (assert-eql 0 (read-count file1))
    (assert-eql 1 (write-count file1))

    (alexandria:write-string-into-file test-content (nfiles:expand file2))
    (assert-equal test-content
                  (nfiles:content file2))
    (assert-eql 1 (read-count file2))
    (assert-eql 0 (write-count file2))
    (sleep 1)      ; Need to sleep 1s because time resolution is to the second.
    (bt:join-thread (setf (nfiles:content file2) test-content2))
    (assert-equal test-content2 (nfiles:content file2))
    (assert-eql 1 (read-count file2))
    (assert-eql 1 (write-count file2))))

(define-test cache-external-modification (:contexts '(with-nfiles-context))
  (let ((file1 (make-instance 'nfiles:file :base-path #p"foo"))
        (file2 (make-instance 'nfiles:file :base-path #p"foo"))
        (test-content "Hello world!")
        (test-content2 "Hello altered world!")
        (test-content3 "Hello imposing world!"))
    (bt:join-thread (setf (nfiles:content file1) test-content))
    (sleep 1)       ; Need to sleep 1s because time resolution is to the second.
    (alexandria:write-string-into-file test-content2 (nfiles:expand file1)
                                       :if-exists :supersede)
    (assert-error 'nfiles:external-modification
                  (nfiles:content file1))
    (assert-error 'nfiles:external-modification
                  (nfiles:content file1))

    (setf (nfiles:on-external-modification file1) 'nfiles:reload)
    (setf (nfiles:on-external-modification file2) 'nfiles:reload)
    (assert-equal test-content2
                  (nfiles:content file1))
    (assert-equal test-content2
                  (nfiles:content file2))

    (bt:join-thread (setf (nfiles:content file1) test-content3))
    (sleep 1)       ; Need to sleep 1s because time resolution is to the second.
    (alexandria:write-string-into-file test-content2 (nfiles:expand file1)
                                       :if-exists :supersede)
    (setf (nfiles:on-external-modification file1) 'nfiles:overwrite)
    (setf (nfiles:on-external-modification file2) 'nfiles:overwrite)

    (assert-equal test-content3
                  (nfiles:content file1))
    (assert-equal test-content3
                  (nfiles:content file2))

    ;;  We wait until we are done writing, because we don't have access to the
    ;;  thread.  TODO: Fix it?
    (sleep 1)
    (assert-equal test-content3
                  (alexandria:read-file-into-string (nfiles:expand file1)))))

(define-test cache-invalidation (:contexts '(with-nfiles-context))
  (let ((file1 (make-instance 'nfiles:file :base-path #p"foo"))
        (file2 (make-instance 'nfiles:file :base-path #p"foo"))
        (test-content "Hello world!")
        (test-content2 "Hello altered world!"))
    (bt:join-thread (setf (nfiles:content file1) test-content))
    (alexandria:write-string-into-file test-content2 (nfiles:expand file1)
                                       :if-exists :supersede)
    ;; Hack the cache to make it artificially more  up-to-date than it really is.
    (setf (nfiles::last-update (gethash (uiop:native-namestring (nfiles:expand file2))
                                        nfiles::*cache*))
          (get-universal-time))
    (assert-equal test-content
                  (nfiles:content file2))
    (assert-equal test-content2
                  (nfiles:content file2 :force-read t))
    (assert-equal test-content2
                  (nfiles:content file1))))

(define-test skip-useless-writes (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'counter-file :base-path #p"qux"))
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
    (assert-false (nfiles::worker (nfiles::cache-entry file)))
    (assert-eql 2 (write-count file))
    (assert-equal (format nil "~a: ~a" test-content (1- limit)) (nfiles:content file))))


(define-test deserialization-error-forward (:contexts '(with-nfiles-context))
  "Deserialization error: Forward it."
  (let ((corrupted-path "corrupt.lisp"))
    (alexandria:write-string-into-file "(" corrupted-path)
    (let* ((errors 0)
           (corrupted-file (make-instance 'nfiles:lisp-file :base-path #p"corrupt"
                                          :read-handler (lambda (c)
                                                          (incf errors)
                                                          (invoke-restart 'nfiles::forward-condition c)))))
      (multiple-value-bind (value error)
          (nfiles:content corrupted-file)
        (assert-false value)
        (assert-typep 'end-of-file error))
      (assert-eql 1 errors)
      (multiple-value-bind (value error)
          (nfiles:content corrupted-file)
        (assert-false value)
        (assert-typep 'end-of-file error))
      (assert-eql errors 2)
      (assert-true (uiop:file-exists-p (nfiles:expand corrupted-file)))
      (setf (nfiles:on-deserialization-error corrupted-file) 'nfiles:delete)
      (nfiles:content corrupted-file)
      (assert-false (uiop:file-exists-p (nfiles:expand corrupted-file))))))

;; When SBCL threads abort, `bt:join-thread' errors out.  This is not the case
;; in all compilers though.
#+sbcl
(define-test deserialization-error-abort (:contexts '(with-nfiles-context))
  "Deserialization error: Abort."
  (let ((corrupted-path "corrupt.lisp"))
    (alexandria:write-string-into-file "(" corrupted-path)
    (let* ((errors 0)
           (corrupted-file (make-instance 'nfiles:lisp-file :base-path #p"corrupt"
                                          :read-handler (lambda (c)
                                                          (declare (ignore c))
                                                          (incf errors)
                                                          (abort)))))
      (assert-false (nfiles:content corrupted-file))
      (assert-eql 1 errors)
      (assert-false (nfiles:content corrupted-file) nil)
      (assert-eql 2 errors)
      (assert-true (uiop:file-exists-p (nfiles:expand corrupted-file)))
      (setf (nfiles:on-deserialization-error corrupted-file) 'nfiles:delete)
      (nfiles:content corrupted-file)
      (assert-false (uiop:file-exists-p (nfiles:expand corrupted-file))))))

(define-test backup (:contexts '(with-nfiles-context))
  (let ((corrupted-path "corrupt.lisp"))
    (alexandria:write-string-into-file "(" corrupted-path)
    (let ((corrupted-file (make-instance 'nfiles:lisp-file :base-path #p"corrupt"
                                         :on-deserialization-error 'nfiles:backup)))
      (assert-false (nfiles:content corrupted-file) nil)
      (assert-true (find-if (lambda (filename) (search "-backup" filename))
                            (mapcar #'pathname-name (uiop:directory-files *test-dir*)))))))

(defclass* nil-file (nfiles:virtual-file)
    ())
(defmethod nfiles:resolve ((profile nfiles:profile) (file nil-file))
  #p"")

(define-test with-paths (:contexts '(with-nfiles-context))
  (let ((file1 (make-instance 'nfiles:file))
        (file2 (make-instance 'nfiles:file :base-path "alt"))
        (nil-file (make-instance 'nil-file)))
    (assert-equal
     (nfiles:with-paths ((path1 file1)
                         (path2 file2))
       (list path1 path2))
     (list (nfiles:expand file1)
           (nfiles:expand file2)))
    (assert-false
     (nfiles:with-paths ((nil-path nil-file)
                         (not-evaluated (make-instance 'nfiles:file :base-path (error "Should not reach here"))))
       not-evaluated))))

(defclass* slow-file (nfiles:file)
  ()
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:read-file ((profile nfiles:profile) (file slow-file) &key)
  (sleep 1)
  (call-next-method))

(define-test async-read-test (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'slow-file :base-path #p"slow"))
        (test-content "Slow world"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (assert-true (uiop:file-exists-p (nfiles:expand file)))
    (nfiles::clear-cache)
    (multiple-value-bind (result maybe-thread)
        (nfiles:content file :wait-p nil)
      (assert-false result)
      (assert-true (bt:threadp maybe-thread)))
    (assert-equal test-content
                  (nfiles:content file))))

(define-test gpg-test (:contexts '(with-nfiles-gpg-context))
  (let ((file (make-instance 'nfiles:gpg-file :base-path #p"fog.gpg"))
        (test-content "Cryptic world"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (assert-true (uiop:file-exists-p (nfiles:expand file)))
    #+sbcl
    (assert-error 'error
                  (alexandria:read-file-into-string (nfiles:expand file)))
    #+ccl
    (assert-false (equal (alexandria:read-file-into-string (nfiles:expand file))
                         test-content))
    (nfiles::clear-cache)
    (let ((synonym-file (make-instance 'nfiles:gpg-file :base-path #p"fog.gpg")))
      (assert-equal test-content (nfiles:content synonym-file)))))

(define-test gpg-backup (:contexts '(with-nfiles-gpg-context))
  (let ((corrupted-file (make-instance 'nfiles:gpg-file :base-path #p"corrupt.lisp.gpg")))
    (bt:join-thread (setf (nfiles:content corrupted-file) "("))
    (assert-true (uiop:file-exists-p "corrupt.lisp.gpg"))
    ;; Clear the cache so that next file tries reading the corrupted file.
    (nfiles::clear-cache)
    (let ((corrupted-lisp-file (make-instance 'nfiles:gpg-lisp-file :base-path #p"corrupt.gpg"
                                              :on-read-error 'nfiles:backup)))
      (assert-equal (uiop:ensure-pathname "corrupt.lisp.gpg" :truenamize t)
                    (nfiles:expand corrupted-lisp-file))
      (assert-false (nfiles:content corrupted-lisp-file))
      (assert-true (find-if (lambda (filename) (search "-backup" filename))
                            (mapcar #'pathname-name (uiop:directory-files *test-dir*)))))))

(defclass* broken-write-file (nfiles:gpg-lisp-file)
  ((appended-value 1))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:write-file ((profile nfiles:profile) (file broken-write-file) &key destination)
  (nfiles/gpg:with-gpg-file (stream destination :direction :output)
    (nfiles:serialize profile file stream))
  (print (/ 1 (appended-value file))))

(define-test gpg-preserve-permissions (:contexts '(with-nfiles-gpg-context))
  "Staged GPG file with preserved permissions."
  (let ((file (make-instance 'broken-write-file :base-path #p"fog"
                             :write-handler (lambda (c) (declare (ignore c)) (abort))))
        (test-content '(a b))
        (new-content "More cryptic world!"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (let ((permissions (nfiles:permissions (nfiles:expand file)))
          (last-write-date (uiop:safe-file-write-date (nfiles:expand file))))
      (if (member :other-read permissions)
          (setf permissions (remove :other-read permissions))
          (push :other-read permissions))
      (setf (nfiles:permissions (nfiles:expand file))
            permissions)
      (setf (appended-value file) 0)
      (sleep 1)
      ;; Some implementation forward the conditions, others do not:
      #+sbcl
      (assert-error 'error
                    (bt:join-thread (setf (nfiles:content file) new-content)))
      #-sbcl
      (bt:join-thread (setf (nfiles:content file) new-content))
      (assert-equal new-content (nfiles:content file))
      (assert-equal last-write-date (uiop:safe-file-write-date (nfiles:expand file)))
      (assert-equal permissions
                    (nfiles:permissions (nfiles:expand file))))))

(defun local-fetcher (uri)
  (serapeum:trim-whitespace
   (alexandria:read-file-into-string (quri:uri-file-pathname uri))))

(defclass* remote-counter-file (counter-file nfiles:remote-file)
  ((download-count
    0
    :type unsigned-byte))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:fetch ((profile nfiles:profile) (file remote-counter-file) &key)
  (incf (download-count file))
  (local-fetcher (nfiles:url file)))

(defmethod nfiles:check ((profile nfiles:profile) (file remote-counter-file) content &key)
  "Terrible and insecure checker, only for testing purposes, don't use it."
  (write-to-string (length content)))

(defvar *remote-file-url* (quri.uri.file:make-uri-file :path (asdf:system-relative-pathname :nfiles "test-data/dummy")))

(define-test remote-file-test (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'remote-counter-file
                             :base-path #p"local-dummy"
                             :url *remote-file-url*))
        (test-content "Dummy file."))
    (assert-equal test-content
                  (nfiles:content file))
    (assert-true (uiop:file-exists-p (nfiles:expand file)))
    (assert-eql 1 (download-count file))
    (assert-eql 1 (write-count file))
    (nfiles::clear-cache)
    (assert-equal test-content
                  (nfiles:content file))
    (assert-eql 1 (download-count file))
    (assert-eql 1 (write-count file))))

(define-test remote-file-in-memory (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'remote-counter-file
                             :base-path #p""
                             :url *remote-file-url*))
        (test-content "Dummy file."))
    (assert-equal test-content
                  (nfiles:content file))
    (assert-false (uiop:file-exists-p (nfiles:expand file)))
    (assert-eql 1 (download-count file))
    (assert-equal test-content
                  (nfiles:content file))
    (assert-eql 1 (download-count file))))

(define-test remote-file-test-always-download (:contexts '(with-nfiles-context))
  (let ((file (make-instance 'remote-counter-file
                             :base-path #p""
                             :url *remote-file-url*))
        (test-content "Dummy file."))
    (assert-equal test-content
                  (nfiles:content file))
    (assert-false (uiop:file-exists-p (nfiles:expand file)))
    (assert-eql 1 (download-count file))
    (nfiles::clear-cache)
    (assert-equal test-content
                  (nfiles:content file))
    (assert-eql 2 (download-count file))))

(define-test remote-file-checksum-test (:contexts '(with-nfiles-context))
  (let* ((test-content "Dummy file.")
         (file (make-instance 'remote-counter-file
                              :read-handler (lambda (c)
                                              (invoke-restart 'nfiles::forward-condition c))
                              :base-path #p"local-dummy"
                              :url *remote-file-url*
                              :checksum (write-to-string (length test-content)))))
    (assert-equal test-content
                  (nfiles:content file)))
  (let* ((test-content "Dummy file.")
         (file (make-instance 'remote-counter-file
                              :read-handler (lambda (c)
                                              (invoke-restart 'nfiles::forward-condition c))
                              :base-path #p"local-dummy2"
                              :url *remote-file-url*
                              :checksum (write-to-string (1+ (length test-content))))))
    (multiple-value-bind (value error)
        (nfiles:content file)
      (assert-false value)
      (assert-typep 'nfiles:invalid-checksum error)
      (assert-eql 1 (download-count file)))
    (multiple-value-bind (value error)
        (nfiles:content file)
      (assert-false value)
      (assert-typep 'nfiles:invalid-checksum error)
      (assert-eql 2 (download-count file))))
  (let* ((test-content "Dummy file.")
         (file (make-instance 'remote-counter-file
                              :read-handler (lambda (c)
                                              (invoke-restart 'nfiles::forward-condition c))
                              :on-invalid-checksum 'nfiles:discard
                              :base-path #p"local-dummy3"
                              :url *remote-file-url*
                              :checksum (write-to-string (1+ (length test-content))))))
    (multiple-value-bind (value)
        (nfiles:content file)
      (assert-false value)
      (assert-eql 1 (download-count file)))
    (setf (nfiles:checksum file) (write-to-string (length test-content)))
    (assert-equal test-content
                  (nfiles:content file))
    (assert-eql 2 (download-count file))))

(define-test remote-file-auto-update-test (:contexts '(with-nfiles-context))
  (let* ((test-content "Remote file.")
         (test-content2 "Altered file.")
         (test-content3 "Changed file.")
         (remote-file "remote.file")
         (file (make-instance 'remote-counter-file
                              :base-path #p"local-dummy"
                              :url (quri.uri.file:make-uri-file :path remote-file))))
    (alexandria:write-string-into-file test-content remote-file :if-exists :supersede)
    (assert-equal test-content
                  (nfiles:content file))
    (alexandria:write-string-into-file test-content2 remote-file :if-exists :supersede)
    (assert-equal test-content
                  (nfiles:content file))
    (assert-equal test-content2
                  (nfiles:content file :force-update t))
    (alexandria:write-string-into-file test-content3 remote-file :if-exists :supersede)
    (setf (nfiles:update-interval file) 1)
    (assert-equal test-content2
                  (nfiles:content file))
    (sleep 2)
    (assert-equal test-content3
                  (nfiles:content file))))

(define-test virtual-files-and-profiles (:contexts '(with-nfiles-context))
  (let* ((test-content "Test file.")
         (file1 (make-instance 'nfiles:virtual-file
                               :base-path #p"virtual-file"))
         (file2 (make-instance 'nfiles:virtual-file
                               :base-path #p"virtual-file"))
         (file3 (make-instance 'nfiles:virtual-file
                               :base-path #p"other-virtual")))
    (setf (nfiles:content file1) test-content)
    (assert-pathname-equal (uiop:merge-pathnames* "virtual-file" *test-dir*)
                           (nfiles:expand file1))
    (assert-pathname-equal (uiop:merge-pathnames* "virtual-file" *test-dir*)
                           (nfiles:expand file2))
    (assert-pathname-equal (uiop:merge-pathnames* "other-virtual" *test-dir*)
                           (nfiles:expand file3))
    (assert-false (uiop:file-exists-p (nfiles:expand file1)))
    (assert-equal test-content
                  (nfiles:content file1))
    (assert-equal test-content
                  (nfiles:content file2))
    (assert-false (nfiles:content file3)))
  (let* ((nfiles::*default-profile* (make-instance 'nfiles:virtual-profile))
         (test-content "Test file.")
         (file (make-instance 'nfiles:file
                              :base-path #p"yet another file")))
    (setf (nfiles:content file) test-content)
    (assert-pathname-equal uiop:*nil-pathname*
                           (nfiles:expand file))

    (assert-pathname-equal (uiop:merge-pathnames* "yet another file" *test-dir*)
                           (nfiles:resolve (make-instance 'nfiles:profile) file))
    (assert-false (uiop:file-exists-p (nfiles:expand file)))
    (assert-equal test-content
                  (nfiles:content file))))
