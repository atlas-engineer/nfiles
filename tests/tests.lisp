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

(defmacro nfile-gpg-test (name &body body)
  `(if nfiles/gpg:*gpg-default-recipient*
       (nfile-test ,name (progn ,@body))
       (warn "Skipping GPG tests, set the `nfiles/gpg:*gpg-default-recipient*' to enable.")))

(subtest "Basename"
  (is (nfiles:basename "")
      nil)
  (is (nfiles:basename "foo/bar")
      "bar")
  (is (nfiles:basename #p"")
      nil)
  (is (nfiles:basename #p"/foo/bar/baz")
      "baz")
  (is (nfiles:basename #p"/foo/bar/baz/")
      "baz")
  (is (nfiles:basename #p"/foo/bar/baz.ext")
      "baz.ext")
  (is (nfiles:basename #p"foo/bar/baz.ext")
      "baz.ext"))

(subtest "Join"
  (is (nfiles:join "foo")
      #p"foo"
      :test 'uiop:pathname-equal)
  (is (nfiles:join #p"foo" "bar")
      #p"foobar"
      :test 'uiop:pathname-equal)
  (is (nfiles:join #p"foo" "bar" #p"baz")
      #p"foobarbaz"
      :test 'uiop:pathname-equal)
  (is (nfiles:join #p"foo" "bar/baz")
      #p"foo/bar/baz"
      :test 'uiop:pathname-equal)
  (is (nfiles:join #p"foo.txt" "bar/baz")
      #p"foo.txt/bar/baz"
      :test 'uiop:pathname-equal)
  (is (nfiles:join #p"foo.txt" "bar.ext")
      #p"foo.txtbar.ext"
      :test 'uiop:pathname-equal))

(nfile-test "Simple path check"
  (let ((file (make-instance 'nfiles:file :base-path #p"foo")))
    (is (nfiles:expand file)
        (uiop:merge-pathnames* "foo" *test-dir*)
        :test 'uiop:pathname-equal)))

(nfile-test "Special character support"
  (let ((file (make-instance 'nfiles:file :base-path "["))) ; REVIEW: OK to use string here?
    (is (nfiles:expand file)
        (uiop:ensure-pathname (uiop:strcat (namestring *test-dir*) "["))
        :test 'uiop:pathname-equal)))

(nfile-test "Tilde = home directory"
  (let ((file1 (make-instance 'nfiles:file :base-path "~/[foo")) ; REVIEW: OK to use string here?
        (file2 (make-instance 'nfiles:file :base-path #p"~/\\[bar")))
    (is (nfiles:expand file1)
        (uiop:ensure-pathname (uiop:strcat (namestring (user-homedir-pathname)) "[foo"))
        :test 'uiop:pathname-equal)
    (is (nfiles:expand file2)
        (uiop:merge-pathnames* #p"\\[bar" (user-homedir-pathname))
        :test 'uiop:pathname-equal)))

(nfile-test "Current dir change"
  (let* ((file (make-instance 'nfiles:file :base-path #p"foo"))
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
  (let ((file (make-instance 'myapp-config-file :base-path #p"init")))
    (is (nfiles:expand file)
        (uiop:xdg-config-home "myapp/init.lisp")
        :test 'uiop:pathname-equal)))

(nfile-test "Read-only file"
  (let ((file (make-instance 'nfiles:read-only-file :base-path #p"should-not-exist")))
    (setf (nfiles:content file) "foo")
    (is (nfiles:expand file)
        (uiop:merge-pathnames* "should-not-exist" *test-dir*)
        :test #'uiop:pathname-equal)
    (is (find-if (lambda (filename) (search "should-not-exist" filename))
                 (mapcar #'pathname-name (uiop:directory-files *test-dir*)))
        nil)))

(nfile-test "File without base-path / directory as path"
  (let ((test-content "foo")
        (file (make-instance 'nfiles:file)))
    (setf (nfiles:content file) test-content)
    (is (uiop:file-exists-p (nfiles:expand file))
        nil)
    (is (nfiles:content file)
        test-content)))

(subtest "Symlinks"
  (uiop:with-current-directory ((asdf:system-relative-pathname :nfiles "test-data"))

    (let ((file (make-instance 'nfiles:file :base-path #p"link-to-dummy")))
      (is (nfiles:expand file)
          (uiop:ensure-pathname "dummy" :truenamize t))
      (let ((content (nfiles:content file)))
        (setf (nfiles:content file) content)
        (is (nfiles:expand (make-instance 'nfiles:file :base-path #p"link-to-dummy"))
            (uiop:ensure-pathname "dummy" :truenamize t))))))

(nfile-test "Simple write"
  (let ((file (make-instance 'nfiles:file :base-path #p"foo"))
        (test-content "Hello world!"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (is (alexandria:read-file-into-string (nfiles:expand file))
        test-content)
    (is (nfiles:content file)
        test-content)))

(nfile-test "Parent creation"
  (let ((file (make-instance 'nfiles:file :base-path #p"foo/bar"))
        (test-content "Hello world!"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (is (alexandria:read-file-into-string (nfiles:expand file))
        test-content)))

(nfile-test "Preserve attributes"
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
      (is (nfiles:permissions (nfiles:expand file))
          permissions))))

(nfile-test "Read non-existing file"
  (let ((file (make-instance 'nfiles:file :base-path #p"bar")))
    (is (nfiles:content file)
        nil)))

(defclass* counter-file (nfiles:file)
    ((write-count
      0
      :type unsigned-byte)
     (read-count
      0
      :type unsigned-byte))
    (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:write-file ((profile nfiles:profile) (file counter-file) &key)
  (incf (write-count file))
  (call-next-method))

(defmethod nfiles:read-file ((profile nfiles:profile) (file counter-file) &key)
  (incf (read-count file))
  (call-next-method))

(nfile-test "Cache"
  (let ((file1 (make-instance 'counter-file :base-path #p"foo"))
        (file2 (make-instance 'counter-file :base-path #p"bar"))
        (test-content "Hello world!")
        (test-content2 "Hello new world!"))

    (is (nfiles:content file1) nil)
    (is (read-count file1) 0)
    (is (write-count file1) 0)
    (bt:join-thread (setf (nfiles:content file1) test-content))
    (is (read-count file1) 0)
    (is (write-count file1) 1)

    (is (nfiles:content file1)
        test-content)
    (is (read-count file1) 0)
    (is (write-count file1) 1)

    (alexandria:write-string-into-file test-content (nfiles:expand file2))
    (is (nfiles:content file2)
        test-content)
    (is (read-count file2) 1)
    (is (write-count file2) 0)
    (sleep 1)      ; Need to sleep 1s because time resolution is to the second.
    (bt:join-thread (setf (nfiles:content file2) test-content2))
    (is (nfiles:content file2) test-content2)
    (is (read-count file2) 1)
    (is (write-count file2) 1)))

(nfile-test "Cache external modification"
  (let ((file1 (make-instance 'nfiles:file :base-path #p"foo"))
        (file2 (make-instance 'nfiles:file :base-path #p"foo"))
        (test-content "Hello world!")
        (test-content2 "Hello altered world!")
        (test-content3 "Hello imposing world!"))
    (bt:join-thread (setf (nfiles:content file1) test-content))
    (sleep 1)       ; Need to sleep 1s because time resolution is to the second.
    (alexandria:write-string-into-file test-content2 (nfiles:expand file1)
                                       :if-exists :supersede)
    (is-error (nfiles:content file1)
              nfiles:external-modification)
    (is-error (nfiles:content file1)
              nfiles:external-modification)

    (setf (nfiles:on-external-modification file1) 'nfiles:reload)
    (setf (nfiles:on-external-modification file2) 'nfiles:reload)
    (is (nfiles:content file1)
        test-content2)
    (is (nfiles:content file2)
        test-content2)

    (bt:join-thread (setf (nfiles:content file1) test-content3))
    (sleep 1)       ; Need to sleep 1s because time resolution is to the second.
    (alexandria:write-string-into-file test-content2 (nfiles:expand file1)
                                       :if-exists :supersede)
    (setf (nfiles:on-external-modification file1) 'nfiles:overwrite)
    (setf (nfiles:on-external-modification file2) 'nfiles:overwrite)

    (is (nfiles:content file1)
        test-content3)
    (is (nfiles:content file2)
        test-content3)

    ;;  We wait until we are done writing, because we don't have access to the
    ;;  thread.  TODO: Fix it?
    (sleep 1)
    (is (alexandria:read-file-into-string (nfiles:expand file1))
        test-content3)))

(nfile-test "Cache invalidation"
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
    (is (nfiles:content file2)
        test-content)
    (is (nfiles:content file2 :force-read t)
        test-content2)
    (is (nfiles:content file1)
        test-content2)))

(nfile-test "Skip useless writes"
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
    (is (nfiles::worker (nfiles::cache-entry file)) nil)
    (is (write-count file) 2)
    (is (nfiles:content file) (format nil "~a: ~a" test-content (1- limit)))))


(nfile-test "Deserialization error (forward)"
  (let ((corrupted-path "corrupt.lisp"))
    (alexandria:write-string-into-file "(" corrupted-path)
    (let* ((errors 0)
           (corrupted-file (make-instance 'nfiles:lisp-file :base-path #p"corrupt"
                                          :read-handler (lambda (c)
                                                          (incf errors)
                                                          (invoke-restart 'nfiles::forward-condition c)))))
      (multiple-value-bind (value error)
          (nfiles:content corrupted-file)
        (is value nil)
        (is-condition error 'end-of-file))
      (is errors 1)
      (multiple-value-bind (value error)
          (nfiles:content corrupted-file)
        (is value nil)
        (is-condition error 'end-of-file))
      (is errors 2)
      (ok (uiop:file-exists-p (nfiles:expand corrupted-file)))
      (setf (nfiles:on-deserialization-error corrupted-file) 'nfiles:delete)
      (nfiles:content corrupted-file)
      (is (uiop:file-exists-p (nfiles:expand corrupted-file))
          nil))))

;; When SBCL threads abort, `bt:join-thread' errors out.  This is not the case
;; in all compilers though.
#+sbcl
(nfile-test "Deserialization error (abort)"
  (let ((corrupted-path "corrupt.lisp"))
    (alexandria:write-string-into-file "(" corrupted-path)
    (let* ((errors 0)
           (corrupted-file (make-instance 'nfiles:lisp-file :base-path #p"corrupt"
                                          :read-handler (lambda (c)
                                                          (declare (ignore c))
                                                          (incf errors)
                                                          (abort)))))
      (is (nfiles:content corrupted-file) nil)
      (is errors 1)
      (is (nfiles:content corrupted-file) nil)
      (is errors 2)
      (ok (uiop:file-exists-p (nfiles:expand corrupted-file)))
      (setf (nfiles:on-deserialization-error corrupted-file) 'nfiles:delete)
      (nfiles:content corrupted-file)
      (is (uiop:file-exists-p (nfiles:expand corrupted-file))
          nil))))

(nfile-test "Backup"
  (let ((corrupted-path "corrupt.lisp"))
    (alexandria:write-string-into-file "(" corrupted-path)
    (let ((corrupted-file (make-instance 'nfiles:lisp-file :base-path #p"corrupt"
                                         :on-deserialization-error 'nfiles:backup)))
      (is (nfiles:content corrupted-file) nil)
      (ok (find-if (lambda (filename) (search "-backup" filename))
                   (mapcar #'pathname-name (uiop:directory-files *test-dir*)))))))

(defclass* nil-file (nfiles:virtual-file)
    ())
(defmethod nfiles:resolve ((profile nfiles:profile) (file nil-file))
  #p"")

(nfile-test "with-paths"
  (let ((file1 (make-instance 'nfiles:file))
        (file2 (make-instance 'nfiles:file :base-path "alt"))
        (nil-file (make-instance 'nil-file)))
    (is
     (nfiles:with-paths ((path1 file1)
                         (path2 file2))
       (list path1 path2))
     (list (nfiles:expand file1)
           (nfiles:expand file2)))
    (is
     (nfiles:with-paths ((nil-path nil-file)
                         (not-evaluated (make-instance 'nfiles:file :base-path (error "Should not reach here"))))
       not-evaluated)
     nil)))

(defclass* slow-file (nfiles:file)
  ()
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:read-file ((profile nfiles:profile) (file slow-file) &key)
  (sleep 1)
  (call-next-method))

(nfile-test "Async read test"
  (let ((file (make-instance 'slow-file :base-path #p"slow"))
        (test-content "Slow world"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (ok (uiop:file-exists-p (nfiles:expand file)))
    (nfiles::clear-cache)
    (multiple-value-bind (result maybe-thread)
        (nfiles:content file :wait-p nil)
      (is result nil)
      (ok (bt:threadp maybe-thread)))
    (is (nfiles:content file)
        test-content)))

(nfile-gpg-test "GPG test"
  (let ((file (make-instance 'nfiles:gpg-file :base-path #p"fog.gpg"))
        (test-content "Cryptic world"))
    (bt:join-thread (setf (nfiles:content file) test-content))
    (ok (uiop:file-exists-p (nfiles:expand file)))
    #+sbcl
    (is-error (alexandria:read-file-into-string (nfiles:expand file))
              'error)
    #+ccl
    (isnt (alexandria:read-file-into-string (nfiles:expand file))
          test-content)
    (nfiles::clear-cache)
    (let ((synonym-file (make-instance 'nfiles:gpg-file :base-path #p"fog.gpg")))
      (is (nfiles:content synonym-file) test-content))))

(nfile-gpg-test "GPG Backup"
  (let ((corrupted-file (make-instance 'nfiles:gpg-file :base-path #p"corrupt.lisp.gpg")))
    (bt:join-thread (setf (nfiles:content corrupted-file) "("))
    (ok (uiop:file-exists-p "corrupt.lisp.gpg"))
    ;; Clear the cache so that next file tries reading the corrupted file.
    (nfiles::clear-cache)
    (let ((corrupted-lisp-file (make-instance 'nfiles:gpg-lisp-file :base-path #p"corrupt.gpg"
                                              :on-read-error 'nfiles:backup)))
      (is (nfiles:expand corrupted-lisp-file) (uiop:ensure-pathname "corrupt.lisp.gpg" :truenamize t))
      (is (nfiles:content corrupted-lisp-file) nil)
      (ok (find-if (lambda (filename) (search "-backup" filename))
                   (mapcar #'pathname-name (uiop:directory-files *test-dir*)))))))

(defclass* broken-write-file (nfiles:gpg-lisp-file)
  ((appended-value 1))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nfiles:write-file ((profile nfiles:profile) (file broken-write-file) &key destination)
  (nfiles/gpg:with-gpg-file (stream destination :direction :output)
    (nfiles:serialize profile file stream))
  (print (/ 1 (appended-value file))))

(nfile-gpg-test "Staged GPG file with preserved permissions"
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
      (is-error (bt:join-thread (setf (nfiles:content file) new-content))
                'error)
      (is (nfiles:content file) new-content)
      (is (uiop:safe-file-write-date (nfiles:expand file)) last-write-date)
      (is (nfiles:permissions (nfiles:expand file))
          permissions))))

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

(nfile-test "Remote file test"
  (let ((file (make-instance 'remote-counter-file
                             :base-path #p"local-dummy"
                             :url *remote-file-url*))
        (test-content "Dummy file."))
    (is (nfiles:content file)
        test-content)
    (ok (uiop:file-exists-p (nfiles:expand file)))
    (is (download-count file) 1)
    (nfiles::clear-cache)
    (is (nfiles:content file)
        test-content)
    (is (download-count file) 1)))

(nfile-test "Remote file in-memory"
  (let ((file (make-instance 'remote-counter-file
                             :base-path #p""
                             :url *remote-file-url*))
        (test-content "Dummy file."))
    (is (nfiles:content file)
        test-content)
    (is (uiop:file-exists-p (nfiles:expand file))
        nil)
    (is (download-count file) 1)
    (is (nfiles:content file)
        test-content)
    (is (download-count file) 1)))

(nfile-test "Remote file test: always download"
  (let ((file (make-instance 'remote-counter-file
                             :base-path #p""
                             :url *remote-file-url*))
        (test-content "Dummy file."))
    (is (nfiles:content file)
        test-content)
    (is (uiop:file-exists-p (nfiles:expand file))
        nil)
    (is (download-count file) 1)
    (nfiles::clear-cache)
    (is (nfiles:content file)
        test-content)
    (is (download-count file) 2)))

(nfile-test "Remote file checksum test"
  (let* ((test-content "Dummy file.")
         (file (make-instance 'remote-counter-file
                              :read-handler (lambda (c)
                                              (invoke-restart 'nfiles::forward-condition c))
                              :base-path #p"local-dummy"
                              :url *remote-file-url*
                              :checksum (write-to-string (length test-content)))))
    (is (nfiles:content file)
        test-content))
  (let* ((test-content "Dummy file.")
         (file (make-instance 'remote-counter-file
                              :read-handler (lambda (c)
                                              (invoke-restart 'nfiles::forward-condition c))
                              :base-path #p"local-dummy2"
                              :url *remote-file-url*
                              :checksum (write-to-string (1+ (length test-content))))))
    (multiple-value-bind (value error)
        (nfiles:content file)
      (is value nil)
      (is-error error nfiles:invalid-checksum)
      (is (download-count file) 1))
    (multiple-value-bind (value error)
        (nfiles:content file)
      (is value nil)
      (is-error error nfiles:invalid-checksum)
      (is (download-count file) 2)))
  (let* ((test-content "Dummy file.")
         (file (make-instance 'remote-counter-file
                              :read-handler (lambda (c)
                                              (invoke-restart 'nfiles::forward-condition c))
                              :on-invalid-checksum 'nfiles:discard
                              :base-path #p"local-dummy3"
                              :url *remote-file-url*
                              :checksum (write-to-string (1+ (length test-content))))))
    (multiple-value-bind (value error)
        (nfiles:content file)
      (is value nil)
      (is (download-count file) 1))
    (setf (nfiles:checksum file) (write-to-string (length test-content)))
    (is (nfiles:content file)
        test-content)
    (is (download-count file) 2)))

(nfile-test "Remote file auto-update test"
  (let* ((test-content "Remote file.")
         (test-content2 "Altered file.")
         (test-content3 "Changed file.")
         (remote-file "remote.file")
         (file (make-instance 'remote-counter-file
                              :base-path #p"local-dummy"
                              :url (quri.uri.file:make-uri-file :path remote-file))))
    (alexandria:write-string-into-file test-content remote-file :if-exists :supersede)
    (is (nfiles:content file)
        test-content)
    (alexandria:write-string-into-file test-content2 remote-file :if-exists :supersede)
    (is (nfiles:content file)
        test-content)
    (is (nfiles:content file
                        :force-update t)
        test-content2)
    (alexandria:write-string-into-file test-content3 remote-file :if-exists :supersede)
    (setf (nfiles:update-interval file) 1)
    (is (nfiles:content file)
        test-content2)
    (sleep 2)
    (is (nfiles:content file)
        test-content3)))

(finalize)
