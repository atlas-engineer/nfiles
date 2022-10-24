;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nfiles"
  :version "1.0.0"
  :description "Manage file persistence and loading."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nfiles"
  :license "BSD 3-Clause"
  :in-order-to ((test-op (test-op "nfiles/tests")))
  :depends-on (alexandria
               hu.dwim.defclass-star
               #-sbcl
               iolib/os
               quri
               serapeum
               trivial-garbage
               trivial-package-local-nicknames
               trivial-types)
  :components
  ((:file "pathname-helpers")
   (:file "package")
   (:file "conditions")
   (:file "gpg")
   (:file "nfiles")))

(defsystem "nfiles/tests"
  :depends-on (nfiles lisp-unit2)
  :pathname "tests/"
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (let* ((*debugger-hook* nil)
                           (test-results (symbol-call :lisp-unit2 :run-tests
                                                      :package :nfiles/tests
                                                      :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
                      (when (or
                             (uiop:symbol-call :lisp-unit2 :failed test-results)
                             (uiop:symbol-call :lisp-unit2 :errors test-results))
                        ;; Arbitrary but hopefully recognizable exit code.
                        (quit 18)))))
