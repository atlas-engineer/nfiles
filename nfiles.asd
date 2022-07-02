;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nfiles"
  :version "0.4.2"
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
                    (symbol-call :lisp-unit2 :run-tests :package :nfiles/tests
                                 :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
