;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nfiles"
  :version "1.1.4"
  :description "Manage file persistence and loading."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nfiles"
  :bug-tracker "https://github.com/atlas-engineer/nfiles/issues"
  :source-control (:git "https://github.com/atlas-engineer/nfiles.git")
  :license "BSD 3-Clause"
  :depends-on (alexandria
               #-(and sbcl (not android))
               iolib/os
               nclasses
               quri
               serapeum
               trivial-garbage
               trivial-package-local-nicknames
               trivial-types)
  :components ((:file "pathname-helpers")
               (:file "package")
               (:file "conditions")
               (:file "gpg")
               (:file "nfiles"))
  :in-order-to ((test-op (test-op "nfiles/tests"))))

(defsystem "nfiles/tests"
  :depends-on ("nfiles" "lisp-unit2")
  :pathname "tests/"
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :nfiles/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))
