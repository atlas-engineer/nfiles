;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nfiles"
  :version "0.2.2"
  :description "Manage file persistence and loading."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nfiles"
  :license "BSD 3-Clause"
  :in-order-to ((test-op (test-op "nfiles/tests")))
  :depends-on (alexandria
               hu.dwim.defclass-star
               #-sbcl
               iolib/os
               serapeum
               trivial-garbage
               trivial-package-local-nicknames
               trivial-types)
  :components
  ((:file "package")
   (:file "conditions")
   (:file "pathname-helpers")
   (:file "gpg")
   (:file "nfiles")))

(defsystem "nfiles/tests"
  :depends-on (nfiles prove)
  :perform (test-op (op c)
                    (symbol-call :prove :run "tests/")))
