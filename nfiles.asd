;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nfiles"
  :version "1.0.0"
  :description "Manage file persistence and loading."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nfiles"
  :license "BSD 3-Clause"
  :in-order-to ((test-op (test-op "nfiles/tests")
                         ;; Temporarily disabled until PR is merged
                         ;; https://github.com/atlas-engineer/nfiles/issues/14
                         ;; (test-op "nfiles/tests/compilation")
                         ))
  :depends-on (alexandria
               hu.dwim.defclass-star
               #-(and sbcl (not android))
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

(defsystem "nfiles/submodules"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-submodule-system)

(defsystem "nfiles/tests"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-test-system
  :depends-on (nfiles)
  :targets (:package :nfiles/tests)
  :components ((:file "tests/tests")))

(defsystem "nfiles/tests/compilation"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-compilation-test-system
  :depends-on (nfiles)
  :packages (:nfiles))
