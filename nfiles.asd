;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nfiles"
  :version "0.0.0"
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nfiles"
  :license "BSD 3-Clause"
  :in-order-to ((test-op (test-op "nfiles/tests")))
  :depends-on (alexandria
               calispel
               hu.dwim.defclass-star
               serapeum
               trivial-garbage)
  :components
  ((:file "package")
   (:file "nfiles")))

(defsystem "nfiles/tests"
  :depends-on (nfiles prove)
  :perform (test-op (op c)
                    (nyxt-run-test c "tests/")))
