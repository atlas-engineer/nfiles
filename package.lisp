;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package nfiles
  (:use #:common-lisp)
  (:import-from #:hu.dwim.defclass-star
                #:defclass*)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:import-from #:trivial-types
                #:pathname-designator))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nfiles)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nfiles)
  (trivial-package-local-nicknames:add-package-local-nickname :class* :hu.dwim.defclass-star :nfiles))
