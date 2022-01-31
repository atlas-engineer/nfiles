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
                #:pathname-designator)
  (:documentation "The main data structures are `file' and `profile'.
Call `expand' to return the final `file' path.
Call `content' (setf-able) to get the `file' content.

Specialize `resolve' to configure how a file path is expanded depending on the
file type and the `profile'.

The content serialization and deserialization can be specialized via the
`serialize' and `deserialize' methods.

The file reading and writing can be specialized via the `file-read' and
`file-write' methods."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nfiles)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nfiles)
  (trivial-package-local-nicknames:add-package-local-nickname :class* :hu.dwim.defclass-star :nfiles))

(uiop:define-package nfiles/gpg
  (:use #:common-lisp)
  (:import-from #:hu.dwim.defclass-star
                #:defclass*)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:import-from #:trivial-types
                #:pathname-designator)
  (:documentation "A thin wrapper around the GPG command line tool."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nfiles/gpg)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nfiles/gpg)
  (trivial-package-local-nicknames:add-package-local-nickname :class* :hu.dwim.defclass-star :nfiles/gpg))
