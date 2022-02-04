;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nfiles)

(export-always 'external-modification)
(define-condition external-modification (error)
  ((path :initarg :path))
  (:report (lambda (c stream)
             (format stream "External modification on file ~s." (slot-value c 'path)))))
