;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nfiles)

(export-always 'external-modification)
(define-condition external-modification (error)
  ((path :initarg :path))
  (:report (lambda (c stream)
             (format stream "External modification on file ~s." (slot-value c 'path)))))

(export-always 'process-error)
(define-condition process-error (uiop:subprocess-error)
  ((message :initarg :message))
  (:report (lambda (c stream)
             (let ((*print-pretty* nil))
               (format stream "Command~%~s~%failed with~%~s"
                       (uiop:subprocess-error-command c)
                       (slot-value c 'message))))))

(define-condition read-error (error)
  ()
  (:documentation "This condition is used internally to cancel a file read."))

(export-always 'invalid-checksum)
(define-condition invalid-checksum (error)
  ((path :initarg :path)
   (wanted-checksum :initarg :wanted-checksum)
   (wrong-checksum :initarg :wrong-checksum))
  (:report (lambda (c stream)
             (let ((*print-pretty* nil))
               (format stream "File ~s expected checksum~%~s~%  but has~%~s"
                       (slot-value c 'path)
                       (slot-value c 'wanted-checksum)
                       (slot-value c 'wrong-checksum))))))

(export-always 'fetch-error)
(define-condition fetch-error (error)
  ((url :initarg :url)
   (message :initarg :message))
  (:report (lambda (c stream)
             (format stream "URL ~s could not be fetched: ~a"
                     (slot-value c 'url)
                     (slot-value c 'message)))))
