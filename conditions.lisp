;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nfiles)

(export-always 'external-modification)
(define-condition external-modification (error)
  ((path :initarg :path))
  (:documentation "Modification of the file that NFiles is not responsible for.")
  (:report (lambda (c stream)
             (format stream "External modification on file ~s." (slot-value c 'path)))))

(export-always 'process-error)
(define-condition process-error (uiop:subprocess-error)
  ((message :initarg :message))
  (:documentation "Error happening when `uiop:run-program' exits abnormally.
Wrapper around `uiop:subprocess-error'.")
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
  (:documentation "Checksum of the file content doesn't match the initially provided one.")
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
  (:documentation "Problem when fetching remote file.")
  (:report (lambda (c stream)
             (format stream "URL ~s could not be fetched: ~a"
                     (slot-value c 'url)
                     (slot-value c 'message)))))
