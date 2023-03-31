;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nfiles/gpg)

(export-always '*gpg-program*)
(defvar *gpg-program* "gpg"
  "The program to use for GPG key management.")

(export-always '*gpg-default-recipient*)
(defvar *gpg-default-recipient* nil
  "The default recipient (likely email) for the keys.")

(define-class gpg-uid ()
  ((validity)
   (user-id))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "User ID for the given GPG key."))

(define-class gpg-key ()
  ((key-length)
   ;; See https://tools.ietf.org/html/rfc4880#page-62 for the meaning of the algorithm ID.
   (algorithm)
   (key-id)
   (creation-date
    :documentation "Stored as in Unix format.")
   (expiry-date
    :documentation "Stored as in Unix format.")
   (uids)
   (fingerprint)
   (keygrip))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Representation of GPG key as given by `*gpg-program*'."))

(defun parse-gpg-secret-keys-output (output-string)
  "Return the list of sections as a list of strings."
  (mapcar (alex:rcurry #'sera:string-join #\newline)
          (let ((result '()))
            (dolist (line (sera:lines output-string))
              (if (sera:string-prefix-p "sec" line)
                  (push (list line) result)
                  (nconc (first result) (list line))))
            (nreverse result))))

(export-always 'gpg-private-keys)
(defun gpg-private-keys ()
  "Return list of private `gpg-key's."
  (let* ((entries (parse-gpg-secret-keys-output
                   (uiop:run-program (list *gpg-program* "--list-secret-keys" "--with-colons")
                                     :output '(:string :stripped t))))
         (entries (mapcar (lambda (s) (delete-if #'uiop:emptyp (sera:lines s))) entries))
         (entries (mapcar (lambda (entry) (mapcar (lambda (s) (sera:split-sequence #\: s)) entry)) entries)))
    (mapcar (lambda (entry)
              (let ((key (first entry))
                    (uids (remove-if (lambda (e) (not (string= "uid" (first e)))) entry)))
                (make-instance 'gpg-key
                 :key-length (parse-integer (third key) :junk-allowed t)
                 :algorithm (fourth key)
                 :key-id (fifth key)
                 :creation-date (ignore-errors (parse-integer (sixth key)))
                 :expiry-date (ignore-errors (parse-integer (seventh key)))
                 :uids (mapcar (lambda (uid-entry)
                                 (make-instance 'gpg-uid
                                  :validity (second uid-entry)
                                  :user-id (nth 9 uid-entry)))
                               uids)
                 :fingerprint (nth 9 (assoc "fpr" entry :test #'string=))
                 :keygrip (nth 9 (assoc "grp" entry :test #'string=)))))
            entries)))

(defun gpg-recipient (file)             ; TODO: Find a proper way to do this.
  "Return the key of FILE's recipient if any, `*gpg-default-recipient*' otherwise.
As second value the email.
As third value the name."
  (let ((file (uiop:native-namestring file)))
    (if (uiop:file-exists-p file)
        (handler-case
            (let* ((output (sera:lines (with-output-to-string (s)
                                         (uiop:run-program (list *gpg-program* "--decrypt" file)
                                                           :output nil :error-output s))))
                   (first-line-tokens (sera:tokens (first output)))
                   (key (let ((key-string (second (member "ID" first-line-tokens :test #'string=))))
                          (if (sera:string-suffix-p "," key-string)
                              (sera:slice key-string 0 -1)
                              key-string)))
                   (second-line (sera:trim-whitespace (second output)))
                   (mail-start (position #\space second-line :from-end t))
                   (mail (sera:trim-whitespace
                          (reduce (lambda (target rep) (sera:string-replace-all rep target ""))
                                  '(">" "<" "\"") :initial-value (subseq second-line mail-start))))
                   (name (sera:string-replace-all "\"" (subseq second-line 0 mail-start) "")))
              (values key mail name))
          (error ()
            *gpg-default-recipient*))
        *gpg-default-recipient*)))

(defun read-new-value (prompt)
  "PROMPT takes no trailing colon nor trailing space."
  (format *query-io* "~a: " prompt)
  (finish-output *query-io*)
  (list (read *query-io*)))

(defun run-program* (command &key input)
  "Like `uiop:run-program' but raise a `nfiles:process-error' on error."
  (multiple-value-bind (output error-output status)
      (uiop:run-program
       command
       :output '(:string :stripped t)
       :error-output '(:string :stripped t)
       :ignore-error-status t
       :input input)
    (if (= 0 status)
        output
        (error 'nfiles:process-error :command command :message error-output))))

(defun gpg-write (stream gpg-file &optional recipient)
  "Write STREAM to GPG-FILE using RECIPIENT key.
If RECIPIENT is not provided, use default key."
  (let ((native-file (uiop:native-namestring gpg-file)))
    (flet ((call-gpg (&optional recipient)
             (run-program* `(,*gpg-program* "--output" ,native-file
                                            ,@(if recipient
                                                  `("--recipient" ,recipient)
                                                  '("--default-recipient-self"))
                                            "--batch" "--yes" "--encrypt")
                           :input stream)))
      (restart-case (call-gpg recipient)

        (use-recipient (new-recipient)
          :report "Set recipient for encryption (a string)"
          :interactive (lambda () (read-new-value "Enter a new recipient (a string)"))
          (call-gpg new-recipient))))))

(defun call-with-gpg-file (gpg-file options fun)
  "Like `call-with-open-file' but use `gpg' to read and write to file.
OPTIONS are as for `open''s `:direction'.
Other options are not supported.  File is overwritten if it exists, while
nothing is done if file is missing."
  ;; TODO: Support all `open' options.
  (if (member (getf options :direction) '(:io :input nil))
      (when (uiop:file-exists-p gpg-file)
        (let ((clear-data (run-program* (list *gpg-program* "--decrypt" (uiop:native-namestring gpg-file)))))
          (with-input-from-string (stream clear-data)
            (prog1                ; TODO: Shouldn't we `unwind-protect' instead?
                (funcall fun stream)
              (when (eq (getf options :direction) :io)
                ;; TODO: Need to handle error when gpg-file key is not available.
                (gpg-write stream gpg-file (gpg-recipient gpg-file)))))))
      (let ((result nil)
            (recipient (or (gpg-recipient gpg-file)
                           *gpg-default-recipient*)))
        (with-input-from-string (in (with-output-to-string (stream)
                                      (setf result (funcall fun stream))))
          (gpg-write in gpg-file recipient))
        result)))

(export-always 'with-gpg-file)
(defmacro with-gpg-file ((var pathname &rest keys) &body body)
  "Trivial wrapper around `call-with-gpg-file'."
  `(call-with-gpg-file ,pathname
                       ',keys
                       (lambda (,var) ,@body)))
