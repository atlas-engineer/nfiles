;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defvar *gpg-default-recipient* nil)

(defstruct gpg-uid
  validity
  user-id)

(defstruct gpg-key
  length
  algorithm                             ; See https://tools.ietf.org/html/rfc4880#page-62 for the meaning of the algorithm ID.
  key-id
  creation-date
  expiry-date
  uids
  fingerprint
  keygrip)

(defun gpg-private-keys ()
  "Return list of private `gpg-key's."
  (let* ((entries (delete ""
                          (ppcre:split "\\bsec"
                                       (with-output-to-string (s)
                                         (uiop:run-program (list *gpg-program* "--list-secret-keys" "--with-colons")
                                                           :output s)))
                          :test #'string=))
         (entries (mapcar (lambda (s) (str:concat "sec" s)) entries))
         (entries (mapcar (lambda (s) (str:split +newline+ s :omit-nulls t)) entries))
         (entries (mapcar (lambda (entry) (mapcar (lambda (s) (str:split ":" s)) entry)) entries)))
    (mapcar (lambda (entry)
              (let ((key (first entry))
                    (uids (remove-if (lambda (e) (not (string= "uid" (first e)))) entry)))
                (make-gpg-key
                 :length (parse-integer (third key) :junk-allowed t)
                 :algorithm (fourth key)
                 :key-id (fifth key)
                 :creation-date (ignore-errors (local-time:unix-to-timestamp (parse-integer (sixth key))))
                 :expiry-date (ignore-errors (local-time:unix-to-timestamp (parse-integer (seventh key))))
                 :uids (mapcar (lambda (uid-entry)
                                 (make-gpg-uid
                                  :validity (second uid-entry)
                                  :user-id (nth 9 uid-entry)))
                               uids)
                 :fingerprint (nth 9 (assoc "fpr" entry :test #'string=))
                 :keygrip (nth 9 (assoc "grp" entry :test #'string=)))))
            entries)))

(defun gpg-recipient (file)             ; TODO: Find a proper way to do this.
  "Return the key of FILE's recipient if any, `*gpg-recipient*' otherwise.
As second value the email.
As third value the name."
  (let ((file (uiop:native-namestring file)))
    (if (uiop:file-exists-p file)
        (handler-case
            (let* ((output (sera:lines (with-output-to-string (s)
                                         (uiop:run-program (list *gpg-program* "--decrypt" file)
                                                           :output nil :error-output s))))
                   (first-line-tokens (str:split " " (first output)))
                   (key (let ((key-string (second (member "ID" first-line-tokens :test #'string=))))
                          (if (str:ends-with? "," key-string)
                              (sera:slice key-string 0 -1)
                              key-string)))
                   (second-line (str:trim (second output)))
                   (mail-start (position #\space second-line :from-end t))
                   (mail (str:trim (reduce (lambda (target rep) (str:replace-all rep "" target))
                                           '(">" "<" "\"") :initial-value (subseq second-line mail-start))))
                   (name (str:replace-all "\"" "" (subseq second-line 0 mail-start))))
              (values key mail name))
          (error ()
            *gpg-default-recipient*))
        *gpg-default-recipient*)))

(defun gpg-write (stream gpg-file &optional recipient)
  "Write STREAM to GPG-FILE using RECIPIENT key.
If RECIPIENT is not provided, use default key."
  (let ((native-file (uiop:native-namestring gpg-file)))
    (uiop:run-program
     `(,*gpg-program* "--output" ,native-file
                      ,@(if recipient
                            `("--recipient" ,recipient)
                            '("--default-recipient-self"))
                      "--batch" "--yes" "--encrypt")
     :input stream)))

(defun gpg-write (stream gpg-file &optional recipient)
  "Write STREAM to GPG-FILE using RECIPIENT key.
If RECIPIENT is not provided, use default key."
  (let ((native-file (uiop:native-namestring gpg-file)))
    (uiop:run-program
     `(,*gpg-program* "--output" ,native-file
                      ,@(if recipient
                            `("--recipient" ,recipient)
                            '("--default-recipient-self"))
                      "--batch" "--yes" "--encrypt")
     :input stream)))

(defun call-with-gpg-file (gpg-file options fun)
  "Like `call-with-open-file' but use `gpg' to read and write to file.
OPTIONS are as for `open''s `:direction'.
Other options are not supported.  File is overwritten if it exists, while
nothing is done if file is missing."
  ;; TODO: Support all of `open' options.
  (if (member (getf options :direction) '(:io :input nil))
      (when (uiop:file-exists-p gpg-file)
        (let ((clear-data (with-output-to-string (out)
                            (uiop:run-program
                             (list *gpg-program* "--decrypt" gpg-file)
                             :output out))))
          (with-input-from-string (stream clear-data)
            (prog1                ; TODO: Shouldn't we `unwind-protect' instead?
                (funcall fun stream)
              (when (eq (getf options :direction) :io)
                ;; TODO: Need to handle error when gpg-file key is not available.
                (gpg-write stream gpg-file (gpg-recipient gpg-file)))))))
      (let ((result nil)
            (recipient (or (gpg-recipient gpg-file)
                           (and *browser*
                                (ignore-errors
                                 (gpg-key-key-id
                                  (first (prompt
                                          :prompt "Recipient:"
                                          :sources '(gpg-key-source)))))))))
        (with-input-from-string (in (with-output-to-string (stream)
                                      (setf result (funcall fun stream))))
          (gpg-write in gpg-file recipient))
        result)))
