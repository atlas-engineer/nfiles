;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package nfiles
  (:use #:common-lisp #:nfiles/pathname)
  (:reexport #:nfiles/pathname)
  (:shadow #:delete)                    ; TODO: Rename (and unshadow) with 2.0.0.
  (:import-from #:nclasses
                #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:import-from #:trivial-types
                #:pathname-designator)
  (:documentation "The main data structures are `nfiles:file' and `nfiles:profile'.
Call `nfiles:expand' to return the final `nfiles:file' path.
Call `nfiles:content' (setf-able) to get the `nfiles:file' content.

A basic session:

    (defvar *config-file* (make-instance 'nfiles:config-file :base-path #p\"my-app/init.lisp\"))

    (nfiles:expand *config-file*)
    ; => #P\"/home/johndoe/.config/my-app/init.lisp\"

    (setf (nfiles:content *config-file*) \"Hello file!\") ; The file is written to disk.

    (nfiles:content *config-file*)
    ; => \"Hello file!\"

The following convenience macro ensures the file is updated when done with the
body:

    (nfiles:with-file-content (content *config-file*)
      (format t \"Length: ~a~%\" (length content))
      (setf content (serapeum:string-replace \"file\" content \"config\")))

The `nfiles:with-paths' helper allows for let-style bindings of the expanded paths:

    (let ((file1 (make-instance 'nfiles:file))
          (file2 (make-instance 'nfiles:file :base-path #p\"alt\")))
      (nfiles:with-paths ((path1 file1)
                          (path2 file2))
        (list path1 path2)))

Specialize `nfiles:resolve' to configure how a file path is expanded depending
on the file type and the `nfiles:profile'.

The content serialization and deserialization can be specialized via the
`nfiles:serialize' and `nfiles:deserialize' methods.

The file reading and writing can be specialized via the `nfiles:read-file' and
`nfiles:write-file' methods.  These specializations are in charge for calling
the (de)serialization methods.



A `nfiles:remote-file' works the same but needs some specialization to handle
the remote fetching and checksum validation:

    (defmethod nfiles:fetch ((profile nfiles:profile) (file remote-counter-file) &key)
      (dex:get (nfiles:url file)))

    ;; Optional:
    (defmethod nfiles:check ((profile nfiles:profile) (file remote-counter-file) content &key)
      (let ((path (nfiles:expand file)))
        (ironclad:byte-array-to-hex-string
         (ironclad:digest-file :sha3 path))))

    (let ((file (make-instance 'nfiles:remote-file
                               ;; The URL to download from if the file is not found on disk.
                               :url (quri:uri \"https://example.org\")
                               ;; Without base-path, the file won't be saved to disk.
                               :base-path #p\"/tmp/index.html\"
                               :checksum \"794df316afac91572b899b52b54f53f04ef71f275a01c44b776013573445868c95317fc4a173a973e90addec7792ff8b637bdd80b1a6c60b03814a6544652a90\")))
      ;; On access, file is automatically downloaded if needed and the checksum is verified:
      (nfiles:content file)
      ;; ...
      )



A word of caution: sometimes you may need the handle both the raw form and the
deserialized form.  Then what should `nfiles:content' return?

In this use case, your class has 2 semantic values: that of carrying the raw
form and that of being an `nfiels:file' which carries the deserialized form.
To keep things simple, use a dedicated slot to store the raw form.
For instance `nfiles:remote-file' has a `nfiles:url-content' slot.  If your file
is stored remotely, a `nfiles:remote-file' gives access to both forms.



Also avoid calling `nfiles:content' (or its setf-function) in the
`nfiles:read-file' - `nfiles:deserialize' / `nfiles:write-file' -
`nfiles:serialize' pipeline as it would make it horribly confusing to the
user (and probably break the logic)."))

(uiop:define-package nfiles/gpg
  (:use #:common-lisp)
  (:import-from #:nclasses
                #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:import-from #:trivial-types
                #:pathname-designator)
  (:documentation "A thin wrapper around the GPG command line tool.
Do not expect too much from it."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (package '(:nfiles/gpg :nfiles))
    (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria package)
    (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum package)))
