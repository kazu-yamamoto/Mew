;;; mew-ext.el --- Message/External-Body support for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 13, 1996

;;; Code:

(require 'mew)
(eval-when-compile
  (cond
;;   ((mew-which-el "efs")
;;    (require 'efs))
   ((mew-which-el "ange-ftp")
    (require 'ange-ftp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables for encoding
;;;

(defvar mew-ext-default-access-type "anon-ftp")

(defvar mew-ext-ftp-server-list
  '("ftp.example.org" "ftp.example.jp"))

(defvar mew-ext-encode-switch
  '(("ftp"         . mew-ext-encode-ftp)
;;    ("tftp"        . mew-ext-encode-tftp)
    ("anon-ftp"    . mew-ext-encode-anon-ftp)
    ("local-file"  . mew-ext-encode-local-file)
    ("mail-server" . mew-ext-encode-mail-server)
    ("url"         . mew-ext-encode-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Variables for decoding
;;;

(defvar mew-ext-switch
  '(("ftp"         mew-ext-ftp         mew-ext-ftp-ext)
    ("tftp"        mew-ext-tftp        nil)
    ("anon-ftp"    mew-ext-anon-ftp    mew-ext-anon-ftp-ext)
    ("mail-server" mew-ext-mail-server mew-ext-mail-server-ext)
    ("url"         mew-ext-url         mew-ext-url-ext))) ;; RFC 2017

(defun mew-ext-get-func (acc-type)
  (nth 1 (mew-assoc-match acc-type mew-ext-switch 0)))

(defun mew-ext-get-func-ext (acc-type)
  (nth 2 (mew-assoc-match acc-type mew-ext-switch 0)))

(defvar mew-ext-include-switch
  '(("local-file" mew-ext-include-local-file)))

(defun mew-ext-include-get-func (acc-type)
  (nth 1 (mew-assoc-case-equal acc-type mew-ext-include-switch 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; environment variables
;;;

(cond
;;  ((mew-which-el "efs")
;;   (defun mew-ext-file-name-completion (file path)
;;     (require 'efs)
;;     (let ((efs-tmp-name-template mew-temp-file))
;;       (efs-file-name-completion file path)))
;;   (defun mew-ext-file-name-all-completions (file path)
;;     (require 'efs)
;;     (let ((efs-tmp-name-template mew-temp-file))
;;       (efs-file-name-all-completions file path)))
;;   (defun mew-ext-expand-dir (host user dir)
;;     (require 'efs)
;;     (let ((efs-tmp-name-template mew-temp-file) exp)
;;       (setq exp (efs-expand-file-name (format "/%s@%s:%s" user host dir)))
;;       (if (string-match ".*:\\(.*\\)$" exp)
;;	 (match-string 1 exp))))
;;   (defun mew-ext-copy-file-internal (remote local passwd)
;;     (require 'efs)
;;     (let ((efs-tmp-name-template mew-temp-file)
;;	  (efs-generate-anonymous-password passwd)
;;	  (parsed (efs-ftp-path remote)))
;;       (efs-copy-file-internal remote parsed local nil
;;			      nil nil nil nil t 'image))))
 ((mew-which-el "ange-ftp")
  (defun mew-ext-file-name-completion (file path)
    (require 'ange-ftp)
    (let ((ange-ftp-tmp-name-template mew-temp-file))
      (ange-ftp-file-name-completion file path)))
  (defun mew-ext-file-name-all-completions (file path)
    (require 'ange-ftp)
    (let ((ange-ftp-tmp-name-template mew-temp-file))
      (ange-ftp-file-name-all-completions file path)))
  (defun mew-ext-expand-dir (host user dir)
    (require 'ange-ftp)
    (let ((ange-ftp-tmp-name-template mew-temp-file) exp)
      (setq exp (ange-ftp-expand-file-name (format "/%s@%s:%s" user host dir)))
      (if (string-match ".*:\\(.*\\)$" exp)
	  (match-string 1 exp))))
  (defun mew-ext-copy-file-internal (remote local passwd)
    (require 'ange-ftp)
    (let ((ange-ftp-tmp-name-template mew-temp-file)
	  (ange-ftp-generate-anonymous-password passwd))
      (ange-ftp-copy-file-internal remote local t nil nil nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encode
;;;

(defun mew-attach-external-body ()
  "Specify external body."
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Cannot insert external-body here")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   file filepath ct)
      ;; attachdir / {subdir/} dir
      (if (not (string= subdir ""))
	  (setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir 1 nil mew-ext-suffix))
      (if (file-exists-p filepath)
	  (message "Cannot make a file for external-body, sorry")
	(setq file (file-name-nondirectory filepath))
	(setq ct (mew-ext-encode filepath))
	(setq mew-encode-syntax
	      (mew-syntax-insert-entry
	       mew-encode-syntax
	       nums
	       (mew-encode-syntax-single file ct)))
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-create-content-id ()
  ;; this is not unique if used with very short interval.
  ;; but it's ok
  (format "<%s.%s.%s@%s>" (nth 0 (current-time)) (nth 1 (current-time))
	  (emacs-pid) (system-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-encode (filename)
  (let (ret access-type ct name)
    (with-temp-buffer
      ;;content-header
      (setq access-type (mew-input-general
			 "Access type" mew-ext-encode-switch t
			 mew-ext-default-access-type))
      (setq ret (funcall (cdr (assoc access-type mew-ext-encode-switch))))
      ;;message-header
      (cond
       ((string= access-type "url")
	(setq name "URL")
	(setq ct "Text/Html"))
       ((string= access-type "mail-server")
	(setq name "Mail server's file")
	(setq ct mew-ct-apo))
       (t
	(setq name (file-name-nondirectory
		    (mew-chop (mew-syntax-get-param ret "name"))))
	;; name is quoted
	(if (string= name "")
	    (setq ct mew-ct-apo)
	  (setq ct (or (mew-ctdb-ct (mew-ctdb-by-file name))
		       (mew-content-type (mew-tinfo-get-case)))))))
      (setq ct (mew-input-type "Type for %s (%s): " name ct
			       mew-mime-content-type-list))
      (mew-header-insert mew-ct: ct)
      (mew-header-insert mew-cid: (mew-create-content-id))
      (insert "\n")
      (when (string= access-type "mail-server")
	;;message-body
	(insert (read-string "Input message to the mail-server: "))
	(insert "\n"))
      (write-file filename))
    (cons mew-ct-ext (cons (list "access-type" access-type) ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-encode-ftp ()
  ;; "name" "site" "directory" "mode"
  (let ((mew-ext-host (mew-input-general
		       "FTP server"
		       (if mew-ext-ftp-server-list
			   (mapcar 'list mew-ext-ftp-server-list))
		       nil
		       (car mew-ext-ftp-server-list)))
	mew-ext-user path dir file ret)
    (setq ret (list (list "site" mew-ext-host)))
    (setq mew-ext-user (read-string (format "User name at %s: " mew-ext-host)
				    (mew-user)))
    (setq path (mew-input-rfile "Filename:"))
    (setq file (file-name-nondirectory path))
    (setq dir (file-name-directory path))
    (if (and dir (string-match mew-home dir))
	(setq dir (mew-ext-expand-dir mew-ext-host mew-ext-user dir)))
    (cond
     (dir
      (setq ret (cons (list "directory" dir) ret))
      (setq ret (cons (list "name" file) ret)))
     (t
      (setq ret (cons (list "name" file) ret))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-encode-tftp ()
  ;; xxx not yet
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-encode-anon-ftp ()
  ;; "name" "site" "directory" "mode"
  (let ((mew-ext-user "anonymous")
	(mew-ext-host (mew-input-general
		       "FTP server"
		       (if mew-ext-ftp-server-list
			   (mapcar 'list mew-ext-ftp-server-list))
		       nil
		       (car mew-ext-ftp-server-list)))
	path dir file ret)
    (setq ret (list (list "site" mew-ext-host)))
    (setq path (mew-input-rfile "Filename:"))
    (setq file (file-name-nondirectory path))
    (setq dir (file-name-directory path))
    (if (and dir (string-match mew-home dir))
	(setq dir (mew-ext-expand-dir mew-ext-host mew-ext-user dir)))
    (cond
     (dir
      (setq ret (cons (list "directory" dir) ret))
      (setq ret (cons (list "name" file) ret)))
     (t
      (setq ret (cons (list "name" file) ret))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-encode-local-file ()
  ;; "name" "site"
  (let ((file (mew-draft-input-file-name "File name: ")))
    (list (list "name" (expand-file-name file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-encode-mail-server ()
  ;; "server" "subject"
  (let (server subject)
    (setq server (car (mew-input-address "Server address: ")))
    (setq subject (read-string (concat mew-subj: " ")))
    (list (list "server" server)
	  (list "subject" subject))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-encode-url ()
  ;; "url"
  (let ((url (read-string "URL: ")))
    (list (list "url" url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decode
;;;

;;
;; exclude
;;

(defun mew-mime-external-body (cache begin end &optional params)
  ;; message-buffer
  (let* ((access-type (mew-syntax-get-param params "access-type"))
	 (func (mew-ext-get-func access-type)))
    (if (and (symbolp func) (fboundp func))
	(funcall func cache begin end params))))

(defun mew-mime-external-body-ext (cache begin end &optional params)
  (let* ((access-type (mew-syntax-get-param params "access-type"))
	 (func (mew-ext-get-func-ext access-type)))
    (if (and (symbolp func) (fboundp func))
	(funcall func cache begin end params))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-ftp (cache begin end &optional params)
  (mew-elet
   (let* ((site (mew-syntax-get-param params "site"))
	  (directory (mew-syntax-get-param params "directory"))
	  (name (mew-syntax-get-param params "name"))
	  (size (mew-syntax-get-param params "size"))
	  filepath)
     (if directory
	 (setq filepath (mew-concat-folder directory name))
       (setq filepath name))
     (setq filepath (mew-remove-drive-letter filepath))
     (mew-erase-buffer)
     (insert " ####### ####### ######  \n"
	     " #          #    #     # \n"
	     " #          #    #     # \n"
	     " #####      #    ######  \n"
	     " #          #    #       \n"
	     " #          #    #       \n"
	     " #          #    #       \n"
	     "\n\n")
     (insert "You can get the message content by FTP\n\n")
     (mew-insert "Site:\t%s\n" site)
     (mew-insert "File:\t%s\n" filepath)
     (mew-insert "Size:\t%s bytes\n" size)
     (insert "\n")
     (mew-insert-manual
      "To get this file, type "
      "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"))))

(defun mew-ext-ftp-ext (cache begin end &optional params)
  (let* ((site (mew-syntax-get-param params "site"))
	 (directory (mew-syntax-get-param params "directory"))
	 (name (mew-syntax-get-param params "name"))
	 (getit t) (username "")
	 filepath localfile lfname remotefile)
    (if directory
	(setq filepath (mew-concat-folder directory name))
      (setq filepath name))
    (setq filepath (mew-remove-drive-letter filepath))
    (setq username (read-string (format "User name at %s: " site) (mew-user)))
    (setq remotefile (format "/%s@%s:%s" username site filepath))
    (setq localfile (mew-summary-input-file-name "Save to: " name))
    (setq lfname (file-name-nondirectory localfile))
    (if (file-exists-p localfile)
	(if (y-or-n-p (format "%s exists. Overwrite? " lfname))
	    (mew-delete-file localfile)
	  (setq getit nil)
	  (message "The file was not retrieved")))
    (if getit (mew-ext-copy-file-internal remotefile localfile nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-tftp (cache begin end &optional params)
  (message "Access-type TFTP is not supported yet"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-anon-ftp (cache begin end &optional params)
  (mew-elet
   (let* ((site (mew-syntax-get-param params "site"))
	  (directory (mew-syntax-get-param params "directory"))
	  (name (mew-syntax-get-param params "name"))
	  (size (mew-syntax-get-param params "size"))
	  filepath)
     (if directory
	 (setq filepath (mew-concat-folder directory name))
       (setq filepath name))
     (setq filepath (mew-remove-drive-letter filepath))
     (mew-erase-buffer)
     (insert " Anonymous \n"
	     " ####### ####### ######  \n"
	     " #          #    #     # \n"
	     " #          #    #     # \n"
	     " #####      #    ######  \n"
	     " #          #    #       \n"
	     " #          #    #       \n"
	     " #          #    #       \n"
	     "\n\n")
     (insert "You can get the message content by FTP\n\n")
     (mew-insert "Site:\t%s\n" site)
     (mew-insert "File:\t%s\n" filepath)
     (mew-insert "Size:\t%s bytes\n" size)
     (insert "\n")
     (mew-insert-manual
      "To get this file, type "
      "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"))))

(defun mew-ext-anon-ftp-ext (cache begin end &optional params)
  (let* ((site (mew-syntax-get-param params "site"))
	 (directory (mew-syntax-get-param params "directory"))
	 (name (mew-syntax-get-param params "name"))
	 (getit t)
	 filepath localfile lfname remotefile)
    (if directory
	(setq filepath (mew-concat-folder directory name))
      (setq filepath name))
    (setq filepath (mew-remove-drive-letter filepath))
    (setq remotefile (format "/%s@%s:%s" "anonymous" site filepath))
    (setq localfile (mew-summary-input-file-name "Save to: " name))
    (setq lfname (file-name-nondirectory localfile))
    (if (file-exists-p localfile)
	(if (y-or-n-p (format "%s exists. Overwrite? " lfname))
	    (mew-delete-file localfile)
	  (setq getit nil)
	  (message "The file was not retrieved")))
    (if getit (mew-ext-copy-file-internal remotefile localfile (mew-mail-address)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-mail-server (cache begin end &optional params)
  (mew-elet
   (let* ((server (mew-syntax-get-param params "server"))
	  (size (mew-syntax-get-param params "size")))
     (mew-erase-buffer)
     (insert " #     #    #      ###   #\n"
	     " ##   ##   # #      #    #\n"
	     " # # # #  #   #     #    #\n"
	     " #  #  # #     #    #    #\n"
	     " #     # #######    #    #\n"
	     " #     # #     #    #    #\n"
	     " #     # #     #   ###   #######\n"
	     "\n\n")
     (insert "You can get the message by e-mail\n\n")
     (mew-insert "Server:\t\t%s\n" server)
     (mew-insert "Size:\t%s bytes\n" size)
     (insert "\n")
     ;; picked up source from 'mew-send
     (mew-insert-manual
      "To send this mail, type "
      "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"))))

(defun mew-ext-mail-server-ext (cache begin end &optional params)
  (let* ((server (mew-syntax-get-param params "server"))
	 (subject (mew-syntax-get-param params "subject"))
	 (syntax (mew-ext-decode-message-header cache begin end))
	 (start (mew-syntax-get-begin syntax)))
    (mew-summary-send server nil subject)
    (goto-char (point-max))
    (mew-insert-buffer-substring cache start end)
    (mew-draft-make-message)))

(defun mew-ext-decode-message-header (cache begin end)
  (let (syntax start)
    (with-current-buffer cache
      (save-restriction
	(narrow-to-region begin end)
	(goto-char (point-min))
	(setq syntax (mew-decode-mime-header))
	(setq start (point)))
      (mew-syntax-set-key syntax 'phantom)
      (mew-syntax-set-begin syntax start)
      (mew-syntax-set-end syntax end)
      syntax)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-ext-url (cache begin end &optional params)
  (mew-elet
   (let* ((url (mew-syntax-get-param params "url"))
	  (size (mew-syntax-get-param params "size"))
	  (process-connection-type mew-connection-type1)
	  (syntax (mew-ext-decode-message-header cache begin end))
	  (mct (car (mew-syntax-get-ct syntax)))
	  (spec mew-prog-text/html-ext)
	  (prog (mew-progspec-get-prog spec)))
     (mew-erase-buffer)
     (insert "#     # ######  #\n"
	     "#     # #     # #\n"
	     "#     # #     # #\n"
	     "#     # ######  #\n"
	     "#     # #   #   #\n"
	     "#     # #    #  #\n"
	     " #####  #     # #######\n"
	     "\n\n")
     (mew-insert "URL:\t\t%s\n" url)
     (mew-insert "Content-Type:\t%s\n" mct)
     (mew-insert "Size:\t%s bytes\n" size)
     (mew-insert "Operation:\t%s\n" (or prog "not defined"))
     (insert "\n")
     (mew-insert-manual
      "To operate this URL, type "
      "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"))))

(defun mew-ext-url-ext (cache begin end &optional params)
  (let* ((url (mew-syntax-get-param params "url"))
	 (process-connection-type mew-connection-type1)
	 (spec mew-prog-text/html-ext)
	 (prog (mew-progspec-get-prog spec))
	 (args (mew-progsec-args-convert (mew-progspec-get-args spec) url)))
    (apply 'call-process prog nil nil nil args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; include
;;

(defun mew-ext-include-local-file (params)
  (mew-flet
   (let* ((file (mew-syntax-get-param params "name")))
     (if (file-exists-p file)
	 (mew-insert-file-contents file)))))

(provide 'mew-ext)

;;; Copyright Notice:

;; Copyright (C) 1996-2011 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-ext.el ends here
