;;; mew-smime.el --- S/MIME for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 12, 2004

;;; Code:

;; for gpgsm only

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; S/MIME program
;;;

(defvar mew-prog-smime "gpgsm")
(defvar mew-smime-ver nil)

(defvar mew-prog-smime-options '("--include-certs" "3" "--status-fd" "1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; S/MIME check
;;;

(defun mew-smime-setup ()
  (if (mew-which-exec mew-prog-smime)
      (setq mew-smime-ver t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; S/MIME signing
;;;

(defvar mew-smime-type "smime-type")
(defvar mew-smime-file-name "smime")
(defvar mew-smime-signature-suffix ".p7s") ;; application/pkcs7-signature
(defvar mew-smime-mime-suffix      ".p7m") ;; application/pkcs7-mime

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Password
;;;

(defun mew-smime-passtag () "GPGSM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Micalg
;;;

(defvar mew-smime-hash-alist
  '(("1"  . "md5")
    ("2"  . "sha1")
    ("3"  . "ripemd160")
    ("5"  . "md2")
    ("6"  . "tiger192")
    ("7"  . "haval-5-160")
    ("8"  . "sha256")
    ("9"  . "sha384")
    ("10" . "sha512")))

(defun mew-smime-get-micalg ()
  (with-temp-buffer
    (insert mew-smime-string)
    (goto-char (point-min))
    (if (re-search-forward "SIG_CREATED [A-Z] [0-9]+ \\([0-9]+\\)" nil t)
	(cdr (assoc (mew-match-string 1) mew-smime-hash-alist))
      "sha1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal variables
;;;

(defvar mew-smime-running nil)
(defvar mew-smime-string nil)
(defvar mew-smime-failure nil)

(defvar mew-smime-decrypt-msg nil)
(defvar mew-smime-sign-msg nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defvar mew-smime-prompt-enter-pass "Enter S/MIME pass phrase: ")

(defun mew-smime-passphrase ()
  (let ((prompt mew-smime-prompt-enter-pass)
	(tag (mew-smime-passtag)))
    (mew-input-passwd prompt tag)))

(defvar mew-smime-msg-enter-pass       "Enter passphrase:")
(defvar mew-smime-msg-bad-pass         ": Bad")

(defvar mew-smime-result-sec-succ "S/MIME decrypted")
(defvar mew-smime-result-pass     "Pass phrase is wrong")

(defun mew-smime-debug (label string)
  (when (mew-debug 'smime)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-smime-process-filter (process string)
  (save-excursion
    ;; sign or decrypt
    (mew-smime-debug "S/MIME filter" string)
    (setq mew-smime-string (concat mew-smime-string string))
    (cond
     ;; pass phrase for sign or decrypt
     ((string-match mew-smime-msg-enter-pass string)
      (process-send-string process (format "%s\n" (mew-smime-passphrase))))

     ;; pass phrases were wrong three times
     ((string-match mew-smime-msg-bad-pass string)
      (setq mew-smime-failure t) ;; password failure
      (mew-passwd-set-passwd (mew-smime-passtag) nil)) ;; cancel anyway

     ((string-match "INV_RECP \\([0-9]*\\) \\([^ \n]*\\)" string)
      (setq mew-smime-failure (cdr (assoc (mew-match-string 1 string) mew-smime-inv-recp-alist)))
      (setq mew-smime-failure (concat mew-smime-failure " for " (mew-match-string 2 string)))))))

(defun mew-smime-process-sentinel (process event)
  (save-excursion
    (let ((decrypted mew-smime-result-sec-succ)
	  (msg ""))
      (if (not mew-smime-failure)
	  (cond
	   ((eq mew-smime-running 'decrypting)
	    (setq mew-smime-decrypt-msg decrypted))
	   ((eq mew-smime-running 'signing)
	    (setq mew-smime-sign-msg nil)))
	(cond
	 ;; sign or decrypt
	 ((eq mew-smime-failure t)
	  (setq msg mew-smime-result-pass))
	 ((stringp mew-smime-failure)
	  (setq msg mew-smime-failure)))
	(cond
	 ((eq mew-smime-running 'decrypting)
	  (setq mew-smime-decrypt-msg msg))
	 ((eq mew-smime-running 'signing)
	  (setq mew-smime-sign-msg msg))))
      (setq mew-smime-running nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking
;;;

(defun mew-smime-verify-check ()
  (let (addr warning result trust ret)
    (goto-char (point-min))
    (if (re-search-forward "EMail=\\(.*\\)" nil t)
	(setq addr (mew-match-string 1)))
    (goto-char (point-min))
    (cond
     ((search-forward "GOODSIG" nil t)
      (setq result "Good S/MIME sign"))
     ((search-forward "EXPSIG" nil t)
      (setq result "Good S/MIME sign")
      (setq warning "signature has expired"))
     ((search-forward "EXPKEYSIG" nil t)
      (setq result "Good S/MIME sign")
      (setq warning "certificate has expired"))
     ((search-forward "REVKEYSIG" nil t)
      (setq result "Good S/MIME sign")
      (setq warning "certificate has been revoked"))
     ((search-forward "BADSIG" nil t)
      (setq result "BAD S/MIME sign"))
     ((search-forward "ERRORSIG" nil t)
      (setq result "BAD S/MIME sign")) ;; xxx
     (t
      (setq result "BAD S/MIME sign")))
    (goto-char (point-min))
    ;; xxx error code check for TRUST_UNDEFINED/NEVER?
    (if (re-search-forward "TRUST_\\([A-Z]*\\)" nil t)
	(setq trust (mew-match-string 1)))
    (setq ret result)
    (if addr (setq ret (concat ret " <" addr ">")))
    (if trust (setq ret (concat ret " " trust)))
    (if warning (setq ret (concat ret " -" warning)))
    ret))

(defvar mew-smime-inv-recp-alist
  '(( "0" . "No specific reason given")
    ( "1" . "Not Found")
    ( "2" . "Ambigious specification")
    ( "3" . "Wrong key usage")
    ( "4" . "Key revoked")
    ( "5" . "Key expired")
    ( "6" . "No CRL known")
    ( "7" . "CRL too old")
    ( "8" . "Policy mismatch")
    ( "9" . "Not a secret key")
    ("10" . "Key not trusted")
    ("11" . "Missing certifciate")))

(defun mew-smime-encrypt-check ()
  (let (ret) ;; this should be nil
    (goto-char (point-min))
    (when (re-search-forward "INV_RECP \\([0-9]*\\) \\([^ \n]*\\)" nil t)
      (setq ret (cdr (assoc (mew-match-string 1) mew-smime-inv-recp-alist)))
      (setq ret (concat ret " for " (mew-match-string 2))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; S/MIME short cut
;;;

(defun mew-smime-sign-message (&optional arg)
  "Sign the entire draft with S/MIME. Input your passphrase."
  (interactive "P")
  (mew-smime-encode-message 'smime-signature arg))

(defun mew-smime-encrypt-message ()
  "Encrypt the entire draft with S/MIME."
  (interactive)
  (mew-smime-encode-message 'smime-encryption))

(defun mew-smime-sign-encrypt-message (&optional arg)
  "Sign then encrypt the entire draft with S/MIME. Input your passphrase."
  (interactive "P")
  (mew-smime-encode-message 'smime-signature-encryption))

(defun mew-smime-encrypt-sign-message (&optional arg)
  "Encrypt then sign the entire draft with S/MIME. Input your passphrase."
  (interactive "P")
  (mew-smime-encode-message 'smime-encryption-signature))

(defun mew-smime-encode-message (type &optional ask-signer)
  (if (null mew-smime-ver)
      (message "%s does not exist" mew-prog-smime)
    (if (and ask-signer (string-match "signature" (symbol-name type)))
	(mew-draft-make-message type (car (mew-input-address "Who's key?: ")))
      (mew-draft-make-message type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encoding
;;;

(defun mew-encode-smime (type cte decrypters)
  (let ((file1 (mew-save-transfer-form (point-min) (point-max) nil cte))
	file2 errmsg file2-errmsg)
    (cond
     ((string= type mew-ct-smm-sig)
      (setq file2-errmsg (mew-smime-sign file1)))
     ((string= type mew-ct-smm-enc)
      (setq file2-errmsg (mew-smime-encrypt file1 decrypters))))
    (mew-set '(file2 errmsg) file2-errmsg)
    (if errmsg
	(progn
	  (mew-delete-file file1)
	  (mew-delete-file file2)
	  (mew-tinfo-set-privacy-err t)
	  (mew-tinfo-set-privacy-type nil)
	  (mew-draft-mode-name)
	  (mew-encode-error errmsg))
      (mew-encode-singlepart
       (mew-encode-syntax-single file2
				 (list mew-ct-smm
				       (list mew-smime-type type)
				       (list "name" (concat mew-smime-file-name mew-smime-mime-suffix)))
				 mew-b64)))
    (mew-delete-file file1)
    (mew-delete-file file2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Signature
;;;

(defun mew-smime-sign (file1)
  (setq mew-smime-running 'signing)
  (setq mew-smime-string nil)
  (setq mew-smime-sign-msg nil)
  (setq mew-smime-failure nil)
  (let* ((process-connection-type mew-connection-type2)
	 (file2 (mew-make-temp-name))
	 (prog mew-prog-smime)
	 (opts `("--sign" ,@mew-prog-smime-options
		 "--local-user" ,mew-inherit-encode-smime-signer
		 "--output" ,file2 ,file1))
	 process)
    (message "S/MIME signing...")
    (setq process (apply 'mew-start-process-lang "GPGSM sign" nil prog opts))
    (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
    (set-process-filter process 'mew-smime-process-filter)
    (set-process-sentinel process 'mew-smime-process-sentinel)
    (mew-rendezvous mew-smime-running)
    (unless (file-exists-p file2) ;; for unpredictable error
      (mew-passwd-set-passwd (mew-smime-passtag) nil))
    (message "S/MIME signing...done")
    (list file2 mew-smime-sign-msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encryption
;;;

(defvar mew-smime-cipher "AES")

(defun mew-smime-encrypt (file1 decrypters)
  (let* ((process-connection-type mew-connection-type2)
	 (file2 (mew-make-temp-name))
	 (decs (mew-gpg-roption decrypters "--recipient"))
	 (prog mew-prog-smime)
	 (opts `("--encrypt" ,@mew-prog-smime-options
		 ,@decs
		 "--cipher-algo" ,mew-smime-cipher
		 "--output" ,file2 ,file1))
	 check)
    (message "S/MIME encrypting...")
    (with-temp-buffer
      (apply 'mew-call-process-lang prog nil t nil opts)
      (setq check (mew-smime-encrypt-check)))
    (message "S/MIME encrypting...done")
    (list file2 check)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decoding
;;;

(defun mew-decode-smime (syntax cnt)
  (let* ((ctl (mew-syntax-get-ct syntax))
	 (type (mew-syntax-get-param ctl mew-smime-type))
	 file1 file2 syntax2 result privacy)
    ;; xxx smime-type is optional, sigh
    (if (null type) (setq type mew-ct-smm-enc))
    (mew-syntax-set-ct syntax mew-type-apo)
    (setq file1 (mew-save-decode-form syntax))
    (setq file2 (mew-make-temp-name))
    (delete-region (point-min) (point-max))
    (cond
     ((mew-case-equal type mew-ct-smm-sig)
      (setq result (mew-smime-verify file1 file2)))
     ((mew-case-equal type mew-ct-smm-enc)
      (setq result (mew-smime-decrypt file1 file2)))
     (t
      (setq syntax2 syntax)
      (mew-syntax-set-ct syntax mew-type-apo)))
    (if (and (file-exists-p file2)
	     (> (mew-file-get-size file2) 0)) ;; shell creates file2 anyway
	(mew-flet
	 (mew-insert-file-contents file2)
	 (put-text-property (point-min) (point-max) 'mew-noncontents nil)
	 (mew-decode-crlf-magic))
      (insert "\n") ;; CT: text/plain; charset=us-ascii
      (insert (format "%s could not be decrypted.\n" mew-ct-smm))
      (mew-xinfo-set-not-decrypted t))
    ;; Throw away garbage
    (mew-delete-file file1)
    (mew-delete-file file2)
    (goto-char (point-min))
    (setq syntax2 (mew-decode-singlepart cnt nil nil))
    (setq privacy (mew-syntax-get-privacy syntax2))
    (if privacy (setq result (concat result "\n\t")))
    (mew-syntax-set-privacy
     syntax2 (cons (mew-make-privacy-dinfo :ct mew-ct-smm :proto type :result result) privacy))
    syntax2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Verification
;;;

(defun mew-smime-verify (file1 file2)
  (let ((prog mew-prog-smime)
	(opts `("--verify" ,@mew-prog-smime-options
		"--output" ,file2 ,file1))
	result)
    (message "S/MIME verifying...")
    (with-temp-buffer
      (apply 'mew-call-process-lang prog nil t nil opts)
      (setq result (mew-smime-verify-check)))
    (message "S/MIME verifying...done")
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decription
;;;

(defun mew-smime-decrypt (file1 file2)
  (setq mew-smime-running 'decrypting)
  (setq mew-smime-string nil)
  (setq mew-smime-decrypt-msg nil)
  (setq mew-smime-failure nil)
  (let ((process-connection-type mew-connection-type2)
	(prog mew-prog-smime)
	(opts `("--decrypt"
		"--output" ,file2 ,file1))
	process)
    (message "S/MIME decrypting...")
    (setq process (apply 'mew-start-process-lang "GPGSM decrypt" nil prog opts))
    (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
    (set-process-filter process 'mew-smime-process-filter)
    (set-process-sentinel process 'mew-smime-process-sentinel)
    (mew-rendezvous mew-smime-running)
    (unless (file-exists-p file2) ;; for unpredictable error
      (mew-passwd-set-passwd (mew-smime-passtag) nil))
    (message "S/MIME decrypting...done")
    mew-smime-decrypt-msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multipart/Signed
;;;

(defun mew-smime-detach-verify (file1 file2)
  (let ((prog mew-prog-smime)
	(opts `("--verify" ,@mew-prog-smime-options
		,file2 ,file1))
	ret)
    (message "S/MIME verifying...")
    (with-temp-buffer
      (apply 'mew-call-process-lang prog nil t nil opts)
      (setq ret (mew-smime-verify-check)))
    (message "S/MIME verifying...done")
    ret))

(defun mew-smime-detach-sign (file1)
  (setq mew-smime-running 'signing)
  (setq mew-smime-string nil)
  (setq mew-smime-sign-msg nil)
  (setq mew-smime-failure nil)
  (let* ((process-connection-type mew-connection-type2)
	 (file2 (concat (mew-make-temp-name mew-smime-file-name)
			mew-smime-signature-suffix))
	 (prog mew-prog-smime)
	 (opts `("--detach-sign" ,@mew-prog-smime-options
		 "--local-user" ,mew-inherit-encode-smime-signer
		 "--output" ,file2 ,file1))
	 process)
    (message "S/MIME signing...")
    (setq process (apply 'mew-start-process-lang "GPGSM sign" nil prog opts))
    (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
    (set-process-filter process 'mew-smime-process-filter)
    (set-process-sentinel process 'mew-smime-process-sentinel)
    (mew-rendezvous mew-smime-running)
    (unless (file-exists-p file2) ;; for unpredictable error
      (mew-passwd-set-passwd (mew-smime-passtag) nil))
    (message "S/MIME signing...done")
    (list file2 mew-b64 (mew-smime-get-micalg) mew-smime-sign-msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GPG
;;;

(defun mew-gpg-roption (decrypters roption)
  (if (and mew-encrypt-to-myself
	   (not (member mew-inherit-encode-smime-signer decrypters)))
      (setq decrypters (cons mew-inherit-encode-smime-signer decrypters)))
  (let (decs)
    (dolist (decrypter decrypters)
      (setq decs (cons decrypter (cons roption decs))))
    (nreverse decs)))

(provide 'mew-smime)

;;; Copyright Notice:

;; Copyright (C) 2004-2014 Mew developing team.
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

;;; mew-smime.el ends here
