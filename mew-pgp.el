;;; mew-pgp.el --- PGP/MIME for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug 17, 1994

;;; Code:

(require 'mew)

;;;
;;; PGP 2.6.x is supported.
;;; PGP 5.x is supported. But very ad-hoc.
;;; GNUPG 1.0.2 or later is supported.
;;; PGP 6.x is supported. But very ad-hoc.
;;;

(defvar mew-pgp-ver nil
  "Automatically set 0 if PGP version is 2.
Set 1 if 5. Set 2 if 6. Set 3 if GNUPG.")

(defconst mew-pgp-ver2 0)
(defconst mew-pgp-ver5 1)
(defconst mew-pgp-ver6 2)
(defconst mew-pgp-verg 3)
(defconst mew-pgp-list '("PGPv2" "PGPv5" "PGPv6" "GNUPG"))
(defconst mew-pgp-keys '(pgpv2 pgpv5 pgpv6 gnupg)) ;; use symbols, cases are string

;; mew-prog-pgp is used only for version check
(defvar mew-prog-pgp2  "pgp") ;; "pgp263i", PGP selection
(defvar mew-prog-pgp5  "pgp") ;; PGP selection
(defvar mew-prog-pgp5e "pgpe")
(defvar mew-prog-pgp5s "pgps")
(defvar mew-prog-pgp5v "pgpv")
(defvar mew-prog-pgp5k "pgpk")
(defvar mew-prog-pgp6  "pgp") ;; "pgp651i", PGP selection
(defvar mew-prog-gpg   "gpg") ;; PGP selection

(defvar mew-prog-pgpe
  `(,mew-prog-pgp2 ,mew-prog-pgp5e ,mew-prog-pgp6 ,mew-prog-gpg))
(defvar mew-prog-pgps
  `(,mew-prog-pgp2 ,mew-prog-pgp5s ,mew-prog-pgp6 ,mew-prog-gpg))
(defvar mew-prog-pgpv
  `(,mew-prog-pgp2 ,mew-prog-pgp5v ,mew-prog-pgp6 ,mew-prog-gpg))
(defvar mew-prog-pgpd
  `(,mew-prog-pgp2 ,mew-prog-pgp5v ,mew-prog-pgp6 ,mew-prog-gpg))
(defvar mew-prog-pgpk
  `(,mew-prog-pgp2 ,mew-prog-pgp5k ,mew-prog-pgp6 ,mew-prog-gpg))

(defconst mew-prog-pgpe-arg
  '(("-ea" "+language=en" "+batchmode=on" "+armorlines=0")
    ("-a" "+language=en" "+batchmode=on" "+armorlines=0")
    ("-ea" "+language=en" "+batchmode=on" "+armorlines=0")
    ("--encrypt" "--armor" "--batch")))

(defconst mew-prog-pgpd-arg
  '(("+language=en" "+batchmode=off")
    ("+language=en" "+batchmode=off")
    ("+language=en" "+batchmode=off")
    ("--decrypt")))

(defvar mew-prog-pgps-arg ;; local binding
  '(("-sba" "+language=en" "+batchmode=off")
    ("-ba" "+language=en" "+batchmode=off")
    ("-sba" "+language=en" "+batchmode=off")
    ("--detach-sign" "--armor" "--status-fd" "1")))

(defconst mew-prog-pgpv-arg
  '(("+batchmode=on" "+language=en")
    ("+batchmode=on" "+language=en" "+force=on")
    ("+batchmode=on" "+language=en")
    ("--verify" "--batch")))

(defconst mew-prog-old-pgpv-arg
  '(("+batchmode=on" "+language=en")
    ("+batchmode=on" "+language=en" "+force=on")
    ("+batchmode=on" "+language=en")
    ("--decrypt" "--batch"))) ;; --verify does not extract original data

(defconst mew-prog-pgp-arg-output  '("-o" "-o" "-o" "--output"))
(defconst mew-prog-pgp-arg-input   '(nil  "-o" nil  nil))
(defconst mew-prog-pgp-arg-luserid '("-u" "-u" "-u" "--local-user"))
(defconst mew-prog-pgp-arg-ruserid '(nil  "-r" nil  "--remote-user"))

(defconst mew-prog-pgpk-add-arg
  '(("-ka" "+batchmode=on")
    ("-a" "+batchmode=on")
    ("-ka" "+batchmode=on")
    ("--import" "--batch")))

(defconst mew-prog-pgpk-ext-arg
  '(("-kxfa") ("-xa") ("+force" "-kxfa") ("--export" "--armor" "--batch")))

(defconst mew-pgp-msg-signature
  '("\n\\(.*\\) signature from user \\(.*\\)\\."
    "\n\\(.*\\) signature made"
    "\\([a-zA-Z0-9]*\\) signature from user \\(.*\\)\\."
    " \\(.*\\) signature from \"\\(.*\\)\""))

(defconst mew-pgp-msg-key-id
  '("Key ID \\([a-zA-Z0-9]+\\) not found"
    ": 0x\\([a-zA-Z0-9]+\\)"
    ": 0x\\([a-zA-Z0-9]+\\)"
    "key ID \\([a-zA-Z0-9]+\\)"))

(defconst mew-pgp-msg-bad-pass
  '("No passphrase"
    "Cannot unlock private key\\|It can only be decrypted"
    "Bad pass phrase"
    "bad passphrase"))

(defconst mew-pgp-msg-enter
  '("Enter" "Enter" "Enter" "xxx"))

(defconst mew-pgp-msg-overwrite
  '("Overwrite (y/N)\\? "
    "Overwrite (y/N)\\? "
    "Overwrite (y/N)\\? "
    "Overwrite (y/N)\\? "))

(defconst mew-pgp-msg-enter-pass
  '("Enter pass phrase: "
    "Enter pass phrase: "
    "Enter pass phrase: "
    "Enter passphrase: "))

(defconst mew-pgp-msg-reenter-pass
  '("Enter pass phrase: "
    "Enter pass phrase: "
    "Enter pass phrase: "
    "Enter passphrase: "))

(defconst mew-pgp-msg-no-enckey
  '("Key matching"
    "No encryption keys"
    "public key matching"
    "public key not found"))

(defconst mew-pgp-msg-no-validkey
  '("DUMMY"
    "^WARNING:[ -9;-~\n]+belongs? to:"
    "^WARNING:[ -9;-~\n]+belongs? to:"
    "There is no indication that this key really belongs to the owner"))

(defconst mew-pgp-msg-pubkey-expired
  '("xxx"
    "xxx"
    "xxx"
    "encryption failed: unusable public key"))

(defconst mew-pgp-msg-no-vrfkey
  '("Key matching" "unknown keyid" "key does not meet" "public key not found"))

(defconst mew-pgp-msg-no-keyring
  '("Keyring file" "Keyring file" "NO MESSAGE" "public key not found"))

(defconst mew-pgp-msg-no-seckey-or-secring
  '("You do not have the secret key"
    "Cannot find a private key"
    "Signature error\\|You do not have the secret key"
    "failed: secret key not available"))

(defconst mew-pgp-msg-unsupported
  '("Unsupported packet format" ;; including algorithms and packets
    "Unsupported packet format\\|None of the signatures were understood"
    "Unsupported packet format" ;; including algorithms and packets
    "xxx"))

(defconst mew-pgp-msg-nocross
  '("xxx"
    "xxx"
    "xxx"
    "is not cross-certified"))

(defconst mew-pgp-verify-addr
  '(".* \\(signature from user\\) "
    "\\(   \\)"
    ".* \\(signature from user\\) "
    "gpg: .* \\(from\\|aka\\) "))

;; 2: ASCII armor corrupted
;; 3:
;; 5:

;; 2: ERROR: or Error:

(defconst mew-pgp-msg-no-export-key
  '("Key not found" "No keys" "Key not found" "nothing exported"))

(defvar mew-pgp-micalg '("pgp-md5" "pgp-sha1" "pgp-sha1" "pgp-sha1"))

(defvar mew-pgp-hash-alist
  '(("1"  . "pgp-md5")
    ("2"  . "pgp-sha1")
    ("3"  . "pgp-ripemd160")
    ("5"  . "pgp-md2")
    ("6"  . "pgp-tiger192")
    ("7"  . "pgp-haval-5-160")
    ("8"  . "pgp-sha256")
    ("9"  . "pgp-sha384")
    ("10" . "pgp-sha512")))

;;
;;

(defvar mew-pgp-string nil)
(defvar mew-pgp-running nil)
(defvar mew-pgp-failure nil)

(defvar mew-pgp-decrypt-msg nil)
(defvar mew-pgp-sign-msg nil)

(defconst mew-pgp-encryption-begin "-----BEGIN PGP MESSAGE-----")
(defconst mew-pgp-signature-begin  "-----BEGIN PGP SIGNED MESSAGE-----")
(defconst mew-pgp-key-begin "-----BEGIN PGP PUBLIC KEY BLOCK-----")
(defconst mew-pgp-key-end   "-----END PGP PUBLIC KEY BLOCK-----")

(defconst mew-pgp-err-pass    'mew-err-pass)
(defconst mew-pgp-err-pubring 'mew-err-pubring)
(defconst mew-pgp-err-secring 'mew-err-secring)
(defconst mew-pgp-err-pubkey  'mew-err-pubkey)
(defconst mew-pgp-err-seckey  'mew-err-seckey)
(defconst mew-pgp-err-seckey-or-secring 'mew-err-seckey-or-secring)
(defconst mew-pgp-err-other   'mew-err-other)

(defvar mew-pgp-result-pass     "Pass phrase is wrong")
(defvar mew-pgp-result-pubring  "No public keyring")
(defvar mew-pgp-result-secring  "No secret keyring")
(defvar mew-pgp-result-pubkey   "No his/her public key")
(defvar mew-pgp-result-expired  "His/her public key is expired or not signed by yourself")
(defvar mew-pgp-result-invalid
  "His/her public key is invalid. Sign the key by yourself, first")
(defvar mew-pgp-result-seckey   "No your secret key")
(defvar mew-pgp-result-seckey-or-secring
  "No secret keyring or no your secret key")
(defvar mew-pgp-result-other    "PGP failed for some reasons")
(defvar mew-pgp-result-sec-succ "PGP decrypted")
(defvar mew-pgp-result-dec-fail "PGP NOT decrypted for some reasons")
(defvar mew-pgp-result-unsup    "PGP unsupported signature")
(defvar mew-pgp-result-nocross  "His/her Public key is not cross-certified")

(defvar mew-pgp-prompt-enter-pass   "Enter pass phrase (%s): ")
(defvar mew-pgp-prompt-reenter-pass "Re-enter pass phrase (%s): ")

(defun mew-pgp-get (list-or-vec)
  (elt list-or-vec mew-pgp-ver))

(defun mew-pgp-set (vec val)
  (aset vec mew-pgp-ver val))


(defun mew-pgp-debug (label string)
  (when (mew-debug 'pgp)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PGP version check
;;;

(defun mew-pgp-setup ()
  (cond
   ((mew-which-exec mew-prog-pgp)
    (with-temp-buffer
      (mew-call-process-lang mew-prog-pgp nil t nil)
      (goto-char (point-min))
      (if (search-forward "PGP is now invoked" nil t)
	  (setq mew-pgp-ver mew-pgp-ver5)
	(goto-char (point-min))
	(if (search-forward "Pretty Good Privacy(tm) 2" nil t)
	    (setq mew-pgp-ver mew-pgp-ver2)
	  (goto-char (point-min))
	  (if (search-forward "gpg" nil t)
	      (setq mew-pgp-ver mew-pgp-verg)
	    (goto-char (point-min))
	    (if (search-forward "Pretty Good Privacy(tm) Version 6" nil t)
		(setq mew-pgp-ver mew-pgp-ver6)
	      (setq mew-pgp-ver nil)))))))
   (t (setq mew-pgp-ver nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PGP selection
;;;

(defun mew-pgp-select ()
  "Select PGP version and set up environment for selected PGP."
  (interactive)
  (setq mew-prog-pgp
	(completing-read
	 "PGP name: "
	 (mapcar 'list (list mew-prog-pgp2 mew-prog-pgp5 mew-prog-gpg))
	 nil t))
  (mew-pgp-setup))

;; xxx how about multiple users on local machine?
(defun mew-pgp-passtag ()
  (mew-pgp-get mew-pgp-list))

(defun mew-pgp-passphrase (&optional again)
  (let ((prompt (if again
		    mew-pgp-prompt-reenter-pass
		  mew-pgp-prompt-enter-pass))
	(msg (mew-pgp-passtag)))
    (setq prompt (format prompt msg))
    (mew-input-passwd prompt msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PGP verifying
;;;

(defun mew-pgp-verify-check ()
  (let (ret keyid)
    (goto-char (point-min))
    (if (not (re-search-forward (mew-pgp-get mew-pgp-msg-signature) nil t))
	;; this is verification, so the error is about public key
	(progn
	  (goto-char (point-min))
	  (if (search-forward (mew-pgp-get mew-pgp-msg-no-vrfkey) nil t)
	      (progn
		(goto-char (point-min))
		(if (not (re-search-forward (mew-pgp-get mew-pgp-msg-key-id) nil t))
		    (setq keyid "not found")
		  (setq keyid (format "0x%s"(mew-match-string 1))))
		(setq ret (concat mew-pgp-result-pubkey ": ID = " keyid)))
	    (if (search-forward (mew-pgp-get mew-pgp-msg-no-keyring) nil t)
		(setq ret mew-pgp-result-pubring)
	      (if (re-search-forward (mew-pgp-get mew-pgp-msg-unsupported) nil t)
		  (setq ret mew-pgp-result-unsup)
		(if (re-search-forward (mew-pgp-get mew-pgp-msg-nocross) nil t)
		    (setq ret mew-pgp-result-nocross)
		  ;; this line must be nil since this function is used
		  ;; by the decryption function, too, for signed-then-encrypted
		  ;; messages. We cannot tell whether or not signatures exist
		  ;; from the outside of the cipher.
		  )))))
      ;; Signature result is found.
      (setq ret (concat (mew-match-string 1) " PGP sign "))
      (goto-char (point-max))
      (if (and mew-inherit-decode-signer
	       (re-search-backward (concat (mew-pgp-get mew-pgp-verify-addr) ".*" mew-inherit-decode-signer) nil t))
	  (progn
	    (beginning-of-line)
	    (looking-at (concat (mew-pgp-get mew-pgp-verify-addr) "\\(.*\\)"))
	    (setq ret (concat ret (mew-match-string 2))))
	(goto-char (point-max))
	(re-search-backward (concat (mew-pgp-get mew-pgp-verify-addr) "\\(.*\\)") nil t)
	(setq ret (concat ret (mew-match-string 2))))
      ;; xxx
      (goto-char (point-min))
      (if (re-search-forward "not certified with \\(enough\\|sufficiently\\)" nil t)
	  (setq ret (concat ret " MARGINAL"))
	(goto-char (point-min))
	(if (search-forward "not trusted" nil t)
	    (setq ret (concat ret " UNTRUSTED"))
	  (goto-char (point-min))
	  (if (search-forward "not certified with a" nil t)
	      ;; PGP uses "unknown" for validity internally, but
	      ;; prints "undefined" instead of "unknown".
	      (setq ret (concat ret " UNDEFINED"))
	    (goto-char (point-min))
	    (if (search-forward "expired" nil t)
		(setq ret (concat ret " EXPIRED"))
	      (setq ret (concat ret " COMPLETE")))))))
    ret))

(defun mew-pgp-verify (file1 file2)
  (message "PGP verifying...")
  (let ((ioption (mew-pgp-get mew-prog-pgp-arg-input)) ;; detached signature
	(voptions (mew-pgp-get mew-prog-pgpv-arg))
	(pgpv (mew-pgp-get mew-prog-pgpv))
	files ret)
    (with-temp-buffer
      (if ioption
	  (setq files (list ioption file1 file2))
	(setq files (list file2 file1)))
      (apply 'mew-call-process-lang pgpv nil t nil (append voptions files))
      (setq ret (mew-pgp-verify-check)))
    (message "PGP verifying...done")
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PGP encrypting
;;;

(defun mew-pgp-encrypt-check ()
  (let (ret) ;; this should be nil
    (goto-char (point-min))
    (if (re-search-forward (mew-pgp-get mew-pgp-msg-no-validkey) nil t)
        (setq ret mew-pgp-result-invalid)
      (goto-char (point-min))
      (if (search-forward (mew-pgp-get mew-pgp-msg-no-enckey) nil t)
          (setq ret mew-pgp-result-pubkey)
        (goto-char (point-min))
        (if (search-forward (mew-pgp-get mew-pgp-msg-no-keyring) nil t)
            (setq ret mew-pgp-result-pubring)
	  (goto-char (point-min))
	  (if (search-forward (mew-pgp-get mew-pgp-msg-pubkey-expired) nil t)
	      (setq ret mew-pgp-result-expired)))))
    ret))

(defun mew-pgp-encrypt (file1 decrypters)
  (message "PGP encrypting...")
  (let ((roption (mew-pgp-get mew-prog-pgp-arg-ruserid))
	(ooption (mew-pgp-get mew-prog-pgp-arg-output))
	(eoptions (mew-pgp-get mew-prog-pgpe-arg))
	(pgpe (mew-pgp-get mew-prog-pgpe))
	(file2 (mew-make-temp-name))
	check file3 decs args) ;; not unique if file3 is made here
    (with-temp-buffer
      (insert "Version: 1\n")
      (write-region (point-min) (point-max) file2 nil 'no-msg))
    (setq file3 (concat (mew-make-temp-name) mew-pgp-ascii-suffix))
    (if (and mew-encrypt-to-myself
	     (not (member mew-inherit-encode-pgp-signer decrypters)))
	(setq decrypters (cons mew-inherit-encode-pgp-signer decrypters)))
    (if (not roption)
	(setq args (append (list ooption file3 file1) eoptions decrypters))
      (mapc
       (lambda (x) (setq decs (cons roption (cons x decs))))
       decrypters)
      (setq args (append eoptions decs (list ooption file3 file1))))
    (with-temp-buffer
      (apply 'mew-call-process-lang pgpe nil t nil args)
      (setq check (mew-pgp-encrypt-check)))
    (message "PGP encrypting...done")
    (list file2 mew-7bit file3 mew-7bit check))) ;; both ctes are 7bit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PGP decrypting
;;;

(defun mew-pgp-decrypt (file1 file2)
  ;; file1 is a key file. just ignore.
  ;; file2 is an encrypted file with PGP.
  (message "PGP decrypting...")
  (setq mew-pgp-running 'decrypting)
  (setq mew-pgp-string nil)
  (setq mew-pgp-decrypt-msg nil)
  (setq mew-pgp-failure nil)
  (let ((process-connection-type mew-connection-type2)
	(ooption (mew-pgp-get mew-prog-pgp-arg-output))
	(doptions (mew-pgp-get mew-prog-pgpd-arg))
	(pgpd (mew-pgp-get mew-prog-pgpd))
	file3 process verify)
    (setq file3 (mew-make-temp-name))
    (setq process
	  (apply 'mew-start-process-lang
		 "PGP decrypt"
		 (current-buffer)
		 pgpd
		 (append doptions (list ooption file3 file2))))
    (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
    (set-process-filter process 'mew-pgp-process-filter1)
    (set-process-sentinel process 'mew-pgp-process-sentinel)
    (mew-rendezvous mew-pgp-running)
    (message "PGP decrypting...done")
    (if (file-exists-p file3)
	(progn
	  (with-temp-buffer
	    (insert mew-pgp-string)
	    (setq verify (mew-pgp-verify-check)))
	  (when verify
	    (setq mew-pgp-decrypt-msg (concat mew-pgp-decrypt-msg "\n\t" verify))))
      ;; unpredictable error
      (mew-passwd-set-passwd (mew-pgp-passtag) nil)
      (if (string= mew-pgp-decrypt-msg mew-pgp-result-sec-succ)
	  (setq mew-pgp-decrypt-msg mew-pgp-result-dec-fail)))
    (list file3 mew-pgp-decrypt-msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PGP signing
;;;

(defun mew-pgp-canonicalize ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil t)
      (forward-line))))

(defun mew-pgp-get-micalg ()
  (let ((micalg (mew-pgp-get mew-pgp-micalg)) alg)
    (if (/= mew-pgp-ver mew-pgp-verg)
	micalg
      (with-temp-buffer
	(insert mew-pgp-string)
	(goto-char (point-min))
	(if (re-search-forward "SIG_CREATED [A-Z] [0-9]+ \\([0-9]+\\)" nil t)
	   (setq alg (cdr (assoc (mew-match-string 1) mew-pgp-hash-alist)))))
      (or alg micalg))))

(defun mew-pgp-sign (file1)
  (message "PGP signing...")
  (setq mew-pgp-running 'signing)
  (setq mew-pgp-string nil)
  (setq mew-pgp-sign-msg nil)
  (setq mew-pgp-failure nil)
  (let ((process-connection-type mew-connection-type2)
	(loption (mew-pgp-get mew-prog-pgp-arg-luserid))
	(ooption (mew-pgp-get mew-prog-pgp-arg-output))
	(soptions (mew-pgp-get mew-prog-pgps-arg))
	(pgps (mew-pgp-get mew-prog-pgps))
	file2 process)
    (setq file2 (concat (mew-make-temp-name) mew-pgp-ascii-suffix))
    ;; not perfectly unique but OK
    (setq process
	  (apply 'mew-start-process-lang
		 "PGP sign"
		 (current-buffer) ;; xxx nil?
		 pgps
		 (append soptions (list loption mew-inherit-encode-pgp-signer ooption file2 file1))))
    (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
    (set-process-filter process 'mew-pgp-process-filter1)
    (set-process-sentinel process 'mew-pgp-process-sentinel)
    (mew-rendezvous mew-pgp-running)
    (message "PGP signing...done")
    (unless (file-exists-p file2) ;; for unpredictable error
      (mew-passwd-set-passwd (mew-pgp-passtag) nil))
    (list file2 nil (mew-pgp-get-micalg) mew-pgp-sign-msg))) ;; return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PGP process functions
;;;

(defun mew-pgp-process-sentinel (process event)
  (save-excursion
    (let ((decrypted mew-pgp-result-sec-succ)
	  (msg ""))
      (if (not mew-pgp-failure)
	  (cond
	   ((eq mew-pgp-running 'decrypting)
	    (setq mew-pgp-decrypt-msg decrypted))
	   ((eq mew-pgp-running 'signing)
	    (setq mew-pgp-sign-msg nil)))
	(cond
	 ;; sign or decrypt
	 ((eq mew-pgp-failure mew-pgp-err-pass)
	  (setq msg mew-pgp-result-pass))
	 ;; decrypt-then-verify
	 ((eq mew-pgp-failure mew-pgp-err-pubring)
	  (setq msg decrypted))
	 ;; decrypt-then-verify
	 ((eq mew-pgp-failure mew-pgp-err-pubkey)
	  (setq msg decrypted))
	 ;; sign
	 ((eq mew-pgp-failure mew-pgp-err-secring)
	  (setq msg mew-pgp-result-secring))
	 ;; sign
	 ((eq mew-pgp-failure mew-pgp-err-seckey)
	  (setq msg mew-pgp-result-seckey))
	 ;; decrypt
	 ((eq mew-pgp-failure mew-pgp-err-seckey-or-secring)
	  (setq msg mew-pgp-result-seckey-or-secring))
	 ;; other
	 (t ;; mew-pgp-err-other or nil
	  (setq msg mew-pgp-result-other)))
	(cond
	 ((eq mew-pgp-running 'decrypting)
	  (setq mew-pgp-decrypt-msg msg))
	 ((eq mew-pgp-running 'signing)
	  (setq mew-pgp-sign-msg msg))))
      (setq mew-pgp-running nil))))

(defun mew-pgp-process-filter1 (process string)
  (save-excursion
    ;; sign or decrypt, not verify
    (mew-pgp-debug "PGP filter1" string)
    (setq mew-pgp-string (concat mew-pgp-string string))
    (cond
     ;; no secret key or no secring for decrypt
     ((string-match (mew-pgp-get mew-pgp-msg-no-seckey-or-secring) string)
      (setq mew-pgp-failure mew-pgp-err-seckey-or-secring)
      (set-process-filter process 'mew-pgp-process-filter3))

     ;; no secring for sign
     ((string-match (mew-pgp-get mew-pgp-msg-no-keyring) string)
      (setq mew-pgp-failure mew-pgp-err-secring)
      ;; Enter secret key filename:
      (process-send-string process "\n")
      (set-process-filter process 'mew-pgp-process-filter3))

     ;; no secret key for sign
     ((string-match (mew-pgp-get mew-pgp-msg-no-enckey) string)
      (setq mew-pgp-failure mew-pgp-err-seckey)
      ;; Enter secret key filename:
      (process-send-string process "\n")
      (set-process-filter process 'mew-pgp-process-filter3))

     ;; pass phrase for sign or decrypt
     ((string-match (mew-pgp-get mew-pgp-msg-enter-pass) string)
      (process-send-string process (format "%s\n" (mew-pgp-passphrase)))
      (set-process-filter process 'mew-pgp-process-filter2))

     ;; overwrite a previous data with a new data. (multiple data is unsupported)
     ((string-match (mew-pgp-get mew-pgp-msg-overwrite) string)
      (process-send-string process "y\n")
      (set-process-filter process 'mew-pgp-process-filter1))

     ;; just in case
     ((string-match (mew-pgp-get mew-pgp-msg-enter) string)
      (setq mew-pgp-failure mew-pgp-err-other)
      ;; Enter PSWD:
      (process-send-string process "\n")
      (set-process-filter process 'mew-pgp-process-filter3)))))

(defun mew-pgp-process-filter2 (process string)
  (save-excursion
    (mew-pgp-debug "PGP filter2" string)
    (setq mew-pgp-string (concat mew-pgp-string string))
    (cond
     ;; re-enter pass phrase
     ((string-match (mew-pgp-get mew-pgp-msg-reenter-pass) string)
      (setq mew-pgp-string nil)
      (mew-passwd-set-passwd (mew-pgp-passtag) nil) ;; cancel anyway
      (process-send-string process (format "%s\n" (mew-pgp-passphrase 'again)))
      (set-process-filter process 'mew-pgp-process-filter2))

     ;; pass phrases were wrong three times
     ((string-match (mew-pgp-get mew-pgp-msg-bad-pass) string)
      (setq mew-pgp-failure mew-pgp-err-pass)
      (mew-passwd-set-passwd (mew-pgp-passtag) nil) ;; cancel anyway
      (set-process-filter process 'mew-pgp-process-filter3))

     ;; no pubring for verify
     ((string-match (mew-pgp-get mew-pgp-msg-no-keyring) string)
      (setq mew-pgp-failure mew-pgp-err-pubring)
      ;; Enter public key filename:
      (process-send-string process "\n")
      (set-process-filter process 'mew-pgp-process-filter3))

     ;; no public key for verify
     ((string-match (mew-pgp-get mew-pgp-msg-no-vrfkey) string)
      (setq mew-pgp-failure mew-pgp-err-pubkey)
      ;; Enter public key filename:
      (process-send-string process "\n")
      (set-process-filter process 'mew-pgp-process-filter3))

     ;; after decrypted secret key, symmetric key is not unknown...
     ;; gpg: unknown cipher algorithm
     ;; no secret key or no secring for decrypt
     ((string-match (mew-pgp-get mew-pgp-msg-no-seckey-or-secring) string)
      (setq mew-pgp-failure mew-pgp-err-seckey-or-secring)
      (set-process-filter process 'mew-pgp-process-filter3))

     ;; overwrite a previous data with a new data. (multiple data is unsupported)
     ((string-match (mew-pgp-get mew-pgp-msg-overwrite) string)
      (process-send-string process "y\n")
      (set-process-filter process 'mew-pgp-process-filter2))

     ;; just in case
     ((string-match (mew-pgp-get mew-pgp-msg-enter) string)
      (setq mew-pgp-failure mew-pgp-err-other)
      ;; Enter PSWD:
      (process-send-string process "\n")
      (set-process-filter process 'mew-pgp-process-filter3)))))

(defun mew-pgp-process-filter3 (process string)
  (save-excursion
    (mew-pgp-debug "PGP filter3" string)
    ;; ending or error
    (setq mew-pgp-string (concat mew-pgp-string string))
    ;; string may contain old "Enter"
    (cond
     ;; just in case
     ((string-match (mew-pgp-get mew-pgp-msg-enter) string)
      ;; (setq mew-pgp-failure mew-pgp-err-other) ;; this is wrong
      ;; Enter PSWD:
      (process-send-string process "\n")
      (set-process-filter process 'mew-pgp-process-filter3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; shortcut methods
;;;

(defun mew-pgp-sign-message (&optional arg)
  "Sign the entire draft with PGP. Input your passphrase."
  (interactive "P")
  (mew-pgp-encode-message 'pgp-signature arg))

(defun mew-pgp-encrypt-message ()
  "Encrypt the entire draft with PGP."
  (interactive)
  (mew-pgp-encode-message 'pgp-encryption))

(defun mew-pgp-sign-encrypt-message (&optional arg)
  "Sign then encrypt the entire draft with PGP. Input your passphrase."
  (interactive "P")
  (mew-pgp-encode-message 'pgp-signature-encryption))

(defun mew-pgp-encrypt-sign-message (&optional arg)
  "Encrypt then sign the entire draft with PGP. Input your passphrase."
  (interactive "P")
  (mew-pgp-encode-message 'pgp-encryption-signature))

(defun mew-pgp-encode-message (type &optional ask-signer)
  (if (null mew-pgp-ver)
      (message "%s does not exist" mew-prog-pgp)
    (let ((func 'mew-draft-make-message))
      (if (mew-use-old-pgp (mew-tinfo-get-case))
	  (setq func 'mew-old-pgp-encode))
      (if (and ask-signer (string-match "signature" (symbol-name type)))
	  (funcall func type (car (mew-input-address "Who's key?: ")))
	(funcall func type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; old PGP encoding
;;;

(defconst mew-prog-old-pgps-arg
  '(("-sat" "+clearsig=on" "+language=en" "+batchmode=off")
    ("-at" "+clearsig=on" "+language=en" "+batchmode=off")
    ("-sat" "+clearsig=on" "+language=en" "+batchmode=off")
    ("--clearsign" "--armor" "--textmode")))

(defconst mew-prog-old-pgpse-arg
  '(("-seat" "+language=en" "+batchmode=off")
    ("-eat"  "+language=en" "+batchmode=off")
    ("-seat" "+language=en" "+batchmode=off")
    ("--sign" "--encrypt" "--armor" "--textmode")))

(defconst mew-prog-old-pgpe-arg
  '(("-eat" "+language=en" "+batchmode=on" "+armorlines=0")
    ("-at" "+language=en" "+batchmode=on" "+armorlines=0")
    ("-eat" "+language=en" "+batchmode=on" "+armorlines=0")
    ("--encrypt" "--armor" "--textmode" "--batch")))

(defun mew-old-pgp-encode (type &optional signer)
  (let (mew-inherit-encode-pgp-signer doit syntax ctl ct charset cs)
    (if (mew-attach-p)
	(if (mew-encode-syntax-have-one-part)
	    (progn
	      (setq syntax (mew-syntax-get-part mew-encode-syntax))
	      (setq ctl (mew-syntax-get-ct syntax))
	      (setq ct (mew-syntax-get-value ctl 'cap))
	      (if (string= ct mew-ct-txt)
		  (progn
		    (setq charset (mew-syntax-get-param ctl "charset"))
		    (setq cs (mew-charset-to-cs charset))
		    (setq doit t)
		    (mew-attach-clear))
		(message "Old PGP cannot be used for non-text")))
	  (message "Old PGP cannot be used if multipart exists"))
      (setq doit t))
    (when doit
      (setq mew-inherit-encode-pgp-signer (or signer
					      (mew-pgp-signer (mew-tinfo-get-case))
					      (mew-get-my-address)))
      (cond
       ((eq type 'pgp-signature)
	(mew-old-pgp-sign cs syntax))
       ((eq type 'pgp-encryption)
	(mew-old-pgp-encrypt cs))
       ((eq type 'pgp-signature-encryption)
	(mew-pgp-old-sign-encrypt cs))
       (t
	(message "Not supported"))))))

(defun mew-old-pgp-sign (cs &optional syntax)
  (let ((file1 (mew-make-temp-name))
	(mew-prog-pgps-arg mew-prog-old-pgps-arg)
	file2 fmc errmsg charset)
    (goto-char (mew-header-end))
    (forward-line)
    (unless cs
      (setq charset (mew-charset-guess-region (point) (point-max)))
      (setq cs (mew-charset-to-cs charset)))
    (mew-frwlet mew-cs-dummy cs
      (write-region (point) (point-max) file1 nil 'no-msg))
    (condition-case nil
	(setq fmc (mew-pgp-sign file1))
      (error
       (mew-delete-file file1)
       (mew-encode-error
	(format "unknown error for %s. Check %s, anyway"
		mew-ct-mls mew-temp-dir))))
    (mew-set '(file2 nil nil errmsg) fmc)
    (if errmsg
	(progn
	  (mew-delete-file file1)
	  (mew-delete-file file2)
	  (error errmsg))
      (delete-region (point) (point-max))
      (mew-frwlet cs mew-cs-dummy
	(insert-file-contents file2))
      (mew-delete-file file1)
      (mew-delete-file file2)
      (setq mew-encode-syntax syntax)
      (mew-draft-make-message))))

(defun mew-old-pgp-encrypt (cs)
  (let ((file1 (mew-make-temp-name))
	(mew-prog-pgps-arg mew-prog-old-pgps-arg)
	(decrypters (mew-header-parse-address-list mew-destination:-list))
	file2 file3 fc errmsg charset)
    (goto-char (mew-header-end))
    (forward-line)
    (unless cs
      (setq charset (mew-charset-guess-region (point) (point-max)))
      (setq cs (mew-charset-to-cs charset)))
    (mew-frwlet mew-cs-dummy cs
      (write-region (point) (point-max) file1 nil 'no-msg))
    (condition-case nil
	(setq fc (mew-pgp-encrypt file1 decrypters))
      (error
       (mew-delete-file file1)
       (mew-encode-error
	(format "unknown error for %s. Check %s, anyway"
		mew-ct-mle mew-temp-dir))))
    (mew-set '(file2 nil file3 nil errmsg) fc)
    (if errmsg
	(progn
	  (mew-delete-file file1)
	  (mew-delete-file file2)
	  (mew-delete-file file3)
	  (error errmsg))
      ;; Create multipart content-header
      (delete-region (point) (point-max))
      (mew-frwlet cs mew-cs-dummy
	(insert-file-contents file3))
      (mew-delete-file file1)
      (mew-delete-file file2)
      (mew-delete-file file3)
      (setq mew-encode-syntax nil)
      (mew-draft-make-message))))

(defun mew-pgp-old-sign-encrypt (cs)
  (message "PGP signing then encrypting...")
  (let ((decrypters (mew-header-parse-address-list mew-destination:-list))
	(roption (mew-pgp-get mew-prog-pgp-arg-ruserid))
	(loption (mew-pgp-get mew-prog-pgp-arg-luserid))
	(ooption (mew-pgp-get mew-prog-pgp-arg-output))
	(eoptions (mew-pgp-get mew-prog-old-pgpse-arg)) ;; xxx
	(pgpe (mew-pgp-get mew-prog-pgpe)) ;; xxx
	(file1 (mew-make-temp-name))
	file2 check decs args process charset)
    (goto-char (mew-header-end))
    (forward-line)
    (unless cs
      (setq charset (mew-charset-guess-region (point) (point-max)))
      (setq cs (mew-charset-to-cs charset)))
    (mew-frwlet mew-cs-dummy cs
      (write-region (point) (point-max) file1 nil 'no-msg))
    (if (and mew-encrypt-to-myself
	     (not (member mew-inherit-encode-pgp-signer decrypters)))
	(setq decrypters (cons mew-inherit-encode-pgp-signer decrypters)))
    (setq file2 (mew-make-temp-name))
    (setq args (list loption mew-inherit-encode-pgp-signer ooption file2 file1))
    (if (not roption)
	(setq args (append args eoptions decrypters))
      (dolist (decrypter decrypters)
	;; nreverse later, take care.
	(setq decs (cons decrypter (cons roption decs))))
      (setq decrypters (nreverse decs))
      (setq args (append eoptions decrypters args)))
    (setq mew-pgp-running 'signing)
    (setq mew-pgp-string nil)
    (setq mew-pgp-sign-msg nil)
    (setq mew-pgp-failure nil)
    (with-temp-buffer
      (setq process (apply 'mew-start-process-lang
			   "PGP sign"
			   (current-buffer)
			   pgpe args))
      (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
      (set-process-filter process 'mew-pgp-process-filter1)
      (set-process-sentinel process 'mew-pgp-process-sentinel)
      (mew-rendezvous mew-pgp-running)
      (message "PGP signing then encrypting...done")
      (setq check (mew-pgp-encrypt-check)))
    (unless (file-exists-p file2) ;; for unpredictable error
      (mew-passwd-set-passwd (mew-pgp-passtag) nil))
    (if (or mew-pgp-sign-msg check)
	(progn
	  (mew-delete-file file1)
	  (mew-delete-file file2)
	  (error (or mew-pgp-sign-msg check)))
      (delete-region (point) (point-max))
      (mew-frwlet cs mew-cs-dummy
	(insert-file-contents file2))
      (mew-delete-file file1)
      (mew-delete-file file2)
      (setq mew-encode-syntax nil)
      (mew-draft-make-message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; old PGP decoding
;;;

(defun mew-old-pgp-check ()
  (goto-char (point-min))
  (if (re-search-forward (concat "^" mew-pgp-encryption-begin) nil t)
      ;; this may be 'pgp-signature, but we hope clearsig is used...
      'pgp-encryption
    (goto-char (point-min))
    (if (re-search-forward (concat "^" mew-pgp-signature-begin) nil t)
	'pgp-signature
      nil)))

(defun mew-old-pgp-verify (file)
  (message "PGP verifying...")
  (let ((voptions (mew-pgp-get mew-prog-old-pgpv-arg))
	(ooption (mew-pgp-get mew-prog-pgp-arg-output))
	(pgpv (mew-pgp-get mew-prog-pgpv))
	(file1 (mew-make-temp-name))
	verify)
    (with-temp-buffer
      (apply 'mew-call-process-lang
	     pgpv nil t nil (append voptions (list ooption file1 file)))
      (setq verify (mew-pgp-verify-check)))
    (message "PGP verifying...done")
    (list file1 verify)))

(defun mew-summary-decode-old-pgp ()
  "Decrypting/verifying old-fashioned PGP messages."
  (interactive)
  (if (null mew-pgp-ver)
      (message "%s does not exist" mew-prog-pgp)
    (mew-summary-msg
     (let ((fld (mew-summary-folder-name))
	   (msg (mew-summary-message-number2))
	   type)
       (with-current-buffer (mew-buffer-message)
	 ;; We need MIME-decoded buffer to check PGP boundaries.
	 (setq type (mew-old-pgp-check))
	 (if type
	    (mew-old-pgp-decode fld msg type)
	   (message "No PGP data found")))))))

(defun mew-old-pgp-decode (fld msg type)
  ;; in Message buffer
  (let* ((cache (mew-cache-hit fld msg 'must-hit))
	 (syntax (mew-cache-decode-syntax cache))
	 (file (mew-expand-msg fld msg))
	 mew-inherit-decode-signer
	 file1 file2 result xmew win start cte)
    (with-temp-buffer
      (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
	(mew-insert-file-contents file)
	(re-search-forward mew-eoh nil t)
	(forward-line)
	(delete-region (point-min) (point))
	(when (eq type 'pgp-signature)
	  (setq cte (mew-syntax-get-cte (mew-syntax-get-part syntax)))
	  (mew-decode-mime-body nil mew-ct-txt cte))
	(setq file1 (mew-make-temp-name))
	(write-region (point-min) (point-max) file1 nil 'no-msg)))
    (mew-elet
     (widen)
     (setq win (get-buffer-window (current-buffer)))
     (setq start (window-start win))
     (setq mew-inherit-decode-signer
	   (mew-addrstr-parse-address
	    (mew-header-get-value mew-from:)))
     (goto-char (mew-header-end))
     (forward-line)
     (delete-region (point) (point-max))
     (cond
      ((eq type 'pgp-encryption)
       (setq result (mew-pgp-decrypt 'dummy file1)))
      ((eq type 'pgp-signature)
       (setq result (mew-old-pgp-verify file1))))
     (mew-set '(file2 xmew) result)
     (unless xmew (setq xmew "CRC error")) ;; xxx
     (mew-xinfo-set-pri-result
      (concat mew-x-mew: " <body> " xmew "\n"))
     (save-excursion
       (mew-header-delete-lines (list mew-x-mew:))
       (goto-char (mew-header-end))
       (mew-decode-syntax-insert-privacy))
     (when (and file1 (file-exists-p file1))
       (mew-delete-file file1))
     (when (and file2 (file-exists-p file2))
       (mew-frwlet mew-cs-text-for-read mew-cs-dummy
	 (mew-insert-file-contents file2))
       (mew-delete-file file2))
     (mew-cs-decode-region
      (point) (point-max)
      ;; This is valid for PGP signature only.
      ;; Since cipher text is encoded with Radix 64, charset should
      ;; be "US-ASCII". I don't know this code is valid for
      ;; cipher text in the case that a wrong charset is specified.
      (or (mew-charset-to-cs
	   (mew-syntax-get-param
	    (mew-syntax-get-ct (mew-syntax-get-part syntax)) "charset"))
	  mew-cs-autoconv))
     (mew-highlight-body-region (point) (point-max))
     (mew-message-set-end-of)
     (set-window-start win start)
     (set-buffer-modified-p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Key distribution
;;;

(defun mew-attach-pgp-public-key ()
  "Extract the PGP key for the specified user on '.'. in attachments"
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Cannot link here")
    (let* ((error nil)
	   (nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   user file filepath begin end)
      ;; attachdir / {subdir/} dir
      (unless (string= subdir "")
	(setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir 1 nil mew-pgp-key-suffix))
      (if (null filepath)
	  (message "Cannot make a file for pgp key, sorry")
	(setq file (file-name-nondirectory filepath))
	(setq user (car (mew-input-address
			 "Who's key? (%s): " (mew-get-my-address))))
	(with-temp-buffer
	  (apply 'mew-call-process-lang
		 (mew-pgp-get mew-prog-pgpk)
		 nil t nil
		 (append
		  (mew-pgp-get mew-prog-pgpk-ext-arg)
		  (list user)))
	  (goto-char (point-min))
	  (if (search-forward (mew-pgp-get mew-pgp-msg-no-export-key) nil t)
	      (setq error t)
	    (goto-char (point-min))
	    (if (not (search-forward mew-pgp-key-begin nil t))
		(setq error t)
	      (beginning-of-line)
	      (setq begin (point))
	      (if (not (search-forward mew-pgp-key-end nil t))
		  (setq error t)
		(beginning-of-line)
		(forward-line)
		(setq end (point)))
	      (write-region begin end filepath nil 'no-msg))))
	(if error
	    (message "Cannot extract pgp key for %s" user)
	  (setq mew-encode-syntax
		(mew-syntax-insert-entry
		 mew-encode-syntax
		 nums
		 (mew-encode-syntax-single file mew-type-apk nil user)))
	  (mew-encode-syntax-print mew-encode-syntax))))))

(defvar mew-pgp-tmp-file nil)

(defun mew-mime-pgp-keys (cache begin end &optional params)
  "A function to add PGP keys in Application/PGP-Keys to your
public keyring."
  (mew-elet
   (insert " ######   #####  ######  #     # ####### #     #\n"
	   " #     # #     # #     # #    #  #        #   #\n"
	   " #     # #       #     # #   #   #         # #\n"
	   " ######  #  #### ######  ####    #######    #\n"
	   " #       #     # #       #   #   #          #\n"
	   " #       #     # #       #    #  #          #\n"
	   " #        #####  #       #     # #######    #\n"
	   "\n\n")
   (mew-insert-manual
    "To add this key to your pubring, type "
    "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n")))

(defun mew-mime-pgp-keys-ext (cache begin end &optional params)
  "A function to add PGP keys to your public keyring."
  (if (not mew-pgp-ver)
      (message "PGP not found")
    (when (y-or-n-p "Add this PGP key onto your public keyring? ")
      (setq mew-pgp-tmp-file (mew-make-temp-name))
      (with-current-buffer cache
	(mew-frwlet mew-cs-dummy mew-cs-autoconv
	  (write-region begin end mew-pgp-tmp-file nil 'no-msg))
	(set-buffer (mew-buffer-message))
	(mew-elet
	 (message "Adding PGP keys...")
	 (apply 'mew-call-process-lang
		(mew-pgp-get mew-prog-pgpk)
		nil t nil
		(append (mew-pgp-get mew-prog-pgpk-add-arg)
			(list mew-pgp-tmp-file)))
	 (message "Adding PGP keys...done")
	 (insert "\n\n"
		 "**************** IMPORTANT NOTE ****************\n"
		 "When Mew adds PGP keys onto your public keyring,\n"
		 "it is careless about both TRUST and VALIDITY.\n"
		 "It is YOU who set these values. Please use\n")
	 (cond
	  ((eq mew-pgp-ver mew-pgp-ver2)
	   (insert "\"pgp -ke\" and \"pgp -ks\" to change them.\n"))
	  ((eq mew-pgp-ver mew-pgp-ver5)
	   (insert "\"pgpk -e\" and \"pgpk -s\" to change them.\n"))
	  ((eq mew-pgp-ver mew-pgp-ver6)
	   (insert "\"pgp -ke\" and \"pgp -ks\" to change them.\n"))
	  ((eq mew-pgp-ver mew-pgp-verg)
	   (insert "\"gpg --edit-key\" to change them.\n")))
	 (insert "If you do not know what TRUST and VALIDITY is,\n"
		 "you should learn the web of trust system BEFORE\n"
		 "using PGP to protect your privacy.\n"
		 "**************** IMPORTANT NOTE ****************\n")))
      (mew-delete-file mew-pgp-tmp-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; key fetch
;;;

(defun mew-pgp-fetch-key-by-xuri ()
  (let (val uri)
    (with-current-buffer (mew-buffer-message)
      (catch 'loop
	(dolist (key mew-x-pgp-key-list)
	  (setq val (mew-header-get-value key))
	  (when (and val (string-match "http:[^ \t\n]*" val))
	    (setq val (mew-match-string 0 val))
	    (if (y-or-n-p (format "Fetch a PGP public key from %s? " val))
		(throw 'loop (setq uri val)))))))
    (if uri
	(mew-pgp-fetch uri)
      (message "A PGP key is not fetched"))))

(defun mew-pgp-fetch-key (arg)
  "Fetch a PGP public key.

A PGP public key is fetch from one of the PGP PKS servers with a
key ID which is extracted from the X-Mew: field. The list of the
PGP PKS servers is specified by 'mew-pgp-pks-servers'.

If called with \\[universal-argument], a PGP public key is fetched
according to a URL in a field specified by 'mew-x-pgp-key-list'."
  (interactive "P")
  (if arg
      (mew-pgp-fetch-key-by-xuri)
    (let (keyid userid xmew)
      (with-current-buffer (mew-buffer-message)
	(setq userid (mew-header-parse-address mew-from:))
	(setq xmew (mew-header-get-value mew-x-mew:)))
      (cond
       ((and xmew
	     (string-match " ID = \\(0x[0-9a-fA-F]+\\)" xmew nil)
	     (setq keyid (mew-match-string 1 xmew)))
	(if (y-or-n-p (format "Fetch the PGP public key for %s? " keyid))
	    (mew-pgp-fetch-key-from-pks keyid)))
       (userid
	(if (y-or-n-p (format "Fetch the PGP public key for %s? " userid))
	    (mew-pgp-fetch-key-from-pks userid)))
       (t
	(message "A PGP key is not fetched"))))))

(defun mew-pgp-fetch-key-from-pks (&optional id)
  (let* ((alist mew-pgp-pks-servers)
	 (server (car (car alist)))
	 uri)
    (unless id
      (setq id (read-string "PGP key id or e-mail address: ")))
    (setq server (mew-input-general "PGP key server" alist nil server))
    (unless (mew-pgp-pks-http-port-p server alist)
      (setq server (concat server ":" (mew-*-to-string mew-pgp-pks-port))))
    (setq uri (concat "http://" server mew-pgp-pks-lang id))
    (mew-pgp-fetch uri)))

(defun mew-pgp-fetch (uri)
  (message "PGP key fetching...")
  (let (pro)
    (mew-piolet mew-cs-autoconv mew-cs-autoconv
      (setq pro (apply 'start-process
		       "Mew PGP fetch"
		       (generate-new-buffer mew-buffer-prefix)
		       mew-prog-pgpkey
		       (append mew-prog-pgpkey-args (list uri)))))
    (set-process-sentinel pro 'mew-pgp-fetch-process-sentinel)))

(defun mew-pgp-fetch-process-sentinel (process event)
  (message "PGP key fetching...done")
  (let ((buf (process-buffer process)))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (and (search-forward mew-pgp-key-begin nil t)
	       (search-forward mew-pgp-key-end nil t))
	  (mew-mime-pgp-keys-ext (current-buffer) (point-min) (point-max))
	(message "PGP key fetch failed")))
    (mew-remove-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <Decrypt> or <Decrypt-then-Verify>
;;
;; no seckey no secring:: <1>
;; You do not have the secret key needed to decrypt this file.
;;
;;    <Verify> <2>
;;	Keyring file 'pubring.pgp' does not exist.
;;	Enter public key filename:
;;
;;	Key matching expected Key ID 1B8BF431 not found in file 'pubring.pgp'.
;;	Enter public key filename:
;;
;; <Sign> <1>
;;
;; A secret key is required to make a signature.
;; Keyring file 'secring.pgp' does not exist.
;; Enter secret key filename:
;;
;; A secret key is required to make a signature.
;; Key matching userid 'foo' not found in file 'secring.pgp'.
;; Enter secret key filename:
;;
;; <Encrypt>
;;
;; Keyring file 'pubring.pgp' does not exist.
;;
;; Key matching userid 'foo' not found in file 'pubring.pgp'.
;;
;; <Verify>
;; Keyring file 'pubring.pgp' does not exist.
;;
;; Key matching expected Key ID 1B8BF431 not found in file 'pubring.pgp'.

(provide 'mew-pgp)

;;; Copyright Notice:

;; Copyright (C) 1994-2014 Mew developing team.
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

;;; mew-pgp.el ends here
