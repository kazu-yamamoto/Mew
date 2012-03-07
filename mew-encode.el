;;; mew-encode.el --- MIME syntax encoder for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

(defvar mew-prog-mime-encode-switch
  `((,mew-b64 "-b")
    (,mew-qp  "-q")
    (,mew-xg  "-g")))

(defvar mew-prog-mime-encode-text-switch
  `((,mew-b64 "-b" "-t")
    (,mew-qp  "-q")
    (,mew-xg  "-g""-t")))

(defun mew-prog-mime-encode-get-opt (cte switch)
  (cdr (mew-assoc-case-equal cte switch 0)))

(defvar mew-encode-multipart-encrypted-switch
  `((,mew-ct-pge mew-pgp-encrypt)))

(defvar mew-encode-multipart-signed-switch
  `((,mew-ct-pgs mew-pgp-sign mew-pgp-canonicalize)
    (,mew-ct-sms mew-smime-detach-sign)))

;;

(defun mew-encode-get-security-func (proto switch)
  (nth 1 (mew-assoc-case-equal proto switch 0)))

(defun mew-encode-get-canonicalize-func (proto switch)
  (nth 2 (mew-assoc-case-equal proto switch 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME encoder
;;;

(defun mew-encode-error (error-msg)
  (mew-tinfo-set-encode-err error-msg)
  (error ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RFC 2822 comments
;;;

;; RFC 2822 defines the limitations of the number of fields.
;;
;; * Overriding
;;    Date: (exactly once)
;;    Message-Id:
;;
;; * Put multiple fields to a single field
;;    From: (exactly once)
;;    Reply-To:
;;    To:
;;    Cc:
;;
;; * Error and undo
;;    Sender: (if exist, exactly one address)
;;
;;    In-Reply-To:
;;    References:
;;    Subject:
;;

(defconst mew-multiple-field-error-list
  (list mew-subj: mew-in-reply-to: mew-references:))

(defconst mew-multiple-field-combine-list
  (list mew-from: mew-reply-to: mew-to: mew-cc:))

(defun mew-header-count-field (field)
  (let ((case-fold-search t)
	(regex (format "^%s" field))
	(i 0))
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (setq i (1+ i))
      (forward-line))
    i))

(defun mew-header-combine-field (field)
  (let ((case-fold-search t)
	(regex (format "^%s" field))
	beg med here value)
    (goto-char (point-min))
    (when (re-search-forward regex nil t)
      (forward-line)
      (mew-header-goto-next)
      (setq here (1- (point)))
      (while (re-search-forward regex nil t)
	(setq beg (match-beginning 0))
	(setq med (match-end 0))
	(forward-line)
	(mew-header-goto-next)
	(setq value (mew-buffer-substring med (1- (point))))
	(delete-region beg (point))
	(save-excursion
	  (goto-char here)
	  (insert ",\n\t" value)
	  (setq here (point)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header sanity check
;;;

(defun mew-encode-remove-invalid-fields ()
  (when (mew-header-end)
    (save-excursion
      (save-restriction
	(narrow-to-region (point-min) (mew-header-end))
	(dolist (field mew-multiple-field-error-list)
	  (if (> (mew-header-count-field field) 1)
	      (mew-encode-error (format "Multiple %s!" field))))
	(mapcar 'mew-header-combine-field mew-multiple-field-combine-list)))))

(defun mew-encode-ask-subject ()
  (when (and mew-ask-subject (not (mew-header-existp mew-subj:)))
    ;; Subject: does not exist
    (let ((subj (read-string (concat mew-subj: " "))))
      (mew-header-replace-value mew-subj: subj))))

(defun mew-encode-fcc-folder-check (fld)
  (let ((folder (mew-canonicalize-folder fld)))
    (when (and mew-ask-fcc
	       (mew-folder-localp folder)
	       (not (file-directory-p (mew-expand-folder folder))))
      (if (y-or-n-p (format "%s does not exist. Create it? " folder))
	  (mew-local-folder-check folder)
	(mew-encode-error "Edit Fcc:")))
    folder))

(defun mew-encode-ask-fcc (resentp)
  (let* ((target (if resentp mew-resent-fcc: mew-fcc:))
	 (folders (mew-header-get-value target)))
    (when folders
      (mapcar
       'mew-encode-fcc-folder-check
       (mew-addrstr-parse-value-list2 folders)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To:, Cc:
;;;

(defvar mew-draft-append-domain-p t)

(defun mew-draft-append-domain (addr)
  (if (string-match "@" addr)
      addr
    (if mew-draft-append-domain-p
	(concat addr "@" (mew-mail-domain (mew-tinfo-get-case)))
      (throw 'jump addr))))

(defun mew-encode-canonicalize-address (resentp)
  (catch 'jump
    (let ((case-fold-search t)
	  (regex (mew-make-field-regex
		  (if resentp mew-resent-dest:-list mew-destination:-list)))
	  key start vals val addrs addr ret insl ins prefix suffix)
      (save-excursion
	(save-restriction
	  (narrow-to-region (point-min) (mew-header-end))
	  (goto-char (point-min))
	  (while (re-search-forward regex nil t)
	    (setq key (match-string 1))
	    (setq start (match-end 0))
	    (forward-line)
	    (while (looking-at mew-lwsp)
	      (delete-char -1)
	      (forward-line))
	    (setq val (mew-buffer-substring start (1- (point))))
	    (delete-region start (1- (point)))
	    (backward-char 1)
	    ;; single quote is allowed for local-part
	    (setq vals (mapcar 'mew-chop (mew-split-quoted val ?, ?: ?\; 'no-single)))
	    (dolist (val vals)
	      (setq ins nil addrs nil)
	      (cond
	       ;; phrase:addr1,addr2;
	       ((string-match "[^:]+:[^;]+;" val)
		(let ((mew-alias-expand-prefix nil))
		  (setq addrs (mew-alias-expand val mew-addrbook-alist 0))
		  (setq addrs (mapcar 'mew-draft-append-domain addrs))
		  (setq ins (list (concat mew-alias-expand-prefix ":;")))))
	       ;; Name <addr>
	       ((and (setq addr (mew-addrstr-parse-address val))
		     (string-match (concat "\\(.*<\\)" (regexp-quote addr) "\\(>.*\\)")
				   val))
		(setq prefix (match-string 1 val))
		(setq suffix (match-string 2 val))
		(setq addr (mew-draft-append-domain addr))
		(setq addrs (list addr))
		(setq ins (list (concat prefix addr suffix))))
	       ;; addr
	       (t
		(let ((mew-alias-expand-prefix nil))
		  (setq addrs (mew-alias-expand val mew-addrbook-alist 0))
		  (setq addrs (mapcar 'mew-draft-append-domain addrs))
		  (if mew-alias-expand-prefix
		      (setq ins (list (concat mew-alias-expand-prefix ":;")))
		    (setq ins (copy-sequence addrs))))))
	      (setq insl (nconc insl ins))
	      (unless (string-match "bcc" key) ;; Removing Bcc:
		(setq ret (nconc ret addrs)))) ;; end of dolist
	    (insert " " (mapconcat 'identity insl ", "))
	    (setq insl nil)
	    (forward-line))))
      ret)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dcc:, Bcc:
;;;

(defun mew-encode-delete-dcc (resentp)
  (let* ((target (list (if resentp mew-resent-dcc: mew-dcc:)))
	 (dcc (mew-header-parse-address-list target)))
    ;; Dcc: is already contained in recipients.
    ;; So, just delete it.
    ;; Delete {Resent-,}Dcc: anyway.
    (mew-header-delete-lines (list mew-dcc: mew-resent-dcc:))
    dcc))

(defun mew-encode-delete-bcc (resentp)
  (let* ((target (list (if resentp mew-resent-bcc: mew-bcc:)))
	 (bcc (mew-header-parse-address-list target)))
    (mew-header-delete-lines (list mew-bcc: mew-resent-bcc:)) ;; anyway
    bcc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message-Id:, Date:
;;;

(defun mew-encode-id-date (pnm msgid &optional resentp)
  (let ((time (current-time)))
    (cond
     (resentp
      (mew-header-delete-lines (list mew-resent-date: mew-resent-message-id:))
      (save-excursion
	(goto-char (point-min))
	(mew-header-insert mew-resent-date: (mew-time-ctz-to-rfc time))
	(mew-header-insert mew-resent-message-id: msgid)))
     (t
      (mew-header-delete-lines (list mew-date: mew-message-id:))
      (save-excursion
	(goto-char (point-min))
	(mew-header-insert mew-date: (mew-time-ctz-to-rfc time))
	(mew-header-insert mew-message-id: msgid))))
    (list msgid (mew-time-ctz-to-logtime time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From:, Sender:
;;;

(defun mew-encode-check-sender (resentp)
  (if (< 1 (length (mew-addrstr-parse-address-list
		    (mew-header-get-value
		     (if resentp mew-resent-sender: mew-sender:)))))
      (mew-encode-error "Sender: must contain one address!")))

(defun mew-encode-from (case resentp)
  (unless (mew-header-existp mew-from:)
    (let ((from (mew-from case)))
      (save-excursion
	(goto-char (point-min))
	(if resentp
	    (mew-header-insert mew-resent-from: from)
	  (mew-header-insert mew-from: from))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Learning aliases
;;;

;; xxx Bcc: and/or Dcc:?
(defun mew-encode-learn-aliases (resentp)
  (if (and mew-use-auto-alias mew-addrbook-append-domain-p)
      ;; If mew-addrbook-append-domain-p is nil, automatic
      ;; short names would be conflicted to local users.
      (mapcar 'mew-addrbook-alias-add
	      (mew-header-parse-address-list
	       (if resentp
		   (list mew-resent-to: mew-resent-cc:)
		 (list mew-to: mew-cc:))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun mew-encode-set-privacy (pnm privacy case)
  (unless (or (mew-syntax-get-privacy mew-encode-syntax) ;; specified
	      ;; encryption previously failed, so bypass.
	      (and (null privacy) (mew-tinfo-get-privacy-err)))
    (let (type)
      (cond
       (privacy
	(setq type privacy))
       ((mew-tinfo-get-privacy-type)
	(setq type (mew-tinfo-get-privacy-type)))
       ((and (mew-tinfo-get-encrypted-p) (mew-protect-privacy-encrypted case))
	(setq type (mew-protect-privacy-encrypted-type case)))
       ((mew-protect-privacy-always case)
	(setq type (mew-protect-privacy-always-type case))))
      (mew-syntax-set-privacy
       mew-encode-syntax
       (mew-pcdb-ct (mew-pcdb-by-service type)))
      ;; recipients are ignored when signing
      (mew-syntax-set-decrypters
       mew-encode-syntax (mew-smtp-get-recipients pnm)))))

(defun mew-encode-make-header (&optional addsep resentp)
  (unless (mew-header-existp mew-mv:)
    (goto-char (mew-header-end))
    (mew-header-insert mew-mv: mew-mv:-num))
  (mew-header-encode-region (point-min) (mew-header-end) resentp)
  (cond
   (addsep ;; reedit
    ;; To:
    ;; Content-*
    ;; ----
    (mew-header-clear) ;; mew-in-header-p() returns nil
    ;; To:
    ;; Content-*
    (insert "\n"))
   (t
    ;; To:
    ;; ----
    ;; Content-*
    (mew-header-clear) ;; mew-in-header-p returns nil
    ;; To:
    ;; Content-*
    ))
  (mew-header-goto-end)
  (mew-highlight-header-region (point-min) (point)))

(defun mew-encode-save-draft ()
  (mew-frwlet mew-cs-dummy mew-cs-text-for-write
    (write-region (point-min) (point-max) (buffer-file-name) nil 'no-msg)
    (clear-visited-file-modtime)
    (mew-delete-file buffer-auto-save-file-name)
    (set-buffer-modified-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encoding a message
;;;

(defun mew-smtp-encode (pnm case resentp fcc &optional privacy signer headerp)
  (let ((ret t))
    (if (mew-debug 'encode)
	(let ((debug-on-error t))
	  (mew-smtp-encode-message pnm case resentp fcc privacy signer headerp))
      (condition-case nil
	  (mew-smtp-encode-message pnm case resentp fcc privacy signer headerp)
	(error
	 (mew-encode-undo)
	 (message "%s" (mew-tinfo-get-encode-err))
	 (setq ret nil))
	(quit
	 (mew-encode-undo)
	 (message "quit")
	 (setq ret nil))))
    ret))

(defun mew-smtp-encode-message (pnm case resentp fcc &optional privacy signer headerp)
  (mew-set-buffer-multibyte t)
  (if (buffer-modified-p) (save-buffer)) ;; to make backup
  (widen)
  (let (multip recipients msgid logtime)
    (mew-smtp-set-raw-header
     pnm (mew-buffer-substring (point-min) (mew-header-end)))
    (unless headerp
      ;; Let's backup
      (if (mew-attach-p)
	  (progn
	    (setq multip t)
	    (mew-attach-clear))
	(unless mew-encode-syntax
	  (setq mew-encode-syntax
		(mew-encode-syntax-single "text.txt" (list mew-ct-txt)))))
      (mew-encode-make-backup))
    ;; Errors can be caused from here.
    (goto-char (point-min))
    (mew-encode-remove-invalid-fields)
    ;; Destination check
    (setq recipients (mew-encode-canonicalize-address resentp))
    ;; Bcc: is not included.
    (mew-smtp-set-recipients pnm recipients)
    (mew-smtp-set-orig-recipients pnm recipients)
    (cond
     ((null recipients)
      (mew-encode-error "No recipient!"))
     ((stringp recipients)
      (mew-encode-error (format "'%s' is not in the right form!" recipients))))
    ;; Header modifications which are not remained.
    (mew-header-delete-lines (list mew-fcc: mew-resent-fcc:)) ;; anyway
    (mew-encode-delete-dcc resentp)
    (mew-smtp-set-bcc pnm (mew-encode-delete-bcc resentp))
    (mew-smtp-set-fcc pnm fcc)
    ;;
    (mew-encode-check-sender resentp)
    (mew-encode-from case resentp)
    ;;
    (mew-set '(msgid logtime) (mew-encode-id-date pnm (mew-smtp-message-id case) resentp))
    (mew-smtp-set-msgid pnm msgid)
    (mew-smtp-set-logtime pnm logtime)
    ;;
    (goto-char (mew-header-end))
    (forward-line) ;; necessary for PGP
    ;;
    (message "Making a message...")
    ;; save syntax before setting privacy
    (unless headerp
      (mew-encode-set-privacy pnm privacy case)
      (let (mew-inherit-encode-pgp-signer
	    mew-inherit-encode-smime-signer)
	(setq mew-inherit-encode-pgp-signer
	      (or signer
		  (mew-pgp-signer (mew-tinfo-get-case))
		  (mew-get-my-address)))
	(setq mew-inherit-encode-smime-signer
	      (or signer
		  (mew-smime-signer (mew-tinfo-get-case))
		  (mew-get-my-address)))
	(goto-char (mew-header-end)) ;; due to invalid null lines in the header
	(forward-line)
	(if multip
	    (mew-encode-make-multi)
	  (mew-encode-make-single))))
    (mew-encode-make-header headerp resentp)
    ;; Learn aliases after no error occurred
    (mew-encode-learn-aliases resentp)
    (mew-encode-save-draft)
    (mew-overlay-delete-buffer)
    (message "Making a message...done")))

(defun mew-nntp2-encode (pnm case fcc &optional privacy signer headerp)
  (let ((ret t))
    (if (mew-debug 'encode)
	(let ((debug-on-error t))
	  (mew-nntp2-encode-message pnm case fcc privacy signer headerp))
      (condition-case nil
	  (mew-nntp2-encode-message pnm case fcc privacy signer headerp)
	(error
	 (mew-encode-undo)
	 (message "%s" (mew-tinfo-get-encode-err))
	 (setq ret nil))
	(quit
	 (mew-encode-undo)
	 (message "quit")
	 (setq ret nil))))
    ret))

(defun mew-nntp2-encode-message (pnm case fcc &optional privacy signer headerp)
  (mew-set-buffer-multibyte t)
  (if (buffer-modified-p) (save-buffer)) ;; to make backup
  (widen)
  (let (multip newsgroups msgid logtime)
    (mew-nntp2-set-raw-header
     pnm (mew-buffer-substring (point-min) (mew-header-end)))
    ;; Let's backup
    (unless headerp
      (if (mew-attach-p)
	  (progn
	    (setq multip t)
	    (mew-attach-clear))
	(setq mew-encode-syntax
	      (mew-encode-syntax-single "text.txt" (list mew-ct-txt))))
      (mew-encode-make-backup))
    ;; Errors can be caused from here.
    (goto-char (point-min))
    (mew-encode-remove-invalid-fields)
    ;; Newsgroups check
    (setq newsgroups (mew-header-get-value mew-newsgroups:))
    (mew-nntp2-set-newsgroups pnm newsgroups)
    (unless newsgroups (mew-encode-error "No newsgroups!"))
    ;; Header modifications which are not remained.
    (mew-header-delete-lines (list mew-fcc: mew-resent-fcc:)) ;; anyway
    (mew-nntp2-set-fcc pnm fcc)
    ;;
    (mew-encode-from case nil)
    ;;
    (mew-set '(msgid logtime) (mew-encode-id-date pnm (mew-nntp-message-id case)))
    (mew-nntp2-set-msgid pnm msgid)
    (mew-nntp2-set-logtime pnm logtime)
    ;;
    (goto-char (mew-header-end))
    (forward-line) ;; necessary for PGP
    ;;
    (message "Making a message...")
    ;; save syntax before setting privacy
    (unless headerp
      (mew-encode-set-privacy pnm privacy case)
      (let (mew-inherit-encode-pgp-signer
	    mew-inherit-encode-smime-signer)
	(setq mew-inherit-encode-pgp-signer
	      (or signer
		  (mew-pgp-signer (mew-tinfo-get-case))
		  (mew-get-my-address)))
	(setq mew-inherit-encode-smime-signer
	      (or signer
		  (mew-smime-signer (mew-tinfo-get-case))
		  (mew-get-my-address)))
	(goto-char (mew-header-end)) ;; due to invalid null lines in the header
	(forward-line)
	(if multip
	    (mew-encode-make-multi)
	  (mew-encode-make-single))))
    (mew-encode-make-header nil)
    (mew-encode-save-draft)
    (mew-overlay-delete-buffer)
    (message "Making a message...done")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encoding singlepart
;;;

(defun mew-encode-make-single ()
  ;; Just after the header
  ;; Using Multipart/Signed if the body is a single part.
  (if (equal (mew-syntax-get-privacy mew-encode-syntax)
	     `((,mew-ct-smm ,mew-ct-smm-sig)))
      (mew-syntax-set-privacy mew-encode-syntax `((,mew-ct-mls ,mew-ct-sms))))
  (mew-encode-singlepart mew-encode-syntax nil nil t 'cover))

(defun mew-encode-charset-8bitp (charset)
  (let ((case-fold-search t))
    (if (or (mew-case-equal mew-us-ascii charset)
	    (string-match "^iso-2022" charset))
	nil
      t)))

(defun mew-encode-file-8bitp (file)
  (if (and (mew-which-exec mew-prog-8bit) (file-readable-p file))
      (eq (call-process mew-prog-8bit file nil nil "-8") 1)))

(defun mew-encode-limit-7bitp (privacy)
  ;; Only parts to be signed FIRST are limited to 7bit.
  (and privacy (mew-case-equal mew-ct-mls (car (car privacy)))))

(defvar mew-encode-max-line-length 990)
;; RFC 2822 defines 1000 but Sendmail uses 990

(defvar mew-ask-encoding t)
(defvar mew-default-encoding mew-b64)

(defun mew-encode-mime-body (ctl cte file no-encoding)
  ;; If file is 't', target is buffered.
  ;; text should be buffered
  ;;	- specified charset is a rare case
  ;;	- copy overhead may be small
  (let* ((ct (mew-syntax-get-value ctl 'cap))
         (textp (mew-ct-textp ct))
	 (charset (if textp (mew-syntax-get-param ctl "charset")))
	 (icharset (if textp (mew-syntax-get-param ctl "icharset")))
         (linebasep (or textp (mew-ct-linebasep ct)))
         (switch (if linebasep
                     mew-prog-mime-encode-text-switch
                   mew-prog-mime-encode-switch))
         (beg (point))
	 buffer cs opt last flowed delsp flowed-delsp)
    (if (and (stringp file)
	     (setq buffer (get-file-buffer file))
	     (buffer-modified-p buffer))
	(with-current-buffer buffer
	  (save-buffer)))
    (when textp
      (when (and (stringp file) (file-readable-p file))
	(when icharset
	  (setq cs (mew-charset-to-cs icharset))
	  (unless (mew-coding-system-p cs)
	    (mew-encode-error
	     (format "Unknown coding system %s in the body"
		     (symbol-name cs)))))
	(mew-frwlet (or cs mew-cs-autoconv) mew-cs-dummy
	  (mew-insert-file-contents file)))
      ;; A user may specify charset to convey JIS X 0201 Katakana.
      ;; So, we need to avoid the sanity check.
      (unless charset
	(if mew-use-charset-sanity-check
	    (mew-charset-sanity-check beg (point-max)))
	(setq charset (mew-charset-guess-region beg (point-max))))
      (setq cs (mew-charset-to-cs charset))
      (when (string= mew-ct-txt ct)
	(cond
	 ((mew-tinfo-get-flowed)
	  (setq flowed t)
	  (if (string= (mew-tinfo-get-flowed) "yes")
	      (setq delsp t)))
	 ((mew-tinfo-get-use-flowed)
	  (setq flowed-delsp (mew-encode-flowed beg (point-max) charset))
	  (mew-set '(flowed delsp) flowed-delsp))))
      (unless (mew-coding-system-p cs)
	(mew-encode-error
	 (format "Unknown coding system %s in the body" (symbol-name cs))))
      (mew-cs-encode-region beg (point-max) cs))
    ;; decide cte
    (cond
     (textp
      (cond
       ((and cte (or (mew-case-equal cte mew-b64) (mew-case-equal cte mew-qp)))
	;; user specified and in 7bit domain, so do nothing.
	)
       (t
	(if (or (null cte) (mew-case-equal cte mew-bin))
	    ;; retain the user-specified cte
	    (setq cte (mew-charset-to-cte charset)))
	(if (or (null cte) (mew-case-equal cte mew-bin))
	    ;; unknown charset
	    (setq cte mew-default-encoding))
	(cond
	 (no-encoding
	  (if (mew-encode-charset-8bitp charset)
	      (setq cte mew-8bit)
	    (setq cte mew-7bit)))
	 (mew-inherit-7bit
	  (if (mew-case-equal cte mew-8bit)
	      (setq cte mew-default-encoding)))
	 (mew-use-8bit
	  (if (mew-encode-charset-8bitp charset)
	      (setq cte mew-8bit))))
	(when (and (not no-encoding)
		   (or (mew-case-equal cte mew-7bit)
		       (mew-case-equal cte mew-8bit)))
	  (save-excursion
	    (goto-char beg)
	    (mew-set-buffer-multibyte nil)
	    (catch 'long-line
	      (while t
		(setq last (point))
		(if (/= (forward-line) 0) (end-of-line))
		(when (>= (- (point) last) mew-encode-max-line-length) ;; including LF
		  (if (not mew-ask-encoding)
		      (setq cte mew-default-encoding)
		    (setq cte (mew-input-encoding)))
		  (throw 'long-line nil))
		(if (eobp) (throw 'long-line nil))))
	    (mew-set-buffer-multibyte t))))))
     ((string= ct mew-ct-msg)
      (if (mew-encode-file-8bitp file)
	  (setq cte mew-8bit)
	(setq cte mew-7bit)))
     (t
      (if (and no-encoding cte (mew-case-equal cte mew-b64))
	  (setq cte mew-bin))
      ;; There are 7bit ascii bodies such as
      ;; application/pgp-encrypted and message/external-body.
      ;; If 7bit or 8bit, it should be linebase.
      (if (null cte) (setq cte mew-7bit))))
    (cond
     ((mew-case-equal cte mew-bin)
      (mew-frwlet mew-cs-binary mew-cs-dummy
	(mew-insert-file-contents file)))
     ((or (mew-case-equal cte mew-7bit) (mew-case-equal cte mew-8bit))
      ;; Certainly linebase here.
      (unless textp
	(mew-frwlet mew-cs-text-for-read mew-cs-dummy
	  (mew-insert-file-contents file)))
      (when (and (string= ct mew-ct-msg)
		 (or mew-inherit-7bit (not mew-use-8bit))
		 (mew-case-equal cte mew-8bit))
	(save-restriction
	  (setq cte mew-7bit)
	  (narrow-to-region beg (point-max))
	  (mew-convert-message))))
     ((and (mew-case-equal cte mew-b64) (fboundp 'base64-encode-region))
      (unless textp
	(mew-frwlet (if linebasep mew-cs-text-for-read mew-cs-binary) mew-cs-dummy
	  (mew-insert-file-contents file)))
      (when linebasep
	(goto-char beg)
	(mew-lf-to-crlf))
      (base64-encode-region beg (point-max))
      (goto-char (point-max))
      (insert "\n"))
     ((mew-which-exec mew-prog-mime-encode)
      (setq opt (mew-prog-mime-encode-get-opt cte switch))
      (if (null opt)
	  (mew-encode-error (concat "Unknown CTE: " cte))
	(when textp
	  (setq file (mew-make-temp-name))
	  (mew-frwlet mew-cs-dummy mew-cs-text-for-write
	    ;; NEVER use call-process-region for privacy reasons
	    (write-region beg (point-max) file nil 'no-msg))
	  (delete-region beg (point-max)))
	(mew-piolet mew-cs-text-for-read mew-cs-dummy
	  (apply 'call-process mew-prog-mime-encode file t nil opt))
	(if textp (mew-delete-file file))))
     (t
      (mew-encode-error (concat mew-prog-mime-encode " does not exist"))))
    (list cte charset flowed delsp)))

(defun mew-encode-no-mime-encoding (privacy)
  (let ((first (car privacy)))
    (and (listp first)
	 (or (string= (car first) mew-ct-mle)
	     (string= (car first) mew-ct-smm)))))

(defun mew-broken-name (cdpl)
  (let ((mew-encode-word-max-length 1000) ;; xxx
	(file (nth 1 (assoc "filename" cdpl)))
	name)
    (when (and file (string-match mew-regex-nonascii file))
      (setq name (car (mew-header-encode-string file)))
      ;; name must not be double-quoted here.
      ;; mew-heaer-insert will do this later
      (list "name" name))))

(defun mew-encode-flowed (beg end charset)
  "Encoding lines with RFC 3676"
  (let (flowed delsp column)
    (save-excursion
      (goto-char beg)
      (save-restriction
	(narrow-to-region beg end)
	(when (mew-encode-flowed-check)
	  (setq flowed t)
	  (setq delsp (mew-charset-to-delsp charset))
	  (setq column mew-flowed-fold-length)
	  (if delsp (setq column (1- column)))
	  (while (not (eobp))
	    (mew-encode-flowed-line column delsp)
	    (forward-line)))))
    (list flowed delsp)))

(defun mew-encode-flowed-remove-trailing-sp ()
  (while (and (not (bobp)) (= (char-before) mew-sp))
    (delete-char -1)))

(defun mew-encode-flowed-line (column delsp)
  (let (prefix beg)
    (looking-at "^>*")
    (setq prefix (mew-match-string 0))
    (goto-char (match-end 0))
    (if (looking-at " ")
	(if (string= prefix "")
	    (insert mew-flowed-stuffed)
	  (progn
	    (setq prefix (format "%s%c" prefix mew-flowed-stuffed))
	    (forward-char)))
      (when (looking-at "From ")
	(insert mew-flowed-stuffed)
	(setq prefix (format "%s%c" prefix mew-flowed-stuffed))))
    (setq beg (point))
    (save-excursion
      (end-of-line)
      (unless (and (= (- (point) beg) 3)
		   (string= (mew-buffer-substring beg (point)) "-- "))
	(mew-encode-flowed-remove-trailing-sp)))
    (move-to-column column)
    (catch 'loop
      (while (not (eolp))
	(cond
	 (delsp
	  (while (> (current-column) column)
	    (backward-char 1))
	  (insert " \n"))
	 (t
	  (if (search-backward " " beg t)
	      (forward-char)
	    (unless (search-forward " " (save-excursion (end-of-line) (point)) t)
	      (throw 'loop (end-of-line))))
	  (insert "\n")))
	(insert prefix)
	(setq beg (point))
	(move-to-column column)))))

(defun mew-encode-flowed-check ()
  (catch 'loop
    (save-excursion
      (save-restriction
	(narrow-to-region (point-min) (min (point-max) mew-file-max-size))
	(goto-char (point-min))
	(while (not (eobp))
	  (end-of-line)
	  (if (> (current-column) mew-flowed-fold-threshold)
	      (throw 'loop t))
	  (forward-line))))))

(defun mew-encode-singlepart (syntax &optional path depth buffered coverp)
  ;; path is nil if called make-single or security multipart
  ;; buffered is t if called make-single
  (run-hook-with-args 'mew-encode-singlepart-hook
                      syntax path depth buffered)
  (let* ((file (expand-file-name (mew-syntax-get-file syntax) path))
	 (ctl (mew-syntax-get-ct syntax))
         (ct (mew-syntax-get-value ctl 'cap))
	 (cte (mew-syntax-get-cte syntax))
	 (cd (mew-syntax-get-cd syntax))
	 (cdpl (mew-syntax-get-cdp syntax))
	 (privacy (mew-syntax-get-privacy syntax))
	 (no-encoding (mew-encode-no-mime-encoding privacy))
	 (mew-inherit-7bit (mew-encode-limit-7bitp privacy))
	 (beg (point))
	 mret charset bodybeg cst ask-cst broken-name flowed delsp)
    (setq mret (mew-encode-mime-body ctl cte (or buffered file) no-encoding))
    (goto-char beg)
    (mew-set '(cte charset flowed delsp) mret)
    (setq ctl (mew-delete "icharset" ctl))
    (when charset
      (setq ctl (mew-syntax-get-params ctl))
      (setq ctl (mew-delete "charset" ctl))
      (setq ctl (cons ct (cons (list "charset" charset) ctl))))
    (when flowed
      (setq ctl (nconc ctl (list (list "format" "flowed"))))
      (if delsp (setq ctl (nconc ctl (list (list "delsp" "yes"))))))
    ;;
    (setq ctl (mew-delete "name" ctl))
    (setq broken-name (mew-broken-name cdpl))
    (if broken-name (setq ctl (nconc ctl (list broken-name))))
    ;;
    (mew-header-insert mew-ct: ctl)
    (mew-header-insert mew-cte: cte)
    (and cd (mew-header-insert mew-cd: cd))
    (and cdpl (mew-header-insert mew-cdp: cdpl))
    (insert "\n")
    ;; header "\n" (cur) [text]
    (setq bodybeg (point))
    (goto-char (point-max))
    (when (and (string= ct mew-ct-msg) mew-field-delete-for-forwarding)
      (save-restriction
	(narrow-to-region bodybeg (point-max))
	(mew-header-delete-lines mew-field-delete-common)
	(mew-header-delete-lines mew-field-delete-for-forwarding)))
    (when privacy
      (mew-encode-security-multipart
       beg privacy depth (mew-syntax-get-decrypters syntax) cte))
    (goto-char (point-max))
    (when (and coverp (setq cst charset))
      (cond
       ((null mew-ask-charset)
	;; not ask
	)
       ((eq mew-ask-charset t)
	(if (mew-member-case-equal cst mew-cs-m17n-list)
	    (setq ask-cst t)))
       ((listp mew-ask-charset)
	(unless (mew-member-case-equal cst mew-ask-charset)
	  (setq ask-cst t))))
      (if (and ask-cst
	       (not (y-or-n-p (format "%s is used. OK? " cst))))
	  (mew-encode-error "Modify body")))
    (mew-case-equal cte mew-8bit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encoding multipart
;;;

(defun mew-encode-make-multi ()
  ;; Just after the header
  (let (beg path multip buffered privacy decrypters first ct)
    (if (mew-encode-syntax-have-one-part)
	;; A user may want to do any MIME stuff to the body
	(progn
	  (setq mew-encode-syntax (mew-syntax-get-part mew-encode-syntax))
	  (setq buffered t))
      ;; See if a cover page is empty or not
      (setq beg (point))
      (save-excursion
	(while (and (looking-at "^$") (not (eobp)))
	  (forward-line))
	(unless (eobp)
	  ;; The cover page exists.
	  (setq buffered t)))
      (setq path (mew-expand-folder mew-attach-folder))
      (if buffered
	  ;; The cover page exists.
	  (setq multip t)
	;; The cover page does not exist.
	(delete-region beg (point-max))
	;; Remove the cover page entry from the syntax.
	(setq mew-encode-syntax
	      (mew-syntax-remove-entry mew-encode-syntax '(1)))
	;; After removing the over page, see if this message has
	;; only one text part.
	(if (not (mew-encode-syntax-have-one-part))
	    (setq multip t)
	  (setq first (mew-syntax-get-part mew-encode-syntax))
	  (setq ct (mew-syntax-get-value (mew-syntax-get-ct first) 'cap))
	  (if (not (mew-ct-textp ct))
	      (setq multip t)		
	    (setq path (expand-file-name
			(mew-syntax-get-file mew-encode-syntax) path))
	    (setq privacy (mew-syntax-get-privacy mew-encode-syntax))
	    (setq decrypters (mew-syntax-get-decrypters mew-encode-syntax))
	    (setq mew-encode-syntax (mew-syntax-get-part mew-encode-syntax))
	    (mew-syntax-set-privacy mew-encode-syntax privacy)
	    (mew-syntax-set-decrypters mew-encode-syntax decrypters)))))
    (if multip
	(mew-encode-multipart mew-encode-syntax path 0 buffered)
      (mew-encode-singlepart mew-encode-syntax path nil nil 'cover))))

(defvar mew-default-boundary "--%s(%s_%s)--")

(defun mew-boundary-get (&optional string)
  ;; boundary is less than or equal to 70
  (unless string (setq string "Next_Part"))
  (format mew-default-boundary
	  string
	  (mew-replace-character
	   (mew-replace-character (current-time-string) mew-sp ?_) ?: ?_)
	  (mew-random-string 3 t)))

(defun mew-encode-multipart (syntax path depth &optional buffered cte)
  (let* ((boundary
	  (mew-boundary-get ;; 0 is nil for Next_Part
	   (if (> depth 0) (format "BOUNDARY%s" (number-to-string depth)))))
	 (fullname (expand-file-name (mew-syntax-get-file syntax) path))
	 (ctl (mew-syntax-get-ct syntax))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (cd (mew-syntax-get-cd syntax))
	 (privacy (mew-syntax-get-privacy syntax))
	 (mew-inherit-7bit (mew-encode-limit-7bitp privacy))
	 (len (length syntax))
	 (beg (point))
	 (cnt mew-syntax-magic)
	 (8bit-cnt 0)
	 8bitp cte-pos cover)
    (mew-header-insert mew-ct: (list ct (list "boundary" boundary)))
    (setq cte-pos (point))
    (and cd (mew-header-insert mew-cd: cd))
    (while (< cnt len)
      (insert "\n--" boundary "\n")
      (if (mew-syntax-multipart-p (aref syntax cnt))
	  (setq 8bitp (mew-encode-multipart
		       (aref syntax cnt)
		       fullname
		       (1+ depth)))
	(if (and (= depth 0) (= cnt mew-syntax-magic ))
	    (setq cover t)
	  (setq cover nil))
	(setq 8bitp (mew-encode-singlepart
		     (aref syntax cnt)
		     fullname
		     (1+ depth)
		     (if (eq cnt mew-syntax-magic) buffered nil)
		     cover)))
      (if 8bitp (setq 8bit-cnt (1+ 8bit-cnt)))
      (setq cnt (1+ cnt)))
    (insert "\n--" boundary "--\n")
    (save-excursion
      (goto-char cte-pos)
      (mew-header-insert mew-cte: (if (> 8bit-cnt 0) mew-8bit mew-7bit)))
    (when privacy
      (mew-encode-security-multipart
       beg privacy depth (mew-syntax-get-decrypters syntax) cte))
    (goto-char (point-max))
    (> 8bit-cnt 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Privacy services
;;;

(defun mew-encode-security-multipart (beg privacy depth decrypters cte)
  (save-restriction
    (narrow-to-region beg (point-max))
    (let (proto ct)
      (dolist (ent privacy)
	(goto-char (point-min))
	(mew-set '(ct proto) ent)
	(setq ct (mew-capitalize ct))
	(cond
	 ((string= ct mew-ct-mle)
	  (mew-encode-multipart-encrypted ct proto depth decrypters cte))
	 ((string= ct mew-ct-mls)
	  (mew-encode-multipart-signed ct proto depth))
	 ((string= ct mew-ct-smm)
	  (mew-encode-smime proto cte decrypters)))))))

(defun mew-security-multipart-boundary (depth)
   (if depth
       (mew-boundary-get (format "Security_Multipart%s" (number-to-string depth)))
     (mew-boundary-get "Security_Multipart")))

(defun mew-save-transfer-form (beg end retain &optional cte)
  ;; called in the narrowed region
  (let ((sbeg beg) (send end) (draft-buf (current-buffer))
	(ocs mew-cs-text-for-net)
	tmpbuf file)
    (if retain
	(progn
	  (setq tmpbuf (generate-new-buffer mew-buffer-prefix))
	  (set-buffer tmpbuf)
	  (mew-erase-buffer)
	  (mew-insert-buffer-substring draft-buf beg end)
	  (setq sbeg (point-min) send (point-max))))
    (goto-char sbeg) ;; just in case
    (if (and cte (mew-case-equal cte mew-bin)
	     (re-search-forward mew-eoh))
	(progn
	  (setq ocs mew-cs-binary)
	  (forward-line)
	  (save-restriction
	    (narrow-to-region sbeg (point))
	    (goto-char sbeg)
	    (mew-lf-to-crlf)))
      (unless mew-cs-text-for-net
	(goto-char sbeg) ;; just in case
	(mew-lf-to-crlf)))
    (setq send (point-max))
    (setq file (mew-make-temp-name))
    (mew-frwlet mew-cs-dummy ocs
      (write-region sbeg send file nil 'no-msg))
    (if retain
	(mew-remove-buffer tmpbuf)
      (delete-region sbeg send))
    (set-buffer draft-buf)
    file)) ;; return value

(defun mew-encode-multipart-encrypted (ct proto depth decrypters cte)
  ;; called in the narrowed region
  (let* ((boundary (mew-security-multipart-boundary depth))
	 (switch mew-encode-multipart-encrypted-switch) ;; save length
	 (func (mew-encode-get-security-func proto switch))
	 file1 file2 file3 cte2 cte3 fc errmsg)
    ;; Write the part converting line breaks.
    (setq file1 (mew-save-transfer-form (point-min) (point-max) nil cte))
    ;; The narrowed region stores nothing
    ;; Call the protocol function
    (condition-case nil
	(setq fc (funcall func file1 decrypters))
      (error
       (mew-delete-file file1)
       (mew-encode-error
	(format "unknown error for %s. Check %s, anyway"
		mew-ct-mle mew-temp-dir))))
    (mew-set '(file2 cte2 file3 cte3 errmsg) fc)
    (if errmsg
	(progn
	  (mew-delete-file file1)
	  (mew-delete-file file2)
	  (mew-delete-file file3)
	  (mew-tinfo-set-privacy-err t)
	  (mew-tinfo-set-privacy-type nil)
	  (mew-draft-mode-name)
	  (mew-encode-error errmsg))
      ;; Create multipart content-header
      (mew-header-insert mew-ct: (list ct
				       (list "protocol" proto)
				       (list "boundary" boundary)))
      (mew-header-insert mew-cte: mew-7bit)
      (insert (format "\n--%s\n" boundary))
      ;; Insert control keys
      (mew-encode-singlepart
       (mew-encode-syntax-single file2 (list proto) cte2))
      (insert (format "\n--%s\n" boundary))
      ;; Insert encrypted body
      (mew-encode-singlepart
       (mew-encode-syntax-single file3 mew-type-apo cte3))
      (insert (format "\n--%s--\n" boundary))
      ;; Throw away the garbage
      (mew-delete-file file1)
      (mew-delete-file file2)
      (mew-delete-file file3))))

(defun mew-encode-multipart-signed (ct proto depth)
  ;; called in the narrowed region
  (let* ((boundary (mew-security-multipart-boundary depth))
	 (switch mew-encode-multipart-signed-switch) ;; save length
	 (func (mew-encode-get-security-func proto switch))
	 (canon-func (mew-encode-get-canonicalize-func proto switch))
	 file1 file2 micalg cte2 fmc errmsg ct2 cdp2)
    (if (fboundp canon-func) (funcall canon-func))
    (setq file1 (mew-save-transfer-form (point-min) (point-max) 'retain))
    ;; The narrowed region still the ORIGINAL part (i.e. line breaks are LF)
    ;; Call the protocol function
    (condition-case nil
	(setq fmc (funcall func file1))
      (error
       (mew-delete-file file1)
       (mew-encode-error
	(format "unknown error for %s. Check %s, anyway"
		mew-ct-mls mew-temp-dir))))
    (mew-set '(file2 cte2 micalg errmsg ct2 cdp2) fmc)
    (if errmsg
	(progn
	  (mew-delete-file file1)
	  (mew-delete-file file2)
	  (mew-tinfo-set-privacy-err t)
	  (mew-tinfo-set-privacy-type nil)
	  (mew-draft-mode-name)
	  (mew-encode-error errmsg))
      (goto-char (point-min))
      ;; Before the signed part
      ;; Create multipart content-header
      (mew-header-insert mew-ct: (list ct
				       (list "protocol" proto)
				       (list "micalg" micalg)
				       (list "boundary" boundary)))
      (mew-header-insert mew-cte: mew-7bit)
      (insert (format "\n--%s\n" boundary))
      (goto-char (point-max))
      ;; After the signed part
      (insert (format "\n--%s\n" boundary))
      (mew-encode-singlepart
       (mew-encode-syntax-single file2 (or ct2 (list proto)) cte2 nil nil cdp2))
      (insert (format "\n--%s--\n" boundary))
      ;; Throw away the garbage
      (mew-delete-file file1)
      (mew-delete-file file2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 8bit to 7bit conversion for Multipart/Security
;;;

(defun mew-convert-mime-body (beg end cte linebasep)
  (let* ((switch (if linebasep
                     mew-prog-mime-encode-text-switch
                   mew-prog-mime-encode-switch))
	 file opt)
    (save-restriction
      (narrow-to-region beg end)
      (cond
       ((and (mew-case-equal cte mew-b64) (fboundp 'base64-encode-region))
	(when linebasep
	  (goto-char (point-min))
	  (mew-lf-to-crlf))
	(base64-encode-region (point-min) (point-max))
	(goto-char (point-max))
	(insert "\n"))
       ((mew-which-exec mew-prog-mime-encode)
	(setq opt (mew-prog-mime-encode-get-opt cte switch))
	(if (null opt)
	    (mew-encode-error (concat "Unknown CTE: " cte))
	  (setq file (mew-make-temp-name))
	  (mew-frwlet mew-cs-dummy (if linebasep mew-cs-text-for-write mew-cs-binary)
	    ;; NEVER use call-process-region for privacy reasons
	    (write-region (point-min) (point-max) file nil 'no-msg)
	    (delete-region (point-min) (point-max)))
	  (mew-piolet mew-cs-text-for-read mew-cs-dummy
	    (apply 'call-process mew-prog-mime-encode file t nil opt))
	  (mew-delete-file file)))
       (t
	(mew-encode-error (concat mew-prog-mime-encode " does not exist")))))))

(defun mew-convert-message ()
  ;; called on the beginning of a header
  (let ((case-fold-search t)
	(buf (current-buffer))
	hd-end mimep charset cte body-beg body-end)
    (unless (re-search-forward mew-eoh nil t)
      (goto-char (point-max)))
    (setq hd-end (point-marker))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" mew-mv:) (marker-position hd-end) t)
	(setq mimep t))
    (cond
     (mimep
      (goto-char (point-min))
      (mew-convert-singlepart)
      (mew-header-delete-lines (list mew-mv:))
      (goto-char hd-end)
      (mew-header-insert mew-mv: (concat mew-mv:-num " " mew-field-comment)))
     (t
      (goto-char hd-end)
      (forward-line)
      (setq body-beg (point))
      (setq body-end (point-max))
      (with-temp-buffer
	(mew-insert-buffer-substring buf body-beg body-end)
	(mew-cs-decode-region (point-min) (point-max) mew-cs-autoconv)
	(setq charset (mew-charset-guess-region (point-min) (point-max))))
      (when (mew-encode-charset-8bitp charset)
	(setq cte (mew-charset-to-cte charset))
	(if (null cte) (setq cte mew-b64))
	(mew-convert-mime-body body-beg (point-max) cte 'linebasep)
	(goto-char (point-min))
	(mew-header-delete-lines (list "Content-"))
	(goto-char hd-end)
	(mew-header-insert mew-mv: mew-mv:-num)
	(mew-header-insert mew-ct: (list mew-ct-txt (list "charset" charset)))
	(mew-header-insert mew-cte: cte))))))

(defun mew-convert-singlepart (&optional dctl)
  ;; called on the beginning of a content-header
  (let ((case-fold-search t)
	cthd-end ctbody-beg
	ct-val cte-val
	ctl ct cte charset)
    (unless (re-search-forward mew-eoh nil t)
      (goto-char (point-max)))
    (setq cthd-end (point-marker))
    (forward-line)
    (setq ctbody-beg (point))
    (goto-char (point-min))
    (if (not (re-search-forward (concat "^" mew-cte: "[ \t]*") nil t))
	(setq cte mew-7bit)
      (setq cte-val (point-marker))
      (forward-line)
      (mew-header-goto-next)
      (setq cte (mew-addrstr-parse-value
		 (mew-buffer-substring cte-val (1- (point))))))
    (when (or (mew-case-equal cte mew-8bit) (mew-case-equal cte mew-bin))
      (goto-char (point-min))
      (if (not (re-search-forward (concat "^" mew-ct: "[ \t]*") nil t))
	  (setq ctl (or dctl mew-type-txt))
	(setq ct-val (point))
	(forward-line)
	(mew-header-goto-next)
	(setq ctl (mew-param-decode
		   (mew-buffer-substring ct-val (1- (point)))))
	(setq ct (mew-syntax-get-value ctl 'cap))
	(cond
	 ((mew-ct-multipartp ct)
	  (if (or (string= ct mew-ct-mle) (string= ct mew-ct-mls))
	      ()
	    (mew-convert-multipart ctl))
	  (setq cte mew-7bit))
	 ((mew-ct-messagep ct)
	  (if (string= ct mew-ct-msg)
	      (mew-convert-message))
	  (setq cte mew-7bit))
	 ((mew-ct-textp ct)
	  (setq charset (mew-syntax-get-param ctl "charset"))
	  (setq cte (mew-charset-to-cte charset))
	  (if (or (null cte)
		  (mew-case-equal cte mew-8bit)
		  (mew-case-equal cte mew-bin))
	      (setq cte mew-b64))
	  (unless (mew-case-equal cte mew-7bit)
	    (mew-convert-mime-body ctbody-beg (point-max) cte 'linebasep)))
	 (t
	  ;; rare case
	  (setq cte (mew-ctdb-cte (mew-ctdb-by-ct ct)))
	  (if (or (null cte)
		  (mew-case-equal cte mew-8bit)
		  (mew-case-equal cte mew-bin))
	      (setq cte mew-b64))
	  (unless (mew-case-equal cte mew-7bit)
	    (mew-convert-mime-body
	     ctbody-beg (point-max) cte (mew-ct-linebasep ct))))
	 (goto-char (point-min)))
	(mew-header-delete-lines (list mew-cte:))
	(goto-char cthd-end)
	(mew-header-insert mew-cte: (concat cte " " mew-field-comment))))))

(defun mew-convert-multipart (ctl)
  (let* ((case-fold-search nil) ;; boundary is case sensitive
	 (ct (mew-syntax-get-value ctl 'cap))
	 (dctl (if (string= ct mew-ct-mld) mew-type-msg))
	 (boundary (regexp-quote (mew-syntax-get-param ctl "boundary")))
	 obound ebound bregex start break)
    (unless boundary
      (mew-encode-error "No boundary parameter for multipart"))
    (setq obound (concat "--" boundary))
    (setq ebound (concat "--" boundary "--"))
    (setq bregex (concat "^--" boundary "\\(\\|--\\)$"))
    (unless (re-search-forward (concat "^" obound "$") nil t)
      (mew-encode-error (format "No first boundary for %s" ct)))
    (forward-line)
    (setq start (point)) ;; the beginning of the part
    (catch 'multipart
      (while t
	(unless (re-search-forward bregex nil t)
	  (mew-encode-error (format "No last boundary for %s" ct)))
	(setq break (string= (regexp-quote (match-string 0)) ebound))
	(forward-line) ;; the beginning of the next part
	(save-excursion
	  (forward-line -1)
	  (beginning-of-line) ;; just in case
	  (forward-char -1) ;; skip the preceding CRLF
	  ;; the end of the part
	  (save-restriction
	    (narrow-to-region start (point))
	    (goto-char (point-min))
	    ;; the beginning of the part
	    (mew-convert-singlepart dctl)))
	(setq start (point)) ;; the beginning of the part
	(if break (throw 'multipart nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Backup and undo
;;;

(defun mew-encode-make-backup ()
  (let* ((file (buffer-file-name))
	 (back (mew-prepend-prefix file mew-backup-prefix))
	 (info (concat file mew-draft-info-suffix)))
    (mew-frwlet mew-cs-dummy mew-cs-m17n
      (write-region (point-min) (point-max) back nil 'no-msg))
    (mew-lisp-save
     info
     (list
      (cons "Syntax:" mew-encode-syntax)
      (cons "Case:" (mew-tinfo-get-case))
      (cons "Flowed:" (mew-tinfo-get-flowed))
      (cons "Use-Flowed:" (mew-tinfo-get-use-flowed))
      (cons "Message:" (mew-tinfo-get-hdr-file))) ;; Header mode
     'nobackup)
    nil)) ;; to save

(defun mew-encode-load-syntax ()
  (let* ((file (buffer-file-name))
	 (info (concat file mew-draft-info-suffix))
	 syntax)
    (when (file-exists-p info)
      (setq syntax (mew-lisp-load info))
      (setq mew-encode-syntax (cdr (assoc "Syntax:" syntax)))
      (if (and mew-encode-syntax
	       (mew-syntax-singlepart-p mew-encode-syntax))
	  (setq mew-encode-syntax nil))
      (mew-tinfo-set-case (cdr (assoc "Case:" syntax)))
      (mew-tinfo-set-flowed (cdr (assoc "Flowed:" syntax)))
      (mew-tinfo-set-flowed (cdr (assoc "Use-Flowed:" syntax)))
      (mew-tinfo-set-hdr-file (cdr (assoc "Message:" syntax))) ;; Header mode
      t)))

(defun mew-encode-insert-backup ()
  (let* ((file (buffer-file-name))
	 (back (mew-prepend-prefix file mew-backup-prefix)))
    (when (file-exists-p back)
      (mew-frwlet mew-cs-m17n mew-cs-dummy
	(mew-insert-file-contents back)))))

(defun mew-encode-undo ()
  "Get back to the draft before making MIME message."
  (interactive)
  (mew-elet
   (if (not (mew-encode-load-syntax))
       (message "Cannot undo")
     (mew-erase-buffer)
     (if (not (mew-encode-insert-backup))
	 (message "Cannot undo")
       (mew-header-clear) ;; erase the old header separator
       (mew-header-prepared)
       (if mew-encode-syntax (mew-draft-prepare-attachments))
       (mew-draft-toolbar-update)
       (setq buffer-undo-list nil)))))

(provide 'mew-encode)

;;; Copyright Notice:

;; Copyright (C) 1996-2012 Mew developing team.
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

;;; mew-encode.el ends here
