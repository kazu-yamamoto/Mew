;;; mew-edit.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov  9, 1999

;;; Code:

(require 'mew)

(defun mew-decode-for-edit ()
  "Decode a message without limitations.
Execute mew-dinfo-set before calling this."
  ;; See also mew-decode.
  (mew-decode-syntax-clear)
  (mew-xinfo-set-text-body t)
  (mew-set-buffer-multibyte t)
  (let ((mew-header-max-length nil)
	(mew-header-max-depth nil))
    (if (mew-debug 'decode)
	(let ((debug-on-error t))
	  (setq mew-decode-syntax
		(mew-decode-message (mew-decode-syntax-rfc822-head) 0)))
      (condition-case nil
	  (setq mew-decode-syntax
		(mew-decode-message (mew-decode-syntax-rfc822-head) 0))
	(error
	 (error "MIME decoding error: %s" (mew-xinfo-get-decode-err)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header mode
;;;

(defun mew-header-mode ()
  "A major mode for composing a MIME message by editing its header only.

\\{mew-header-mode-map}"
  (interactive)
  (mew-draft-set-local-variables)
  (setq major-mode 'mew-header-mode)
  (use-local-map mew-header-mode-map)
  (cd (expand-file-name mew-home))
  (mew-header-setup-decoration)
  (mew-draft-mode-name 'header)
  (force-mode-line-update)
  (run-hooks 'mew-header-mode-hook)
  ;; auto-fill-function is set by mew-header-mode-hook
  (when auto-fill-function
    (make-local-variable 'auto-fill-function)
    (setq auto-fill-function 'mew-draft-auto-fill)))

(defmacro mew-summary-header-mode (fields &rest body)
  ;; (declare (indent 1))
  `(mew-summary-msg-or-part
    (mew-summary-display 'redisplay)
    (mew-current-set-window-config)
    (let* ((draft (mew-folder-new-message mew-draft-folder))
	   (attachdir (mew-attachdir draft))
	   (fid (mew-frame-id))
	   (fld (mew-current-get-fld fid))
	   (msg (mew-current-get-msg fid)))
      (mew-summary-prepare-draft
       (mew-draft-find-and-switch draft)
       (mew-delete-directory-recursively attachdir)
       ,@body
       (mew-header-mode)
       ;; (mew-header-set "\n") is enough. But highlighting delayed.
       (mew-header-prepared)
       (goto-char (point-min))
       (end-of-line)
       (mew-tinfo-set-hdr-file (mew-expand-msg fld msg))
       (mew-tinfo-set-field-del ,fields)))))
(put 'mew-summary-header-mode 'lisp-indent-function 1)

(defun mew-summary-send-to-others ()
  "Prepare Header mode so that this message is to be sent
other people by a header re-write."
  (interactive)
  (mew-summary-header-mode mew-field-delete-for-others
    (let ((case (mew-tinfo-get-case)))
      (mew-draft-header-insert mew-to: "")
      (mew-draft-header-insert mew-cc: (mew-cc case))
      (mew-draft-header-insert mew-from: (mew-from case))
      (mew-draft-header-insert mew-fcc: (mew-fcc case))
      (mew-draft-header-insert mew-bcc: (mew-bcc case))
      (mew-draft-header-insert mew-dcc: (mew-dcc case))
      (mew-draft-header-insert mew-organization: (mew-organization case)))))

(defun mew-summary-resend ()
  "Prepare Header mode so that this message is to be forwarded
by a header re-write."
  (interactive)
  (mew-summary-header-mode mew-field-delete-for-resending
    (let ((case (mew-tinfo-get-case)))
      (mew-draft-header-insert mew-resent-to: "")
      (mew-draft-header-insert mew-resent-cc: (mew-cc case))
      (mew-draft-header-insert mew-resent-from: (mew-from case))
      (mew-draft-header-insert mew-resent-fcc: (mew-fcc case))
      (mew-draft-header-insert mew-resent-bcc: (mew-bcc case))
      (mew-draft-header-insert mew-resent-dcc: (mew-dcc case)))))

(defun mew-header-process-message (action)
  (if (not (file-readable-p (mew-tinfo-get-hdr-file)))
      (message "No corresponding message!")
    (if (mew-header-existp mew-newsgroups:)
	(mew-header-nntp-process-message action)
      (mew-header-smtp-process-message action))))

(defun mew-header-smtp-process-message (action)
  (let* ((buf (current-buffer))
	 (case (mew-tinfo-get-case))
	 (pnm (mew-smtp-info-name case))
	 (queue (mew-queue-folder case))
	 sendit resentp fcc msg err)
    (if (get-process pnm)
	(message "Another message is being sent. Try later")
      (mew-draft-remove-invalid-fields)
      (goto-char (point-min))
      (setq resentp (mew-draft-resent-p (mew-header-end)))
      (setq fcc (mew-encode-ask-fcc resentp))
      (if (and (eq action 'send) mew-ask-send)
	  (setq sendit (y-or-n-p "Really send this message? "))
	(setq sendit t))
      (when sendit
	(mew-current-get-window-config)
	(mew-redraw)
	(save-excursion
	  (with-current-buffer buf
	    (mew-encode-make-backup)
	    (mew-header-clear)
	    ;; the end of the header
	    (save-restriction
	      (narrow-to-region (point) (point))
	      (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
		(mew-insert-file-contents (mew-tinfo-get-hdr-file)))
	      (goto-char (point-min))
	      (mew-header-goto-end)
	      (mew-header-set mew-header-separator)
	      (mew-header-delete-lines mew-field-delete-common)
	      (mew-header-delete-lines (mew-tinfo-get-field-del)))
	    (if (mew-smtp-encode pnm case resentp fcc nil nil 'header)
		(let ((mdi (concat (buffer-file-name) mew-draft-info-suffix)))
		  (mew-delete-file mdi)
		  (setq msg (mew-smtp-queue case "from Draft mode"))
		  (mew-remove-buffer buf)
		  (if (eq action 'send)
		      (mew-smtp-send-message case queue (list msg))))
	      (setq err t))))
	(if err
	    (progn
	      (mew-current-set-window-config)
	      (switch-to-buffer buf)
	      (delete-other-windows))
	  (if (and (eq action 'queue) mew-visit-queue-after-sending)
	      (mew-summary-visit-folder queue)))))))

(defun mew-header-nntp-process-message (action)
  (let* ((buf (current-buffer))
	 (case (mew-tinfo-get-case))
	 (pnm (mew-nntp2-info-name case))
	 (postq (mew-postq-folder case))
	 sendit resentp fcc msg err)
    (if (get-process pnm)
	(message "Another message is being posted. Try later")
      (mew-draft-remove-invalid-fields)
      (goto-char (point-min))
      (setq resentp (mew-draft-resent-p (mew-header-end)))
      (setq fcc (mew-encode-ask-fcc resentp))
      (if (and (eq action 'send) mew-ask-send)
	  (setq sendit (y-or-n-p "Really post this message? "))
	(setq sendit t))
      (when sendit
	(mew-current-get-window-config)
	(mew-redraw)
	(save-excursion
	  (with-current-buffer buf
	    (mew-encode-make-backup)
	    (mew-header-clear)
	    ;; the end of the header
	    (save-restriction
	      (narrow-to-region (point) (point))
	      (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
		(mew-insert-file-contents (mew-tinfo-get-hdr-file)))
	      (goto-char (point-min))
	      (mew-header-goto-end)
	      (mew-header-set mew-header-separator)
	      (mew-header-delete-lines mew-field-delete-common)
	      (mew-header-delete-lines (mew-tinfo-get-field-del)))
	    (if (mew-nntp2-encode pnm case fcc nil nil 'header)
		(let ((mdi (concat (buffer-file-name) mew-draft-info-suffix)))
		  (mew-delete-file mdi)
		  (setq msg (mew-nntp2-queue case "from Draft mode"))
		  (mew-remove-buffer buf)
		  (if (eq action 'send)
		      (mew-nntp2-send-message  case postq (list msg))))
	      (setq err t))))
	(if err
	    (progn
	      (mew-current-set-window-config)
	      (switch-to-buffer buf)
	      (delete-other-windows))
	  (if (and (eq action 'queue) mew-visit-queue-after-sending)
	      (mew-summary-visit-folder postq)))))))

(defun mew-header-make-message ()
  "Compose a MIME message from the header and its body
then put it into a queue folder."
  (interactive)
  (mew-header-process-message 'queue))

(defun mew-header-send-message ()
  "Compose a MIME message from the header and its body
then send it."
  (interactive)
  (mew-header-process-message 'send))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saving
;;;

(defun mew-summary-save (&optional askcs)
  "Save any parts. If the target is a message, you are asked which
you want to save, the entire message or its body. If the target is
a non-message part, the part is saved (with line delimiter conversion
if it is a text object). When executed with '\\[universal-argument]', coding-system for
a text object to be saved can be specified."
  (interactive "P")
  (mew-summary-msg-or-part
   ;; need to make a cache or a message buffer.
   (mew-summary-toggle-disp-msg 'on)
   (mew-summary-display)
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number2))
	  (num (mew-syntax-number))
	  (nums (mew-syntax-number-to-nums num))
	  (cbuf (mew-cache-hit fld msg))
	  (alt  (mew-cache-dinfo-get-use-alt cbuf))
	  (cache (or cbuf (mew-buffer-message)))
	  (syntax (mew-syntax-get-entry (mew-cache-decode-syntax cache) nums))
	  (action "Save")
	  PLUS1P limit have-hdrp bodyp beg end cdpl file ct ctl
	  doit append-p)
     ;; Due to mew-decode-broken, the filename may be changed.
     ;; So, save it here.
     (and syntax (setq cdpl (mew-syntax-get-cdp syntax)))
     (setq ctl (mew-syntax-get-ct syntax))
     (setq file (mew-syntax-get-filename cdpl ctl))
     ;; First of all, we should determine which part the user want to
     ;; save due to the ambiguity.
     ;; "y" on Message/Rfc822
     ;;     - msg/txt      the entire msg or its body?
     ;;     - msg/mul/txt  the entire msg or its part 1?
     ;; We have to make use of mew-decode-syntax
     ;; in the cache buffer due to the PGP/MIME dilemma.
     ;; We need the correct LIMIT.
     (if (mew-syntax-message-p syntax)
	 (let ((bodyname "the body") body bct plus1p)
	   (setq body (mew-syntax-get-part syntax))
	   (when (mew-syntax-multipart-p body)
	     (setq plus1p t)
	     (setq bodyname "the part 1 text")
	     (setq body (mew-syntax-get-entry body '(1))))
	   (setq bct (mew-syntax-get-value (mew-syntax-get-ct body) 'cap))
	   (if (mew-ct-textp bct)
	       (if (y-or-n-p (format "Save the entire message (y) or %s (n)? " bodyname))
		   (setq have-hdrp t)
		 (if plus1p
		     (setq nums (nconc nums '(1)))
		   (setq bodyp t))
		 (setq PLUS1P (mew-syntax-get-privacy body)))
	     (setq have-hdrp t))))
     ;; Now, let's analyze the message in the burst buffer.
     ;; This is lengthy, though, avoidable.
     (setq limit (length nums))
     (if (or PLUS1P bodyp) ;; VERY important for PGP/MIME
	 (setq limit (1+ limit)))
     ;;
     (with-temp-buffer
       (mew-insert-message fld msg mew-cs-text-for-read nil)
       (mew-dinfo-set limit 'no-cs-conv t alt)
       (mew-decode-for-edit)
       ;;
       (setq syntax (mew-syntax-get-entry mew-decode-syntax nums))
       (if bodyp (setq syntax (mew-syntax-get-part syntax)))
       (setq beg (mew-syntax-get-begin syntax))
       (if (mew-syntax-message-p syntax)
	   (setq end (mew-syntax-get-end (mew-syntax-get-part syntax)))
	 (setq end (mew-syntax-get-end syntax)))
       (setq ctl (mew-syntax-get-ct syntax))
       (setq cdpl (mew-syntax-get-cdp syntax))
       (setq file (mew-syntax-get-filename cdpl ctl))
       ;;
       (if (and mew-use-samba-encoding
		file
		(string-match mew-regex-nonascii file))
	   (setq file (mew-samba-encoding file)))
       (setq file (mew-summary-input-file-name nil file))
       (if (not (file-exists-p file))
	   (setq doit t)
	 (if (null mew-file-append-p)
	     (setq action "Overwrite")
	   (setq action "Append")
	   (setq append-p t))
	 (if (y-or-n-p (format "File exists. %s it to %s? " action file))
	     (setq doit t)))
       ;;
       (if (not doit)
	   (message "Did not save anything")
	 (let (linebasep fromcs tocs charset writecs)
	   (save-restriction
	     (narrow-to-region beg end)
	     (goto-char (point-min))
	     (setq ct (mew-syntax-get-value ctl 'cap))
	     ;; Allow Message/Rfc822. It's user's risk.
	     (setq linebasep (or (mew-ct-textp ct) (mew-ct-linebasep ct)))
	     (if linebasep
		 (setq writecs mew-cs-text-for-write)
	       (setq writecs mew-cs-binary))
	     (when (and askcs linebasep)
	       (setq tocs (read-coding-system "Coding-system: "))
	       (if (mew-cs-raw-p tocs)
		   (setq writecs tocs tocs nil)))
	     (when have-hdrp
	       (goto-char (point-min))
	       (mew-header-delete-lines mew-field-delete-common)
	       (mew-header-delete-lines mew-field-delete-for-saving))
	     (when tocs
	       (setq charset (mew-syntax-get-param ctl "charset"))
	       (if charset (setq fromcs (mew-charset-to-cs charset)))
	       ;; When saving an entire message or its body,
	       ;; its charset is unknown since it is not analyzed.
	       ;; So, we make use of mew-cs-autoconv.
	       (unless (mew-coding-system-p fromcs)
		 (error "Unknown coding system %s" (symbol-name fromcs)))
	       (unless (mew-coding-system-p tocs)
		 (error "Unknown coding system %s" (symbol-name tocs)))
	       (mew-cs-decode-region
		(point-min) (point-max) (or fromcs mew-cs-autoconv))
	       (mew-cs-encode-region (point-min) (point-max) tocs))
	     (mew-frwlet mew-cs-dummy writecs
	       ;; do not specify 'no-msg
	       (write-region (point-min) (point-max) file append-p)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local cs
;;;

(defun mew-summary-convert-local-cs (&optional askcs)
  "Convert the character set of body by using autoconv
according to a specified language.
If executed with '\\[universal-argument]', coding-system is asked."
  (interactive "P")
  (mew-summary-msg-or-part
   ;; need to make a cache or a message buffer.
   (mew-summary-toggle-disp-msg 'on)
   (mew-summary-display)
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number2))
	  (num (mew-syntax-number))
	  (nums (mew-syntax-number-to-nums num))
	  (cbuf (mew-cache-hit fld msg))
	  (alt  (mew-cache-dinfo-get-use-alt cbuf))
	  (cache (or cbuf (mew-buffer-message)))
	  (syntax (mew-syntax-get-entry (mew-cache-decode-syntax cache) nums))
	  (buf (generate-new-buffer mew-buffer-prefix))
	  (win (selected-window))
	  (orig-lang current-language-environment)
	  PLUS1P limit bodyp hbeg hend beg end start
	  lang prompt tocs cs-env)
     (save-excursion
       ;; First of all, we should determine which part the user want to
       ;; save due to the ambiguity.
       ;; "y" on Message/Rfc822
       ;;     - msg/txt      the entire msg or its body?
       ;;     - msg/mul/txt  the entire msg or its part 1?
       ;; We have to make use of mew-decode-syntax
       ;; in the cache buffer due to the PGP/MIME dilemma.
       ;; We need the correct LIMIT.
       (when (mew-syntax-message-p syntax)
	 (let (body plus1p)
	   (setq body (mew-syntax-get-part syntax))
	   (when (mew-syntax-multipart-p body)
	     (setq plus1p t)
	     (setq body (mew-syntax-get-entry body '(1))))
	   (if plus1p
	       (setq nums (nconc nums '(1)))
	     (setq bodyp t))
	   (setq PLUS1P (mew-syntax-get-privacy body))))
       ;; Now, let's analyze the message in the burst buffer.
       ;; This is lengthy, though, avoidable.
       (setq limit (length nums))
       (if (or PLUS1P bodyp) ;; VERY important for PGP/MIME
	   (setq limit (1+ limit)))
       ;;
       (set-buffer buf)
       (mew-erase-buffer)
       (mew-insert-message fld msg mew-cs-text-for-read nil)
       ;;
       (cond
	(askcs
	 (setq prompt
	       (format "Coding-system (autoconv for %s): " orig-lang))
	 (setq tocs (or (read-coding-system prompt) mew-cs-autoconv)))
	(t
	 (setq tocs mew-cs-autoconv)
	 (setq prompt (format "Language (%s): " orig-lang))
	 (setq lang (read-language-name nil prompt orig-lang))
	 (setq cs-env (mew-set-language-environment-coding-systems lang))))
       (mew-dinfo-set limit 'no-cs-conv t alt)
       (mew-decode-for-edit)
       ;;
       (setq syntax (mew-syntax-get-entry mew-decode-syntax nums))
       (when bodyp
	 (setq hbeg (mew-syntax-get-begin syntax))
	 (setq hend (mew-syntax-get-end syntax))
	 (setq syntax (mew-syntax-get-part syntax)))
       (setq beg (mew-syntax-get-begin syntax))
       (setq end (mew-syntax-get-end syntax))
       ;;
       (set-buffer (mew-buffer-message))
       (select-window (get-buffer-window (current-buffer)))
       (widen)
       (mew-elet
	(delete-region (point-min) (point-max))
	(when bodyp
	  ;; This must be "insert-buffer-substring".
	  (insert-buffer-substring buf hbeg hend)
	  (mew-header-arrange (point-min) (point-max)))
	(setq start (point))
	(save-excursion
	  (mew-insert-buffer-substring buf beg end)
	  (mew-cs-decode-region start (point-max) tocs)
	  (mew-highlight-body-region start (point-max)))
	;; Page breaks
	(when mew-break-pages
	  (goto-char (point-min))
	  (mew-message-narrow-to-page))
	(when (and (not askcs) cs-env)
	  (mew-reset-coding-systems (car cs-env) (cdr cs-env))))
       (mew-summary-display-postscript 'no-hook)
       (select-window win))
     (mew-remove-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Burst
;;;

(defvar mew-burst-last-folder nil)

(defun mew-input-burst-folder ()
  (let (default)
     (if (and mew-use-burst-folder-history mew-burst-last-folder)
	 (setq default mew-burst-last-folder)
       (setq default (mew-inbox-folder))) ;; local folder
     (setq mew-burst-last-folder (mew-input-local-folder default))
     mew-burst-last-folder))

(defun mew-summary-burst-part (part folder newmsg &optional num)
  (let (n len nums entry file)
    (setq n 1)
    (cond
     (num
      (setq nums (mew-syntax-number-to-nums num))
      (setq len 1))
     (t
      (setq nums (list n))
      (setq len (- (length part) mew-syntax-magic))))
    (while (<= n len)
      (setq entry (mew-syntax-get-entry part nums))
      (cond
       ((mew-syntax-multipart-p entry)
	(setq newmsg (mew-summary-burst-part entry folder newmsg)))
       ((string= (mew-syntax-get-value (mew-syntax-get-ct entry) 'cap)
		 mew-ct-msg)
	(setq file (mew-expand-new-msg folder (number-to-string newmsg)))
	(mew-frwlet mew-cs-dummy mew-cs-text-for-write
	  (write-region
	   (mew-syntax-get-begin entry)
	   ;; This is RFC 822 message.
	   ;; So, body is a single text/plain.
	   (mew-syntax-get-end (mew-syntax-get-part entry))
	   file nil 'no-msg))
	(mew-set-file-modes file)
	(setq newmsg (1+ newmsg))))
      (setq n (1+ n))
      (setq nums (list n)))
    newmsg))

(defun mew-summary-burst-body (fld msg folder &optional num)
  (let (ret errmsg mstr m)
    (with-temp-buffer
      (mew-insert-message fld msg mew-cs-text-for-read nil)
      (mew-dinfo-set 1 nil nil nil)
      (mew-decode-for-edit)
      (cond
       ((not (mew-syntax-multipart-p (mew-syntax-get-part mew-decode-syntax)))
	(message "Cannot burst"))
       ((not (setq mstr (mew-folder-new-message folder 'num-only)))
	(setq errmsg (format "Error in %s. Nothing was processed" folder)))
       (t
	(setq m (mew-summary-burst-part (mew-syntax-get-part mew-decode-syntax)
					folder (string-to-number mstr) num))
	(if (= m (string-to-number mstr))
	    (message "Nothing was processed")
	  (mew-touch-folder folder)
	  (setq ret (list mstr (number-to-string (1- m)))))))
      (if errmsg (error "%s" errmsg))
      ret)))

(defun mew-summary-burst-files (fld msg dir)
  (with-temp-buffer
    (let (cnt syntax len stx ctl ct)
      (mew-insert-message fld msg mew-cs-text-for-read nil)
      ;; must be a message, not a part, so limit is nil
      (mew-dinfo-set nil 'no-cs-conv t nil)
      (mew-decode-for-edit)
      (setq syntax (mew-syntax-get-part mew-decode-syntax))
      (if (mew-syntax-singlepart-p syntax)
	  (progn
	    (mew-syntax-set-cdp syntax (mew-syntax-cdp-format mew-ct-txt mew-draft-coverpage))
	    (mew-d2e-singlepart syntax dir))
	(setq cnt mew-syntax-magic)
	(setq len (length syntax))
	(while (< cnt len)
	  (setq stx (aref syntax cnt))
	  (setq ctl (mew-syntax-get-ct stx))
	  (setq ct (mew-syntax-get-value ctl 'cap))
	  (cond
	   ((string= mew-ct-msg ct)
	    (mew-d2e-message stx dir))
	   ((mew-ct-multipartp ct)
	    (mew-d2e-multipart stx (mew-random-filename dir 1 nil)))
	   (t
	    (if (= cnt mew-syntax-magic)
		(mew-syntax-set-cdp stx (mew-syntax-cdp-format mew-ct-txt mew-draft-coverpage)))
	    (mew-d2e-singlepart stx dir)))
	  (setq cnt (1+ cnt)))))))

(defun mew-summary-burst (&optional arg)
  "Decompose messages embedded in this message.
You are asked a folder for the decomposition.

If executed on a part, only the message part is saved to
a folder which you specify.

If called with '\\[universal-argument]', any files in this message
are decomposed. You are asked a new directory name for the decomposition."
  (interactive "P")
  (mew-summary-msg-or-part
   (let ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number2))
	 (num (mew-syntax-number))
	 folder dir ret)
     (cond
      (arg ;; file decomposition
       (setq dir (read-file-name "New directory: " mew-home))
       (while (file-exists-p dir)
	 (setq dir (read-file-name (format "\"%s\" exists. New directory: " dir) mew-home)))
       (mew-make-directory dir)
       (message "Bursting...")
       (mew-summary-burst-files fld msg dir)
       (message "Bursting...done"))
      (t
       (setq folder (mew-input-burst-folder))
       (when (and folder (mew-local-folder-check folder 'ask))
	 (message "Bursting...")
	 (setq ret (mew-summary-burst-body fld msg folder num))
	 (when ret
	   (message "Bursting...done")
	   (when mew-ask-folder-after-burst
	     (if (y-or-n-p (format "Go to %s? " folder))
		 (mew-summary-visit-folder folder 'goend)))
	   (message "Messages from %s to %s were extracted in %s"
		    (nth 0 ret) (nth 1 ret) folder))))))))

(defun mew-summary-burst-multi ()
  "Decompose messages embedded in the messages marked with '*'."
  (interactive)
  (mew-summary-multi-msgs
   (let ((folder (mew-input-burst-folder))
	 (targets FLD-MSG-LIST))
     (when (and folder (mew-local-folder-check folder 'ask))
       (message "Bursting...")
       (dolist (target targets)
	 (mew-summary-burst-body (car target) (cdr target) folder))
       (message "Bursting...done")
       (when mew-ask-folder-after-burst
	 (if (y-or-n-p (format "Go to %s? " folder))
	     (mew-summary-visit-folder folder 'goend)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Old burst
;;;

(defun mew-summary-old-burst ()
  "Decompose old-fashioned encapsulated messages."
  (interactive)
  (mew-summary-msg
   (let ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number))
	 (sep1 (make-string 70 ?-))
	 (sep2 (make-string 30 ?-))
	 (folder (mew-input-burst-folder))
	 start mstr m done)
     (with-temp-buffer
       (mew-insert-message fld msg mew-cs-text-for-read nil)
       (mew-set-buffer-multibyte t)
       (if (not (re-search-forward sep1 nil t))
	   (message "Cannot burst this message")
	 (message "Bursting...")
	 (forward-line 2)
	 (setq start (point))
	 (setq mstr (mew-folder-new-message folder 'num-only))
	 (setq m (string-to-number mstr))
	 (while (re-search-forward sep2 nil t)
	   (save-excursion
	     (beginning-of-line)
	     (save-restriction
	       (narrow-to-region start (1- (point)))
	       (goto-char (point-min))
	       (while (re-search-forward "^- " nil t)
		 (replace-match "" nil t))
	       (mew-frwlet mew-cs-dummy mew-cs-text-for-write
		 (write-region (point-min) (point-max)
			       (mew-expand-new-msg folder mstr)
			       nil 'no-msg))))
	   (forward-line 2)
	   (setq start (point))
	   (setq m (1+ m))
	   (setq mstr (number-to-string m)))
	 (message "Bursting...done")
	 (mew-touch-folder folder)
	 (setq done t)))
     (when mew-ask-folder-after-burst
       (if (and done (y-or-n-p (format "Go to %s? " folder)))
	   (mew-summary-visit-folder folder 'goend))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Re-editing
;;;

(defun mew-summary-reedit (&optional keep-hdr)
  "Edit this message again to retry sending. Or edit this RFC822 part
typically included in a MIME-encapsulated error message.

1. In +draft, the message is just edited.
2. In +queue or +postq, the message is moved to +draft and is edited.
3. Otherwise, the message is copied to +draft and is edited.

For the other folders, if mew-case-guess-when-prepared is 't', each
fields of the original header is replaced according to a guessed
sending case.  If called with '\\[universal-argument]', the original
header is reserved.

See also mew-summary-edit-again."
  (interactive "P")
  (mew-summary-msg-or-part
   (let ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number2))
	 (part (mew-syntax-nums))
	 draftname)
     (cond
      ((string= fld mew-draft-folder)
       (setq draftname (mew-path-to-folder (mew-expand-msg fld msg)))
       (if (get-buffer draftname)
	   (progn
	     (mew-current-set-window-config) ;; xxx necessary?
	     (mew-window-configure 'draft)
	     (switch-to-buffer draftname))
	 (mew-summary-reedit-for-draft fld msg)
	 (run-hooks 'mew-draft-mode-reedit-draft-hook)))
      ((and (or (mew-folder-queuep fld) (mew-folder-postqp fld)) (null part))
       (mew-summary-reedit-for-queue fld msg)
       (run-hooks 'mew-draft-mode-reedit-queue-hook))
      (t
       (if part
	   (let* ((cache (mew-cache-hit fld msg 'must-hit))
		  (alt nil)
		  (syntax (mew-cache-decode-syntax cache))
		  (stx (mew-syntax-get-entry syntax part))
		  (ct (mew-syntax-get-value (mew-syntax-get-ct stx) 'cap)))
	     (if (not (string= mew-ct-msg ct))
		 (message "Cannot reedit here")
	       (mew-summary-reedit-for-message fld msg part keep-hdr alt)
	       (run-hooks 'mew-draft-mode-reedit-hook)))
	 (mew-summary-reedit-for-message fld msg nil keep-hdr nil)
	 (run-hooks 'mew-draft-mode-reedit-hook)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header
;;;

(defun mew-summary-edit-header-common ()
  (mew-header-delete-lines (list mew-x-mailer:))
  (mew-header-goto-end)
  ;; X-Mailer: must be the last
  (if (mew-use-x-mailer (mew-tinfo-get-case))
      (mew-draft-header-insert mew-x-mailer: mew-x-mailer))
  (mew-header-clear) ;; erase the old header separator
  (mew-header-set "\n"))

(defun mew-summary-edit-header-for-draft ()
  (mew-elet
   (mew-summary-edit-header-common)))

(defun mew-summary-edit-header-for-queue (hdr)
  (mew-elet
   (mew-header-goto-end)
   (delete-region (point-min) (point))
   (insert hdr)
   (mew-summary-edit-header-common)))

(defun mew-summary-edit-header-for-message (keep-hdr)
  (mew-elet
   (mew-header-delete-lines mew-field-delete-common)
   (mew-header-delete-lines mew-field-delete-for-reediting)
   (mew-header-sort mew-field-order-for-reediting)
   (unless keep-hdr
     (if mew-case-guess-when-prepared
	 (mew-draft-set-case-by-guess))
     (mew-draft-replace-fields nil))
   (mew-summary-edit-header-common)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Body
;;;

(defun mew-summary-reedit-for-draft (fld msg)
  "Edit a message in the draft folder. Use the message as draft.
Note that the message is not a valid MIME message."
  ;; Message mode will be invalid so hide it.
  (mew-mark-kill-line)
  (set-buffer-modified-p nil)
  ;; Need to delete Message window because the target message will be
  ;; modified and its content will be changed.
  (mew-window-configure 'summary)
  (mew-current-set-window-config)
  (mew-window-configure 'draft)
  (mew-summary-prepare-draft
   ;; mew-cs-m17n
   ;; case is copied here
   (mew-draft-find-and-switch (mew-expand-new-msg fld msg))
   (mew-summary-edit-header-for-draft)
   (mew-draft-mode)
   (mew-draft-rehighlight-body)
   (save-excursion
     (when (and (mew-encode-load-syntax) mew-encode-syntax)
       (mew-encode-syntax-delete 'all)
       (mew-draft-prepare-attachments t)))
   (mew-draft-mode-name)))

(defun mew-summary-reedit-for-queue (fld msg)
  "Edit a message in a folder other than the draft folder.
The message is assumed to be a valid MIME message."
  ;; Message mode will be invalid so hide it.
  (mew-decode-syntax-delete)
  (mew-mark-kill-line)
  (set-buffer-modified-p nil)
  ;; Need to delete Message window because the target message will be
  ;; modified and its content wiil be changed.
  (mew-current-set-window-config)
  (mew-window-configure 'draft)
  ;; main part
  (mew-summary-prepare-draft
   (mew-summary-edit-message fld msg nil nil)
   (let* ((file (mew-expand-msg fld msg))
	  (work (concat file mew-queue-work-suffix))
	  (info (concat file mew-queue-info-suffix))
	  inf hdr)
     (when (file-readable-p file)
       (rename-file file work 'override)
       (setq inf (mew-lisp-load info))
       (cond
	((mew-folder-queuep fld)
	 (setq hdr (mew-smtp-get-raw-header inf))
	 (mew-tinfo-set-case (mew-smtp-get-case inf)))
	((mew-folder-postqp fld)
	 (setq hdr (mew-nntp2-get-raw-header inf))
	 (mew-tinfo-set-case (mew-nntp2-get-case inf))))
       (mew-queue-backup work mew-queue-info-suffix)
       (and hdr (mew-summary-edit-header-for-queue hdr))))
   (mew-draft-mode)
   (mew-draft-rehighlight-body)
   (when mew-encode-syntax
     (save-excursion
       (mew-draft-prepare-attachments t)))
   (mew-draft-mode-name)))

(defun mew-summary-reedit-for-message (fld msg part keep-hdr alt)
  "Edit a message in a folder other than the draft folder.
The message is assumed to be a valid MIME message."
  (mew-current-set-window-config)
  (mew-window-configure 'draft)
  ;; main part
  (mew-summary-prepare-draft
   ;; case is copied here
   (mew-summary-edit-message fld msg part alt)
   ;; get info before backup the original
   (mew-summary-edit-header-for-message keep-hdr)
   (mew-draft-mode)
   (mew-draft-rehighlight-body)
   (when mew-encode-syntax
     (save-excursion
       (mew-draft-prepare-attachments t)))
   (mew-draft-mode-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit again
;;;

(defvar mew-summary-edit-again-regex
  "----- Original message follows -----\\|----- Unsent message follows -----\\|--- Undelivered message follows ---\\|--- Below this line is a copy of the message.")

(defun mew-summary-edit-again ()
  "Edit an old fashioned error message in which the original message
is encapsulated after strings defined in 'mew-summary-edit-again-regex'
An example is \"----- Original message follows -----\". See also
mew-summary-reedit."
  (interactive)
  (mew-summary-msg
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (nfld mew-temp-dir)
	  (nmsg (mew-folder-new-message nfld 'num-only))
	  nfile)
     (with-temp-buffer
       ;; First, let's remove garbage and make a valid message
       ;; into a file.
       (mew-insert-message fld msg mew-cs-text-for-read nil)
       (mew-set-buffer-multibyte t)
       (goto-char (point-min))
       (if (not (re-search-forward mew-summary-edit-again-regex nil t))
	   (message "Cannot edit this message again")
	 (forward-line)
	 ;; skip blank lines
	 (while (looking-at "^$") (forward-line))
	 (delete-region (point-min) (point))
	 (setq nfile (mew-expand-new-msg nfld nmsg))
	 (mew-frwlet mew-cs-dummy mew-cs-text-for-write
	   (write-region (point-min) (point-max) nfile nil 'no-msg))
	 ;; A new message is stored in the temporary directory
	 (mew-summary-reedit-for-message nfld nmsg nil nil nil)
	 (run-hooks 'mew-draft-mode-edit-again-hook)
	 (mew-delete-file nfile))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defun mew-ct-suffix (ct)
  "Get an appropriate suffix from content type."
  (let* ((base (mew-ctdb-regex (mew-ctdb-by-ct ct)))
	 (len (length base))
	 (ret (mew-make-string len))
	 (j 0) char)
    (if (not (stringp base))
	nil
      (dotimes (i len)
	(setq char (aref base i))
	(if (or (char-equal char ?.)
		(and (>= char ?a) (<= char ?z)))
	    (progn
	      (aset ret j char)
	      (setq j (1+ j)))))
      (setq ret (substring ret 0 j))
      (if (string-match "^\\.[a-z]+$" ret)
	  ret
	nil))))

(defun mew-summary-edit-message (fld msg part alt)
  "Prepare a header and a body, optionally attachments."
  (let* ((draft (mew-folder-new-message mew-draft-folder))
	 (attachdir (mew-attachdir draft))
	 (buf (generate-new-buffer mew-buffer-prefix))
	 syntax hbeg hend beg end len cnt stx ctl ct charset start cs)
    ;; prepare draft file
    (mew-draft-find-and-switch draft)
    (mew-delete-directory-recursively attachdir)
    ;; De-compose the message in the burst buffer.
    ;; do not decode Message/Rfc822 parts.
    ;; They will be just a attachment message.
    (with-current-buffer buf
      ;; Let's decode the message without limitations
      (mew-erase-buffer)
      (mew-insert-message fld msg mew-cs-text-for-read nil)
      (mew-dinfo-set (1+ (length part)) 'no-cs-conv t alt 'encap-html)
      (mew-decode-for-edit)
      (setq syntax (mew-syntax-get-entry mew-decode-syntax part))
      ;; syntax is now for the message
      (setq hbeg (mew-syntax-get-begin syntax))
      (setq hend (mew-syntax-get-end syntax))
      ;; Outer 'message is not necessary for mew-encoding-syntax
      ;; syntax is now for the multipart
      (setq syntax (mew-syntax-get-part syntax))
      (if (mew-syntax-singlepart-p syntax)
	  (progn
	    (setq beg (mew-syntax-get-begin syntax))
	    (setq end (mew-syntax-get-end syntax))
	    (setq ctl (mew-syntax-get-ct syntax))
	    (setq charset (mew-syntax-get-param ctl "charset"))
	    (setq syntax nil))
	;; Let's store each part
	(setq cnt mew-syntax-magic)
	(unless (string= mew-ct-txt
			 (mew-syntax-get-value
			  (mew-syntax-get-ct (aref syntax cnt)) 'cap))
	  (setq syntax (mew-syntax-insert-entry
			syntax '(1) (mew-encode-syntax-text))))
	(setq len (length syntax))
	(mew-make-directory attachdir)
	(while (< cnt len)
	  (setq stx (aref syntax cnt))
	  (setq ctl (mew-syntax-get-ct stx))
	  (setq ct (mew-syntax-get-value ctl 'cap))
	  (cond
	   ((string= mew-ct-msg ct)
	    (mew-d2e-message stx attachdir))
	   ((mew-ct-multipartp ct)
	    (mew-d2e-multipart stx (mew-random-filename attachdir 1 nil)))
	   (t
	    (if (/= cnt mew-syntax-magic)
		(mew-d2e-singlepart stx attachdir)
	      (setq beg (mew-syntax-get-begin stx))
	      (setq end (mew-syntax-get-end stx))
	      (setq charset (mew-syntax-get-param ctl "charset"))
	      (mew-delete "charset" ctl)
	      (mew-syntax-set-file stx mew-draft-coverpage)
	      (mew-syntax-set-decrypters stx nil))))
	  (setq cnt (1+ cnt)))
	(mew-syntax-set-file
	 syntax
	 (file-name-as-directory (file-name-nondirectory attachdir)))
	(mew-syntax-set-decrypters syntax nil)
	(mew-syntax-set-privacy syntax nil)
	(mew-syntax-set-ct syntax mew-type-mlm)
	(mew-syntax-set-cte syntax nil)
	(mew-syntax-set-cd syntax nil)
	(mew-syntax-set-cid syntax nil)
	(mew-syntax-set-cdp syntax nil)))
    ;; draft buffer
    ;; header
    (mew-insert-buffer-substring buf hbeg hend)
    (insert "\n")
    ;; coverpage
    (setq start (point))
    ;; The first part may not be Text/Plain.
    (if (and beg end) (mew-insert-buffer-substring buf beg end))
    (if (or (null charset) (string= charset mew-us-ascii))
	(setq cs mew-cs-autoconv) ;; for RFC822
      (setq cs (mew-charset-to-cs charset)))
    (unless (mew-coding-system-p cs)
      (error "Unknown coding system %s" (symbol-name cs)))
    (mew-cs-decode-region start (point-max) cs)
    (setq mew-encode-syntax syntax)
    (mew-remove-buffer buf)))

(defun mew-d2e-singlepart (syntax dir)
  "De-compose singlepart"
  (let* ((ctl (mew-syntax-get-ct syntax))
	 (ct  (mew-syntax-get-value ctl 'cap))
	 (cdpl (mew-syntax-get-cdp syntax))
	 (file (mew-syntax-get-filename cdpl ctl))
	 (beg (mew-syntax-get-begin syntax))
	 (end (mew-syntax-get-end syntax))
	 (linebasep (or (mew-ct-textp ct) (mew-ct-linebasep ct))))
    (if (stringp file)
	(setq file (expand-file-name file dir))
      (setq file (mew-random-filename dir 2 nil (mew-ct-suffix ct)))
      (mew-syntax-set-cdp syntax nil))
    (mew-frwlet mew-cs-dummy (if linebasep mew-cs-text-for-write mew-cs-binary)
      (write-region beg end file nil 'no-msg))
    (if (and (mew-ct-textp ct) (mew-syntax-get-param ctl "charset"))
	(mew-delete "charset" ctl)) ;; side effect
    (mew-syntax-set-file syntax (file-name-nondirectory file))
    (mew-syntax-set-decrypters syntax nil)
    (mew-syntax-set-privacy syntax nil)
    (mew-syntax-set-cid syntax nil)))

(defun mew-d2e-multipart (syntax dir)
  "De-compose multipart"
  (let ((len (length syntax))
	(cnt mew-syntax-magic)
	ct part)
    (mew-make-directory dir)
    (while (< cnt len)
      (setq part (aref syntax cnt))
      (setq ct (mew-syntax-get-value (mew-syntax-get-ct part) 'cap))
      (cond
       ((string= mew-ct-msg ct)
	(mew-d2e-message part dir))
       ((mew-ct-multipartp ct)
	(mew-d2e-multipart part (mew-random-filename dir 1 nil)))
       (t
	(mew-d2e-singlepart part dir)))
      (setq cnt (1+ cnt)))
    (mew-syntax-set-file
     syntax
     (file-name-as-directory (file-name-nondirectory dir)))
    (mew-syntax-set-decrypters syntax nil)
    (mew-syntax-set-privacy syntax nil)
    (mew-syntax-set-ct syntax mew-type-mlm)
    (mew-syntax-set-cte syntax nil)
    (mew-syntax-set-cd syntax nil)
    (mew-syntax-set-cid syntax nil)
    (mew-syntax-set-cdp syntax nil)))

(defun mew-d2e-message (syntax dir)
  "De-compose message like singlepart"
  (let ((file (mew-random-filename dir 2 t))
	(beg (mew-syntax-get-begin syntax))
	(end (mew-syntax-get-end (mew-syntax-get-part syntax))))
    (mew-frwlet mew-cs-dummy mew-cs-text-for-write
      (write-region beg end file nil 'no-msg))
    (mew-syntax-set-key syntax 'single)
    (mew-syntax-set-file syntax (file-name-nondirectory file))
    (mew-syntax-set-decrypters syntax nil)
    (mew-syntax-set-privacy syntax nil)
    (mew-syntax-set-cid syntax nil)
    (mew-syntax-set-cdp syntax nil)
    (mew-syntax-set-part syntax nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit mode
;;;

(defun mew-summary-edit ()
  (interactive)
  (mew-summary-not-in-queue
   (mew-summary-not-in-draft
    (mew-summary-msg-or-part
     (let ((fld (mew-summary-folder-name))
	   (msg (mew-summary-message-number2))
	   (part (mew-syntax-nums)))
       (if part
	   (let* ((cache (mew-cache-hit fld msg 'must-hit))
		  (alt nil)
		  (syntax (mew-cache-decode-syntax cache))
		  (stx (mew-syntax-get-entry syntax part))
		  (ct (mew-syntax-get-value (mew-syntax-get-ct stx) 'cap)))
	     (if (not (string= mew-ct-msg ct))
		 (message "Cannot edit here")
	       (mew-summary-edit-for-message fld msg part alt)
	       (run-hooks 'mew-draft-mode-edit-hook)))
	 (mew-summary-edit-for-message fld msg nil nil)
	 (setq mode-name "Edit")
	 (force-mode-line-update)
	 (run-hooks 'mew-draft-mode-edit-hook)))))))

(defun mew-summary-edit-for-message (fld msg part alt)
  (mew-current-set-window-config)
  (mew-window-configure 'draft)
  ;; main part
  (mew-summary-prepare-draft
   ;; case is copied here
   (mew-summary-edit-message fld msg part alt)
   ;; get info before backup the original
   (mew-summary-edit-header-for-edit)
   (let ((fields (mew-summary-edit-obtain-fields-key)))
     (mew-tinfo-set-preserved-header (mew-summary-edit-obtain-fields fld msg fields)))
   (mew-tinfo-set-src-folder fld)
   (mew-draft-mode)
   (mew-draft-rehighlight-body)
   (when mew-encode-syntax
     (save-excursion
       (mew-draft-prepare-attachments t)))
   (mew-draft-mode-name)))

(defun mew-summary-edit-header-for-edit ()
  (mew-elet
   (mew-header-delete-lines '("From " ">From"))
   (mew-header-delete-lines mew-field-delete-for-editing)
   (mew-header-sort mew-field-order-for-editing)
   (mew-header-clear) ;; erase the old header separator
   (mew-header-set "\n")))

(defun mew-summary-edit-obtain-fields-key ()
  (let ((case-fold-search t) fields)
    (save-excursion
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (not (eobp))
	  (if (not (looking-at mew-keyval))
	      (forward-line)
	    (setq fields (cons (mew-match-string 1) fields))
            (forward-line)
	    (mew-header-goto-next))))
      fields)))

(defun mew-summary-edit-obtain-fields (fld msg fields)
  (with-temp-buffer
    (mew-insert-message
     fld msg mew-cs-text-for-read mew-header-reasonable-size)
    (mew-header-goto-end)
    (delete-region (point) (point-max))
    (mew-header-delete-lines fields)
    (mew-header-delete-lines (list mew-mv: "Content-" "X-Mew-"))
    (mew-buffer-substring (point-min) (point-max))))

(defun mew-edit-make-delete-window (buf)
  (if (and (mew-tinfo-get-other-frame) (> (length (frame-list)) 1))
      (delete-frame)
    (mew-current-get-window-config)
    (delete-windows-on buf))) ;; just in case

(defun mew-edit-make-message (buf)
  (let (multip)
    (save-excursion
      (save-window-excursion
	(set-buffer buf)
	(mew-set-buffer-multibyte t)
	(if (buffer-modified-p) (save-buffer)) ;; to make backup
	(widen)
	(if (mew-attach-p)
	    (progn
	      (setq multip t)
	      (mew-attach-clear))
	  (unless mew-encode-syntax
	    (setq mew-encode-syntax
		  (mew-encode-syntax-single "text.txt" (list mew-ct-txt)))))
	(goto-char (mew-header-end))
	(forward-line)
	(if multip
	    (mew-encode-make-multi)
	  (mew-encode-make-single))
	(mew-encode-make-header)
	(mew-encode-save-draft)
	(mew-overlay-delete-buffer)
	(goto-char (point-min))
	(insert (mew-tinfo-get-preserved-header))
	(save-buffer)
	(buffer-file-name)))))

(defun mew-edit-make-refile (case:srcfld srcfile)
  (let* ((case:dstfld (car (mew-input-folders case:srcfld)))
	 (dstfld (mew-case:folder-folder case:dstfld))
	 (case (mew-case:folder-case case:dstfld))
	 dstfile)
    (cond
     ((mew-folder-imapp dstfld)
      (mew-imap2-copy-message case 'move (list srcfile) dstfld))
     (t
      (setq dstfile (mew-folder-new-message dstfld))
      (rename-file srcfile dstfile)
      (mew-set-file-modes dstfile)
      (mew-touch-folder dstfld)
      (message "Saved in %s" dstfld)))))

(defun mew-edit-make ()
  (interactive)
  (let ((buf (current-buffer))
	(case:srcfld (mew-tinfo-get-src-folder))
	srcfile)
    (mew-edit-make-delete-window buf)
    (setq srcfile (mew-edit-make-message buf))
    (mew-remove-buffer buf)
    (mew-edit-make-refile case:srcfld srcfile)))

(provide 'mew-edit)

;;; Copyright Notice:

;; Copyright (C) 1999-2010 Mew developing team.
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

;;; mew-edit.el ends here
