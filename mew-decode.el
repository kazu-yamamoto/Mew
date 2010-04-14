;;; mew-decode.el --- MIME syntax decoder for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decode info
;;;

;; limit:
;;   Depth of RFC 822 messages.
;;   The cnt starts with 0.
;;   Decode if cnt <= limit
;;   nil means that limit is 0.
;;
;; decode-text:
;;   If t, decode a text MIME body.
;;   Otherwise, it is treated as 'no-cs-conv
;;
;; decode-binary:
;;   If t, decode a binary (non-text) MIME body.
;;
;; use-alt:
;;   If t, one part in Multipart/Alternative is used.
;;   Otherwise, it is treated as Multipart/Mixed.

(defvar mew-dinfo-list '("limit" "decode-text" "decode-binary" "use-alt" "encap-html"))

(mew-blinfo-defun 'mew-dinfo mew-dinfo-list)

(defun mew-dinfo-set (lim dt db alt &optional encap)
  (mew-dinfo-set-limit lim)
  (mew-dinfo-set-decode-text dt)
  (mew-dinfo-set-decode-binary db)
  (mew-dinfo-set-use-alt alt)
  (mew-dinfo-set-encap-html encap))

(defun mew-cache-dinfo-get-use-alt (buf)
  (when buf
    (with-current-buffer buf
      (mew-dinfo-get-use-alt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decode switch
;;;

(defvar mew-prog-mime-decode-switch
  `((,mew-b64 "-d" "-b")
    (,mew-qp  "-d" "-q")
    (,mew-xg  "-d" "-g")
    (,mew-xuu "-d" "-u")))

(defvar mew-prog-mime-decode-text-switch
  `((,mew-b64 "-d" "-b" "-t")
    (,mew-qp  "-d" "-q")
    (,mew-xg  "-d" "-g" "-t")
    (,mew-xuu "-d" "-u")))

(defun mew-prog-mime-decode-get-opt (cte switch)
  (cdr (mew-assoc-case-equal cte switch 0)))

(defvar mew-decode-multipart-encrypted-switch
  `((,mew-ct-pge mew-pgp-decrypt mew-pgp-ver mew-prog-pgp)))

(defvar mew-decode-multipart-signed-switch
  `((,mew-ct-pgs  mew-pgp-verify          mew-pgp-ver   mew-prog-pgp)
    (,mew-ct-sms  mew-smime-detach-verify mew-smime-ver mew-prog-smime)
    (,mew-ct-xsms mew-smime-detach-verify mew-smime-ver mew-prog-smime)))

;;

(defun mew-decode-get-security-func (proto switch)
  (nth 1 (mew-assoc-case-equal proto switch 0)))

(defun mew-decode-get-security-existence (proto switch)
  (symbol-value (nth 2 (mew-assoc-case-equal proto switch 0))))

(defun mew-decode-get-security-prog (proto switch)
  (symbol-value (nth 3 (mew-assoc-case-equal proto switch 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME decoder
;;;

(defun mew-decode-error (error-msg)
  (mew-xinfo-set-decode-err error-msg)
  (error error-msg))

;;;
;;; Decoding a header
;;;

(defmacro mew-decode-narrow-to-header (&rest body)
  `(progn
     (if (re-search-forward mew-eoh nil t)
	 (beginning-of-line)
       (goto-char (point-max))
       (insert "\n"))
     (when (and (integerp mew-header-max-length)
		(> (count-lines (point-min) (point)) mew-header-max-length))
       (mew-xinfo-set-action
	(mew-substitute-for-summary
	 "Too long header. To see the message, type '\\[mew-summary-analyze-again]'"))
       (error ""))
     (save-restriction
       (narrow-to-region (point-min) (point))
       (goto-char (point-min))
       ,@body)))

(defun mew-summary-toggle-header-veil ()
  "If 'mew-use-header-veil' is non-nil, field lines of To: and Cc:
over 'mew-header-veil-count' are covered with invisible veils.
This commands toggles visibility of these lines."
  (interactive)
  (when (and mew-use-header-veil (get-buffer (mew-buffer-message)))
    (mew-summary-msg-or-part
     (with-current-buffer (mew-buffer-message)
       (when (mew-msghdr-p)
	 (let* ((win (get-buffer-window (current-buffer)))
		(pos (window-start win)))
	   (mew-toggle-header-veil (mew-minfo-get-veil-to))
	   (mew-toggle-header-veil (mew-minfo-get-veil-cc))
	   (set-window-start win pos)))))))

(defun mew-header-veil-put (key ov)
  (let ((case-fold-search t)
	(regex (concat "^" key)))
    (goto-char (point-min))
    (when (re-search-forward regex nil t)
      (beginning-of-line)
      (let ((start (point)) end)
	(forward-line)
	(mew-header-goto-next)
	(setq end (point))
	(when (> (count-lines start end) mew-header-veil-count)
	  (goto-char start)
	  (forward-line mew-header-veil-count)
	  (move-overlay ov (1- (point)) (1- end)))))))

(defun mew-header-veil ()
  (when mew-use-header-veil
    (unless (mew-minfo-get-veil-to)
      (mew-minfo-set-veil-to (mew-header-veil-make)))
    (unless (mew-minfo-get-veil-cc)
      (mew-minfo-set-veil-cc (mew-header-veil-make)))
    (mew-header-veil-put "To:" (mew-minfo-get-veil-to))
    (mew-header-veil-put "Cc:" (mew-minfo-get-veil-cc))))

(defun mew-header-arrange (beg end)
  (let (vispos)
    (save-restriction
      (narrow-to-region beg end)
      (mew-elet
       (let (ch-beg ch-end vs-beg vs-end contents cbeg)
	 (setq ch-beg (next-single-property-change (point-min) 'mew-noncontents))
	 (when ch-beg
	   (setq vs-beg (next-single-property-change (point-min) 'mew-visible))
	   (cond
	    (vs-beg
	     (setq vs-end (next-single-property-change vs-beg 'mew-visible)))
	    (mew-field-other-visible
	     (setq vs-beg (next-single-property-change (point-min) 'mew-others))
	     (if vs-beg
		 (setq vs-end (next-single-property-change vs-beg 'mew-others))
	       (setq vs-beg ch-beg)
	       (setq vs-end ch-beg)))
	    (t
	     (setq vs-beg ch-beg)
	     (setq vs-end ch-beg)))
	   (setq ch-end (point-max))
	   (unless vs-end (setq vs-end (point-max)))
	   (mew-decode-header-property-region ch-beg ch-end)
	   ;; This must be "buffer-substring".
	   (setq contents (buffer-substring ch-beg ch-end))
	   (delete-region ch-beg ch-end)
	   (cond
	    ((mew-nspec-visiblep (mew-nspec-by-key "Content-"))
	     ;; visible
	     (goto-char vs-end)
	     (setq cbeg (point))
	     (insert contents)
	     (put-text-property cbeg (point) 'mew-visible t)) ;; used later
	    (t
	     ;; invisible
	     (goto-char vs-beg)
	     (setq cbeg (point))
	     (insert contents)))
	   (goto-char (point-max)))
	 (mew-decode-syntax-insert-privacy)
	 (mew-decode-syntax-insert-warning)
	 (save-excursion (mew-highlight-x-face (point-min) (point-max)))
	 (setq vispos (if (get-text-property (point-min) 'mew-visible)
			(point-min)
		      (or (next-single-property-change (point-min) 'mew-visible)
			  (point-max)))))))
    (mew-header-veil)
    (mew-header-goto-end)
    (if (eobp)
	(mew-header-set "\n") ;; analyzed
      (mew-header-set nil))   ;; asis
    ;; Emacs 21.3.50 or later need this.
    (set-window-start (selected-window) vispos)))

(defun mew-decode-header-property-region (BEG END)
  ;; see also mew-highlight-header
  (when mew-use-highlight-header
    (mew-elet
     (let (key beg med nspec key-face val-face)
       (save-restriction
	 (narrow-to-region BEG END)
	 (goto-char (point-min))
	 (while (not (eobp))
	   (if (not (looking-at mew-keyval))
	       (forward-line)
	     (setq key (mew-match-string 1))
	     (setq beg (match-beginning 0))
	     (setq med (match-end 0))
	     (forward-line)
	     (setq nspec (mew-nspec-by-key key))
	     (setq key-face (mew-key-face key nspec))
	     (setq val-face (mew-val-face key nspec))
	     (put-text-property beg med 'face key-face)
	     (put-text-property med (1- (point)) 'face val-face)
	     (while (looking-at mew-lwsp+)
	       (forward-line)
	       (put-text-property (match-end 0) (1- (point))
				  'face val-face)))))))))

(defun mew-decode-rfc822-header (&optional cnt)
  "A function to handle RFC822 header.
Called on the beginning of the header in the narrowed region.
 - Decode and highlight RFC822 fields excluding MIME fields.
 - Delete X-Mew: fields.
 - Arrange decoded-RFC822-fields, mew-mv:, MIME fields in order.
The cursor moves between mew-mv: and MIME fields.
Return the existence MIME-Version: and the value of Subject:."
  (let* ((case-fold-search t)
	 (visibles (make-list (length mew-field-spec) nil))
	 (prop mew-use-highlight-header)
	 key beg med subj from contents others
	 key-face val-face N nspec visiblep
	 mimep mimep2
	 size ext ext-str ext-face)
    (mew-decode-narrow-to-header
     (while (not (eobp))
       (if (not (looking-at mew-keyval))
	   (forward-line)
	 (setq key (mew-capitalize (mew-match-string 1)))
	 (setq beg (match-beginning 0))
	 (setq med (match-end 0))
	 (forward-line)
	 (mew-header-goto-next)
	 (setq nspec (mew-nspec-by-key key))
	 (setq N (mew-nspec-n nspec))
	 (setq visiblep (mew-nspec-visiblep nspec))
	 (if (fboundp visiblep)
	     (setq visiblep (funcall visiblep (mew-buffer-substring med (1- (point))))))
	 (cond
	  ((string= key mew-x-mew:)
	   ;; deleting X-Mew: on the RFC822 header
	   (delete-region beg (point)))
	  ((string= key mew-x-mew-uidl:)
	   (setq size (mew-scan-uid-size (mew-buffer-substring med (1- (point)))))
	   (when (and (eq cnt 1) (mew-msg-truncatedp size))
	     (mew-xinfo-set-action
	      (mew-substitute-for-summary
	       "Too large, truncated (the 'T' mark). To get the entire message, type '\\[mew-summary-retrieve-message]'"))))
	  ((string-match "^Content-" key)
	   ;; Due to PGP/MIME, properties are not put here.
	   :; This must be "buffer-substring".
	   (setq mimep2 t)
	   (setq contents (cons (buffer-substring beg (point)) contents))
	   (delete-region beg (point)))
	  (t
	   ;; Let's decode the field anyway.
	   ;; If a user want to preserve this RFC 822 header, he
	   ;; set the limit, so never reaches here.
	   ;; Moreover this function may be called with
	   ;; mew-dinfo-set unset.
	   (mew-header-decode-region key med (point))
	   (cond
	    ((string= key mew-from:)
	     (setq from (mew-addrstr-parse-address
			 (mew-buffer-substring med (1- (point))))))
	    ((string= key mew-subj:)
	     (setq subj (mew-buffer-substring med (1- (point)))))
	    ((string= key mew-mv:)
	     ;; MIME-Version:
	     (setq mimep (string-match
			  mew-mv:-num
			  (mew-addrstr-parse-value
			   (mew-buffer-substring med (point)))))))
	   (when prop
	     (setq key-face (mew-key-face key nspec))
	     (setq val-face (mew-val-face key nspec))
	     (put-text-property beg med 'face key-face)
	     (goto-char med)
	     (forward-line)
	     (put-text-property med (1- (point)) 'face val-face)
	     (while (looking-at mew-lwsp+)
	       (forward-line)
	       (put-text-property (match-end 0) (1- (point))
				  'face val-face))
	     (when (setq ext (mew-nspec-extraface nspec))
	       (save-restriction
		 (narrow-to-region med (point))
		 (while ext ;; cannot use dolist
		   (goto-char (point-min))
		   (setq ext-str (car ext))
		   (setq ext (cdr ext))
		   (setq ext-face (car ext))
		   (setq ext (cdr ext))
		   (while (re-search-forward ext-str nil t)
		     (put-text-property (match-beginning 0) (match-end 0) 'face ext-face)))
		 (goto-char (point-max)))))
	   (cond
	    ((null nspec) ;; others
	     ;; This must be "buffer-substring".
	     (setq others (cons (buffer-substring beg (point)) others))
	     (delete-region beg (point)))
	    (visiblep
	     ;; This must be "buffer-substring".
	     (setcar (nthcdr N visibles)
		     (concat (nth N visibles)
			     (buffer-substring beg (point))))
	     (delete-region beg (point)))
	    (t ;; invisible
	     ())))))))
    (put-text-property (point-min) (point) 'mew-invisible t)
    (put-text-property (point-min) (point) 'mew-noncontents t)
    (unless mew-field-other-visible
      (setq beg (point))
      (mapc 'insert (nreverse others))
      (put-text-property beg (point) 'mew-others t)
      (put-text-property beg (point) 'mew-noncontents t))
    (setq beg (point))
    (mapc (lambda (x) (and (stringp x) (insert x))) visibles)
    ;; for recenter in Message mode
    (put-text-property beg (point) 'mew-visible t)
    (put-text-property beg (point) 'mew-noncontents t)
    (mew-rear-nonsticky beg (point))
    (when mew-field-other-visible
      (setq beg (point))
      (mapc 'insert (nreverse others))
      (put-text-property beg (point) 'mew-others t)
      (put-text-property beg (point) 'mew-noncontents t)
      (mew-rear-nonsticky beg (point)))
    ;; the beginning of the content header
    (save-excursion (mapc 'insert (nreverse contents)))
    ;; 'mew-contents does not work due to PGP/MIME
    (if (and (null mimep) mimep2)
	(mew-xinfo-set-warning
	 (cons "No MIME-Version\n" (mew-xinfo-get-warning))))
    (list (or mimep mimep2) subj from)))

(defun mew-decode-mime-header (&optional dct)
  "A function to handle content header.
Called on the beginning of the content header in the narrowed region
Return a part syntax after moving the beginning of the content body."
  (let ((case-fold-search t)
	(vec (make-vector (length mew-mime-fields) nil))
	key med attr n act value syntax)
    (mew-decode-narrow-to-header
     (while (not (eobp))
       (if (not (looking-at mew-keyval))
	   (forward-line)
	 (setq key (mew-capitalize (mew-match-string 1)))
	 (setq med (match-end 0))
	 (forward-line)
	 (mew-header-goto-next)
	 (setq attr (assoc key mew-mime-fields))
	 (when attr
	   (mew-set '(nil n act) attr)
	   (cond
	    ((eq act 'analyze)
	     (setq value (mew-param-decode
			  (mew-buffer-substring med (1- (point))))))
	    ((eq act 'extract)
	     (setq value (mew-addrstr-parse-value
			  (mew-buffer-substring med (1- (point))))))
	    ((eq act 'decode)
	     ;; If a user want to preserve this MIME header, he
	     ;; set the limit, so never reaches here.
	     (mew-header-decode-region key med (point))
	     ;; mew-header-decode-region goes to the max point in
	     ;; the narrowed region. So, this must be (point).
	     (setq value (mew-buffer-substring med (1- (point)))))
	    ((eq act 'id)
	     (setq value (mew-buffer-substring med (1- (point))))
	     (setq value (mew-idstr-get-first-id value))))
	   (aset vec n value)))))
    (if (eobp)
	(insert "\n")
      (forward-line))
    ;; the beginning of the content body
    (setq syntax (vconcat (list 'single (point) nil nil) vec))
    (or (mew-syntax-get-ct syntax)
	(mew-syntax-set-ct syntax (or dct mew-type-txt)))
    syntax))

;;;
;;; Decoding a message
;;;

(defun mew-decode ()
  "Decode a message. Execute mew-dinfo-set before calling this."
  ;; in cache buffer
  (mew-set-buffer-multibyte t)
  ;;
  ;; Illegal messages may not have end-of-header.
  ;; Truncated messages may not have end-of-header.
  ;;
  (goto-char (point-min)) ;; just in case
  (unless (re-search-forward mew-eoh nil t)
    (goto-char (point-max))
    (if (not (bolp)) (insert "\n"))
    (insert "\n"))
  (goto-char (point-min))
  ;;
  (if (mew-debug 'decode)
      (let ((debug-on-error t))
	(setq mew-decode-syntax
	      (mew-decode-message (mew-decode-syntax-rfc822-head) 0)))
    (condition-case nil
	(setq mew-decode-syntax
	      (mew-decode-message
	       ;; Call internalform with VIRTUAL content header
	       ;;     CT: message/rfc822 (virtual)
	       ;;
	       ;;     Header(RFC822 header + content header)
	       ;;
	       ;;     Body(content body)
	       (mew-decode-syntax-rfc822-head) 0))
      (error
       (widen)
       (mew-header-goto-body)
       ;; min, point - 1, point, point-max
       (setq mew-decode-syntax (mew-decode-syntax-rfc822))))))

(defun mew-decode-message (syntax cnt)
  ;; Called on the beginning of the RFC822 header in the narrowed region
  ;; hbeg is certainly the beginning of the VIRTUAL content body(i.e. min).
  ;; hend will have to set to the end of PHYSICAL content header(i.e. end)
  ;; after analyzing the physical content header and body since CD:'s
  ;; length in the physical content header will change(no need to say
  ;; about the end of the physical content header).
  ;;
  ;;     Content-Type: Message/Rfc822    == virtual content header
  ;;
  ;;(min)Decoded RFC822 fields           == virtual content body
  ;;     MIME-Version: 1.0
  ;;(cur)MIME fields                     == physical content header
  ;;(end)
  ;;     Content-Body                    == physical content body
  ;;(max)
  (setq cnt (1+ cnt))
  (cond
   ((and (mew-dinfo-get-limit) (> cnt (mew-dinfo-get-limit)))
    ;; Don't analyze recursively.
    ;; We need to analyze the header and the body since a user
    ;; may want to save the body only.
    ;; the beginning of the meaningless physical content header
    (if (re-search-forward mew-eoh nil t)
	(forward-line)
      (mew-decode-error "No end-of-header(null line) in RFC822 message"))
    ;; the beginning of the BODY(i.e. the physical content body)
    (mew-syntax-set-key syntax 'message)
    (mew-syntax-set-end syntax (1- (point)))
    ;; (point-min), (point) - 1, (point), (point-max)
    (mew-decode-syntax-rfc822 syntax)) ;; return value
   (t
    ;; Analyze recursively.
    (let* ((msf (mew-decode-rfc822-header cnt)) ;; on the physical
	   (mimep (nth 0 msf))
	   (subj (nth 1 msf))
	   (mew-inherit-decode-signer (nth 2 msf)) ;; signature verification
	   part)
      ;; the beginning of the physical content header (cur)
      (cond
       (mimep ;; MIME
	(save-restriction
	  (narrow-to-region (point) (point-max))
	  (setq part (mew-decode-singlepart cnt nil 'message))
	  ;; hend is always 1 char smaller than the beginning of
	  ;; the physical content body.
	  ;; If multipart/alternative mew-syntax-get-begin2 gets
	  ;; correct value like other cases.
	  (mew-syntax-set-key syntax 'message)
	  (mew-syntax-set-end syntax (1- (mew-syntax-get-begin2 part)))
	  (or (mew-syntax-get-cd syntax) (mew-syntax-set-cd syntax subj))
	  (mew-syntax-cat syntax part))) ;; return value
       (t ;; RFC822
	;; the beginning of the meaningless physical content header
	(if (re-search-forward mew-eoh nil t)
	    (forward-line)
	  (mew-decode-error "No end-of-header(null line) in RFC822 message"))
	;; the beginning of the BODY(i.e. the physical content body)
	(if (eq (mew-dinfo-get-decode-text) t) ;; not 'no-cs-conv
	    (mew-cs-decode-region (point) (point-max) mew-cs-autoconv))
	(mew-syntax-set-key syntax 'message)
	(mew-syntax-set-end syntax (1- (point)))
	(or (mew-syntax-get-cd syntax) (mew-syntax-set-cd syntax subj))
	;; (point-min), (point) - 1, (point), (point-max)
	(mew-highlight-body-region (point) (point-max))
	(mew-decode-syntax-rfc822
	 syntax (not (mew-xinfo-get-text-body))))))))) ;; return value

;;;
;;; Decoding singlepart
;;;

(defun mew-decode-mime-body (textp ct cte &optional charset)
  ;; ((point), (point-max)) (not point-min)
  (let* ((linebasep (mew-ct-linebasep ct))
	 (beg (point))
	 opt file decoded switch)
    (unless (or (null cte) (mew-cte-composite-p cte))
      (when (and (mew-case-equal cte mew-b64) (fboundp 'base64-decode-region))
	(condition-case nil
	    (setq decoded (base64-decode-region beg (point-max)))
	  (error (setq decoded nil)))
	(when (and decoded linebasep)
	  (goto-char beg)
	  (mew-crlf-to-lf)))
      ;; base64-decode-region may fail if garbage exists.
      (cond
       (decoded
	())
       ((mew-which-exec mew-prog-mime-decode)
	(setq switch (if linebasep
			 mew-prog-mime-decode-text-switch
		       mew-prog-mime-decode-switch))
	(setq opt (mew-prog-mime-decode-get-opt cte switch))
	(if (null opt)
	    ;; Treated as Application/Octet-Stream.
	    ;; Never reach here when decoding.
	    (mew-decode-error (concat "Unknown CTE: " cte))
	  (setq file (mew-make-temp-name))
	  (mew-frwlet mew-cs-dummy mew-cs-text-for-write
	    ;; NEVER use call-process-region for privacy reasons
	    (write-region beg (point-max) file nil 'no-msg))
	  (delete-region beg (point-max))
	  (mew-piolet mew-cs-binary mew-cs-dummy
	    ;; mew-prog-mime-decode converts CRLF to LF, so
	    ;; read input as binary.
	    (apply 'call-process mew-prog-mime-decode file '(t nil) nil opt))
	  (mew-delete-file file)))
       (t
	(mew-decode-error (concat mew-prog-mime-decode " does not exist")))))
    ;; charset conversion
    (if (and textp (eq (mew-dinfo-get-decode-text) t)) ;; not 'no-cs-conv
	(mew-decode-charset-conv charset beg))))

(defun mew-decode-charset-conv (charset beg)
  (let* ((fromcs (and charset (mew-charset-to-cs charset))))
    ;; fromcs is nil if charset is "us-ascii"
    ;; (mew-coding-system-p nil) returns t.
    (unless (mew-coding-system-p fromcs)
      (mew-decode-warning-body 'unknown-charset charset))
    (when (and mew-use-autoconv-when-unknown
	       (not (mew-coding-system-p fromcs)))
      (setq fromcs mew-cs-autoconv))
    (when (and mew-decode-broken
	       (not (eq fromcs mew-cs-autoconv))
	       (or (null charset)
		   (mew-case-equal charset mew-us-ascii))) ;; anyway
      (goto-char beg)
      (when (re-search-forward mew-regex-esc-or-nonascii nil t)
	(mew-decode-warning-body 'no-charset charset)
	(setq fromcs mew-cs-autoconv))
      (goto-char beg))
    (when (and mew-decode-broken
	       (not (eq fromcs mew-cs-autoconv))
	       (or (null charset)
		   (mew-case-equal charset mew-us-ascii)
		   (mew-case-equal charset mew-utf-8)))
      (goto-char beg)
      (when (re-search-forward mew-regex-singlebyte-nonascii nil t)
	(mew-decode-warning-body 'no-charset charset)
	(setq fromcs mew-cs-autoconv))
      (goto-char beg))
    (if (mew-coding-system-p fromcs)
	(mew-cs-decode-region beg (point-max) fromcs))))

(defun mew-decode-singlepart (cnt &optional dct parent)
  ;; Called on the beginning of the content header in the narrowed region
  (let* ((case-fold-search t) (begin (point))
	 (syntax (mew-decode-mime-header dct))
	 (ctl (mew-syntax-get-ct syntax))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (textp (mew-ct-textp ct))
	 (cte (or (mew-syntax-get-cte syntax) mew-7bit))
	 (multi-err t) (encap nil) type hend format delsp)
    ;; the beginning of the content body
    (cond
     ((not (mew-cte-p cte))
      (mew-syntax-set-ct syntax mew-type-apo)
      (if (eq parent 'message) (setq encap t)))
     ((mew-ct-messagep ct)
      (if (not (mew-cte-composite-p cte))
	  (mew-syntax-set-ct syntax mew-type-apo)
	(cond
	 ((string= mew-ct-msg ct)
	  (if (eq parent 'message) (setq encap t))
	  (save-restriction
	    (narrow-to-region (point) (point-max))
	    (setq syntax (mew-decode-message syntax cnt))))
	 ((string= mew-ct-ext ct)
	  (let* ((at (mew-syntax-get-param ctl "access-type"))
		 (func (mew-ext-include-get-func at)))
	    (when (and func (fboundp func))
	      (save-excursion
		(goto-char (point-max)) ;; phantom body
		(funcall func ctl))
	      (delete-region begin (point))
	      (setq syntax (mew-decode-singlepart cnt)))))
	 ((string= mew-ct-sts ct)
	  ;; do nothing
	  )
	 (t
	  ;; xxx how about message/partial?
	  (mew-syntax-set-ct syntax mew-type-apo)
	  ))))
     ((or (string= ct mew-ct-smm) (string= ct mew-ct-xsmm))
      (mew-decode-mime-body nil ct cte)
      (mew-syntax-set-end syntax (point-max))
      (setq syntax (mew-decode-smime syntax cnt)))
     ;; Multipart, decoding is not required
     ((mew-ct-multipartp ct)
      (if (mew-cte-composite-p cte)
	  (setq multi-err nil)
	(mew-decode-warning-body 'multi cte)
	(if mew-decode-broken (setq multi-err nil)))
      (if multi-err
	  (mew-syntax-set-ct syntax mew-type-apo)
	(cond
	 ((string= mew-ct-mld ct)
	  ;; semantics into digest
	  (setq syntax (mew-decode-multipart syntax cnt mew-type-msg)))
	 ((string= mew-ct-mls ct)
	  (setq syntax (mew-decode-multipart-signed syntax cnt)))
	 ((string= mew-ct-mle ct)
	  ;; xxx making use of password cache?
	  (if mew-inherit-prefetching
	      (signal 'quit "")
	    (setq syntax (mew-decode-multipart-encrypted syntax cnt))))
	 (t
	  (setq syntax (mew-decode-multipart syntax cnt nil))))))
     ;; Others
     ((string= mew-ct-trh ct)
      (save-restriction
	;; Eliminating the content the header so that Rfc822 header
	;; comes the beginning.
	(narrow-to-region (point) (point-max))
	(mew-decode-rfc822-header)))
     (t
      (if (and (eq parent 'message)
	       (or (not (mew-xinfo-get-text-body))
		   (if (mew-dinfo-get-encap-html)
		       (not (string= ct mew-ct-txt))
		     (not textp))))
	  (setq encap t))
      ;; even if cte is nil, call mew-decode-mime-body for charset conversion
      (if textp
	  (if (mew-dinfo-get-decode-text)
	      (let ((charset (mew-syntax-get-param ctl "charset")))
		(mew-decode-mime-body t ct cte charset)))
	(if (mew-dinfo-get-decode-binary)
	    (mew-decode-mime-body nil ct cte)))))
    ;; ct may be changed to apo
    ;; or single in multipart/alternative
    ;; or single in multipart/security
    (setq type (mew-syntax-get-value (mew-syntax-get-ct syntax) 'cap))
    (setq hend (mew-syntax-get-end syntax))
    (unless hend
      (mew-syntax-set-end syntax (point-max)))
    (when (and (string= type mew-ct-txt)
	       (setq format (mew-syntax-get-param ctl "format"))
	       (mew-case-equal format "flowed"))
      (setq delsp (mew-syntax-get-param ctl "delsp"))
      (if (and delsp (mew-case-equal delsp "yes"))
	  (setq delsp t)
	(setq delsp nil))
      (mew-decode-flowed (mew-syntax-get-begin syntax) (point-max) delsp)
      (setq hend nil))
    ;; highlight in the cache buffers only
    (when (and (string= type mew-ct-txt)
	       (string-match mew-buffer-cache-prefix (buffer-name)))
      (mew-highlight-body-region (mew-syntax-get-begin syntax) (point-max)))
    (unless hend
      (mew-syntax-set-end syntax (point-max))) ;; ajusting
    (if encap
	;; Mew allows text/plain and multipart/* for body.
	;; If other CT: is embedded under message, it should be
	;; encapsulated in multipart/mixed.
	(let ((head (mew-decode-syntax-multi-head)))
	  ;; begin for multipart syntax is important because
	  ;; the begin will be used by the parent to set hend
	  (mew-syntax-set-begin head (mew-syntax-get-begin syntax))
	  (mew-syntax-set-end head (point-max))
	  (mew-syntax-cat head syntax)) ;; return value
      ;; return value
      syntax)))

(defun mew-decode-flowed-level ()
  (cond
   ((looking-at "^$") -1)
   ((looking-at "^>*")
    (let ((level (- (match-end 0) (match-beginning 0))))
      (goto-char (match-end 0))
      ;; we don't remove space-stuff after >
      (if (and (/= level 0) (looking-at " ")) (forward-char))
      level))))

(defconst mew-flowed-quoted  ?>)
(defconst mew-flowed-stuffed mew-sp)
(defconst mew-flowed-break   mew-sp)
(defconst mew-flowed-netnews "-- ")
(defvar mew-flowed-broken-list '("----- Original Message ----- ")) ;; OE

(defun mew-decode-flowed (beg end delsp)
  "Decoding wrapped lines encoded with RFC 3676"
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let (level prev-level softbreak)
      (setq level (mew-decode-flowed-level))
      (if (= level 0) (mew-decode-flowed-remove-stuffed))
      (setq softbreak (mew-decode-flowed-soft-breakp delsp))
      (while (= (forward-line) 0)
	(setq prev-level level)
	(setq level (mew-decode-flowed-level))
	(cond
	 ((and (= prev-level level) softbreak)
	  (mew-decode-flowed-remove-quoted-stuffed softbreak))
	 (t
	  (if (= level 0) (mew-decode-flowed-remove-stuffed))))
	(setq softbreak (mew-decode-flowed-soft-breakp delsp))))))

(defun mew-decode-flowed-remove-quoted-stuffed (beg)
  (delete-region beg (point)))

(defun mew-decode-flowed-remove-stuffed ()
  (if (and (char-after) (char-equal (char-after) mew-flowed-stuffed))
      (delete-char 1)))

(defun mew-decode-flowed-soft-breakp (delsp)
  (let ((beg (point)) line)
    (end-of-line)
    (setq line (mew-buffer-substring beg (point)))
    (if (string= line mew-flowed-netnews)
	nil
      (if (and mew-decode-broken (member line mew-flowed-broken-list))
	  (progn
	    (mew-xinfo-set-warning
	     (cons "Illegal flowed lines\n" (mew-xinfo-get-warning)))
	    nil)
	(if (and (char-before) (char-equal (char-before) mew-flowed-break))
	    (if delsp (1- (point)) (point))
	  nil)))))

;;;
;;; Decoding multipart
;;;

(defun mew-decode-multipart-boundary-regex (boundary)
  ;; for broken MUAs which uses non-ASCII boundaries
  ;; This works only on Emacs 21.
  (let ((mboundary (mew-set-string-multibyte boundary)))
    ;; Some MUAs use invalid character like "<>".
    (when (or (string-match "[!-&*;<>@[-^`{-~]" mboundary)
	      (not (string= mboundary boundary)))
      (if mew-decode-broken
	  (mew-decode-warning-params "boundary" 'char)
	(mew-decode-error "Boundary has invalid character")))
    (concat "^--" (regexp-quote mboundary) "\\(--\\|\\)[ \t]*$")))

(defun mew-decode-multipart-end-boundary-regex (boundary)
  (let ((mboundary (mew-set-string-multibyte boundary)))
    (concat "^--" (regexp-quote mboundary) "--[ \t]*$")))

(defun mew-decode-multipart-boundary-cont ()
  (string= (mew-match-string 1) ""))

(defun mew-decode-multipart-boundary-end ()
  (string= (mew-match-string 1) "--"))

(defun mew-decode-multipart-beginning-of-first-part (bregex ct)
  ;; RFC 2046 says, "An exact match of the entire candidate
  ;; line is not required; it is sufficient that the boundary
  ;; appear in its entirety following the CRLF".
  (unless (and (re-search-forward bregex nil t)
	       (mew-decode-multipart-boundary-cont))
    (mew-decode-error (format "No first boundary for %s" ct)))
  (forward-line)) ;; the beginning of the first part

(defun mew-decode-multipart-beginning-of-part (break)
  (if break
      () ;; close-delimiter need not be followed by CRLF
    (forward-line))) ;; the beginning of the next part

(defun mew-decode-multipart-end-of-part (break)
  (unless break (forward-line -1))
  (beginning-of-line)
  (forward-char -1) ;; skip the preceding CRLF
  ;; the end of the part
  )

(defun mew-decode-multipart-singlepart (break start cnt dct)
  (save-excursion
    (mew-decode-multipart-end-of-part break)
    (save-restriction
      (narrow-to-region start (point))
      (goto-char (point-min))
      ;; the beginning of the part
      (mew-decode-singlepart cnt dct nil))))

(defun mew-decode-multipart-duplicate-boundary (start cnt dct err boundary bregex)
  (if (not (string-match "^No first boundary" (nth 1 err)))
      (error "")
    ;; duplicate boundary error recovering, sigh
    (widen) ;; breaking save-restriction
    (goto-char start) ;; breaking save-excursion
    (let ((eregex (mew-decode-multipart-end-boundary-regex boundary))
	  break part)
      (if (not (and (re-search-forward eregex nil t)
		    (forward-line)
		    (re-search-forward bregex nil t)))
	  (error "")
	(mew-xinfo-set-warning
	 (cons "Duplicate boundary\n" (mew-xinfo-get-warning)))
	(mew-xinfo-set-decode-err nil)
	(setq break (mew-decode-multipart-boundary-end))
	(mew-decode-multipart-beginning-of-part break)
	(setq part (mew-decode-multipart-singlepart break start cnt dct))
	(list part break)))))

(defun mew-decode-multipart-alternative-choice (part prefpart lastpref lastatpref)
  (let ((pref (or (mew-multipart-alternative-preference part)
		  (length mew-mime-multipart-alternative-list)))
	(atpref (mew-mime-external-body-preference part)))
    ;; returns '(prefpart lastpref lastatpref)
    (cond
     ((or (null prefpart) (< pref lastpref))
      (list part pref atpref))
     ((and (= pref lastpref) atpref
	   (or (null lastatpref) (< atpref lastatpref)))
      ;; If external-body and internal-body are alternative,
      ;; what should we do?
      (list part lastpref atpref))
     (t
      (list prefpart lastpref lastatpref)))))

(defun mew-decode-multipart-alternative-part (prefpart syntax)
  (let* ((beg (mew-syntax-get-begin prefpart))
	 (ctl (mew-syntax-get-ct prefpart))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (multibeg (mew-syntax-get-begin syntax)))
    (mew-syntax-set-begin prefpart (cons beg multibeg))
    (mew-xinfo-set-info
     (cons
      (format "%s in Multipart/Alternative as a singlepart\n" ct)
      (mew-xinfo-get-info)))
    prefpart))

(defun mew-decode-multipart (syntax cnt &optional dct)
  (let* ((case-fold-search nil) ;; boundary is case sensitive
	 (ctl (mew-syntax-get-ct syntax))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (boundary (mew-syntax-get-param ctl "boundary"))
	 (count 0)
	 (parts []) part
	 (use-alt (and (mew-dinfo-get-use-alt) (string= ct mew-ct-mla)))
	 prefpart lastpref lastatpref
	 bregex start break)
    (unless boundary
      (mew-decode-error "No boundary parameter for multipart"))
    (mew-syntax-set-key syntax 'multi)
    (setq bregex (mew-decode-multipart-boundary-regex boundary))
    (mew-decode-multipart-beginning-of-first-part bregex ct)
    (setq start (point)) ;; the beginning of the first part
    (catch 'multipart
      (while (re-search-forward bregex nil t)
	(setq break (mew-decode-multipart-boundary-end))
	(mew-decode-multipart-beginning-of-part break)
	(condition-case err
	    (setq part (mew-decode-multipart-singlepart break start cnt dct))
	  (error
	   ;; rescue duplicate boundary
	   (let ((pb (mew-decode-multipart-duplicate-boundary start cnt dct err boundary bregex)))
	     (mew-set '(part break) pb))))
	(setq count (1+ count))
	(setq start (point)) ;; the beginning of the part
	(if use-alt
	    (let ((pll (mew-decode-multipart-alternative-choice part prefpart lastpref lastatpref)))
	      (mew-set '(prefpart lastpref lastatpref) pll))
	  (setq parts (vconcat parts (vector part))))
	(when break
	  (if use-alt
	      (throw 'multipart (mew-decode-multipart-alternative-part prefpart syntax))
	    (throw 'multipart (vconcat syntax parts)))))
      ;; Let's try to decode the first part even if boundary is not found.
      ;; This should save truncated messages.
      (when (= count 0)
	(save-restriction
	  (narrow-to-region start (point-max))
	  (goto-char (point-min))
	  (mew-decode-singlepart cnt dct nil)))
      (mew-decode-error (format "No last boundary for %s" ct)))))

;;;
;;; Privacy services
;;;

(mew-defstruct privacy-dinfo ct proto result)

(defun mew-decode-multipart-encrypted (syntax cnt)
  ;; called in narrowed region
  ;;
  ;;     CT: M/E; proto; bound;
  ;;
  ;;(cur)--bound
  ;;             (the key part)
  ;;     --bound
  ;;             (the encrypted part)
  ;;     --bound--
  (let* ((case-fold-search nil) ;; boundary is case sensitive
	 (ctl (mew-syntax-get-ct syntax))
	 (boundary (mew-syntax-get-param ctl "boundary"))
	 (switch mew-decode-multipart-encrypted-switch)
	 file1 file2 file3 syntax1 syntax3 func unknown existp proto
	 start result file3result privacy bregex)
    (unless boundary
      (mew-decode-error "No boundary parameter for multipart"))
    (setq bregex (mew-decode-multipart-boundary-regex boundary))
    ;;
    (unless (and (re-search-forward bregex nil t)
		 (mew-decode-multipart-boundary-cont))
      (mew-decode-error "No first boundary for Multipart/Encrypted"))
    (forward-line) ;; the beginning of the key part
    (setq start (point))
    ;;
    (unless (and (re-search-forward bregex nil t)
		 (mew-decode-multipart-boundary-cont))
      (mew-decode-error "No second boundary for Multipart/Encrypted"))
    (beginning-of-line)
    (setq syntax1 (mew-decode-security-singlepart start (1- (point))))
    (setq proto (mew-syntax-get-value (mew-syntax-get-ct syntax1) 'cap))
    (setq func (mew-decode-get-security-func proto switch))
    (setq existp (mew-decode-get-security-existence proto switch))
    (if func
	(if existp (setq file1 (mew-save-decode-form syntax1)))
      (setq unknown t))
    (forward-line) ;; the beginning of the encrypted part
    (setq start (point))
    ;;
    (unless (and (re-search-forward bregex nil t)
		 (mew-decode-multipart-boundary-end))
      (mew-decode-error "No third boundary for Multipart/Encrypted"))
    (beginning-of-line)
    (if (and func existp)
	(setq file2 (mew-save-decode-form
		     (mew-decode-security-singlepart start (1- (point))))))
    ;;
    (delete-region (point-min) (point-max))
    ;;
    ;; Call protocol function
    (cond
     (unknown
      (setq result (concat "unknown protocol " proto)))
     ((not existp)
      (setq result (concat (mew-decode-get-security-prog proto switch)
			   " does not exist")))
     (t
      (setq file3result (funcall func file1 file2))
      (mew-set '(file3 result) file3result)))
    ;;
    (if (and func existp (file-exists-p file3))
	(mew-flet
	 (mew-insert-file-contents file3)
	 (put-text-property (point-min) (point-max) 'mew-noncontents nil)
	 ;; because of RICH functionality of RFC1847... Gee dirty!
	 (mew-decode-crlf-magic))
      (insert "\n") ;; CT: text/plain; charset=us-ascii
      (insert "Multipart/Encrypted could not be decrypted.\n")
      (mew-xinfo-set-not-decrypted t))
    ;; Throw away garbage
    (mew-delete-file file1)
    (mew-delete-file file2)
    (mew-delete-file file3)
    ;; Analyze the decrypted part
    (goto-char (point-min))
    (setq syntax3 (mew-decode-singlepart cnt nil nil))
    (setq privacy (mew-syntax-get-privacy syntax3))
    (if privacy (setq result (concat result "\n\t")))
    (mew-syntax-set-privacy
     syntax3 (cons (mew-make-privacy-dinfo :ct mew-ct-mle :proto proto :result result) privacy))
    syntax3))

(defun mew-decode-multipart-signed (syntax cnt)
  ;; called in narrowed region
  ;;
  ;;     CT: M/S; proto; bound; micalg;
  ;;
  ;;(cur)--bound
  ;;             (the signed part)
  ;;     --bound
  ;;             (the key part)
  ;;     --bound--
  (let* ((case-fold-search nil) ;; boundary is case sensitive
	 (ctl (mew-syntax-get-ct syntax))
	 (boundary (mew-syntax-get-param ctl "boundary"))
	 (switch mew-decode-multipart-signed-switch)
	 file1 file2 syntax2 syntax3 func unknown existp proto
	 end1 start2 result privacy bregex)
    (unless boundary
      (mew-decode-error "No boundary parameter for multipart"))
    (setq bregex (mew-decode-multipart-boundary-regex boundary))
    ;;
    (unless (and (re-search-forward bregex nil t)
		 (mew-decode-multipart-boundary-cont))
      (mew-decode-error "No first boundary for Multipart/Signed"))
    (forward-line) ;; the beginning of the signed part
    (delete-region (point-min) (point)) ;; deleting content-header
    (goto-char (point-min)) ;; just in case
    ;;
    (unless (and (re-search-forward bregex nil t)
		 (mew-decode-multipart-boundary-cont))
      (mew-decode-error "No second boundary for Multipart/Signed"))
    (beginning-of-line)
    (setq end1 (1- (point))) ;; the end of the signed part
    (forward-line) ;; the beginning of the key part
    (setq start2 (point))
    ;;
    (unless (and (re-search-forward bregex nil t)
		 (mew-decode-multipart-boundary-end))
      (mew-decode-error "No third boundary for Multipart/Signed"))
    (beginning-of-line) ;; the end of the encrypted part + 1
    (setq syntax2 (mew-decode-security-singlepart start2 (1- (point))))
    (setq proto (mew-syntax-get-value (mew-syntax-get-ct syntax2) 'cap))
    (setq func (mew-decode-get-security-func proto switch))
    (setq existp (mew-decode-get-security-existence proto switch))
    (if func
	(when existp
	  (setq file1 (mew-save-transfer-form (point-min) end1 'retain))
	  (setq file2 (mew-save-decode-form syntax2 (concat file1 ".sig"))))
      (setq unknown t))
    ;;
    (delete-region end1 (point-max))
    ;; Now the signed part only
    ;; Call protocol function
    (cond
     (unknown
      (setq result (concat "unknown protocol " proto)))
     ((not existp)
      (setq result (concat (mew-decode-get-security-prog proto switch)
			   " does not exist")))
     (t
      (setq result (funcall func file1 file2))))
    ;; Throw away garbage
    (mew-delete-file file1)
    (mew-delete-file file2)
    ;; Analyze the signed part
    (goto-char (point-min))
    (setq syntax3 (mew-decode-singlepart cnt nil nil))
    (setq privacy (mew-syntax-get-privacy syntax3))
    (if privacy (setq result (concat result "\n\t")))
    (mew-syntax-set-privacy
     syntax3 (cons (mew-make-privacy-dinfo :ct mew-ct-mls :proto proto :result result) privacy))
    syntax3))

(defun mew-decode-security-singlepart (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (mew-decode-singlepart 0)))) ;; 0 is dummy

(defun mew-save-decode-form (syntax &optional file)
  (mew-flet
   (unless file (setq file (mew-make-temp-name)))
   (write-region (mew-syntax-get-begin syntax)
		 (mew-syntax-get-end syntax)
		 file nil 'no-msg)
   file))

(defun mew-decode-crlf-magic ()
  (let ((case-fold-search t)
	(cte mew-7bit)
	key start match)
    (save-excursion
      (goto-char (point-min))
      (catch 'header
	(while (re-search-forward
		"^\r?$\\|^Content-Transfer-Encoding:[ \t]*" nil t)
	  (setq key (mew-match-string 0))
	  (setq start (match-end 0))
	  (when (string-match "^\r?$" key)
	    (save-restriction
	      (if (string-match mew-bin cte) ;; cte may include \r
		  (narrow-to-region (point-min) (1+ start))
		(narrow-to-region (point-min) (point-max)))
	      (goto-char (point-min))
	      (mew-crlf-to-lf))
	    (throw 'header nil))
	  (forward-line)
	  (mew-header-goto-next)
	  (setq match (mew-buffer-substring start (1- (point))))
	  (setq cte (mew-addrstr-parse-value match)))))))

(provide 'mew-decode)

;;; Copyright Notice:

;; Copyright (C) 1996-2010 Mew developing team.
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

;;; mew-decode.el ends here
