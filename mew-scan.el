;;; mew-scan.el --- Scanning messages for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan info
;;;

(defvar mew-scan-info-list '("folder" "message"))
;; See mew-scan-fields. 0th is fld, 1st is msg (ie num).

(mew-info-defun "mew-scan-" mew-scan-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-summary-form-size-unit '("" "k" "M" "G" "T"))
(defvar mew-vec [0 1 2 3 4 5 6 8 9 10 11 12 13 14 15 16 17 18 19 20]
  "Just for test of (MEW-FOO).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setup
;;;

(defun mew-scan-setup ()
  "Define functions (MEW-FOO) according 'mew-scan-fields-alias'."
  (dotimes (i (length mew-scan-fields-alias))
    (fset (intern (concat "MEW-" (nth i mew-scan-fields-alias)))
	  `(lambda () (aref mew-vec ,i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pre-defined functions for mew-summary-form
;;;

(defun mew-summary-form-mark ()
  "A function to return a mark.
'mew-summary-form-mark-delete' and 'mew-summary-form-mark-review'
effect to this function."
  (let ((mark-delete mew-mark-delete)
	(mark-review mew-mark-review)
	(mark-spam   mew-mark-delete)
	(unread (and mew-use-unread-mark (mew-sinfo-get-unread-mark)))
	duplicated spam review id body md5)
    (when mew-summary-form-mark-delete
      (if (mew-characterp mew-summary-form-mark-delete)
	  (setq mark-delete mew-summary-form-mark-delete))
      (when (setq id (mew-idstr-get-first-id (MEW-ID)))
	(if (member id (mew-sinfo-get-scan-id))
	    (unless (mew-scan-message-invalidp)
	      (setq duplicated t))
	  (mew-sinfo-set-scan-id (cons id (mew-sinfo-get-scan-id))))))
    (when (and mew-summary-form-mark-spam
	       (mew-sinfo-get-inboxp))
      (if (mew-characterp mew-summary-form-mark-spam)
	  (setq mark-spam mew-summary-form-mark-spam))
      (setq body (MEW-BODY))
      (unless (string= body "")
	(setq md5 (mew-md5 body))
	(if (member md5 (mew-sinfo-get-scan-md5))
	    (unless (mew-scan-message-invalidp)
	      (setq spam t))
	  (mew-sinfo-set-scan-md5 (cons md5 (mew-sinfo-get-scan-md5))))))
    (when mew-summary-form-mark-review
      (if (mew-characterp mew-summary-form-mark-review)
	  (setq mark-review mew-summary-form-mark-review))
      (let* ((mew-header-max-depth nil)
	     (to (mew-addrstr-parse-address-list (MEW-TO)))
	     (cc (mew-addrstr-parse-address-list (MEW-CC))))
	(setq to (nconc to cc))
	(catch 'loop
	  (dolist (cto to)
	    (if (mew-is-my-address mew-regex-my-address-list cto)
		(throw 'loop (setq review t)))))))
    (cond
     (duplicated (char-to-string mark-delete))
     (spam       (char-to-string mark-spam))
     (review     (char-to-string mark-review))
     (unread     (char-to-string mew-mark-unread))
     (t          (char-to-string mew-mark-read)))))

(defvar mew-type-mark-invalid     "#")
(defvar mew-type-mark-truncated   "T")
(defvar mew-type-mark-signed      "S")
(defvar mew-type-mark-encrypted   "E")
(defvar mew-type-mark-multipart   "M")
(defvar mew-type-mark-alternative "-")
(defvar mew-type-mark-partial     "P")
(defvar mew-type-mark-nothing     " ")

(defun mew-summary-form-type ()
  "A function to return a mark of content type."
  (let ((ct (MEW-CT))
	(case-fold-search t))
    (cond
     ((mew-scan-message-invalidp)         mew-type-mark-invalid)
     ((mew-scan-message-truncatedp)       mew-type-mark-truncated)
     ((string-match mew-ct-mls  ct)       mew-type-mark-signed)
     ((string-match mew-ct-mle  ct)       mew-type-mark-encrypted)
     ((string-match mew-ct-sms  ct)       mew-type-mark-encrypted)
     ((string-match mew-ct-xsms ct)       mew-type-mark-encrypted)
     ((or (string-match mew-ct-smm ct)
	  (string-match mew-ct-xsmm ct))
      ;; checking smime-type
      (cond
       ((string-match mew-ct-smm-sig ct)  mew-type-mark-signed)
       ((string-match mew-ct-smm-enc ct)  mew-type-mark-encrypted)
       ;; smime-type is optional, sigh
       (t                                 mew-type-mark-encrypted)))
     ((mew-ct-alternativep ct)            mew-type-mark-alternative)
     ((mew-ct-multipartp ct)              mew-type-mark-multipart)
     ((mew-ct-partialp ct)                mew-type-mark-partial)
     (t                                   mew-type-mark-nothing))))

(defun mew-summary-form-time ()
  "A function to return a message time, HH:MM"
  (let ((s (MEW-DATE)))
    (if (or (string= s "")
	    (not (string-match mew-time-rfc-regex s)))
	(setq s (mew-time-ctz-to-rfc
		 (mew-file-get-time (mew-expand-msg (MEW-FLD) (MEW-NUM))))))
    (if (string-match mew-time-rfc-regex s)
	(format "%02d:%02d"
		(or (mew-time-rfc-hour) 0)
		(or (mew-time-rfc-min)  0))
      "00:00")))

(defun mew-summary-form-date ()
  "A function to return a date, MM/DD."
  (let ((s (MEW-DATE)))
    (when (or (string= s "")
	      (not (string-match mew-time-rfc-regex s)))
      (setq s (mew-time-ctz-to-rfc
	       (mew-file-get-time (mew-expand-msg (MEW-FLD) (MEW-NUM))))))
    (if (string-match mew-time-rfc-regex s)
	(format "%02d/%02d"
		(mew-time-mon-str-to-int (mew-time-rfc-mon))
		(mew-time-rfc-day))
      "")))

(defun mew-summary-form-year ()
  "A function to return a message year, YYYY"
  (let ((s (MEW-DATE)) year)
    (when (or (string= s "")
	      (not (string-match mew-time-rfc-regex s)))
      (setq s (mew-time-ctz-to-rfc
	       (mew-file-get-time (mew-expand-msg (MEW-FLD) (MEW-NUM))))))
    (if (not (string-match mew-time-rfc-regex s))
	"0000"
      (setq year (mew-time-rfc-year))
      (cond
       ((< year 50)
	(setq year (+ year 2000)))
       ((< year 100)
	(setq year (+ year 1900))))
      (number-to-string year))))

(defun mew-summary-form-size ()
  "A function to return the size of the message. Should be used
with -4. See also 'mew-summary-form-size-0k' and 'mew-summary-form-size-huge'."
  (let ((len-1 (1- (length mew-summary-form-size-unit)))
	(SIZE (mew-scan-uid-size (MEW-UID)))
	(i 0) size unit)
    (if (and SIZE (string-match "^[0-9]+$" SIZE))
	(setq size (string-to-number SIZE))
      (setq size (mew-file-get-size (mew-expand-msg (MEW-FLD) (MEW-NUM)))))
    (while (and (< i len-1) (>= size 1000))
      (setq size (/ size 1000))
      (setq i (1+ i)))
    (if (and mew-summary-form-size-huge (>= size 1000))
	"HUGE"
      (setq unit (nth i mew-summary-form-size-unit))
      (if (and mew-summary-form-size-0k (string= unit ""))
	  "0k"
	(concat
	 (if (integerp size)
	     (number-to-string size)
	   (format "%.0f" size))
	 unit)))))

(defun mew-summary-form-extract-addr (addr)
  "Extract addr according to 'mew-summary-form-extract-rule'."
  (condition-case nil
      (let* ((func (if mew-addrbook-for-summary
		       (mew-addrbook-func mew-addrbook-for-summary)))
	     (raw (or (mew-addrstr-parse-address addr) ""))
	     (rules mew-summary-form-extract-rule)
	     ret nickname)
	(catch 'matched
	  (dolist (rule rules)
	    (cond
	     ((and (eq rule 'name)
		   (or (string-match "^\"\\([^\"]+\\)\"[ \t]*<[^>]+>" addr)
		       (string-match "^\\([^<]+\\)<[^>]+>" addr)))
	      (throw 'matched (setq ret (mew-chop (match-string 1 addr)))))
	     ((and (eq rule 'comment)
		   (string-match "^[^(]+(\\(.+\\))" addr))
	      (throw 'matched (setq ret (mew-chop (match-string 1 addr)))))
	     ((eq rule 'address)
	      (throw 'matched (setq ret raw)))
	     ((and (eq rule 'nickname)
		   ;; set nickname here for efficiency
		   (or nickname
		       (setq nickname (if func (funcall func raw)))))
	      (throw 'matched (setq ret nickname)))
	     ((and (stringp rule)
		   (string-match rule addr))
	      (throw 'matched (setq ret (mew-chop (match-string 1 addr))))))))
	(or ret addr))
    ;; a version of downcase causes error if argument is non-ascii.
    (error mew-error-broken-address)))

(defun mew-summary-form-from ()
  "A function to return an address.
If the message is destined to me AND 'mew-summary-form-from-me-prefix'
is a string, an address on To:, is returned. In this
case, 'mew-summary-form-from-me-prefix' is prepended to the address.

Otherwise, an address on From: is returned.

Address is converted by 'mew-summary-form-extract-addr'. See also
'mew-summary-form-extract-rule'."
  (let* ((FROM (MEW-FROM)) (TO (MEW-TO))
	 (from (or (mew-addrstr-parse-address FROM) "")))
    (cond
     ((string= FROM "")
      "")
     ((and (stringp mew-summary-form-from-me-prefix)
	   (not (string= TO ""))
	   (mew-is-my-address mew-regex-my-address-list from))
      (mew-replace-white-space
       (concat mew-summary-form-from-me-prefix (mew-summary-form-extract-addr TO))))
     (t
      (mew-replace-white-space (mew-summary-form-extract-addr FROM))))))

(defun mew-summary-form-subj ()
  "A function to return Subject:. Unnecessary white spaces are removed."
  ;; The beginning white spaces have been removed in mew-scan-header
  ;; (mew-keyval).
  (let ((subj (MEW-SUBJ)))
    (if (string= subj "") (setq subj mew-error-no-subject))
    (if mew-decode-broken
	subj
      ;; already well-formatted
      (mew-replace-white-space subj))))

(defun mew-summary-form-body ()
  (mew-header-sanity-check-string (MEW-BODY)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan from
;;;

(defun mew-decide-summary-form (folder)
  (mew-folder-spec folder
		   mew-summary-form-list
		   mew-summary-form-list-string-type
		   mew-summary-form-list-list-type))

(defun mew-get-summary-form (folder)
  "Get summary-form from 'mew-summary-form-list',
'mew-summary-form-list-string-type, and 'mew-summary-form-list-list-type'.
'mew-summary-form-header' is prepended. "
  (let* ((form-col (mew-decide-summary-form folder))
	 (form (or (nth 0 form-col) mew-summary-form)))
    (append mew-summary-form-header form)))

(defun mew-get-summary-column (folder)
  (let ((form-col (mew-decide-summary-form folder)))
    (or (nth 1 form-col)
	(mew-thread-column (mew-get-summary-form folder))
	mew-thread-column)))

(defun mew-thread-column (form)
  (let ((col 0))
    (catch 'loop
      (dolist (ent form)
	(cond
	 ((consp ent)
	  (setq col (+ col (abs (car ent)))))
	 ((stringp ent)
	  (setq col (+ col (string-width ent))))
	 ((eq ent t)
	  (throw 'loop col))
	 (t
	  (setq col (1+ col))))))))

(defun mew-get-unread-mark (folder)
  (car (mew-folder-spec folder
			mew-unread-mark-list
			mew-unread-mark-list-string-type
			mew-unread-mark-list-list-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The engine function to call mew-summary-form-*
;;;

(defvar mew-inherit-total nil)
(defvar mew-inherit-width nil)

(defun mew-scan-get-piece (spec)
  (let (func width str rightp nopad)
    (if (stringp spec)
	(progn
	  (setq mew-inherit-total (+ mew-inherit-total (string-width spec)))
	  spec)
      (if (symbolp spec)
	  (setq width 1 func spec)
	(mew-set '(width func) spec))
      (when (and (integerp width) (symbolp func))
	(when (= width 0)
	  (setq width (- mew-inherit-width mew-inherit-total 1))
	  (unless mew-use-spc-padding
	    (setq nopad t)))
	(if (< width 0) (setq width (abs width) rightp t))
	(setq mew-inherit-total (+ mew-inherit-total width))
	(setq func (intern-soft
		    (concat mew-summary-form-func-prefix (symbol-name func))))
	(when (fboundp func)
	  (setq str (funcall func))
	  (if rightp
	      (if (<= (string-width str) width)
		  (format (format "%%%ds" width) str)
		(setq mew-inherit-total (+ (- mew-inherit-total width) (string-width str)))
		str) ;; width may exceed.
	    (mew-substring str width nil nopad)))))))

(defun mew-sumsym-encode-folder (fld)
  (mew-replace-character fld ?  ?\t))

(defun mew-sumsym-decode-folder (fld)
  (mew-replace-character fld ?\t ? ))

(defun mew-scan-get-line (mew-vec mew-inherit-width)
  (let* ((mew-inherit-total 0) (fld "")
	 (line (mapconcat 'mew-scan-get-piece (mew-sinfo-get-summary-form) ""))
	 par-id my-id msg ld uid siz irt-list)
    (setq my-id (or (mew-idstr-get-first-id (MEW-ID)) ""))
    ;; RFC 2822 says: the "In-Reply-To:" field may be used to identify
    ;; the message (or messages) to which the new message is a reply,
    ;; while the "References:" field may be used to identify a
    ;; "thread" of conversation.
    ;;
    ;; However, even if the References field exists, it may not contain
    ;; a parent's ID. So, if the In-Reply-To field contain one ID,
    ;; we use it for thread.
    ;;
    ;; (1) The In-Reply-To contains one ID, use it.
    ;; (2) The References contains one or more IDs, use the last one.
    ;; (3) The In-Reply-To contains two or more IDs, use the first one.
    (setq par-id (or (mew-idstr-get-first-id (MEW-XREF)) ""))
    (when (string= par-id "")
      (setq irt-list (mew-idstr-to-id-list (MEW-IRT)))
      (if (= (length irt-list) 1)
	  (setq par-id (car irt-list))
	(setq par-id (or (mew-idstr-get-last-id (MEW-REF))
			 (car irt-list)
			 ""))))
    (when (mew-virtual-p)
      (setq fld (mew-sumsym-encode-folder
		 (or (cdr (assoc (mew-scan-get-folder mew-vec)
				 (mew-vinfo-get-lra)))
		     ;; Spotlight
		     (MEW-FLD))))) ;; xxx
    (setq msg (mew-scan-get-message mew-vec))
    (setq uid (or (mew-scan-uid-uid (MEW-UID)) ""))
    (setq siz (or (mew-scan-uid-size (MEW-UID)) ""))
    (setq ld (format "\r %s %s %s %s %s %s\n" fld msg my-id par-id uid siz))
    (cons line ld)))

;; See also mew-summary-cook-region
(defun mew-scan-insert-line (folder vec width lmsg &optional mark-or-dst)
  (when (get-buffer folder)
    (with-current-buffer folder
      (let* ((line (mew-scan-get-line vec width))
	     (opos (point))
	     (omax (point-max))
	     beg med face olen nlen mark msg)
	(mew-elet
	 (if (null lmsg)
	     (goto-char (point-max))
	   ;; a message marked with 'T'.
	   (when (mew-summary-search-msg lmsg)
	     (setq mark (mew-summary-get-mark))
	     (setq beg (point))
	     (forward-line)
	     ;; To avoid inserting a line AFTER the cursor underline,
	     ;; keep this line and make it invisible.
	     (put-text-property beg (point) 'invisible t)
	     (forward-line -1)))
	 (setq beg (point))
	 ;; To "insert" just after mew-marker-decode-syntax-end.
	 (insert (car line))
	 (setq med (point))
	 (insert (cdr line))
	 (goto-char beg)
	 (cond
	  ((stringp mark-or-dst) ;; xxx
	   (setq msg (mew-scan-get-message vec))
	   (mew-refile-reset msg)
	   (mew-refile-set msg (mew-split mark-or-dst ?,))
	   (mew-summary-refile-log folder mark-or-dst)
	   (setq med (+ med (mew-summary-refile-override-body mark-or-dst 'force)))
	   (mew-mark-put mew-mark-refile))
	  ((mew-characterp mark-or-dst) ;; mew-inbox-action-alist
	   (mew-mark-put mark-or-dst))
	  (mark
	   (mew-summary-mark-as mark))
	  ((and mew-use-highlight-mark ;; mew-summary-form-mark
		(setq mark (mew-summary-get-mark)) ;; duplicated, etc
		(setq face (mew-highlight-mark-get-face mark)))
	   (put-text-property beg med 'face face)))
	 (if mew-use-highlight-mouse-line
	     (put-text-property
	      beg med 'mouse-face mew-highlight-mouse-line-face))
	 (forward-line)
	 (put-text-property med (1- (point)) 'invisible t)
	 ;; Removing the invisible line.
	 (when lmsg
	   ;; UID information will be removed. So, we need to adjust
	   ;; the position.
	   (setq nlen (- (point) beg))
	   (setq beg (point))
	   (forward-line)
	   (when (> opos beg)
	     (setq olen (- (point) beg))
	     (setq opos (- opos (- olen nlen))))
	   (delete-region beg (point))))
	(if (or (eq opos (mew-sinfo-get-start-point))
		(/= opos omax))
	    ;; move the cursor to the original position.
	    (goto-char opos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub-functions for Scan
;;;

(defun mew-scan-header (&optional draftp)
  (let ((vec (make-vector (length mew-scan-fields) ""))
	(lim (1- mew-scan-max-field-length))
	i key med str n)
    (goto-char (point-min))
    (unless (re-search-forward mew-eoh nil t)
      (goto-char (point-max)))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (looking-at mew-keyval))
	    (forward-line)
	  (setq key (mew-capitalize (mew-match-string 1)))
	  (setq med (match-end 0))
	  ;; Three lines should be enough for Summary mode.
	  (forward-line)
          (setq i 0)
	  (while (and (< i lim) (looking-at mew-lwsp))
	    (forward-line)
	    (setq i (1+ i)))
	  (when (and (setq n (mew-member-case-equal key mew-scan-fields))
		     (string= (aref vec n) "")) ;; avoid multiple times
	    (when (member key mew-scan-decode-fields)
	      (mew-header-decode-region key med (point) draftp))
	    ;; We need to keep composite properties of charset.
	    ;; This must be "buffer-substring".
	    (setq str (buffer-substring med (1- (point))))
	    (aset vec n str))
	  (mew-header-goto-next))))
    vec))

(defun mew-scan-field-to-func (key)
  (let ((n (mew-member-case-equal key mew-scan-fields))
	(len (length mew-scan-fields-alias))
	ali)
    (if (and n (< n len)) (setq ali (nth n mew-scan-fields-alias)))
    (if (stringp ali) (symbol-function (intern-soft (concat "MEW-" ali))))))

(defun mew-scan-inbox-action (mew-vec case)
  (let ((alist (mew-inbox-action-alist case))
	key val val-func ret mark-or-dst regex-list)
    (catch 'loop
      (dolist (ent alist)
	(setq key (car ent))
	(setq val-func (mew-scan-field-to-func key))
	(if (and val-func (functionp val-func))
	    (setq val (funcall val-func))
	  (setq val nil))
	(when val
	  (setq ent (cdr ent))
	  (dolist (act ent)
	    (if (symbolp act)
		(when (fboundp act)
		  (setq ret (funcall act val))
		  (if ret (throw 'loop nil)))
	      (when (listp act)
		(setq mark-or-dst (car act))
		(setq regex-list (cdr act))
		(dolist (rl regex-list)
		  (if (string-match rl val)
		      (throw 'loop (setq ret mark-or-dst))))))))))
    ret))

(defvar mew-regex-ignore-scan-body-list
  '("^[ \t]*$"
    "^[ \t]*[-a-zA-Z0-9]+: "
    "^[ \t]*[>:|#;/_}]"
    "^[ \t]*\\w+\\(['._-]+\\w+\\)*>"
    "^[ \t]*[[</(.-]+ *\\(snip\\|\\.\\.\\)"
    "^   "
    "^--"
    "^- --"
    "^=2D"
    "^.\\{1,100\\}\\(:\\|;\\|/\\)[ \t]*$"
    "^.\\{1,100\\}\\(wrote\\|writes?\\|said\\|says?\\)[^.!\n]?[ \t]*$"
    "^[ \t]*\\(On\\|At\\) .*[^.! \t\n][ \t]*$"
    "^[ \t]*In \\(message\\|article\\|mail\\|news\\|<\\|\"\\|\\[\\|(\\)"))

(defun mew-scan-body (mew-vec &optional draftp)
  (forward-line)
  (let* ((i 0) (I mew-scan-max-body-length)
	 (j 0) (J mew-scan-body-length)
	 (ctr (MEW-CT))
	 (cte (MEW-CTE))
	 (body "")
	 (case-fold-search t)
	 textp charset cs beg skip boundary found regex)
    (catch 'break
      (cond
       (draftp
	(setq textp t)
	(setq cs mew-cs-m17n))
       ((string= ctr "")
	(if (mew-case-equal cte mew-b64) (throw 'break nil))
	(setq textp t)
	(setq cs mew-cs-autoconv))
       (t
	;; The following code is generic but too slow.
	;; (setq ctl (mew-param-decode ctr))
	;; (setq ct (mew-syntax-get-value ctl 'cap))
	;; So, this hard coding is used.
	(when (and (string-match "^Multipart/" ctr)
		   (string-match "boundary=\"?\\([^\"\n\t;]+\\)\"?" ctr))
	  (setq boundary (mew-match-string 1 ctr))
	  (setq boundary (concat "^--" (regexp-quote boundary)))
	  (catch 'loop
	    (while (< i I)
	      (if (looking-at boundary) (throw 'loop (setq found t)))
	      (forward-line)
	      (setq i (1+ i))))
	  (if (not found)
	      (throw 'break nil)
	    (forward-line)
	    (save-restriction
	      (narrow-to-region (point) (point-max))
	      (setq ctr (mew-header-get-value mew-ct:))
	      (setq cte (mew-addrstr-parse-value
			 (mew-header-get-value mew-cte:)))
	      (mew-header-goto-end)) ;; should be in the narrowed region
	    (unless ctr ;; not ""
	      (setq textp t)
	      (setq cs mew-cs-autoconv)
	      (throw 'break nil))))
	(if (and cte (mew-case-equal cte mew-b64)) (throw 'break nil))
	(when (string-match "^Text/Plain" ctr)
	  (when (string-match "charset=\"?\\([^\"\n\t;]+\\)\"?" ctr)
	    (setq charset (mew-match-string 1 ctr)))
	  ;; xxx quoted-printable. not enough DB in mew-mule3.el.
	  (setq textp t)
	  (setq cs (mew-charset-to-cs charset))
	  (if (null cs) (setq cs mew-cs-autoconv)))))) ;; end of 'break
    (set-buffer-multibyte nil)
    (when (and textp (mew-coding-system-p cs))
      (setq i 0)
      (while (and (not (eobp)) (< i I) (< j J))
	(setq regex mew-regex-ignore-scan-body-list)
	(setq skip nil)
	(catch 'matched
	  (dolist (re regex)
	    (if (looking-at re)
		(throw 'matched (setq skip t)))))
	(if skip
	    (forward-line)
	  (when (looking-at "^[ \t]+")
	    (goto-char (match-end 0)))
	  (setq beg (point))
	  (forward-line)
	  (setq body (concat body (mew-buffer-substring beg (1- (point))) " "))
	  (setq j (1+ j)))
	(setq i (1+ i)))
      (set-buffer-multibyte t)
      (setq body (mew-replace-white-space body))
      (setq body (condition-case nil
		     (mew-cs-decode-string body cs)
		   (error body)))
      (aset mew-vec (1- (length mew-vec)) body))))

(defun mew-scan-width ()
  (if (and (integerp mew-summary-scan-width)
	   (> mew-summary-scan-width 40)) ;; xxx
      mew-summary-scan-width
    (max mew-window-magic (window-width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X-Mew-Uidl:
;;;

(defun mew-scan-uid-uid (uid)
  (nth 0 (mew-split uid mew-sp)))

(defun mew-scan-uid-size (uid)
  (nth 1 (mew-split uid mew-sp)))

(defun mew-scan-uid-case (uid)
  (nth 2 (mew-split uid mew-sp)))

(defun mew-header-insert-xmu (uid siz truncated &optional case)
  (when (and (stringp uid) (stringp siz))
    (setq siz (number-to-string (string-to-number siz))) ;; removing 0
    (let (fields)
      (if (not truncated)
	  (setq fields (concat uid " " siz))
	(setq fields (concat uid " 0" siz)) ;; e.g. 0500 == truncated
	(if case (setq fields (concat fields " " case))))
      (save-excursion
	(mew-header-delete-lines (list mew-x-mew-uidl:)))
      (mew-header-insert mew-x-mew-uidl: fields 'no-fold))))

(defun mew-scan-message-truncatedp ()
  (mew-msg-truncatedp (mew-scan-uid-size (MEW-UID))))

(defun mew-scan-message-invalidp ()
  (mew-msg-invalidp (MEW-NUM)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scanning a folder
;;;

(defun mew-summary-ls (&optional header-only goend update)
  "List this folder asynchronously.

In a LOCAL folder: messages in the local folder are scanned according
to the range which you specify.

In a REMOTE folder: messages in the server's folder are cached
according to the range which you specify. If
'mew-pop-header-only'/'mew-imap-header-only'/'mew-nntp-header-only' is
non-nil, only headers of messages are cached. If executed with
'\\[universal-argument]', these variables are considered reversed."
  (interactive "P")
  (mew-summary-only
   (when (mew-summary-exclusive-p)
     (let* ((bnm (mew-summary-folder-name 'ext))
	    (case (mew-sinfo-get-case))
	    (fld (mew-sinfo-get-folder))
	    (askp mew-ask-range)
	    (directive 'scan)
	    (get-body (not header-only))
	    scanp range dir-newp)
       (mew-summary-folder-cache-load)
       (cond
	(update
	 (setq askp nil)
	 (setq range nil) ;; update
	 (setq scanp t))
	((interactive-p) ;; "s"
	 (setq askp t)
	 (setq scanp t))
	((mew-summary-folder-dir-newp) ;; "g"
	 (setq askp nil)
	 (setq scanp t)))
       ;; for mew-summary-exchange-point.
       (mew-sinfo-set-ret-pos (point))
       (if (mew-summary-folder-dir-newp) (setq dir-newp t))
       (if (or (interactive-p) goend) (goto-char (point-max)))
       (set-buffer-modified-p nil)
       (if (not scanp)
	   (progn
	     (run-hooks 'mew-summary-ls-no-scan-hook)
	     t) ;; return value (not scanned)
	 (mew-summary-reset)
	 ;;
	 (mew-sinfo-set-direction 'down)
	 (cond
	  ((and (mew-folder-remotep fld)
		(not (mew-folder-imap-queuep)))
	   (if (and dir-newp (mew-folder-imapp fld))
	       (mew-local-retrieve 'scan bnm (mew-range-update bnm))
	     (if askp (setq range (mew-input-range-remote bnm)))
	     (when (eq range 'sync)
	       (setq range nil)
	       (setq directive 'sync))
	     (cond
	      ((mew-folder-popp fld)
	       (if (mew-pop-header-only case)
		   (setq get-body (not get-body)))
	       (mew-pop-retrieve case directive bnm range get-body))
	      ((mew-folder-imapp fld)
	       (if (mew-imap-header-only case)
		   (setq get-body (not get-body)))
	       (mew-imap-retrieve case directive bnm range get-body))
	      ((mew-folder-nntpp fld)
	       (if (mew-nntp-header-only case)
		   (setq get-body (not get-body)))
	       (mew-nntp-retrieve case directive bnm range get-body)))))
	  (t ;; local
	   (setq range (mew-input-range bnm askp))
	   (if range
	       (mew-local-retrieve 'scan bnm (nth 0 range) (nth 1 range))
	     (message "range is wrong"))))
	 nil))))) ;; return value (scanned)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary file cache
;;;

(defun mew-compare-times (t1 t2)
  ;; Is t1 newer than t2?
  (cond
   ((null t1) nil)
   ((null t2) t) ;; do update
   ((> (nth 0 t1) (nth 0 t2)) t)
   ((= (nth 0 t1) (nth 0 t2))
    (if (> (nth 1 t1) (nth 1 t2)) t nil)) ;; nil if equal
   (t nil)))

(defun mew-summary-folder-dir-newp ()
  (let* ((folder (mew-summary-folder-name 'ext))
	 (dir (file-chase-links (mew-expand-folder folder)))
	 (mfile (expand-file-name mew-summary-touch-file dir))
	 (t1 (mew-file-get-time mfile))
	 (cache (expand-file-name mew-summary-cache-file dir))
	 (t2 (mew-file-get-time cache)))
    (if (and (null t1)
	     (file-directory-p dir)
	     (mew-dir-messages dir))
	t
      (mew-compare-times t1 t2))))

(defun mew-summary-folder-cache-newp ()
  (let* ((folder (mew-summary-folder-name 'ext))
	 (cache (mew-expand-file folder mew-summary-cache-file))
	 (t1 (mew-file-get-time cache))
	 (t2 (mew-sinfo-get-cache-time)))
    (mew-compare-times t1 t2)))

(defun mew-summary-set-count-line ()
  (let* ((ttl-line (mew-count-lines (point-min) (point-max)))
	 (mid-point (/ (buffer-size) 2))
	 (mid-marker (mew-sinfo-get-mid-marker))
	 mid-line)
    (save-excursion
      (goto-char mid-point)
      (beginning-of-line)
      (if (and (mew-thread-p) mew-use-thread-separator
	       (looking-at mew-regex-thread-separator))
	  (forward-line))
      (setq mid-point (point))
      (setq mid-line (mew-count-lines (point-min) (point))))
    (mew-sinfo-set-ttl-line ttl-line)
    (mew-sinfo-set-mid-line mid-line)
    (unless (markerp mid-marker)
      (setq mid-marker (make-marker))
      (mew-sinfo-set-mid-marker mid-marker))
    (set-marker mid-marker mid-point)))

(defun mew-summary-folder-cache-load ()
  (let* ((folder (mew-summary-folder-name 'ext))
	 (cache (mew-expand-file folder mew-summary-cache-file))
	 refile refs)
    (when (and (file-readable-p cache)
	       (mew-summary-folder-cache-newp))
      (mew-elet
       (mew-erase-buffer)
       (mew-frwlet mew-cs-m17n mew-cs-dummy
	 (mew-insert-file-contents cache))
       (mew-sinfo-set-cache-time (mew-file-get-time cache))
       (if (= (point-max) 1)
	   (setq mew-summary-buffer-raw nil)
	 (setq mew-summary-buffer-raw t))
       (mew-sinfo-load)
       (setq refs (mew-summary-mark-collect mew-mark-refile))
       (setq refile (mew-summary-mark-recover
		     (mew-sinfo-get-mark-hist) (mew-sinfo-get-refile) refs))
       (mew-sinfo-set-refile refile)
       (mew-summary-set-count-line)
       (set-buffer-modified-p nil)))))

(defun mew-summary-folder-cache-save ()
  (let* ((folder (mew-summary-folder-name 'ext))
	 (cache (mew-expand-file folder mew-summary-cache-file)))
    (when (file-writable-p cache)
      (mew-touch-folder folder)
      (save-restriction
	(widen)
	(if (mew-decode-syntax-p)
	    (let ((cbuf (current-buffer))
		  (min (point-min))
		  (max (point-max))
		  (beg (mew-decode-syntax-begin))
		  (end (mew-decode-syntax-end)))
	      (with-temp-buffer
		(mew-insert-buffer-substring cbuf min beg)
		(mew-insert-buffer-substring cbuf end max)
		(mew-frwlet mew-cs-dummy mew-cs-m17n
		  (write-region (point-min) (point-max) cache nil 'no-msg))))
	  ;; (write-region 1 1 ...) does not update the file timestamp
	  ;; but does the directory timestamp. So, we need to delete
	  ;; the file to update the file timestamp.
	  (if (= (point-min) (point-max)) (mew-delete-file cache))
	  (mew-frwlet mew-cs-dummy mew-cs-m17n
	    (write-region (point-min) (point-max) cache nil 'no-msg))
	  (mew-set-file-modes cache))
	(mew-summary-set-count-line)
	(mew-sinfo-set-cache-time (mew-file-get-time cache))
	(mew-sinfo-save)
	(mew-sinfo-set-mark-hist nil)))))

;; See also mew-net-folder-clean.
(defun mew-summary-folder-cache-clean (folder)
  "Erase Summary mode then remove and touch the cache file."
  (if (get-buffer folder)
      (with-current-buffer folder
	(mew-erase-buffer)
	(set-buffer-modified-p nil)))
  (let ((cfile (mew-expand-file folder mew-summary-cache-file)))
    (if (file-exists-p cfile)
	(write-region "" nil cfile nil 'no-msg))))

(provide 'mew-scan)

(defvar mew-compiling nil)
(eval-when-compile
  (when mew-compiling
    (require 'mew-varsx)
    (mew-scan-setup)))

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

;;; mew-scan.el ends here
