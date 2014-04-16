;;; mew-syntax.el --- Internal syntax for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mew-encode-syntax
;;
;; <esgl> = [ 'single file (dcr) (<epri>) (CT:)   CTE: CD: nil (CDP:) ]
;; <emul> = [ 'multi  dir/ (dcr) (<epri>) ("mul") CTE: CD: nil (CDP:) 1*<eprt> ]
;; <eprt> = <esgl> | <emul>
;; <epri> = list of (mew-ct-mls mew-ct-pgs)
;;
;; mew-decode-syntax
;;
;; <dmsg> = [ 'message hbeg hend (<dpri>) ("msg") CTE: CD: CID: (CDP:) <dbdy> ]
;; <dsgl> = [ 'single   beg  end (<dpri>) (CT:)   CTE: CD: CID: (CDP:) ]
;; <dmul> = [ 'multi    beg  end (<dpri>) ("mul") CTE: CD: CID: (CDP:) 1*<dprt> ]
;; <dbdy> = <dmul> | Text/Plain <dsgl>
;; <dprt> = <dmsg> | <dsgl> | <dmul>
;; <dpri> = list of (mew-ct-mls mew-ct-pgs RESULT) in reverse order ;; for cons

;;
;;
;;

(defun mew-syntax-singlepart-p (syntax)
  (eq (aref syntax 0) 'single))

(defun mew-syntax-multipart-p (syntax)
  (eq (aref syntax 0) 'multi))

(defun mew-syntax-message-p (syntax)
  (eq (aref syntax 0) 'message))

;;
;;
;;

(defun mew-syntax-get-key (syntax)
  (aref syntax 0))

(defun mew-syntax-get-begin (syntax)
  (let ((beg (aref syntax 1)))
    (if (listp beg) (car beg) beg)))

(defun mew-syntax-get-begin2 (syntax)
  (let ((beg (aref syntax 1)))
    (if (listp beg) (cdr beg) beg)))

(defun mew-syntax-get-end (syntax)
  (aref syntax 2))

(defun mew-syntax-get-privacy (syntax)
  (aref syntax 3))

(defun mew-syntax-get-ct (syntax)
  (aref syntax 4))

(defun mew-syntax-get-cte (syntax)
  (aref syntax 5))

(defun mew-syntax-get-cd (syntax)
  (aref syntax 6))

(defun mew-syntax-get-cid (syntax)
  (aref syntax 7))

(defun mew-syntax-get-cdp (syntax)
  (aref syntax 8))

(defun mew-syntax-get-part (syntax)
  (aref syntax 9))


(defun mew-syntax-set-key (syntax key)
  (aset syntax 0 key))

(defun mew-syntax-set-begin (syntax begin)
  (aset syntax 1 begin))

(defun mew-syntax-set-end (syntax end)
  (aset syntax 2 end))

(defun mew-syntax-set-privacy (syntax privacy)
  (aset syntax 3 privacy))

(defun mew-syntax-set-ct (syntax ct)
  (aset syntax 4 ct))

(defun mew-syntax-set-cte (syntax cte)
  (aset syntax 5 cte))

(defun mew-syntax-set-cd (syntax cd)
  (aset syntax 6 cd))

(defun mew-syntax-set-cid (syntax cid)
  (aset syntax 7 cid))

(defun mew-syntax-set-cdp (syntax cdp)
  (aset syntax 8 cdp))

(defun mew-syntax-set-part (syntax part)
  (aset syntax 9 part))


;; alias for encode syntax

(defun mew-syntax-get-file (syntax)
  (aref syntax 1))

(defun mew-syntax-set-file (syntax file)
  (aset syntax 1 file))

(defun mew-syntax-get-decrypters (syntax)
  (aref syntax 2))

(defun mew-syntax-set-decrypters (syntax decrypters)
  (aset syntax 2 decrypters))

;; for content parameters

(defun mew-syntax-get-value (ctl &optional capitalizep)
  (if capitalizep
      (mew-capitalize (car ctl))
    (car ctl)))

(defun mew-syntax-get-params (ctl)
  (cdr ctl))

;; ctl = (value (pname pvalue) (pname pvalue) ...)
;; ctl = ((pname pvalue) (pname pvalue) ...)
(defun mew-syntax-get-param (ctl member)
  (nth 1 (assoc member ctl)))

(defun mew-syntax-treat-filename-default-function (file)
  (if (stringp file)
      (mew-replace-white-space
       (mew-chop (file-name-nondirectory file)))))

(defun mew-syntax-get-filename (cdpl &optional ctl)
  (let ((file (mew-syntax-get-param cdpl "filename")))
    ;; for broken MUAs
    (if (and (null file) mew-use-name-parameter ctl)
	(setq file (mew-syntax-get-param ctl "name")))
    (if mew-syntax-treat-filename-function
	(setq file (funcall mew-syntax-treat-filename-function file)))
    file))

;; need to setq
(defun mew-syntax-cat (syntax part)
  (vconcat syntax (vector part)))

(defun mew-syntax-cdp-format (ct file &optional cdp force-file)
  ;; If cdp is a string, it is used as a file.
  (let* ((cdpdb (mew-cdpdb-by-ct ct))
	 (val (mew-cdpdb-val cdpdb))
	 (filep (mew-cdpdb-file cdpdb)))
    (if (stringp cdp) (setq file cdp))
    (when val
      (if (and (or filep force-file) file)
	  (list val (list "filename" (file-name-nondirectory (mew-header-sanity-check-string file))))
	(list val)))))

;; Encryption

(defun mew-syntax-encrypted-p (syntax)
  (catch 'loop
    (dolist (ent (mew-syntax-get-privacy (mew-syntax-get-part syntax)))
      (if (or (mew-case-equal (mew-privacy-dinfo-get-ct ent) mew-ct-mle)
	      (mew-case-equal (mew-privacy-dinfo-get-proto ent) mew-ct-smm-enc))
	  (throw 'loop t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Entry functions
;;

(defun mew-syntax-get-entry (syntax nums)
  (cond
   ((null nums) syntax) ;; single & message match
   ((mew-syntax-message-p syntax)
    (let ((body (mew-syntax-get-part syntax)))
      (if (mew-syntax-multipart-p body)
	  (mew-syntax-get-entry (mew-syntax-get-part syntax) nums)
	;; nums should be "1"
	body)))
   ((mew-syntax-multipart-p syntax)
    (if (null nums)
	syntax
      (mew-syntax-get-entry
       (aref syntax (+ mew-syntax-magic (1- (car nums)))) (cdr nums))))))

(defun mew-syntax-insert-entry (syntax nums entry)
  (let* ((root syntax)
	 (child entry)
	 grand parent
	 (nl (length nums))
	 (rev (reverse nums))
	 (n0 (nth 0 rev))
	 (n1 (nth 1 rev))
	 (ns (reverse (nthcdr 2 rev))))
    (cond
     ((= nl 1)
      (setq parent root)
      (mew-syntax-add-entry parent n0 child))
     (t
      (if (= nl 2)
	  (setq grand root)
	(setq grand (mew-syntax-get-entry root ns)))
      (setq parent (mew-syntax-get-entry grand (list n1)))
      (setq parent (mew-syntax-add-entry parent n0 child))
      (aset grand (+ mew-syntax-magic (1- n1)) parent)
      root))))

(defun mew-syntax-add-entry (syntax n entry)
  "Must not use in functions other than mew-syntax-insert-entry"
  (let* ((len (1+ (length syntax)))
	 (vec (make-vector len nil))
	 (cnt 0) (thr (+ mew-syntax-magic (1- n))))
    (while (< cnt thr)
      (aset vec cnt (aref syntax cnt))
      (setq cnt (1+ cnt)))
    (aset vec cnt entry)
    (setq cnt (1+ cnt))
    (while (< cnt len)
      (aset vec cnt (aref syntax (1- cnt)))
      (setq cnt (1+ cnt)))
    vec)) ;; return value

(defun mew-syntax-remove-entry (syntax nums)
  (let* ((root syntax)
	 grand parent
	 (nl (length nums))
	 (rev (reverse nums))
	 (n0 (nth 0 rev))
	 (n1 (nth 1 rev))
	 (ns (reverse (nthcdr 2 rev))))
    (cond
     ((= nl 1)
      (setq parent root)
      (mew-syntax-cut-entry parent n0))
     (t
      (if (= nl 2)
	  (setq grand root)
	(setq grand (mew-syntax-get-entry root ns)))
      (setq parent (mew-syntax-get-entry grand (list n1)))
      (setq parent (mew-syntax-cut-entry parent n0))
      (aset grand (+ mew-syntax-magic (1- n1)) parent)
      root))))

(defun mew-syntax-cut-entry (syntax n)
  "Must not use in functions other than mew-syntax-remove-entry"
  (let* ((len (1- (length syntax)))
	 (vec (make-vector len nil))
	 (cnt 0) (thr (+ mew-syntax-magic (1- n))))
    (while (< cnt thr)
      (aset vec cnt (aref syntax cnt))
      (setq cnt (1+ cnt)))
    (while (< cnt len)
      (aset vec cnt (aref syntax (1+ cnt)))
      (setq cnt (1+ cnt)))
    vec)) ;; return value

(defun mew-syntax-get-entry-by-cid (syntax cid)
  (cond
   ((and (stringp (mew-syntax-get-cid syntax))
	 (string= (mew-syntax-get-cid syntax) cid))
    syntax)
   ((mew-syntax-message-p syntax)
    (mew-syntax-get-entry-by-cid (mew-syntax-get-part syntax) cid))
   ((mew-syntax-multipart-p syntax)
    (let ((i mew-syntax-magic)
	  (len (length syntax))
	  entry)
      (catch 'multi
	(while (< i len)
	  ;; xxx aref (see mew-summary-burst-body)
	  (setq entry (mew-syntax-get-entry-by-cid (aref syntax i) cid))
	  (if entry (throw 'multi entry))
	  (setq i (1+ i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

(defun mew-summary-goto-part (msg part)
  (let (here)
    (save-excursion
      (goto-char (point-min))
      (when (and (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
		 (re-search-forward (mew-regex-jmp-part part) nil t))
	(beginning-of-line)
	(setq here (point))))
    (and here (goto-char here))))

(defun mew-summary-goto-message ()
  (if (eobp) (forward-line -1))
  (when (mew-in-decode-syntax-p)
    (goto-char (mew-decode-syntax-begin))
    (forward-line -1))
  (beginning-of-line))

(defun mew-syntax-number ()
  (if (or (mew-in-attach-p)
	  (mew-in-decode-syntax-p))
      (save-excursion
	(beginning-of-line)
	(if (looking-at mew-regex-part) (mew-sumsyn-part)))))

(defun mew-syntax-number-to-nums (strnum)
  (if strnum
      (mapcar 'string-to-number (mew-split strnum ?.))
    nil))

(defun mew-syntax-nums ()
  (mew-syntax-number-to-nums (mew-syntax-number)))

(defun mew-summary-goto-mark ()
  (beginning-of-line))

(defun mew-summary-goto-body (&optional after)
  (when mew-summary-form-body-starter
    (beginning-of-line)
    (let ((beg (point)))
      (forward-line)
      (if (search-backward "\r" nil t)
	  (if (search-backward mew-summary-form-body-starter beg t)
	      (progn
		(if after (goto-char (match-end 0)))
		t)
	    nil)
	(forward-line -1) ;; just in case
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode syntax
;;;

(defvar mew-use-cut-file-name t)

(defun mew-summary-info (arg)
  "Display the folder name of this message, its number, its size
and its unique id. If this is a cache message, both the unique
id and the size are also shown.  If called with
'\\[universal-argument]', its Message-ID: and its parent's
Message-ID: are displayed instead.  If 'mew-use-cut-file-name' is
non-nil, the file path is pushed to the cut buffer of your window
system."
  (interactive "P")
  (mew-summary-msg
   (when (mew-sumsyn-match mew-regex-sumsyn-long)
     (if arg
	 (message "my-id=%s par-id=%s"
		  (mew-sumsyn-my-id)
		  (mew-sumsyn-parent-id))
       (let* ((fld (mew-sumsyn-folder-name))
	      (msg (mew-sumsyn-message-number))
	      (size (mew-sumsyn-message-size))
	      (uid (mew-sumsyn-message-uid))
	      (file (concat (file-name-as-directory fld) msg))
	      (path (mew-expand-folder file))
	      (rpath (mew-msg-get-filename path)))
	 (if (mew-virtual-p)
	     (setq file (mew-folder-path-to-folder file 'has-proto)))
	 (message "%s size=%s uid=%s" file size uid)
	 (when mew-use-cut-file-name
	   (kill-new rpath)))))))

(defun mew-sumsyn-match (regex)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (when (search-backward "\r" start t)
	(looking-at regex)))))

(defun mew-sumsyn-folder-name ()
  (let ((fn (mew-match-string 1))
	(bn (buffer-name)))
    (cond
     ((not (string= fn "")) (mew-sumsym-decode-folder fn))
     ((mew-virtual-for-one-summary)
      (mew-vinfo-get-physical-folder))
     (t bn))))

(defun mew-sumsyn-message-number () (mew-match-string 2))
(defun mew-sumsyn-my-id          () (mew-match-string 3))
(defun mew-sumsyn-parent-id      () (mew-match-string 4))
(defun mew-sumsyn-message-uid    () (mew-match-string 5))
(defun mew-sumsyn-message-size   () (mew-match-string 6))

(defun mew-sumsyn-part () (mew-match-string 1))

(defun mew-summary-folder-name (&optional ext)
  (cond
   ((or ext (mew-summary-p))
    (buffer-name))
   (t ;; Virtual mode
    (save-excursion
      (mew-summary-goto-message)
      (when (mew-sumsyn-match mew-regex-sumsyn-short)
	(mew-sumsyn-folder-name))))))

(defun mew-summary-message-number ()
  (unless (mew-in-decode-syntax-p)
    (when (mew-sumsyn-match mew-regex-sumsyn-short)
      (mew-sumsyn-message-number))))

(defun mew-summary-message-number2 ()
  (save-excursion
    (mew-summary-goto-message)
    (when (mew-sumsyn-match mew-regex-sumsyn-short)
      (mew-sumsyn-message-number))))

(defun mew-summary-my-id ()
  (when (mew-sumsyn-match mew-regex-sumsyn-long)
    (mew-sumsyn-my-id)))

(defun mew-summary-parent-id ()
  (when (mew-sumsyn-match mew-regex-sumsyn-long)
    (mew-sumsyn-parent-id)))

(defun mew-summary-message-uid ()
  (when (mew-sumsyn-match mew-regex-sumsyn-long)
    (mew-sumsyn-message-uid)))

(defun mew-summary-message-size ()
  (when (mew-sumsyn-match mew-regex-sumsyn-long)
    (mew-sumsyn-message-size)))

(defun mew-syntax-change-message-number (msg)
  (save-excursion
    (when (mew-sumsyn-match mew-regex-sumsyn-short)
      (mew-syntax-change-message-number2 msg))))

;; see also mew-sumsyn-message-number
(defun mew-syntax-change-message-number2 (msg)
  (delete-region (match-beginning 2) (match-end 2))
  (goto-char (match-beginning 2))
  (insert-and-inherit msg))

(defun mew-syntax-change-parent-id (id)
  (save-excursion
    (when (mew-sumsyn-match mew-regex-sumsyn-long)
      (delete-region (match-beginning 4) (match-end 4))
      (goto-char (match-beginning 4))
      (insert-and-inherit id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mew-encode-syntax
;;

(defun mew-encode-syntax-single (file &optional ctl cte cd cid cdp
				      privacy decrypters)
  ;; cid is just for beauty
  ;; if cdp is non-nil, set cdp from file.
  ;; if cdp is a string, set cdp from cdp as a file.
  (let* (ct ctdb)
    (cond
     (ctl
      (setq ct (car ctl))
      (setq ctdb (mew-ctdb-by-ct ct)))
     (t
      (setq ctdb (mew-ctdb-by-file file))
      (setq ct (mew-ctdb-ct ctdb))
      (unless ct
	(setq ct (mew-content-type (mew-tinfo-get-case)))
	(setq ctdb (mew-ctdb-by-ct ct)))
      (setq ctl (list ct))))
    (or cte (setq cte (mew-ctdb-cte ctdb)))
    (if cdp (setq cdp (mew-syntax-cdp-format ct file cdp)))
    (vconcat [single] (list file decrypters privacy ctl cte cd cid cdp))))

(defun mew-encode-syntax-multi (dir ct)
  (unless (string-match (concat mew-path-separator "$") dir)
    (setq dir (file-name-as-directory dir)))
  (vconcat [multi] (list dir) [nil nil] (list ct) [nil nil nil nil]))

(defun mew-encode-syntax-text ()
  (mew-encode-syntax-single mew-draft-coverpage (list mew-ct-txt)))

(defun mew-encode-syntax-initial (dir)
  (vconcat
   (mew-encode-syntax-multi dir mew-type-mlm)
   ;; ensure to guess charset ....
   (list (mew-encode-syntax-text))))

(defun mew-encode-syntax-initial-multi (dir n)
  (let ((i 1) file ret)
    (while (<= i n)
      (setq file (mew-msg-new-filename (number-to-string i)))
      (setq ret (vconcat ret (list (mew-encode-syntax-single
				    file nil nil nil nil 'cdp))))
      (setq i (1+ i)))
    (vconcat (mew-encode-syntax-multi dir mew-type-mlm)
	     (list (mew-encode-syntax-single mew-draft-coverpage
					     (list mew-ct-txt)))
	     ret)))

(defconst mew-encode-syntax-dot
  [nil "." nil nil ("") nil nil nil nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mew-decode-syntax
;;

;; MUST generate a new vector

(defun mew-decode-syntax-rfc822 (&optional msg-head multi)
  ;; msg-head may include CD:
  (unless msg-head (setq msg-head (mew-decode-syntax-rfc822-head t)))
  (if (not multi)
      (vconcat msg-head (vector (mew-decode-syntax-text)))
    (let ((body (mew-decode-syntax-text))
	  (head (mew-decode-syntax-multi-head)))
      (mew-syntax-set-begin head (mew-syntax-get-begin body))
      (mew-syntax-set-end head (mew-syntax-get-end body))
      (vconcat msg-head (vector (mew-syntax-cat head body))))))

(defun mew-decode-syntax-rfc822-head (&optional reg-hend)
  (vector 'message (point-min)
	  (and reg-hend
	       (save-excursion (forward-line -1) (beginning-of-line) (point)))
	  nil mew-type-msg nil nil nil nil))

(defun mew-decode-syntax-text ()
  (vector 'single (point) (point-max) nil mew-type-txt nil nil nil nil))

(defun mew-decode-syntax-multi-head ()
  (vector 'multi nil nil nil mew-type-mlm nil nil nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; syntax printer
;;

(defconst mew-draft-attach-boundary-beg
  "------------------------------ attachments ------------------------------")
(defconst mew-draft-attach-boundary-end
  "--------0-1-2-3-4-5-6-7-8-9----------------------------------------------")

(defun mew-encode-syntax-delete (&optional all)
  (mew-elet
   (let (end)
     (goto-char (point-max))
     (when (re-search-backward mew-regex-attach-end nil t)
       (setq end (point))
       (when (re-search-backward mew-regex-attach-beg nil t)
	 (cond
	  (all
	   (beginning-of-line)
	   (backward-char)
	   (delete-region (point) (point-max)))
	  (t
	   (forward-line)
	   (delete-region (point) end))))))))

(defun mew-encode-syntax-print (syntax)
  (mew-elet
   (let ((nums (mew-syntax-nums)))
     (when (mew-attach-p)
       (mew-encode-syntax-delete)
       (mew-xinfo-set-multi-form nil)
       (mew-xinfo-set-icon-spec nil)
       (mew-syntax-multipart syntax nil nil 'mew-draft-button)
       (mapc 'insert-and-inherit (nreverse (mew-xinfo-get-multi-form)))
       (put-text-property (mew-attach-begin) (point-max) 'read-only t)
       (mew-front-nonsticky (mew-attach-begin) (1+ (mew-attach-begin)))
       (mew-rear-sticky (1- (point-max)) (point-max))
       (mew-attach-goto-number 'here nums)))))

;;
;;
;;

(defun mew-decode-syntax-insert (line)
  (let ((beg (point)))
    (insert line)
    (when (and mew-use-highlight-mouse-line window-system)
      (put-text-property
       beg (1- (point)) 'mouse-face mew-highlight-mouse-line-face))))

(defun mew-decode-syntax-print (sumbuf syntax form icon)
  (let* ((stx (mew-syntax-get-part syntax)))
    (with-current-buffer sumbuf
      (when (mew-syntax-multipart-p stx)
	(forward-line)
	(mew-elet
	 (let ((pos (point)))
	   (mew-decode-syntax-begin-set)
	   (mapc 'mew-decode-syntax-insert form)
	   (if (= pos (point))
	       ;; Nothing was printed.
	       (mew-decode-syntax-remove)
	     (mew-decode-syntax-end-set)))
	 (forward-line -1)
	 (mew-summary-goto-message)
	 (set-buffer-modified-p nil))))))

;;
;;
;;

(defun mew-decode-syntax-clear ()
  (mew-xinfo-clear))

(defun mew-decode-syntax-copy (cache)
  (setq mew-decode-syntax (mew-cache-decode-syntax cache))
  (mew-xinfo-copy cache))

(defun mew-decode-syntax-set ()
  ;; cache buffer
  (let ((part (mew-syntax-get-part mew-decode-syntax)))
    (mew-decode-syntax-arrange-warning)
    (if (mew-syntax-multipart-p part)
	(progn
	  (mew-syntax-multipart part 'decoding nil 'mew-summary-button 'body)
	  (mew-xinfo-set-multi-form (nreverse (mew-xinfo-get-multi-form)))
	  (mew-xinfo-set-icon-spec (nreverse (mew-xinfo-get-icon-spec))))
      (mew-decode-syntax-set-privacy part "body"))))

(defun mew-syntax-multipart (syntax dec part func &optional body)
  (let* ((ct (mew-syntax-get-value (mew-syntax-get-ct syntax) 'cap))
	 (cnt mew-syntax-magic)
	 (num 1)
	 (len (length syntax))
	 (mew-inherit-alt (string= mew-ct-mla ct))
	 strpart subsyntax)
    ;; multipart itself is displayed only when encoding.
    (if dec
	(mew-decode-syntax-set-privacy
	 syntax
	 (concat (if part (concat part " "))
		 (if body "body ")
		 "multi"))
      (mew-syntax-format syntax part dec))
    (while (< cnt len)
      (if part
	  (setq strpart (concat part "." (number-to-string num)))
	(setq strpart (number-to-string num)))
      (setq subsyntax (aref syntax cnt))
      (cond
       ((mew-syntax-multipart-p subsyntax)
	(mew-syntax-multipart subsyntax dec strpart func nil))
       ((mew-syntax-message-p subsyntax)
	(mew-syntax-message subsyntax dec strpart func))
       ((mew-syntax-singlepart-p subsyntax)
	(mew-syntax-singlepart subsyntax dec strpart func
			       (and body (= cnt mew-syntax-magic)))))
      (setq cnt (1+ cnt))
      (setq num (1+ num)))
    (unless dec
      (if part
	  (setq part (concat part "." (number-to-string num)))
	(setq part (number-to-string num)))
      (mew-syntax-format mew-encode-syntax-dot part dec))))

(defun mew-syntax-singlepart (syntax dec part func first)
  ;; part is valid only when called by mew-syntax-multipart.
  (let ((ct (mew-syntax-get-value (mew-syntax-get-ct syntax) 'cap)))
    ;; see also mew-mime-message/rfc822.
    (if (and dec
	     ;; the first singlepart in multipart under message if t
	     ;; the first singlepart under message if 'body
	     first
	     ;; CT: is text/plain but not attached file.
	     mew-use-text-body
	     (mew-ct-textp ct))
	() ;; skip displaying.
      ;; reach here only when called by mew-syntax-multipart.
      (mew-syntax-format syntax part dec))
    (if dec (mew-decode-syntax-set-privacy
	     syntax
	     (if (eq first 'body)
		 (if part (concat part " body") "body")
	       part)))))

(defun mew-syntax-message (syntax dec part func)
  (let ((subsyntax (mew-syntax-get-part syntax)))
    (mew-syntax-format syntax part dec)
    (if dec (mew-decode-syntax-set-privacy
	     syntax
	     (format "%s message" part)))
    (cond
     ((mew-syntax-multipart-p subsyntax)
      (mew-syntax-multipart subsyntax dec part func 'body))
     ((mew-syntax-message-p subsyntax)
      ) ;; never happens
     ((mew-syntax-singlepart-p subsyntax)
      ;; text/plain only
      (mew-syntax-singlepart subsyntax dec part func 'body)))))

;012345678901234567890123456789012345678901234567890123456789012345678901234567
;<4>snss<27-2                   >ss<24+2                    >ss<16            >

(defun mew-syntax-format (syntax number dec)
  (let* ((file (if (not dec) (mew-syntax-get-file syntax)))
	 (ctl (mew-syntax-get-ct syntax))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (char (mew-syntax-get-param ctl "charset"))
	 (ichar (mew-syntax-get-param ctl "icharset"))
	 (cte (mew-syntax-get-cte syntax)) ;; cte may be nil
	 (cd (mew-syntax-get-cd syntax))
	 (cdpl (mew-syntax-get-cdp syntax))
	 (filename (mew-syntax-get-filename cdpl ctl))
	 (decrypters-list (mew-syntax-get-decrypters syntax))
	 (decrypters (and (not dec) decrypters-list
			  (mew-join "," decrypters-list)))
	 (cd-or-dec cd)
	 (privacy (mew-syntax-get-privacy syntax))
	 (space " ") (LT (- (max mew-window-magic (window-width)) 2))
	 (ln (length number))
	 (lm 4) (lt 27)
	 (ld (* (/ (- LT lm lt) 5) 3))
	 (lf (- LT lm ln lt ld 8))
	 (marks (make-string lm mew-sp))
	 (i 0) (N (length privacy))
	 ctm ctp)

    (run-hooks 'mew-syntax-format-hook)

    (if (mew-ct-textp ct)
	(if char
	    (setq ct (concat ct "(" char ")"))
	  (if dec
	      (setq ct (concat ct "(" mew-us-ascii ")"))
	    (if ichar
		(setq ct (concat ct "(*" ichar ")"))
	      (setq ct (concat ct "(guess)"))))))
    (if (and dec mew-inherit-alt) (setq ct (concat ct "*")))
    (setq ct (mew-substring ct lt 'cnt))

    (if (null privacy)
	(when cte
	  (setq cte (downcase cte))
	  (cond
	   ((or (string= cte mew-7bit)
		(string= cte mew-8bit)
		(string= cte mew-bin))
	    ;; no mark
	    )
	   ((string= cte mew-b64) (aset marks 0 ?B))
	   ((string= cte mew-qp)  (aset marks 0 ?Q))
	   ((string= cte mew-xg)  (aset marks 0 ?G))
	   (t                     (aset marks 0 ??))))

      (if dec (setq privacy (reverse privacy)))
      (while (< i N)
	(mew-set '(ctm ctp) (nth i privacy))
	(let ((case-fold-search t))
	  (cond
	   ((string-match "pgp"   ctp) (aset marks (* i 2) ?P))
	   ((string-match "pkcs"  ctm) (aset marks (* i 2) ?S))
	   ((string-match "moss"  ctp) (aset marks (* i 2) ?M)))
	  (cond
	   ((mew-case-equal mew-ct-mle ctm) (aset marks (1+ (* i 2)) ?E))
	   ((mew-case-equal mew-ct-mls ctm) (aset marks (1+ (* i 2)) ?S))
	   ((mew-case-equal "enveloped-data" ctp) (aset marks (1+ (* i 2)) ?E))
	   ((mew-case-equal "signed-data" ctp) (aset marks (1+ (* i 2)) ?S)))
	  (setq i (1+ i)))))

    (if (and (not dec) decrypters) (setq cd-or-dec decrypters))
    (setq cd-or-dec (mew-substring cd-or-dec ld 'cnt))

    (cond
     (filename
      (setq file filename))
     ((and file (not (string= file ".")) (not (string-match "/$" file)))
      (setq file (concat "*" file))))
    (setq file (mew-substring file lf 'cnt))

    (mew-xinfo-set-multi-form
     (cons (concat
	    marks
	    (if number (concat space number))
	    space space
	    ct
	    space space
	    cd-or-dec
	    space space
	    file
	    "\n")
	   (mew-xinfo-get-multi-form)))))

(defun mew-decode-syntax-buffer ()
  (marker-buffer mew-marker-decode-syntax-begin))

(defun mew-decode-syntax-delete ()
  (when (mew-decode-syntax-p)
    (with-current-buffer (mew-decode-syntax-buffer)
      (mew-xinfo-set-icon-spec nil)
      (mew-summary-toolbar-update)
      (save-excursion
	(mew-elet
	 (delete-region (mew-decode-syntax-begin) (mew-decode-syntax-end))))
      (mew-decode-syntax-remove)
      (mew-highlight-cursor-line)
      (set-buffer-modified-p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; decode privacy
;;

(defun mew-decode-syntax-set-privacy (syntax label)
  (let (results)
    (dolist (privacy (mew-syntax-get-privacy syntax))
      (setq results (concat results (nth 2 privacy))))
    (when results
      (mew-xinfo-set-pri-result
       (concat (mew-xinfo-get-pri-result)
	       mew-x-mew: (format " <%s> " label) results "\n")))))

(defun mew-decode-syntax-insert-privacy ()
  (when (mew-xinfo-get-pri-result)
    (let ((beg (point)))
      (insert (mew-xinfo-get-pri-result))
      (mew-decode-header-property-region beg (point))
      (save-excursion
	(save-restriction
	  (narrow-to-region beg (point))
	  (goto-char (point-min))
	  (while (re-search-forward "BAD.*sign" nil t)
	    (put-text-property
	     (match-beginning 0)
	     (match-end 0)
	     'face 'mew-face-header-xmew-bad)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Broken messages
;;

(defun mew-decode-syntax-arrange-warning ()
  (when (mew-xinfo-get-warning)
    (mew-xinfo-set-warning
     (mew-uniq-list (sort (mew-xinfo-get-warning) 'string<)))))

(defun mew-decode-syntax-insert-warning ()
  (when (or (mew-xinfo-get-warning) (mew-xinfo-get-info))
    (let ((beg (point))
	  (pref (make-string (1+ (length mew-x-mew:)) mew-sp)))
      (when (mew-xinfo-get-warning)
	(insert mew-x-mew: " "
		(mapconcat 'identity (mew-xinfo-get-warning) pref)))
      (when (mew-xinfo-get-info)
	(insert mew-x-mew: " "
		(mapconcat 'identity (mew-xinfo-get-info) pref)))
      (mew-decode-header-property-region beg (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; markers
;;

(defvar mew-marker-decode-syntax-begin (make-marker))
(defvar mew-marker-decode-syntax-end (make-marker))

;; location

(defun mew-in-decode-syntax-p ()
  (and (marker-position mew-marker-decode-syntax-begin)
       (marker-position mew-marker-decode-syntax-end)
       (eq (current-buffer) (marker-buffer mew-marker-decode-syntax-begin))
       (>= (point) (marker-position mew-marker-decode-syntax-begin))
       (<  (point) (marker-position mew-marker-decode-syntax-end))))

(defun mew-in-header-p ()
  (let ((end (mew-header-end)))
    (and end (<= (point) end))))

(defun mew-in-attach-p ()
  (let ((beg (mew-attach-begin)))
    (and beg (> (point) beg)))) ;; excluding the preceding \n

;; existence

(defun mew-decode-syntax-p ()
  (and (marker-position mew-marker-decode-syntax-begin)
       (marker-position mew-marker-decode-syntax-end)))

(defun mew-msghdr-p ()
  (save-restriction
    (widen)
    (next-single-property-change (point-min) 'read-only)))

(defun mew-attach-p ()
  (if (/= (point-max) 1)
      (get-text-property (1- (point-max)) 'mew-attach-end)))

(defun mew-encode-syntax-have-one-part ()
  (= (length mew-encode-syntax) (1+ mew-syntax-magic)))

;; point

(defun mew-decode-syntax-begin ()
  (marker-position mew-marker-decode-syntax-begin))

(defun mew-decode-syntax-end ()
  (marker-position mew-marker-decode-syntax-end))

(defun mew-header-end ()
  (mew-msghdr-p))

(defun mew-attach-begin ()
  (if (mew-attach-p)
      (let ((beg (previous-single-property-change
		  (point-max) 'mew-attach-begin)))
	(if beg (1- beg) nil))))

;;

(defun mew-decode-syntax-begin-set ()
  (set-marker mew-marker-decode-syntax-begin (point)))

(defun mew-decode-syntax-end-set ()
  (set-marker mew-marker-decode-syntax-end (point)))

(defun mew-decode-syntax-remove ()
  (set-marker mew-marker-decode-syntax-begin nil)
  (set-marker mew-marker-decode-syntax-end nil))

(defun mew-summary-end-of-message-p ()
  (let (pos beg end)
    (with-current-buffer (mew-decode-syntax-buffer)
      (save-excursion
	(setq pos (point))
	(setq end (mew-decode-syntax-end))
	(goto-char end)
	(forward-line -1)
	(beginning-of-line)
	(setq beg (point))
	(and (<= beg pos) (< pos end))))))

;;

(defun mew-header-set (sep)
  (mew-elet
   (let ((pos (point)))
     (if sep
	 (progn
	   (insert-before-markers sep) ;; for mew-summary-reply
	   (unless (bolp) (forward-line)))
       (forward-line))
     (put-text-property pos (point) 'read-only t)
     (mew-front-nonsticky pos (1+ pos))
     (if (mew-header-p)
	 (mew-rear-sticky (1- (point)) (point))
       (mew-rear-nonsticky (1- (point)) (point)))
     pos)))

(defun mew-header-clear (&optional keep-read-only)
  ;; the cursor moves to the end of the header (with some exceptions)
  (mew-elet
   (mew-header-goto-end) ;; do not use mew-header-end
   (let ((pos (point)))
     (forward-line)
     ;; If the body contains the read-only property, mew-in-header-p()
     ;; makes a mistake. So, remove the read-only property from
     ;; the entire buffer.
     (unless keep-read-only
       (put-text-property (point) (point-max) 'read-only nil))
     (delete-region pos (point)))))

;;

(defun mew-attach-set ()
  (mew-elet
   (let (beg)
     (goto-char (point-max))
     (unless (bolp) (insert "\n"))
     (setq beg (point))
     (insert "\n")
     (insert mew-draft-attach-boundary-beg)
     (insert "\n")
     (insert mew-draft-attach-boundary-end)
     (insert "\n")
     (put-text-property beg (1+ beg) 'mew-attach-begin t)
     (put-text-property (1- (point)) (point) 'mew-attach-end t)
     (beginning-of-line)
     (mew-draft-attach-keymap))))

(defun mew-attach-clear ()
  (when (mew-attach-p)
    (save-excursion
      (mew-elet
       (delete-region (mew-attach-begin) (point-max)))
      (mew-overlay-delete (mew-tinfo-get-attach-keymap))
      (mew-tinfo-set-attach-keymap nil))))

(defun mew-header-prepared ()
  (mew-header-set (concat mew-header-separator "\n"))
  (mew-highlight-header)
  (unless (mew-header-p) (mew-draft-header-keymap)))

(defun mew-draft-header-keymap ()
  (save-excursion
    (if (overlayp (mew-tinfo-get-header-keymap))
	(move-overlay
	 (mew-tinfo-get-header-keymap) (point-min) (1+ (mew-header-end)))
      (mew-tinfo-set-header-keymap
       (mew-overlay-make (point-min) (1+ (mew-header-end))))
      (overlay-put
       (mew-tinfo-get-header-keymap) 'local-map mew-draft-header-map))))

(defun mew-draft-attach-keymap ()
  (if (overlayp (mew-tinfo-get-attach-keymap))
      (move-overlay
       (mew-tinfo-get-attach-keymap) (1+ (mew-attach-begin)) (point-max))
    (mew-tinfo-set-attach-keymap
     (mew-overlay-make (1+ (mew-attach-begin)) (point-max)))
    (overlay-put
     (mew-tinfo-get-attach-keymap) 'local-map mew-draft-attach-map)))

(provide 'mew-syntax)

;;; Copyright Notice:

;; Copyright (C) 1996-2014 Mew developing team.
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

;;; mew-syntax.el ends here
