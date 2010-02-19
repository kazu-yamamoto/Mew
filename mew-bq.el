;;; mew-bq.el --- Base64 and Quoted-Printable encoding for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug 20, 1997

;;; Code:

(require 'mew)

(defvar mew-header-encode-switch
  '(("B" . mew-base64-encode-string)
    ("Q" . mew-q-encode-string)))

(defun mew-header-encode-get-func (b-or-q)
  (cdr (mew-assoc-case-equal b-or-q mew-header-encode-switch 0)))


(defvar mew-header-decode-switch
  `(("B"
     mew-base64-decode-string
     mew-base64-decode-error-string
     ,mew-error-invalid-b-encoding)
    ("Q"
     mew-q-decode-string
     nil
     ,mew-error-invalid-q-encoding)))

(defun mew-header-decode-get-func (b-or-q)
  (nth 1 (mew-assoc-case-equal b-or-q mew-header-decode-switch 0)))

(defun mew-header-decode-get-error-func (b-or-q)
  (nth 2 (mew-assoc-case-equal b-or-q mew-header-decode-switch 0)))

(defun mew-header-decode-get-error-str (b-or-q)
  (nth 3 (mew-assoc-case-equal b-or-q mew-header-decode-switch 0)))

(defconst mew-header-decode-regex
  "=\\?\\([^? \t]+\\)\\?\\(.\\)\\?\\([^? \t]+\\)\\?=")

(defconst mew-header-decode-regex2
  "[ \t]*=\\?\\([^? \t]+\\)\\?\\(.\\)\\?\\([^? \t]+\\)\\?=")

(defconst mew-header-decode-regex3
  "^\\([ \t]+\\)=\\?[^? \t]+\\?.\\?[^? \t]+\\?=")

(defconst mew-header-param-regex
  "^\\([^=*]+\\)\\(\\|\\*[0-9]+\\)\\(\\*?\\)=\\(.*\\)$")

;;;
;;;
;;;

(defun mew-header-sanity-check-string (str)
  (if (null str)
      str
    (let ((regex mew-regex-ctls))
      (while (string-match regex str)
	(setq str (replace-match "" nil t str)))
      str)))

(defun mew-header-sanity-check-string2 (str key)
  (if (null str)
      str
    (let ((regex mew-regex-ctls-wo-tab))
      (if (not (string-match regex str))
	  str
	(mew-decode-warning-fields key 'ctl)
	(setq str (replace-match "" nil t str))
	(while (string-match regex str)
	  (setq str (replace-match "" nil t str)))
	str))))

(defun mew-header-sanity-check-region (beg end &optional key)
  (let ((regex mew-regex-ctls-wo-tab-lf))
    ;; meaningless tabs are removed by mew-header-decode-region
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (when (re-search-forward regex nil t)
	(if key (mew-decode-warning-fields key 'ctl))
	(replace-match "" nil t)
	(while (re-search-forward regex nil t)
	  (replace-match "" nil t))))))

(defun mew-header-encode (charset b-or-q fun hcs str)
  ;; sanity check should be done
  (let* ((estr (mew-cs-encode-string str hcs)))
    (concat "=?" charset "?" b-or-q "?" (funcall fun estr) "?=")))

(defun mew-header-decode (charset b-or-q estr &optional key)
  (let* ((fun (mew-header-decode-get-func b-or-q))
	 (cs (mew-charset-to-cs charset))
	 str)
    (when (and mew-use-autoconv-when-unknown
	       (not (mew-coding-system-p cs)))
      (setq cs mew-cs-autoconv))
    (cond
     ((not (mew-coding-system-p cs))
      mew-error-unknown-charset)
     (fun ;; if cs is nil, mew-cs-decode-string does not cs-decode.
      (setq str (funcall fun estr))
      (unless str (setq str (mew-header-decode-error key b-or-q estr)))
      (if (null str)
	  (mew-header-decode-get-error-str b-or-q)
	(when (and mew-decode-broken
		   (not (eq cs mew-cs-autoconv))
		   (or (null charset)
		       (mew-case-equal charset mew-us-ascii)
		       (mew-case-equal charset mew-utf-8))
		   (string-match mew-regex-singlebyte-nonascii str))
	  (setq cs mew-cs-autoconv))
	(if (null cs)
	    str
	  (mew-cs-decode-string str cs))))
     (t estr))))

(defun mew-header-decode-error (key b-or-q estr)
  (let ((func (mew-header-decode-get-error-func b-or-q)))
    (if func (funcall func key b-or-q estr))))

(defun mew-base64-decode-error-string (key b-or-q estr)
  (let ((str estr) strlen r ret)
    (when (string-match "^\\([^=]*\\)=*$" str)
      (setq str (mew-match-string 1 str)))
    (setq strlen (length str))
    (setq r (% strlen 4))
    (cond
     ((= r 0)
      (setq ret (mew-base64-decode-string str)))
     ((= r 1)
      )
     ((= r 2)
      (setq ret (mew-base64-decode-string (concat str "=="))))
     ((= r 3)
      (setq ret (mew-base64-decode-string (concat str "=")))))
    (if ret (mew-decode-warning-fields key 'padding))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quoted-printable encoding
;;;

(defun mew-q-encode-string (str &optional prefix)
  (let* ((len (length str))
	 (ret (mew-make-string (* len 3)))
	 (j 0) char)
    (dotimes (i len)
      (setq char (aref str i))
      (cond
       ((char-equal char mew-sp)
	(aset ret j ?_))
       ((and (> char mew-sp)
	     (< char 126)
	     (not (char-equal char ?=))
	     (not (char-equal char ??))
	     (not (char-equal char ?_)) ;; space
	     (or (not prefix) (not (memq char '(?# ?@)))))
	(aset ret j char))
       (t
	(aset ret j (or prefix ?=))
	(setq j (1+ j))
	(aset ret j (aref "0123456789ABCDEF" (lsh char -4)))
	(setq j (1+ j))
	(aset ret j (aref "0123456789ABCDEF" (logand char 15)))))
      (setq j (1+ j)))
    (substring ret 0 j)))

(defun mew-samba-encoding (str)
  (setq str (mew-cs-encode-string str mew-cs-samba))
  (let* ((len (length str))
	 (ret (mew-make-string (* len 3)))
	 (j 0) char type)
    (cond
     ((eq mew-use-samba-encoding-type 'cap)
      (setq type 'cap))
     (t
      (setq type 'hex)))
    (dotimes (i len)
      (setq char (aref str i))
      (if (and (eq type 'cap) (< char 128))
	  (aset ret j char)
	(aset ret j ?:)
	(setq j (1+ j))
	(aset ret j (aref "0123456789abcdef" (lsh char -4)))
	(setq j (1+ j))
	(aset ret j (aref "0123456789abcdef" (logand char 15))))
      (setq j (1+ j)))
    (substring ret 0 j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quoted-printable decoding
;;;

(defun mew-hexchar-to-int (hex)
  ;; would return nil if error
  (cond
   ((and (<= ?0 hex) (<= hex ?9)) (- hex ?0))
   ((and (<= ?A hex) (<= hex ?F)) (+ (- hex ?A) 10))
   ((and (<= ?a hex) (<= hex ?f)) (+ (- hex ?a) 10))))

(defun mew-q-decode-string (qpstr &optional okey)
  (condition-case nil
      (let* ((len (length qpstr))
	     (ret (mew-make-string len))
	     (j 0) char key)
	(setq key (or okey ?=))
	(dotimes (i len)
	  (setq char (aref qpstr i))
	  (cond
	   ((and (not okey) (char-equal char ?_))
	    (aset ret j mew-sp))
	   ((char-equal char key)
	    (aset ret j (+ (* (mew-hexchar-to-int (aref qpstr (1+ i))) 16)
			   (mew-hexchar-to-int (aref qpstr (+ i 2)))))
	    (setq i (+ i 2)))
	   (t
	    (aset ret j char)))
	  (setq j (1+ j)))
	(substring ret 0 j))
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RFC 2047 encoding
;;

;; RFC2822 says that each line SHOULD be no more than 78 characters,
;; excluding the CRLF.
;; RFC 2047 says that encoded-word must be less than or equal to 75.
;; RFC 2047 says that each line which includes one or more encoded-words
;; must be less than or equal to 76.

(defvar mew-encode-word-max-length 75)
(defvar mew-field-max-length 76)

;; If possible, mew-header-encode-string should expect the length of
;; results and split 'str' before encoding so that every 'encoded-word'
;; fits in 75 length. However, it is very difficult first because
;; it is very difficult to know actual length of 'str' after convention
;; from the internal representation to charset encoding. Second because
;; ISO-2022-JP cannot be simply split. If split, extra escape sequences
;; appear. Moreover, it is not effective to expect the length of results
;; because 'str' is short enough in most cases. So, we measure the length
;; of results. If it is longer than 75, 'str' is split and 'substr1' and
;; 'substr2' are encoded.... Repeat this recursively but not so deeply.

(defun mew-header-encode-string (str &optional key-len)
  (let* ((ecsdb (or (mew-ecsdb-guess-string str)
		    (mew-charset-to-ecsdb (mew-charset-m17n))))
	 (hcs (mew-ecsdb-hcs ecsdb))
	 charset b-or-q fun)
    (if (not (mew-coding-system-p hcs))
	(mew-encode-error
	 (format "Unknown coding system %s in the header" (symbol-name hcs)))
      (setq charset (mew-cs-to-charset hcs))
      (setq b-or-q (mew-ecsdb-get-b-or-q ecsdb))
      (setq fun (mew-header-encode-get-func b-or-q))
      (if (null fun)
	  (mew-encode-error
	   (format "Unknown encoding function for %s" b-or-q))
	(mew-header-encode-string1 charset b-or-q fun hcs str key-len)))))

(defun mew-header-encode-string1 (charset b-or-q fun hcs str &optional key-len)
  (let* ((max mew-encode-word-max-length)
	 (encoded-word (mew-header-encode charset b-or-q fun hcs str)))
    (if key-len
	(setq max (- max key-len)))
    (if (> (length encoded-word) max)
        (let ((med (/ (length str) 2)))
          (append
           (mew-header-encode-string1
	    charset b-or-q fun hcs (substring str 0 med) key-len)
           (mew-header-encode-string1
	    charset b-or-q fun hcs (substring str med))))
      (list encoded-word))))

(defun mew-header-encode-split-string (str)
  "Split STR to need-to-encode string and non-encode-string."
  (let ((start 0) beg end ret)
    (while (string-match "\\(^\\|[ \t]+\\)[\t -~]+\\($\\|[ \t]+\\)" str start)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (if (= start beg)
	  (setq ret (cons (substring str beg end) ret))
	(setq ret (cons (substring str beg end)
			(cons (substring str start beg) ret))))
      (setq start end))
    (if (/= start (length str))
	(setq ret (cons (substring str start) ret)))
    (nreverse ret)))

(defun mew-header-encode-comma-text (str)
  (let ((str-list (mapcar 'mew-chop (mew-split str ?,))))
    (mew-header-encode-text (car str-list))
    (setq str-list (cdr str-list))
    (dolist (sl str-list)
      (insert ", ") ;; must be fold here
      (mew-header-encode-text sl))))

(defmacro mew-header-encode-cond (c)
  `(cond
    ((> ,c 127) ;; non-ascii
     (when (eq status 'space)
       (insert (substring str bound i))
       (setq bound i))
     (setq status 'non-ascii))
    ;; end of non-ascii
    (t ;; ascii
     (cond
      ((eq status 'space)
       (insert (substring str bound i)) ;; spaces
       (setq bound i)
       (setq status 'ascii))
      ((eq status 'ascii)
       (setq status 'ascii))
      ((eq status 'non-ascii)
       (setq status 'non-ascii))
      ((eq status 'non-ascii-space)
       (mew-header-encode-text (substring str bound SBOUND))
       ;; non-ascii
       (insert (substring str SBOUND i)) ;; spaces
       (setq bound i)
       (setq status 'ascii))))
    ;; end of ascii
    ))

(defmacro mew-header-encode-cond2 (opt)
  `(cond
    ((eq status 'ascii)
     (insert (substring str bound i)))
    ((eq status 'space)
     (insert (substring str bound i)))
    ((eq status 'non-ascii)
     (mew-header-encode-text (substring str bound i)) ,opt)
    ((eq status 'non-ascii-space)
     (mew-header-encode-text (substring str bound SBOUND) ,opt)
     (insert (substring str SBOUND i)))))

(defun mew-header-encode-addr (str)
  (let* ((len (length str))
	 (i 0) (bound 0) (status 'space)
	 SBOUND open c I)
    ;; status space, ascii, non-ascii, non-ascii-space
    ;; assumptions:
    ;;  <> does not contain non-ascii characters.
    ;;  () does not recurse.
    ;; if " " contains non-ascii, cause an error.
    (while (< i len)
      (setq c (aref str i))
      (cond
       ;; quote
       ((char-equal c ?\")
	(setq I (1+ i))
	(setq open t)
	(catch 'quote
	  (while (< I len)
	    (setq c (aref str I))
	    (cond
	     ((char-equal c ?\")
	      (setq open nil)
	      (throw 'quote nil))
	     ((> c 127)
	      (mew-encode-error
	       "Only ASCII is allowed in quoted-string in the header")))
	    (setq I (1+ I))))
	(if open
	    (mew-encode-error "Quote string must be closed in the header"))
	(mew-header-encode-cond ?a)
	(setq i I))
       ;; end of quote
       ;; comment
       ((char-equal c ?\()
	(mew-header-encode-cond2 nil)
	(insert "(")
	(setq i (1+ i))
	(setq bound i)
	(setq status 'ascii)
	(setq open t)
	(let (qp)
	  (catch 'comment
	    (while (< i len)
	      (setq c (aref str i))
	      (cond
	       ((char-equal c ?\))
		(setq open nil)
		(throw 'comment nil))
	       ((char-equal c ?\")
		(setq qp t))
	       ((> c 127)
		(setq status 'non-ascii)))
	      (setq i (1+ i))))
	  (if (and qp (eq status 'non-ascii))
	      (mew-encode-error
	       "Only ASCII is allowed in quoted-string in the header")))
	(if open
	    (mew-encode-error "Comment must be closed in the header"))
	(mew-header-encode-cond2 'comment)
	(if (= i len)
	    ()
	  (insert ")")
	  (setq bound (1+ i)))
	(setq status 'space))
       ;; end of ()
       ;; route
       ((char-equal c ?<)
	(mew-header-encode-cond2 nil)
	(if (or (char-equal (char-before (point)) mew-sp)
		(char-equal (char-before (point)) ?\t))
	    (insert "<")
	  (insert " <"))
	(setq i (1+ i))
	(setq bound i)
	(setq status 'ascii)
	(setq open t)
	(catch 'route
	  (while (< i len)
	    (setq c (aref str i))
	    (cond
	     ((char-equal c ?>)
	      (setq open nil)
	      (throw 'route nil))
	     ((> c 127)
	      (mew-encode-error "<> must contain ASCII only"))
	     (t
	      (insert c)))
	    (setq i (1+ i))))
	(if open
	    (mew-encode-error "<> must be closed in the header"))
	(if (= i len)
	    ()
	  (insert ">")
	  (setq bound (1+ i)))
	(setq status 'space))
       ((char-equal c ?>)
	(mew-encode-error "Unbalanced <> in the header"))
       ;; end of <>

       ;; group
       ;; 'mailbox' between ":" and ";" should be 'addr-spec',
       ;; not be 'phrase route-addr' in Mew because they are to be
       ;; removed.
       ((char-equal c ?:)
	(mew-header-encode-cond2 nil)
	;; RFC 2047 says:
	;;   An 'encoded-word' that appears within a
	;;   'phrase' MUST be separated from any adjacent 'word', 'text' or
	;;   'special' by 'linear-white-space'.
	;; However, we do not care if an 'encoded-word' is followed by
	;; ":". The spec is believed too strict. 'encoded-word' can be
	;; recognized because of the token rule.
	(insert ":")
	(setq i (1+ i))
	(unless (char-equal (aref str i) ?\;)
	  (mew-encode-error ": must be followed by ; in the header"))
	(insert ";")
	(setq bound (1+ i))
	(setq status 'space))
       ;; end of group

       ;; space
       ((or (char-equal c mew-sp) (char-equal c ?\t))
	(cond
	 ((or (eq status 'ascii) (eq status 'space))
	  (insert (substring str bound i)) ;; 'ascii
	  (setq bound i)
	  (setq status 'space))
	 ((eq status 'non-ascii)
	  (setq status 'non-ascii-space)
	  (setq SBOUND i))))
       ;; end of white space
       ;; comma
       ((char-equal c ?,)
	(mew-header-encode-cond2 nil)
	(insert ", ")
	(setq i (1+ i))
	(catch 'comma
	  (while (< i len)
	    (setq c (aref str i))
	    (if (or (char-equal c mew-sp) (char-equal c ?\t) (char-equal c ?\n))
		() ;; loop
	      (throw 'comma nil))
	    (setq i (1+ i))))
	;; get back to the end of white spaces
	(setq bound i)
	(setq c mew-sp)
	(setq i (1- i))
	(setq status 'space))
       ;; end of comma
       ;; the others
       (t (mew-header-encode-cond c)))
      ;; end of outside cond
      (setq i (1+ i)))
    ;; end of while
    (mew-header-encode-cond2 nil)))

(defun mew-header-encode-text (str &optional comment key-len)
  ;; 'comment' means that we are in RFC822 comment "(...)".
  (let ((str-list (mew-header-encode-split-string str))
	head-is-e e-list)
    (if (string-match "^[\t -~]+$" (car str-list))
	(progn
	  ;; ascii
	  (insert (car str-list))
	  (setq str-list (cdr str-list)))
      (setq head-is-e t))
    (while str-list
      ;; encoded-words
      (if (and key-len head-is-e)
	  (progn
	    (setq e-list (mew-header-encode-string (car str-list) key-len))
	    (setq head-is-e nil))
	(setq e-list (mew-header-encode-string (car str-list))))
      (insert (mapconcat 'identity e-list " "))
      ;; ascii
      (setq str-list (cdr str-list))
      (when (car str-list)
	(insert (car str-list))
	(setq str-list (cdr str-list))))))

(defun mew-header-fold-region (beg end med &optional use-tab)
  (let ((limit1 med) limit2)
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (not (eobp))
	(while (> (- (setq limit2 (save-excursion (end-of-line) (point)))
		     (point))
		  mew-field-max-length)
	  (forward-char mew-field-max-length)
	  (if (re-search-backward "[ \t]" limit1 t)
	      (progn
		(insert "\n")
		(if use-tab
		    (progn
		      (delete-char 1)
		      (insert "\t"))))
	    ;; Ugh!
	    (if (re-search-forward "[ \t]" limit2 t) ;; hold on anyway
		(progn
		  (backward-char)
		  (insert "\n")
		  (if use-tab
		      (progn
			(delete-char 1)
			(insert "\t"))))
	      (forward-line))) ;; give up this line
	  (setq limit1 (1+ (point))))
	(forward-line)
	(setq limit1 (1+ (point)))))))

(defun mew-header-resent-p (field)
  (let ((case-fold-search t))
    (string-match mew-resent-regex field)))

(defun mew-header-encode-region (beg end &optional resentp)
  (let (key med type str start last fast-path)
    (save-restriction
      (narrow-to-region beg end)
      (mew-header-sanity-check-region (point-min) (point-max))
      (mew-charset-sanity-check (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(setq start (point))
	(if (not (looking-at mew-keyval))
	    (forward-line)
	  (setq key (mew-match-string 1))
	  (setq med (match-end 0))
	  (setq type (mew-field-type-for-encoding key))
	  (setq fast-path t)
	  (if (eq type 'mailbox)
	      (if resentp
		  (if (mew-header-resent-p key)
		      (setq fast-path nil))
		(if (not (mew-header-resent-p key))
		    (setq fast-path nil))))
	  (forward-line)
	  (mew-header-goto-next)
	  (setq last (1- (point)))
	  (unless (= last med)
	    (if (and (string= (mew-charset-guess-region med last) mew-us-ascii)
		     fast-path)
		() ;; fast path
	      ;; if us-ascii AND mailbox, need to check syntax
	      (setq str (mew-buffer-substring med (1- (point))))
	      ;; excluding \n
	      (delete-region med (point))
	      (cond
	       ((eq type 'mailbox)
		(mew-header-encode-addr str))
	       ((eq type 'mime)
		(mew-header-encode-addr str))
	       ((eq type 'comma-text)
		(mew-header-encode-comma-text str))
	       ((eq type 'text)
		(mew-header-encode-text str nil (length key)))
	       ((eq type 'unstruct)
		(mew-header-encode-text str nil (length key))))
	      (insert "\n")) ;; previously deleted, so insert here
	    (mew-header-fold-region start (point) med)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RFC 2047 decoding
;;

(defun mew-header-unfold-region (type)
  (goto-char (point-min))
  (cond
   ((eq type 'struct)
    ;; If each line does not end with ",", unfold it.
    ;; In Page 5 of RFC822 says, "Unfolding is accomplished by
    ;; regarding CRLF immediately followed by a LWSP-char as
    ;; equivalent to the LWSP-char". However, it also says,
    ;; "In structured field bodies, multiple linear space ASCII
    ;; characters (namely HTABs and SPACEs) are treated as single
    ;; spaces and may freely surround any symbol." So, remove
    ;; continuous white spaces.
    (when (and mew-decode-broken (looking-at "\n[ \t]+"))
      (replace-match "" nil t))
    (goto-char (point-min))
    (while (re-search-forward ",[ \t]+\n" nil t)
      (replace-match ",\n" nil t))
    (goto-char (point-min))
    (while (re-search-forward "\\([^,]\\)[ \t]*\n[ \t]+" nil t)
      (replace-match "\\1 ")))
   ((eq type 'struct2) ;; for Authentication-Results:
    (while (re-search-forward "[ \t]*\n[ \t]+" nil t) (replace-match " "))
    (goto-char (point-min))
    (while (search-forward ";" nil t)
      (insert "\n")
      (if (looking-at "[\t ]+")
	  (replace-match "\t")
	(insert "\t"))))
   ((eq type 'text)
    ;; In Page 5 of RFC822 says, "Unfolding is accomplished by
    ;; regarding CRLF immediately followed by a LWSP-char as
    ;; equivalent to the LWSP-char".
    (while (re-search-forward "\n\\([ \t]\\)" nil t)
      (replace-match "\\1")))
   (t ;; unstruct
    )))

(defun mew-header-decode-string (key)
  (let ((beg (match-beginning 0))
	(end (match-end 0))
	(css (mew-header-decode (mew-match-string 1)
				(mew-match-string 2)
				(mew-match-string 3)
				key)))
    (setq css (mew-header-sanity-check-string2 css key))
    (delete-region beg end)
    (insert css)))

(defun mew-header-decode-in-quotedp ()
  (save-excursion
    (beginning-of-line)
    (char-equal (following-char) ?\")))

(defun mew-header-decode-in-addr-spec ()
  (eq (get-text-property (match-beginning 0) 'mew-addr-spec) t))

(defun mew-decode-warning-body (err msg)
  (mew-xinfo-set-warning
   (cons
    (cond
     ((eq err 'unknown-charset)
      (format "Charset (%s) for body is not supported.\n" msg))
     ((eq err 'no-charset)
      (if msg
	  (format "Charset (%s) for body is mis-matched.\n" msg)
	"Charset for body is not specified.\n"))
     ((eq err 'multi)
      (format "Invalid encoding (%s) for multipart.\n" msg)))
    (mew-xinfo-get-warning))))

(defun mew-decode-warning-fields (key err)
  (unless (member key mew-no-warning-fields)
    (let (wmsg level)
      (cond
       ((eq err 'spc)
	(setq level 1)
	(setq wmsg (concat "tab/spc characters on " key " are simplified.\n")))
       ((eq err 'ctl)
	(setq level 1)
	(setq wmsg (concat "some control code on " key " are removed.\n")))
       ((eq err 'raw)
	(setq level 2)
	(setq wmsg (concat key " has raw text strings.\n")))
       ((eq err 'padding)
	(setq level 2)
	(setq wmsg (concat key " has illegal padding in encoded text.\n")))
       ((eq err 'quoted)
	(setq level 2)
	(setq wmsg (concat key " has encoded-words in quoted text.\n"))))
      (if (>= level mew-warning-field-level)
	  (mew-xinfo-set-warning (cons wmsg (mew-xinfo-get-warning)))))))

(defun mew-decode-warning-params (param err)
  (unless (and param (member (downcase param) mew-no-warning-params))
    (mew-xinfo-set-warning
     (cons
      (cond
       ((eq err 'raw)
	(concat "The '" param "' parameter has raw text.\n"))
       ((eq err 'ctl)
	(concat "The '" param "' has invalid control codes.\n"))
       ((eq err 'enc)
	(concat "The '" param "' parameter has encoded-word.\n"))
       ((eq err 'char)
	(concat "The '" param "' has invalid characters.\n")))
      (mew-xinfo-get-warning)))))

(defun mew-header-decode-region (key rbeg rend &optional draftp)
  "RFC 2047 decoding. This is liberal on the one point from RFC 2047.
That is, each line may be more than 75."
  (setq key (capitalize key))
  (let* ((type (mew-field-type-for-decoding key)) (nl "\0"))
    (save-restriction
      (narrow-to-region rbeg rend)
      ;; Handling invalid raw text.
      (goto-char (point-min))
      (if draftp
	  (mew-cs-decode-region (point-min) (point-max) mew-cs-m17n)
	(when (and mew-decode-broken
		   (re-search-forward mew-regex-esc-or-nonascii nil t))
	  (mew-decode-warning-fields key 'raw)
	  (mew-cs-decode-region (point-min) (point-max) mew-cs-autoconv)))
      ;;
      (mew-header-unfold-region type)
      ;;
      (goto-char (point-min))
      (when (re-search-forward mew-header-decode-regex nil t)
	;; In Page 10 of RFC 2047 says, "When displaying a particular
	;; header field that contains multiple 'encoded-word's, any
	;; 'linear-white-space' that separates a pair of adjacent
	;; 'encoded-word's is ignored". So, use looking-at.
	(cond
	 ((memq type '(text comma-text))
	  ;; This is text field. We do not care quote!
	  ;; make use of the search above
	  (mew-header-decode-string key)
	  (while (looking-at mew-header-decode-regex2)
	    (mew-header-decode-string key))
	  ;;
	  (while (re-search-forward mew-header-decode-regex nil t)
	    (mew-header-decode-string key)
	    (while (looking-at mew-header-decode-regex2)
	      (mew-header-decode-string key))))
	 (t
	  ;; This is structured field.
	  ;; encoded-word in quoted-string should not be decoded
	  ;; according to RFC 2047. However, if users wish
	  ;; (ie mew-decode-broken is *non-nil*), decode it.
	  ;; Regular expression cannot express a quoted string in
	  ;; general case. So, canonicalize as follows:
	  ;;     Not quoted line (may be null line)
          ;;     "quoted line"
	  ;;     Not quoted line
          ;;     "quoted line"
          ;;     ...
	  ;; For this purpose, "\n" is converted to ^\@(\0).
	  (goto-char (point-min))
	  (while (re-search-forward "\n" nil t)
	    (replace-match nl nil t))
	  ;;
	  (goto-char (point-min))
	  (while (re-search-forward mew-regex-id nil t)
	    (put-text-property (match-beginning 0) (match-end 0)
			       'mew-addr-spec t))
	  ;;
	  (goto-char (point-min))
	  ;; Perfect regular expression for quoted strings.
	  ;; See "Mastering Regular Expressions", O'REILLY
	  (while (re-search-forward "\"\\([^\"\\\\]\\|\\\\.\\)*\"" nil t)
	    (goto-char (match-beginning 0))
	    (insert "\n")
	    (goto-char (1+ (match-end 0)))
	    (insert "\n"))
	  ;;
	  (goto-char (point-min)) ;; anyway
	  ;;
	  (while (re-search-forward mew-header-decode-regex nil t)
	    (cond
	     ((mew-header-decode-in-addr-spec)
	      ) ;; do nothing
	     ((mew-header-decode-in-quotedp)
	      (when mew-decode-broken
		(mew-decode-warning-fields key 'quoted)
		(mew-header-decode-string key)
		(while (looking-at mew-header-decode-regex2)
		  (mew-header-decode-string key))))
	     (t
	      (mew-header-decode-string key)
	      (while (looking-at mew-header-decode-regex2)
		(mew-header-decode-string key)))))
	  ;; Getting back to the original
	  (goto-char (point-min))
	  (while (re-search-forward "\n" nil t)
	    (replace-match "" nil t))
	  (goto-char (point-min))
	  (while (re-search-forward nl nil t)
	    (replace-match "\n" nil t))
	  (remove-text-properties (point-min) (point-max) '(mew-addr-spec t)))))
      ;;
      (if (and mew-decode-broken (member key mew-decode-ws-fields))
	  (let (ws)
	    ;; We want to warn only in the case of ^\t.
	    (goto-char (point-min))
	    ;; Because lines are already unfolded, we cannot tell
	    ;; whether or not this TABs locate on the beginning
	    ;; of a line. But this should be acceptable.
	    (while (re-search-forward "\t+" nil t)
	      (or ws (setq ws t))
	      (replace-match " " nil t))
	    (goto-char (point-min))
	    ;; Continuous SPCs are not invalid, just for appearance.
	    (while (re-search-forward "  +" nil t)
	      (or ws (setq ws t))
	      (replace-match " " nil t))
	    (if ws (mew-decode-warning-fields key 'spc))))
      ;;
      (mew-header-sanity-check-region (point-min) (point-max) key)
      (goto-char (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RFC 2231
;;

(defun mew-param-encode (str)
  (let* ((ecsdb (mew-ecsdb-guess-string str))
         (hcs (mew-ecsdb-hcs ecsdb))
	 (charset (mew-cs-to-charset hcs))
         (estr (mew-cs-encode-string str hcs))
	 (len (length estr))
	 (ret (mew-make-string (* len 3)))
	 (j 0) char)
    (dotimes (i len)
      (setq char (aref estr i))
      (if (or (and (<= ?0 char) (<= char ?9))
	      (and (<= ?a char) (<= char ?z))
	      (and (<= ?A char) (<= char ?Z)))
	  (aset ret j char)
	(aset ret j ?%)
	(setq j (1+ j))
	(aset ret j (aref "0123456789ABCDEF" (lsh char -4)))
	(setq j (1+ j))
	(aset ret j (aref "0123456789ABCDEF" (logand char 15))))
      (setq j (1+ j)))
    (concat charset "''" (substring ret 0 j))))

(defun mew-param-decode (whole-value)
  (let* ((value-params (mew-addrstr-parse-value-list whole-value))
	 (value (car value-params))
	 (params (cdr value-params))
	 (max 0) num
	 ret ext ext-sort entry charset cs
	 paramname-value paramname paramvalue)
    (dolist (param params)
      (setq paramname-value (mew-param-analyze param))
      (when paramname-value
	(setcar paramname-value (downcase (nth 0 paramname-value)))
	(if (= (length paramname-value) 2)
	    (setq ret (cons paramname-value ret))
	  (setq ext (cons paramname-value ext))
	  (setq num (nth 2 paramname-value))
	  (if (> num max) (setq max num)))))
    (if (null ext)
	(cons value (nreverse ret)) ;; fast return
      (setq max (1+ max))
      (dolist (ea ext)
	(mew-set '(paramname paramvalue num) ea)
	(if (setq entry (assoc paramname ext-sort))
	    (progn
	      (aset (nth 1 entry) num paramvalue)
	      (when (= num 0)
		(setcar (nthcdr 2 entry) (nth 3 ea)) ;; charset
		(setcar (nthcdr 3 entry) (nth 4 ea)))) ;; lang
	  (setq entry (make-vector max nil))
	  (aset entry num paramvalue)
	  (setq ext-sort
		(cons (list paramname entry (nth 3 ea) (nth 4 ea))
		      ext-sort))))
      (dolist (ea ext-sort)
	(setq paramvalue nil)
	(mew-set '(paramname entry charset) ea)
	(setq num 0)
	(catch 'concat-loop
	  (while (< num max)
	    (if (aref entry num)
		(setq paramvalue (concat paramvalue (aref entry num))) ;; xxx
	      (throw 'concat-loop nil))
	    (setq num (1+ num))))
	(when charset
	  (setq cs (mew-charset-to-cs charset))
	  (when (and mew-use-autoconv-when-unknown
		     (not (mew-coding-system-p cs)))
	    (setq cs mew-cs-autoconv))
	  (when (and mew-decode-broken
		     (not (eq cs mew-cs-autoconv))
		     (or (null charset)
			 (mew-case-equal charset mew-us-ascii)
			 (mew-case-equal charset mew-utf-8))
		     (string-match mew-regex-singlebyte-nonascii paramvalue))
	    (setq cs mew-cs-autoconv))
	  (if (mew-coding-system-p cs)
	      (setq paramvalue (mew-cs-decode-string paramvalue cs))
	    (setq paramvalue mew-error-unknown-charset)))
	(setq ret (cons (list paramname paramvalue) ret)))
      (setq ret (mapcar 'mew-param-sanity-check ret))
      (cons value (nreverse ret))))) ;; late return

(defun mew-param-analyze (param)
  "Return (paramname paramvalue) or
          (paramname paramvalue section charset lang)."
  (let (name section asterisk value charset lang)
    (when (string-match mew-header-param-regex param)
      (setq name (match-string 1 param))
      (setq section (match-string 2 param)) ;; *21
      (setq asterisk (string= (match-string 3 param) "*"))
      (setq value (match-string 4 param))
      (if (string= section "")
	  (if asterisk
	      (setq section 0)
	    (setq section nil))
	(setq section (string-to-number (substring section 1))))
      (if (null asterisk)
	  ;; delete quote
	  (progn
	    (if (and (< 1 (length value))
		     (char-equal (aref value 0) 34)
		     (char-equal (aref value (1- (length value))) 34))
		(setq value (substring value 1 -1)))
	    (if mew-decode-broken
		(setq value (mew-param-analyze-broken value name))))
	(when (and (= section 0)
		   ;; broken UAs quote extended-value, sigh
		   (string-match "^\"?\\([^']*\\)'\\([^']*\\)'\\([^\"]*\\)\"?$" value))
	  (setq charset (match-string 1 value))
	  (setq lang (match-string 2 value))
	  (if (string= lang "") (setq lang nil))
	  (setq value (match-string 3 value))
	  (if (string= value "") (setq value nil)))
	;; broken UAs quote extended-value, sigh
	(if (string-match "^\"\\([^\"]*\\)\"$" value)
	    (setq value (mew-match-string 1 value)))
	(setq value (mew-q-decode-string value ?%)))
      (if section
	  (list name value section charset lang)
	(list name value)))))

(defun mew-param-analyze-broken (value name)
  ;; broken message handling
  (when (and (mew-member-case-equal name mew-broken-parameter-list)
	     (string-match mew-regex-esc-or-nonascii value))
    (mew-decode-warning-params name 'raw)
    (setq value (mew-cs-decode-string value mew-cs-autoconv)))
  (when (string-match mew-header-decode-regex value)
    (let (pre med pst)
      (mew-decode-warning-params name 'enc)
      (while (string-match mew-header-decode-regex value)
	;; We cannot ensure that sub-function does not use match functions.
	;; So, pre/pst prevents infinite loop.
	(setq pre (substring value 0 (match-beginning 0)))
	(setq pst (substring value (match-end 0)))
	(setq med (mew-header-decode (match-string 1 value)
				     (match-string 2 value)
				     (match-string 3 value)))
	(if (string-match mew-header-decode-regex3 pst)
	    (setq pst (substring pst (match-end 1))))
	(setq value (concat pre med pst)))))
  value)

(defun mew-param-sanity-check (ent)
  (let (param value new)
    (mew-set '(param value) ent)
    (setq new (mew-header-sanity-check-string value))
    (unless (string= value new) (mew-decode-warning-params param 'ctl))
    (list param new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Punycode:: RFC 3492
;;;

(defconst mew-puny-base 36)
(defconst mew-puny-tmin 1)
(defconst mew-puny-tmax 26)
(defconst mew-puny-damp 700)
(defconst mew-puny-skew 38)
(defconst mew-puny-initial-bias 72)
(defconst mew-puny-initial-n 128)
(defconst mew-puny-delimiter ?-)

(defun mew-puny-adapt (delta numpoints firsttime)
  (let ((k 0))
    (if firsttime
	(setq delta (/ delta mew-puny-damp))
      (setq delta (/ delta 2)))
    (setq delta (+ delta (/ delta numpoints)))
    (while (> delta (/ (* (- mew-puny-base mew-puny-tmin) mew-puny-tmax) 2))
      (setq delta (/ delta (- mew-puny-base mew-puny-tmin)))
      (setq k (+ k mew-puny-base)))
    (+ k (/ (* (1+ (- mew-puny-base mew-puny-tmin)) delta) (+ delta mew-puny-skew)))))

(defun mew-puny-decode-digit (cp)
  (if (< (- cp 48) 10)
      (- cp 22)
    (if (< (- cp 65) 26)
	(- cp 65)
      (if (< (- cp 97) 26)
	  (- cp 97)
	mew-puny-base))))

(defun mew-puny-encode-digit (d)
  (if (< d 26)
      (+ d 22 75) ;; a-z
    (+ d 22))) ;; 0-9

(defun mew-puny-decode (input)
  (when mew-cs-utf-16be
    (condition-case nil
	(mew-cs-decode-string
	 (mew-puny-decode1 (substring input 4)) ;; xn--
	 mew-cs-utf-16be)
      (error nil))))

(defun mew-puny-decode1 (input)
  (let* ((n mew-puny-initial-n)
	 (bias mew-puny-initial-bias)
	 (len (length input))
	 (in 0) (out 0)
	 (i 0) (b 0)
	 digit thr oldi w k output ret)
    (dotimes (j len)
      (if (= (aref input j) mew-puny-delimiter) (setq b j)))
    (dotimes (j b)
      (setq output (cons (aref input j) output))
      (setq out (1+ out)))
    (setq output (nreverse output))
    (if (> b 0) (setq in (1+ b)) (setq in 0))
    (while (< in len)
      (setq oldi i)
      (setq w 1)
      (setq k mew-puny-base)
      (catch 'loop
	(while t
	  (if (>= in len) (error "punycode bad input"))
	  (setq digit (mew-puny-decode-digit (aref input in)))
	  (if (>= digit mew-puny-base) (error "punycode bad input"))
	  (setq in (1+ in))
	  (setq i (+ i (* digit w)))
	  (if (<= k bias)
	      (setq thr mew-puny-tmin)
	    (if (>= k (+ bias mew-puny-tmax))
		(setq thr mew-puny-tmax)
	      (setq thr (- k bias))))
	  (if (< digit thr) (throw 'loop nil))
	  (setq w (* w (- mew-puny-base thr)))
	  (setq k (+ k mew-puny-base))))
      (setq out (1+ out))
      (setq bias (mew-puny-adapt (- i oldi) out (= oldi 0)))
      (setq n (+ n (/ i out)))
      (setq i (% i out))
      (if (= i 0)
	  (setq output (cons n (nthcdr i output)))
	(setcdr (nthcdr (1- i) output) (cons n (nthcdr i output))))
      (setq i (1+ i)))
    (setq ret (mew-make-string (* out 2)))
    (let ((j 0))
      (dolist (op output)
	(aset ret j (/ op 256))
	(setq j (1+ j))
	(aset ret j (% op 256))
	(setq j (1+ j))))
    ret))

(defun mew-puny-encode (input)
  (when mew-cs-utf-16be
    (condition-case nil
	(concat "xn--" (mew-puny-encode1 (mew-cs-encode-string input mew-cs-utf-16be)))
      (error nil))))

(defun mew-puny-encode1 (input)
  (let* ((len (length input))
	 (h-len (/ len 2))
	 (n mew-puny-initial-n)
	 (bias mew-puny-initial-bias)
	 (delta 0) (out 0)
	 (output (mew-make-string (* len 4)))
	 h b m q k thr uni)
    (dotimes (j len)
      (setq uni (aref input j))
      (setq j (1+ j))
      (setq uni (+ (* uni 256) (aref input j)))
      (when (< uni 128) ;; basic
	(aset output out uni)
	(setq out (1+ out))))
    (setq h out)
    (setq b out)
    (when (> b 0)
      (aset output out mew-puny-delimiter)
      (setq out (1+ out)))
    (while (< h h-len)
      (setq m 65536) ;; 17bits
      (dotimes (j len)
	(setq uni (aref input j))
	(setq j (1+ j))
	(setq uni (+ (* uni 256) (aref input j)))
	(if (and (>= uni n) (< uni m)) (setq m uni)))
      (setq delta (+ delta (* (- m n) (1+ h))))
      (setq n m)
      (dotimes (j len)
	(setq uni (aref input j))
	(setq j (1+ j))
	(setq uni (+ (* uni 256) (aref input j)))
	(when (< uni n)
	  (setq delta (1+ delta))
	  (if (= delta 0) (error "punycode overflow")))
	(when (= uni n)
	  (setq q delta)
	  (setq k mew-puny-base)
	  (catch 'loop
	    (while t
	      (if (<= k bias)
		  (setq thr mew-puny-tmin)
		(if (>= k (+ bias mew-puny-tmax))
		    (setq thr mew-puny-tmax)
		  (setq thr (- k bias))))
	      (if (< q thr) (throw 'loop nil))
	      (aset output out (mew-puny-encode-digit (+ thr (% (- q thr) (- mew-puny-base thr)))))
	      (setq out (1+ out))
	      (setq q (/ (- q thr) (- mew-puny-base thr)))
	      (setq k (+ k mew-puny-base))))
	  (aset output out (mew-puny-encode-digit q))
	  (setq out (1+ out))
	  (setq bias (mew-puny-adapt delta (1+ h) (= h b)))
	  (setq delta 0)
	  (setq h (1+ h))))
      (setq delta (1+ delta))
      (setq n (1+ n)))
    (substring output 0 out)))

(defun mew-puny-encode-url (url)
  (let (beg end idn)
    (with-temp-buffer
      (insert url)
      (goto-char (point-min))
      (if (search-forward "://" nil t)
	  (setq beg (point))
	(setq beg (point-min)))
      (if (search-forward "/" nil t)
	  (setq end (1- (point)))
	(setq end (point-max)))
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (re-search-forward "[^.]?[^.\000-\177][^.]*" nil t)
	  (setq idn (mew-match-string 0))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (mew-puny-encode idn))))
      (mew-buffer-substring (point-min) (point-max)))))

(provide 'mew-bq)

;;; Copyright Notice:

;; Copyright (C) 1997-2010 Mew developing team.
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

;;; mew-bq.el ends here
