;; mew-header.el --- Mail header stuff for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

(defvar mew-anonymous-recipients ":;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header functions
;;;

(defun mew-header-goto-next ()
  (while (looking-at mew-lwsp) (forward-line)))

;; see also mew-header-end
(defun mew-header-goto-end ()
  (goto-char (point-min))
  (if (re-search-forward mew-eoh nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (if (eolp) (insert "\n"))))

(defun mew-header-goto-body ()
  (mew-header-goto-end)
  (forward-line))

(defun mew-header-get-value (field &optional as-list)
  "currently, when no match, it returns nil."
  ;; maybe called in narrowed region.
  ;; we cannot widen for citation.
  (let ((case-fold-search t)
	(regex (format "^%s[ \t]*" field))
	start match ret)
    (save-excursion
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(catch 'only-one
	  (while (re-search-forward regex nil t)
	    (setq start (match-end 0))
	    (forward-line)
	    (mew-header-goto-next)
	    (setq match (mew-buffer-substring start (1- (point))))
	    (unless (string= "" match)
	      (if ret
		  (if as-list
		      (setq ret (cons match ret))
		    (setq ret (concat ret "," match)))
		(if as-list
		    (setq ret (list match))
		  (setq ret match))
		(if (string= field mew-from:) (throw 'only-one nil))))))))
    (if as-list
	(nreverse ret)
      ret)))

(defalias 'mew-header-existp 'mew-header-get-value)

(defun mew-header-replace-value (field value)
  (let ((case-fold-search t)
	(regex (format "^%s[ \t]*" field))
	start)
    (save-excursion
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	;; make value with evaluating
	(if (and (listp value)
		 (stringp (car value)))
	    (setq value (cons 'format value)))
	(setq value
	      (condition-case e
		  (eval value)
		(error
		 (format "*** ERROR: %s: %s ***" (car e) (car (cdr e))))))
	(unless (or (stringp value)
		    (null value))
	  (setq value (format "%s" value)))
	;; at this time, value is string or nil
	(cond
	 ((re-search-forward regex nil t)
	  (setq start (match-beginning 0))
	  (forward-line)
	  (mew-header-goto-next)
	  (delete-region start (point))
	  (if value
	      (mew-draft-header-insert field value)))
	 (value ;; insert when value is exist
	  (mew-header-goto-end)
	  (mew-draft-header-insert field value)))))))

(defun mew-make-field-regex (fields)
  (concat "^\\(" (mapconcat 'identity fields "\\|") "\\)"))

(defun mew-header-delete-lines (fields)
  (when fields
    (let ((case-fold-search t)
	  (regex (mew-make-field-regex fields))
	  start)
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (setq start (match-beginning 0))
	  (forward-line)
	  (mew-header-goto-next)
	  (delete-region start (point)))))))

(defun mew-header-delete-other-lines (fields)
  (when fields
    (let ((case-fold-search t)
	  (regex (mew-make-field-regex fields))
	  start)
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (not (eobp))
	  (if start (delete-region start (point)))
	  (if (looking-at regex)
	      (setq start nil)
	    (setq start (point)))
	  (forward-line)
	  (mew-header-goto-next))
	(if start (delete-region start (point)))))))

(defun mew-header-replace-lines (fields prefix)
  (when fields
    (let ((case-fold-search t)
	  (regex (mew-make-field-regex fields)))
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (beginning-of-line)
	  (insert prefix)
	  (forward-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header sort
;;;

(defun mew-header-sort (order)
  (when order
    (let* ((case-fold-search t)
	   (len (length order))
	   (vec (make-vector len nil))
	   key beg n line other)
      (mew-header-goto-end)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (not (eobp))
	  (if (not (looking-at mew-keyval))
	      (forward-line)
	    (setq key (mew-match-string 1))
	    (setq beg (match-beginning 0))
	    (forward-line)
	    (mew-header-goto-next)
	    (setq line (mew-buffer-substring beg (point)))
	    (delete-region beg (point))
	    (setq n (mew-member-case-equal key order))
	    (if n
		(aset vec n (concat (aref vec n) line))
	      (setq other (concat other line)))))
	(dotimes (i len)
	  (and (aref vec i) (insert (aref vec i))))
	(and other (insert other))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header insertion
;;;

(defun mew-draft-header-insert (field value)
  "No encoding. Just insert."
  (if (and value (stringp field)) (insert field " " value "\n")))

(defun mew-draft-header-fill (field value)
  (unless (mew-header-existp field) (mew-draft-header-insert field value)))

(defun mew-header-insert (key value &optional no-fold)
  (if (and value (stringp key))
      (let ((beg (point)) params med parname parval)
	(when (listp value)
	  (setq params (cdr value))
	  (setq value (car value)))
	(insert key)
	(insert " ")
	(setq med (point))
	(if (string-match "^[\t -~]*$" value)
	    (insert value)
	  (mew-header-encode-text value nil (length key)))
	(dolist (par params)
	  (mew-set '(parname parval) par)
	  (insert ";")
	  (cond
	   ((string-match "^[-a-zA-Z0-9]+$" parval)
	    ) ;; do nothing
	   ((and (string= (mew-charset-guess-string parval) mew-us-ascii)
		 (not (string-match "\"" parval)))
	    (setq parval (concat "\"" parval "\"")))
	   (t
	    (setq parval (mew-param-encode parval))
	    (setq parname (concat parname "*"))))
	  (insert " " parname "=" parval))
	(insert "\n")
	(unless no-fold
	  (mew-header-fold-region beg (point) med)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level functions to parse fields
;;;

;;
;; The purpose of this function is to purse and extract value
;; of address or content header. text is not target!
;;
;; ADDRP is t if STR are addresses.
;; ADDRP is nil if STR is a value of content-*.
;;
;; 'Winnie (The) Pooh <"W. Pooh" (loves "honey") @ (somewhere in) England>'
;; ->
;; '"W. Pooh"@England'
;;
;; For 'destination', SEP is ",".
;; For Content-*, SEP is ";".
;;
;; The special (i.e. SEP) is one of delimiters.
;; 'linear-white-space' is another delimiter.
;; 'quoted-string', 'comment', and 'domain-literal' is self-delimiting.
;; But 'domain-literal' (e.g. [1.2.3.4]) is meaningless.
;; So, the delimiters consists of SEP, SP, TAB, CRLF, "(", ")", and <">.
;;
;; 'qtext' remains. (It will be removed by mew-param-decode.)
;; ## how about 'quoted-pair'?
;;
;; 'comment' must be symmetric. That is, it must have the exact number of
;; ")" against that of ")". The entire 'comment', even if nested, is
;; ignored.
;;
;; 'encoded-word' is meaningless.
;; (1) Remove 'comment' so 'encoded-word' in 'comment' is meaningless.
;; (2) 'phrase' of 'mailbox' will be ignored because 'addr-spec' in
;; 'route-addr' will be extracted. So, 'encoded-word' in 'phrase' is
;; meaningless.

(defmacro mew-addrstr-parse-syntax-list-check-depth (&optional depth)
  `(progn
     (setq sep-cnt (1+ sep-cnt))
     (if (and ,depth (>= sep-cnt ,depth))
	 (throw 'max nil))
     (when (and (integerp mew-header-max-depth)
		(>= sep-cnt mew-header-max-depth))
       (mew-warn "Too many values. Truncate values over mew-header-max-depth")
       (throw 'max nil))))

(defun mew-addrstr-parse-syntax-list (str sep addrp &optional depth allow-spc)
  (if str (mew-addrstr-parse-syntax-list1 str sep addrp depth allow-spc)))

(defun mew-addrstr-parse-syntax-list1 (str sep addrp depth allow-spc)
  (let* ((i 0) (len (length str))
	 (par-cnt 0) (tmp-cnt 0) (sep-cnt 0)
	 ;; Emacs 23 has non-safe aset.
	 (tmp (mew-set-string-multibyte (mew-make-string len)))
	 c ret prevc
	 (do-clear
	  (lambda ()
	    (setq tmp-cnt 0)
	    (mew-addrstr-parse-syntax-list-check-depth depth)))
	 (do-cons-ret
	  (lambda ()
	    (setq ret (cons (substring tmp 0 tmp-cnt) ret))))
	 (do-copy
	  (lambda ()
	    (aset tmp tmp-cnt c)
	    (setq tmp-cnt (1+ tmp-cnt))))
	 (in-fold-quote
	  (lambda ()
	    (setq i (1+ i))
	    (catch 'fold-quote
	      (while (< i len)
		(setq c (aref str i))
		(cond
		 ;; ((or (char-equal c ?\t) (char-equal c mew-sp))
		 ;; (setq i (1+ i)))
		 ((char-equal c ?\")
		  (funcall do-copy)
		  (throw 'quote nil))
		 (t
		  (funcall do-copy)
		  (throw 'fold-quote nil)))))))
	 (in-quote
	  (lambda ()
	    (funcall do-copy)
	    (setq i (1+ i))
	    (catch 'quote
	      (while (< i len)
		(setq prevc c)
		(setq c (aref str i))
		(cond
		 ((and (char-equal c ?\") (not (char-equal prevc ?\\)))
		  (funcall do-copy)
		  (throw 'quote nil))
		 ((char-equal c ?\n)
		  (funcall in-fold-quote))
		 (t
		  (funcall do-copy)))
		(setq i (1+ i))))))
	 (in-comment
	  (lambda ()
	    (setq par-cnt 1)
	    (setq i (1+ i))
	    (catch 'comment
	      (while (< i len)
		(setq c (aref str i))
		(cond
		 ((char-equal c ?\()
		  (setq par-cnt (1+ par-cnt)))
		 ((char-equal c ?\))
		  (setq par-cnt (1- par-cnt))
		  (if (eq par-cnt 0) (throw 'comment nil))))
		(setq i (1+ i))))))
	 (do-skip-until
	  (lambda (target)
	    (while (and (< i len) (not (char-equal (aref str i) target)))
	      (setq i (1+ i)))))
	 (in-addr
	  (lambda ()
	    (cond
	     (addrp
	      (let (rbeg rend)
		(setq i (1+ i))
		(setq rbeg i)
		(funcall do-skip-until ?>)
		(when (> i rbeg)
		  (setq rend i)
		  ;; note: to be used for substring, so not 1-.
		  ;; should not be nested but easy to implement...
		  (setq ret (cons (car (mew-addrstr-parse-syntax-list (substring str rbeg rend) sep t)) ret))))
	      (funcall do-skip-until sep)
	      (funcall do-clear))
	     (t
	      ;; just ignore
	      (funcall do-skip-until ?>)))))
	 (do-sep
	  (lambda ()
	    ;; broken quoted string cannot be rescued because
	    ;; the separator cannot be distinguished
	    (if (> tmp-cnt 0) (funcall do-cons-ret))
	    (funcall do-clear)))
	 (do-rescue
	  (lambda (var)
	    (save-match-data
	      (if (and addrp (string-match "<\\([^>]+\\)>" var))
		  (setq ret (cons (mew-match-string 1 var) ret))
		(funcall do-cons-ret))))))
    ;; main
    (catch 'max
      (while (< i len)
	(setq c (aref str i))
	(cond
	 ((char-equal c ?\")
	  (funcall in-quote))
	 ((char-equal c ?\()
	  (funcall in-comment))
	 ((char-equal c ?<)
	  (funcall in-addr))
	 ((or (char-equal c ?\n) (char-equal c ?\t)
	      (and (not allow-spc) (char-equal c mew-sp)))
	  ) ;; do nothing
	 ((char-equal c sep)
	  (funcall do-sep))
	 (t
	  (funcall do-copy)))
	(setq i (1+ i)))
      ;; broken quoted string can be rescued if it appears solely
      (cond
       ((> par-cnt 0)
	(funcall do-rescue str))
       ((> tmp-cnt 0)
	(funcall do-rescue tmp))))
    (setq ret (delete nil ret))
    (when allow-spc
      (setq ret (mapcar (lambda (str)
			  (if (string-match "^ +\\(.*\\)" str)
			      (mew-match-string 1 str)
			    str))
			ret)))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; High-level functions to parse fields
;;;

(defun mew-addrstr-parse-value (value)
  (car (mew-addrstr-parse-syntax-list value ?\; nil 1)))

(defun mew-addrstr-parse-value-list (value)
  (mew-addrstr-parse-syntax-list value ?\; nil))

(defun mew-addrstr-parse-value2 (value)
  (car (mew-addrstr-parse-syntax-list value ?, nil 1)))

(defun mew-addrstr-parse-value-list2 (value)
  (mew-addrstr-parse-syntax-list value ?, nil nil t))

;;

(defun mew-addrstr-parse-address (address)
  (car (mew-addrstr-parse-syntax-list address ?, t 1)))

(defun mew-addrstr-parse-address-list (address)
  (mew-addrstr-parse-syntax-list address ?, t))

(defun mew-header-parse-address (field)
  (car
   (mew-addrstr-parse-syntax-list
    (mapconcat 'mew-header-get-value (list field) ",")
    ?, t 1)))

(defun mew-header-parse-address-list (field-list)
  (mew-addrstr-parse-syntax-list
   (mapconcat 'mew-header-get-value field-list ",")
   ?, t))

(defun mew-header-parse-address-list2 (field-list)
  "Collect addresses from FIELD-LIST.
Remove anonymous addresses."
  (let ((vals (mew-addrstr-parse-syntax-list
	       (mapconcat 'mew-header-get-value field-list ",")
	       ?, t))
	ret)
    (dolist (val vals)
      (unless (string-match mew-anonymous-recipients val)
	(setq ret (cons val ret))))
    (nreverse ret)))

;;

(defun mew-addrstr-extract-user-list (addr-list)
  (mapcar 'mew-addrstr-extract-user addr-list))

(defun mew-addrstr-extract-user (addr)
  "Extracts username from ADDR"
  (if (string-match "@.*:" addr) ;; xxx what's this?
      (setq addr (substring addr (match-end 0)))
    (setq addr (mew-replace-character addr mew-sp ?_))
    (setq addr (substring addr 0 (string-match "%" addr)))
    (setq addr (substring addr 0 (string-match "@" addr)))
    ;; just for refile:  "To: recipients:;" -> recipients
    (setq addr (substring addr 0 (string-match mew-anonymous-recipients addr)))
    ;; removing Notes domain
    (setq addr (substring addr 0 (string-match "/" addr)))))

;;

(defun mew-addrstr-append-domain (addr)
  (if mew-addrbook-append-domain-p
      (if (string-match "@" addr)
	  addr
	(concat addr "@" (mew-mail-domain))) ;; xxx
    addr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for ID
;;;

(defun mew-idstr-get-first-id (idstr)
  (when (stringp idstr)
    (if (string-match mew-regex-id idstr)
	(mew-replace-white-space2 (match-string 0 idstr)))))

(defun mew-idstr-get-last-id (idstr)
  (when (stringp idstr)
    (let (beg end start)
      (while (string-match mew-regex-id idstr start)
	(setq beg (match-beginning 0))
	(setq end (match-end 0))
	(setq start (match-end 0)))
      (if (and beg end)
	  (mew-replace-white-space2 (substring idstr beg end))))))

(defun mew-idstr-to-id-list (idstr &optional rev)
  (when (stringp idstr)
    (let (ret start)
      (while (string-match mew-regex-id idstr start)
	(setq start (match-end 0))
	(setq ret (cons (mew-replace-white-space2 (match-string 0 idstr)) ret)))
      (if rev
	  ret
	(nreverse ret)))))

(defun mew-id-list-to-idstr (id-list)
  (let (skip)
    (if (integerp mew-references-max-count)
	(setq skip (- (length id-list) mew-references-max-count)))
    (if (and (integerp skip) (> skip 0))
	(setq id-list (nthcdr skip id-list)))
    (mew-join "\n\t" id-list)))

(provide 'mew-header)

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

;;; mew-header.el ends here
