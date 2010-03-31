;; mew-addrbook.el --- Aliases and personal information

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 22, 1999

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Address book
;;;

(defvar mew-addrbook-mode-alias "Alias")
(defvar mew-addrbook-mode-personalinfo "Personal Info")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal syntax of mew-addrbook-alist
;;;

;; for mew-complete
(defalias 'mew-addrbook-alias-hit 'assoc)

;;

(defvar mew-addrbook-orig-alist nil
  "(key (addr1, addr2) nickname name) or (key addr)")

(defun mew-adbkorigent-by-shortname (user)
  (mew-addrbook-alias-hit user mew-addrbook-orig-alist))

;;

(defvar mew-addrbook-alist nil ;; mew-alias-auto-alist is appended
  "(key addr) or (key (addr1, addr2) nickname name)")

(defun mew-adbkent-by-addr-with-alist (addr alist)
  (mew-assoc-member-case-equal addr alist 1))

(defun mew-adbkent-by-addr (addr)
  (mew-adbkent-by-addr-with-alist addr mew-addrbook-alist))

(defun mew-adbkent-by-addr2 (addr)
  ;; for nickname and name, mew-addrbook-orig-alist is enough and fast
  (mew-adbkent-by-addr-with-alist addr mew-addrbook-orig-alist))

(defun mew-adbkent-shortname (adbkent)
  (nth 0 adbkent))

(defun mew-adbkent-addrs (adbkent)
  (nth 1 adbkent))

(defun mew-adbkent-nickname (adbkent)
  (nth 2 adbkent))

(defun mew-adbkent-name (adbkent)
  (nth 3 adbkent))

(defun mew-addrbook-shortname-get (addr)
  (mew-adbkent-shortname (mew-adbkent-by-addr addr)))

(defun mew-addrbook-addrs-get (addr)
  (mew-adbkent-addrs (mew-adbkent-by-addr addr)))

(defun mew-addrbook-nickname-get (addr)
  (mew-adbkent-nickname (mew-adbkent-by-addr2 addr)))

(defun mew-addrbook-name-get (addr)
  (mew-adbkent-name (mew-adbkent-by-addr2 addr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal syntax of mew-alias-auto-alist
;;;

(defvar mew-alias-auto-alist nil
  "(key addr)")

(defun mew-autoent-by-shortname (user)
  (assoc user mew-alias-auto-alist))

(defun mew-autoent-shortname (autoent)
  (nth 0 autoent))

(defun mew-autoent-addr (autoent)
  (nth 1 autoent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setup
;;;

(defun mew-alias-short-to-full (alist)
  (mapcar (lambda (x) (cdr x)) alist))

;; duplicated?
(defun mew-alias-full-to-short (alist)
  (mapcar (lambda (x) (cons (downcase (mew-addrstr-extract-user x)) x)) alist))

(defun mew-addrbook-setup ()
  (if (and mew-alias-auto-file (null mew-alias-auto-alist))
      ;; make auto-alist only at the initialization time
      ;; not at update time (auto-alist have not been saved yet)
      (setq mew-alias-auto-alist (mew-lisp-load mew-alias-auto-file)))
  (if mew-use-full-alias
      (if (mew-autoent-addr (car mew-alias-auto-alist))
	  (setq mew-alias-auto-alist
		(mew-alias-short-to-full mew-alias-auto-alist)))
    (if (null (mew-autoent-addr (car mew-alias-auto-alist)))
	(setq mew-alias-auto-alist
	      (mew-alias-full-to-short mew-alias-auto-alist))))
  (setq mew-addrbook-orig-alist (mew-addrbook-make-alist))
  (mew-addrbook-concat-uniq)
  (add-hook 'kill-emacs-hook 'mew-addrbook-clean-up))

(defun mew-addrbook-concat-uniq ()
  (setq mew-addrbook-alist
	(append mew-addrbook-orig-alist (copy-sequence mew-alias-auto-alist)))
  (setq mew-addrbook-alist (mew-uniq-alist mew-addrbook-alist)))

(defun mew-addrbook-clean-up ()
  (remove-hook 'kill-emacs-hook 'mew-addrbook-clean-up)
  (when (and mew-alias-auto-file mew-alias-auto-alist)
    (mew-lisp-save mew-alias-auto-file mew-alias-auto-alist))
  (setq mew-alias-auto-alist nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Due to the spec of mew-complete, alist must be passed.
;;;

(defvar mew-alias-expand-prefix nil
  "A variable to make 'mew-alias-expand' elegant.")

;; for completion
(defun mew-addrbook-alias-get (key alist)
  (let* ((mew-alias-expand-prefix nil)
	 (addrs (mew-alias-expand key alist 0))
	 (addrx (mapcar 'mew-addrstr-append-domain addrs))
	 (ret (mapconcat 'identity addrx ", ")))
    (if mew-alias-expand-prefix
	(concat mew-alias-expand-prefix ":" ret ";")
      ret)))

(defun mew-alias-expand-addrs (key alist count)
  (let ((keys (delete "" (mapcar 'mew-chop (mew-split key ?,))))
	ret)
   (dolist (key keys)
     (setq ret (nconc ret (mew-alias-expand key alist count))))
   ret))

(defun mew-alias-expand (key alist count)
  "Expand KEY to a list of addresses according to ALIST.
Infinite loop is prevented by COUNT and 'mew-expand-max-depth'.
Before calling, 'mew-alias-expand-prefix' must be set 'nil'.
If \"prefix:a,b,c;\" is given, 'mew-alias-expand-prefix'
is set to \"prefix\", and (expanded-a expanded-b expanded-c) is
returned."
  (cond
   ((> count mew-expand-max-depth) key)
   ((string-match "^\\([^:]+\\):\\([^;]+\\);$" key)
    (if mew-alias-expand-prefix (error ":; must not recurse!"))
    (setq mew-alias-expand-prefix (match-string 1 key))
    (setq key (match-string 2 key))
    (mew-alias-expand-addrs key alist (1+ count)))
   (t
    (let ((crnt (mew-adbkent-addrs (mew-addrbook-alias-hit key alist))))
      (cond
       ((null crnt) (list key))
       ((listp crnt) (list (car crnt)))
       ((string-match "[^:]+:[^;]+;" crnt)
	(mew-alias-expand crnt alist (1+ count)))
       ((string-match "," crnt)
	(mew-alias-expand-addrs crnt alist (1+ count)))
       (t (list crnt)))))))

(defun mew-addrbook-alias-next (key alist)
  (let* ((addrs (mew-adbkent-addrs
		 (mew-adbkent-by-addr-with-alist key alist))))
    (if (and addrs (listp addrs))
	(mew-get-next addrs key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For mew-encode-learn-aliases
;;;

(defun mew-addrbook-alias-add (addr)
  ;; Let's downcase user-name for .mew-alias since it is automatically
  ;; maintained.
  (when (and (stringp addr) (string-match "@" addr)
	     (or (null mew-addrbook-alias-not-learn-list)
		 (not (mew-member-match addr mew-addrbook-alias-not-learn-list))))
    (if mew-use-full-alias
	(unless (member (list addr) mew-alias-auto-alist)
	  (mew-addrbook-alias-cons (list addr)))
      (let* ((user (downcase (mew-addrstr-extract-user addr)))
	     (match-auto (mew-autoent-by-shortname user))
	     (match-adbk-orig (mew-adbkorigent-by-shortname user)))
	(cond
	 ((string= user "")
	  ;; do nothing
	  )
	 (match-adbk-orig
	  ;; do nothing
	  )
	 (match-auto
	  (cond
	   ((string= addr (mew-autoent-addr match-auto))
	    ;; Move the entry to the top for the recent-used-first.
	    (setq mew-alias-auto-alist (delq match-auto mew-alias-auto-alist))
	    (setq mew-alias-auto-alist (cons match-auto mew-alias-auto-alist)))
	   (mew-addrbook-override-by-newone
	    ;; override match-auto by (user addr)
	    (setq mew-alias-auto-alist (delq match-auto mew-alias-auto-alist))
	    (setq mew-addrbook-alist (delete match-auto mew-addrbook-alist))
	    (mew-addrbook-alias-cons (list user addr)))
	   (t
	    ;; the old entry remains
	    )))
	 (t
	  (mew-addrbook-alias-cons (list user addr))))))))

(defun mew-addrbook-alias-cons (user-addr)
  (setq mew-alias-auto-alist (cons user-addr mew-alias-auto-alist))
  ;; Ideally, user-addr should be inserted in between
  ;; mew-addrbook-orig-alist and mew-alist-auto-alist.
  ;; But mew-alist-auto-alist is the newest-first order.
  ;; So, just cons user-addr to mew-addrbook-alist.
  ;; This produces a different alist made by mew-addrbook-setup.
  ;; But we do not care.
  (setq mew-addrbook-alist (cons user-addr mew-addrbook-alist)))

(defun mew-addrbook-alias-delete (addr)
  (when (and (stringp addr) (string-match "@" addr))
    (let* ((user (downcase (mew-addrstr-extract-user addr)))
	   (ent (mew-autoent-by-shortname user)))
      (when (and ent (string= addr (mew-autoent-addr addr)))
	(setq mew-addrbook-alist (delete ent mew-addrbook-alist))
	(setq mew-alias-auto-alist (delq ent mew-alias-auto-alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading Addrbook files
;;;

(defun mew-addrbook-insert-file (file cregex &optional unquote)
  (let* ((case-fold-search t)
	 (pars (mew-split file ?,)) ;; parents
	 (files pars) ;; included
	 par chr path beg end qchar ret)
    ;; include parents files
    (dolist (ent pars)
      (setq par (expand-file-name ent mew-conf-path))
      (when (file-readable-p par)
	(setq ret t)
	(mew-insert-file-contents par)
	(setq path (file-name-directory par))
	;; include children files
	(while (re-search-forward "^<[ \t]*\\([^ \t\n]+\\).*$" nil t)
	  (setq chr (expand-file-name (mew-match-string 1) path))
	  (replace-match "" nil t)
	  (when (and (file-readable-p chr) (not (member chr files)))
	    (mew-insert-file-contents chr)
	    (setq files (cons chr files))))
	(goto-char (point-max))))
    ;; remove comments
    (goto-char (point-min))
    (while (re-search-forward cregex nil t)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (beginning-of-line)
      (if (/= (point) beg)
	  (forward-line)
	(forward-line)
	(setq end (point)))
      (delete-region beg end))
    ;; concat continuation lines
    ;; "\" must locate on the end of line
    (goto-char (point-min))
    (while (re-search-forward "\\\\\n" nil t)
      (replace-match "" nil t))
    ;; concat separated lines by comma
    (goto-char (point-min))
    (while (re-search-forward ",$" nil t)
      (end-of-line)
      (forward-char 1)
      (delete-backward-char 1))
    ;; replace ", " to "\0" inside/outside quote.
    (goto-char (point-min))
    (while (re-search-forward ",[ \t]+" nil t)
      (replace-match ",\0" nil t))
    ;; unquote, replace white spaces to "\0".
    (goto-char (point-min))
    (if unquote
	(catch 'quote
	  (while (re-search-forward "[\"']" nil t)
	    (setq qchar (char-before (point)))
	    ;; (point) is for backward compatibility
	    (backward-delete-char 1) ;; delete quote
	    (setq beg (point))
	    (if (not (re-search-forward (char-to-string qchar) nil t))
		(throw 'quote nil) ;; error
	      (backward-delete-char 1) ;; delete quote
	      (save-restriction
		(narrow-to-region beg (point))
		(goto-char (point-min))
		(while (re-search-forward "[ \t]+" nil t)
		  (replace-match "\0" nil t))
		(goto-char (point-max))))))) ;; just in case
    ;; remove optional white spaces
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match " " nil t))
    ret))

(defun mew-addrbook-strsafe (var)
  (if (or (string= "" var) (string= "*" var))
      nil
    (mew-replace-character var 0 mew-sp)))

(defun mew-addrbook-make-alist ()
  (let (alias colon addrs nick name alist)
    (with-temp-buffer
      (when (mew-addrbook-insert-file
	     mew-addrbook-file mew-addrbook-comment-regex 'unquote)
	(goto-char (point-min))
	(while (re-search-forward "^ ?\\([^ \n:]+\\) ?\\(:?\\) ?\\([^ \t\n]+\\)" nil t)
	  (setq alias (mew-addrbook-strsafe (mew-match-string 1)))
	  (setq colon (mew-match-string 2))
	  (setq addrs (mew-addrbook-strsafe (mew-match-string 3)))
	  (if (string= colon ":")
	      (setq alist (cons (list alias addrs) alist))
	    (and addrs (setq addrs (mapcar 'mew-chop (mew-split addrs ?,))))
	    (if (looking-at " ?\\([^ \n]*\\) ?\\([^ \n]*\\)")
		(progn
		  (setq nick (mew-addrbook-strsafe (mew-match-string 1)))
		  (setq name (mew-addrbook-strsafe (mew-match-string 2))))
	      (setq nick nil)
	      (setq name nil))
	    (setq alist (cons (list alias addrs nick name) alist))))))
    (nreverse alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addrbook mode
;;;

(defun mew-summary-addrbook-add (&optional personalinfo)
  "Adding the value of From: in Message mode to Addrbook. When
executed with '\\[universal-argument]', it will add personal information.  Otherwise,
it will add an alias."
  (interactive "P")
  (let ((fld (mew-summary-folder-name))
	(msg (mew-summary-message-number2))
	from shortname addrs name)
    (save-excursion
      (mew-summary-set-message-buffer fld msg)
      (setq from (mew-header-get-value mew-from:)))
    (if (null from)
	(message "No address to be registered")
      ;; assuming From: contains just one address
      (setq addrs (mew-addrstr-parse-address from))
      (if (mew-is-my-address mew-regex-my-address-list addrs)
	  (if personalinfo
	      (setq addrs (car (mew-header-parse-address-list
				(list mew-to:))))
	    (setq addrs (mapconcat 'identity
				   (mew-header-parse-address-list
				    (list mew-to: mew-cc:))
				   ",")))
	(when (string-match "\\(.*\\)<.*>" from)
	  (setq name (match-string 1 from))
	  (setq name (mew-chop name)))
	(if mew-addrbook-strip-domainpart
	    (setq shortname (downcase (mew-addrstr-extract-user addrs)))
	  (setq shortname (downcase addrs))))
      (if (not addrs)
	  (message "No address to be registered")
	(mew-addrbook-prepare-template
	 personalinfo shortname addrs nil name)))))

(defun mew-addrbook-prepare-template (personalinfop shortname addrs &optional nickname name)
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create mew-buffer-addrbook))
  (mew-erase-buffer)
  (mew-insert-manual
   "#If you want to register this entry, type "
   "'\\<mew-addrbook-mode-map>\\[mew-addrbook-register]'.\n"
   "#If you want to NOT register this entry, type "
   "'\\<mew-addrbook-mode-map>\\[mew-addrbook-kill]'.\n")
  (mew-addrbook-insert-template "Shortname" shortname)
  (mew-addrbook-insert-template "Addresses" addrs)
  (cond
   (personalinfop
    (mew-addrbook-insert-template "Nickname" nickname)
    (mew-addrbook-insert-template "Name" name)
    (mew-addrbook-mode mew-addrbook-mode-personalinfo))
   (t
    (mew-addrbook-mode mew-addrbook-mode-alias)))
  (mew-addrbook-insert-template "Comments" nil)
  (goto-char (point-min))
  (search-forward ": " nil t))

(defun mew-addrbook-insert-template (key val)
  (mew-elet
   (let ((beg (point)))
     (insert key ": ")
     (put-text-property beg (point) 'read-only t)
     (mew-rear-nonsticky beg (point))
     (and val (insert val))
     (insert "\n"))))

(defun mew-addrbook-mode (mname)
  "A major mode to register Addrbook.

\\{mew-addrbook-mode-map}"
  (interactive)
  (setq major-mode 'mew-addrbook-mode)
  (setq mode-name mname)
  (setq mode-line-buffer-identification (mew-mode-line-id))
  (use-local-map mew-addrbook-mode-map)
  (run-hooks 'mew-addrbook-mode-hook)
  (setq buffer-undo-list nil))

(defun mew-addrbook-nconc (ent)
  (setq mew-addrbook-orig-alist (nconc mew-addrbook-orig-alist (list ent)))
  (mew-addrbook-concat-uniq))

(defun mew-addrbook-register ()
  "Register information in Addrbook mode to Addrbook."
  (interactive)
  (let ((shortname (mew-header-get-value "Shortname:"))
	(addrs     (mew-header-get-value "Addresses:"))
	(nickname  (mew-header-get-value "Nickname:"))
	(name      (mew-header-get-value "Name:"))
	(comments  (mew-header-get-value "Comments:"))
	(mode mode-name)
	(uniqp t)
	buf addrsl errmsg)
    (cond
     ((string= mode mew-addrbook-mode-alias)
      (cond
       ((and (null shortname) (null addrs))
	(setq errmsg "Must fill both Shortname and Addresses"))
       ((null shortname)
	(setq errmsg "Must fill Shortname"))
       ((null addrs)
	(setq errmsg "Must fill Addresses"))))
     (t
      (cond
       ((null addrs)
	(setq errmsg "Must fill Addresses"))
       ((and (null shortname) (null nickname) (null name))
	(setq errmsg "Must fill Shortname or Nickname or Name"))
       ((and name (string-match "^\"[^\"]*[^\000-\177]" name))
	(setq errmsg "Remove quote around non-ASCII Name")))))
    (if errmsg
	(message "%s" errmsg)
      (save-excursion
	(setq buf (mew-find-file-noselect2
		   (expand-file-name mew-addrbook-file mew-conf-path)))
	(set-buffer buf)
	(goto-char (point-min))
	(if (and shortname
		 (re-search-forward
		  (concat "^" (regexp-quote shortname) "[ \t]*:?[ \t]+") nil t))
	    (setq uniqp nil))
	(when uniqp
	  ;; All errors are checked.
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (cond
	   ((string= mode mew-addrbook-mode-alias)
	    (insert shortname ":\t" addrs)
	    (mew-addrbook-nconc (list shortname addrs)))
	   (t
	    (setq addrsl (mew-addrstr-parse-address-list addrs))
	    ;; Set mew-addrbook-orig-alist with unquoted strings.
	    (mew-addrbook-nconc (list shortname addrsl nickname name))
	    (unless shortname (setq shortname "*"))
	    (if (and nickname (string-match "^[^\" \t]+[ \t]+.*$" nickname))
		(setq nickname (concat "\"" nickname "\"")))
	    (if (and name (string-match "^[^\" \t]+[ \t]+.*$" name))
		(setq name (concat "\"" name "\"")))
	    (if name
		(insert shortname "\t" addrs "\t" (or nickname "*") "\t" name)
	      (if nickname
		  (insert shortname "\t" addrs "\t" nickname)
		(insert shortname "\t" addrs)))))
	  (if comments
	      (insert "\t#" comments "\n")
	    (insert "\n"))
	  (save-buffer)))
      ;; Addrbook buffer
      (mew-remove-buffer buf)
      (cond
       (uniqp
	(mew-addrbook-kill 'no-msg)
	(message "Registered to Addrbook"))
       (t
	(message "Shortname is already used. Change Shortname"))))))

(defun mew-addrbook-kill (&optional no-msg)
  "Kill Addrbook mode."
  (interactive "P")
  (mew-remove-buffer (current-buffer))
  (or no-msg (message "Not registered")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Editing auto-alist
;;;

(defun mew-summary-alias-read-buffer ()
  (save-excursion
    (goto-char (point-min))
    ;; An error occurs if the expression is broken.
    ;; This is intentional.
    (setq mew-alias-auto-alist (read (current-buffer)))
    (mew-addrbook-concat-uniq)
    nil)) ;; to write

(defun mew-summary-alias-edit ()
  "Editing the auto alias file which contains a list of alias-address
pairs. Remove unnecessary entries and save the file by
'\\[save-buffer]'. After saving, the modification is automatically
reflected."
  (interactive)
  (let ((file mew-alias-auto-file))
    (setq mew-alias-auto-alist
	  (sort mew-alias-auto-alist (lambda (x y) (string< (car x) (car y)))))
    (mew-lisp-save mew-alias-auto-file mew-alias-auto-alist)
    (unless (file-name-absolute-p file)
      (setq file (expand-file-name file mew-conf-path)))
    (switch-to-buffer (mew-find-file-noselect file))
    (emacs-lisp-mode)
    (if (boundp 'write-file-functions)
	(add-hook 'write-file-functions 'mew-summary-alias-read-buffer nil 'local)
      (add-hook 'local-write-file-hooks 'mew-summary-alias-read-buffer))))

(provide 'mew-addrbook)

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

;;; mew-addrbook.el ends here
