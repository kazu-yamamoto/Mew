;;; mew-refile.el --- Refile for Mew

;; Author:  Yoshinari NOMURA <nom@csce.kyushu-u.ac.jp>
;;          Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jun 11, 1994

;;; Code:

(require 'mew)

(defmacro mew-summary-refilable (&rest body)
  `(mew-pickable
    (mew-summary-not-in-draft
     (mew-summary-local-or-imap
      ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-refile-msgid-alist nil
  "Alist of message-id and folder pair")

(defvar mew-refile-from-alist nil
  "Alist of From: address and folder pair")

(defvar mew-refile-last-folder nil
  "Folder name previously you refiled")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialize function
;;;

(defun mew-refile-setup ()
  ;; load message id alist
  (or mew-refile-msgid-alist
      (setq mew-refile-msgid-alist (mew-lisp-load mew-refile-msgid-file)))
  ;; load from alist
  (or mew-refile-from-alist
      (setq mew-refile-from-alist (mew-lisp-load mew-refile-from-file)))
  (mew-assoc-folder-setup)
  (add-hook 'kill-emacs-hook 'mew-refile-clean-up))

(defun mew-refile-clean-up ()
  (remove-hook 'kill-emacs-hook 'mew-refile-clean-up)
  (if (and mew-refile-from-alist
	   (member 'mew-refile-guess-by-from mew-refile-guess-control))
      (mew-lisp-save mew-refile-from-file mew-refile-from-alist))
  (if (and mew-refile-msgid-alist
	   (member 'mew-refile-guess-by-thread mew-refile-guess-control))
      (mew-lisp-save mew-refile-msgid-file mew-refile-msgid-alist))
  (setq mew-refile-from-alist nil)
  (setq mew-refile-msgid-alist nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess functions
;;;

;; We have two types of functions in mew-refile-guess-control,
;; guess function and ctrl function.
;; guess function must return a folder list or folder string or nil.
;; guess function must not have a string "ctrl" in its symbol name.
;; ctrl function must have a string "ctrl" in its symbol name.

;; dispatcher returns: ((guess1 guess2 ..) info1 info2 ...) multi  guess mode
;;                     ((guess1)           info1 info2 ...) single guess mode
;;            info1:   ('guess-func-name guess1 guess2 ...)
;;
;; that is, 'car' is a list of judged  folders.
;;          'cdr' is a alist of opinions by guess functions.
;;

(defun mew-refile-guess (&optional auto show-all)
  (let ((case-fold-search t)
	(funcs mew-refile-guess-control) ret guess info stop)
    (catch 'last
      (dolist (func funcs)
	(if (string-match "ctrl" (symbol-name func))
	    ;; func is control function
	    (when (setq ret (funcall func guess auto))
	      (setq stop t)
	      (or show-all (throw 'last t)))
	  ;; func is guess function
	  (setq ret (funcall func)))
	(unless (listp ret) (setq ret (list ret)))
	(setq info (nconc info (list (cons func ret))))
	(unless stop
	  (dolist (re ret)
	    (mew-addq guess re)))))
    (setq guess (nreverse guess))
    (if (not mew-refile-ctrl-multi) (setq guess (list (car guess))))
    (cons guess info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess control functions
;;;

(defun mew-refile-ctrl-auto-boundary (guess auto)
  (if auto "stop"))

(defun mew-refile-ctrl-throw (guess auto)
  (if guess "stop"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dispatcher
;;;

(defun mew-refile-guess-from-dispatch (func &optional addr)
  "Dispatcher to make mew-refile-guess-by-from-* consider
mew-refile-guess-from-me-is-special."
  (let ((from (downcase (or addr (mew-header-parse-address mew-from:) ""))))
    ;; if From: is my address, addr is the list extracted from To:, Cc:
    (if (and mew-refile-guess-from-me-is-special
	     (mew-is-my-address mew-regex-my-address-list from))
	(let ((addrs (mew-header-parse-address-list mew-refile-guess-key-list))
	      ret adr)
	  (dolist (addr addrs)
	    (setq adr (funcall func addr))
	    (mew-addq ret adr))
	  (nreverse ret))
      (funcall func from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by folder
;;;

(defvar mew-assoc-folder nil)

(defun mew-folder-func (full &optional name)
  (if full (cons full name)))

(defun mew-assoc-folder-setup ()
  (if mew-use-fast-refile
      (setq mew-assoc-folder 'mew-assoc-folder-fast)
    (setq mew-assoc-folder 'mew-assoc-folder-slow)))

(defun mew-assoc-folder-slow (key alist sep)
  ;; Case-sensitive so you can use capital letters for your folders.
  ;; But this is slow.
  (let ((skey (downcase key))
	(node-regex (concat (regexp-quote sep) "$"))
	name ret first)
    (catch 'loop
      (dolist (al alist)
	(setq ret al)
	(setq name (cdr ret))
	(when (and (stringp name) (string= (downcase name) skey))
	  (unless first
	    (setq first ret))
	  (unless (string-match node-regex (car ret))
	    (throw 'loop nil)))
	(setq ret nil)))
    (if mew-use-node-folder
	(or ret first)
      ret)))

(defun mew-assoc-folder-fast (key alist sep)
  ;; Case-insensitive so you cannot use capital letters for your folders.
  ;; But this is fast.
  ;;
  ;; ("+foo/" . "foo")
  ;; ("+foo/foo" . "foo")
  (let* ((skey (downcase key))
	 (node-regex (concat (regexp-quote sep) "$"))
	 ret first)
    (catch 'loop
      (while (setq ret (rassoc skey alist))
	(unless first
	  (setq first ret))
	(unless (string-match node-regex (car ret))
	  (throw 'loop nil))
	(setq alist (cdr (member ret alist)))
	(setq ret nil)))
    (if mew-use-node-folder
	(or ret first)
      ret)))

(defun mew-refile-guess-by-folder ()
  ;; Guess folders by the To:/Cc: field with folder alist.
  ;; Mainly used for mailing-list.
  (let* ((to-cc (mew-header-parse-address-list mew-refile-guess-key-list))
	 (proto mew-inherit-refile-proto)
	 (case mew-inherit-refile-case)
	 (alist (mew-proto-folder-alist proto case))
	 sep ent ret ml-addr ml-name)
    (cond
     ((mew-folder-imapp proto)
      (setq sep (mew-imap-separator case)))
     (t
      (setq sep mew-path-separator)))
    (dolist (tc to-cc)
      (setq ml-addr (mew-addrstr-parse-address (or tc "")))
      (when ml-addr
	(setq ml-name (mew-addrstr-extract-user ml-addr))
	(setq ent (or (funcall mew-assoc-folder ml-addr alist sep)
		      (funcall mew-assoc-folder ml-name alist sep)))
	(mew-addq ret (nth 0 ent))))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by alist
;;;

(defun mew-refile-guess-by-alist ()
  (let ((alist (mew-refile-guess-alist mew-inherit-refile-case))
	(proto mew-inherit-refile-proto))
    ;; Guess folders by user-configured alist.
    (mew-refile-guess-by-alist1 alist proto)))

(defun mew-refile-guess-by-alist1 (rule &optional proto)
  "Return a guessed folder according to the RULE. The syntax of
RULE is as follows:
    rule      ::= '<rule>
    <rule>    ::= ((<key> <alist>) (<key> <alist>) ... [<special>])
    <alist>   ::= (<value> <folder>|<rule>) (<value> <folder>|<rule>) ...
    <special> ::= (t <folder>) | (nil <folder>)

There are two special <key>s: nil and t.

nil is used to specify <folder> to be returned when nothing is
guessed.

t can specify <folder> to be returned in addition to guessed
values."
  (let (key alist val f-or-r header ent ret fin)
    (dolist (rl rule)
      (setq key (car rl))
      (setq alist (cdr rl)) ;; must not use mew-alist-get-value
      (cond
       ((eq key t)
	(mew-addq ret (if (consp alist) (car alist) alist)))
       ((eq key nil)
	(or ret (mew-addq ret (if (consp alist) (car alist) alist))))
       ((setq header (mew-header-get-value key))
	(dolist (al alist)
	  (setq val (mew-alist-get-key al))
	  (setq f-or-r (mew-alist-get-value al))
	  (setq ent nil)
	  (when (and (stringp val) (string-match val header))
	    (cond
	     ((stringp f-or-r)
	      (setq ent (mew-refile-guess-by-alist2 val header f-or-r)))
	     ((listp f-or-r)
	      (setq ent (mew-refile-guess-by-alist1 f-or-r proto)))))
	  (when ent
	    (if (listp ent)
		(dolist (et ent)
		  (mew-addq ret et))
	      (mew-addq ret ent)))))))
    (if proto
	(dolist (ent ret fin)
	  (if (string= proto (substring ent 0 1))
	      (setq fin (cons ent fin))))
      (nreverse ret))))

(defun mew-refile-guess-by-alist2 (regex string template)
  ;; regex: "\\([^@]+\\)@\\([^.]+\\)\\.ad\\.jp"
  ;; string: "admin@example.org"
  ;; template: "+net/\\2/\\1"
  ;; -> "+net/iij/admin"
  (let (str strs match repl)
    (string-match regex string)
    (setq match (cdr (cdr (match-data))))
    (while match
      (setq str nil)
      (if (car match)
	  (setq str (substring string (car match) (car (cdr match)))))
      (setq strs (cons str strs))
      (setq match (cdr (cdr match))))
    (setq strs (cons nil (nreverse strs))) ;; cons nil for 0th
    (while (string-match "\\\\\\([1-9]\\)" template)
      (setq repl (nth (string-to-number (match-string 1 template)) strs))
      (setq template (replace-match (or repl "") nil nil template)))
    template))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by thread
;;;

(defun mew-refile-guess-by-thread ()
  ;; Guess folders by thread with alist created by
  ;; mew-refile-guess-by-thread-learn.
  (let ((msgid (or (mew-idstr-get-last-id
		    (mew-header-get-value mew-references:))
		   (mew-idstr-get-first-id
		    (mew-header-get-value mew-in-reply-to:))))
	(proto mew-inherit-refile-proto))
    (when msgid
      (let ((ret (nth 1 (assoc msgid mew-refile-msgid-alist))))
	(if (and ret (string= proto (substring ret 0 1)))
	    ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by from folder
;;;

(defun mew-refile-guess-by-from-folder (&optional addr)
  ;; Guess folders by the From: field with folders under +from.
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-from-folder-body addr))

(defun mew-refile-guess-by-from-folder-body (&optional addr)
  (let* ((proto mew-inherit-refile-proto)
	 (case mew-inherit-refile-case)
	 (list (mew-proto-friend-folder-list proto case))
	 (sep (if (mew-folder-imapp proto)
		  (mew-imap-separator case)
		mew-path-separator))
	 (qsep (regexp-quote sep))
	 (from (downcase (or addr (mew-header-parse-address mew-from:) "")))
	 (user (mew-addrstr-extract-user from))
	 (from-regex (concat qsep (regexp-quote from) "$"))
	 (user-regex (concat qsep (regexp-quote user) "$")))
    (or
     (mew-member-match2 from-regex list)
     (mew-member-match2 user-regex list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by from
;;;

(defun mew-refile-guess-by-from (&optional addr)
  ;; Guess folders by the From: field with alist created by
  ;; mew-refile-guess-by-from-learn.
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-from-body addr))

(defun mew-refile-guess-by-from-body (&optional addr)
  (let ((from (downcase (or addr (mew-header-parse-address mew-from:) "")))
	(proto mew-inherit-refile-proto))
    (when from
      (let ((ret (cdr (assoc from mew-refile-from-alist))))
	(if (and ret (string= proto (substring ret 0 1)))
	    ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by newsgroup
;;;

(defun mew-refile-guess-by-newsgroups ()
  ;; Guess folders by the Newsgroups field with folder alist.
  (let* ((newsgroups (mew-addrstr-parse-value-list2
		     (mew-header-get-value mew-newsgroups:)))
	 (proto mew-inherit-refile-proto)
	 (case mew-inherit-refile-case)
	 (alist (mew-proto-folder-alist proto case)) ;; local folders
	 ent ret)
    (dolist (newsgroup newsgroups)
      (setq ent (funcall mew-assoc-folder newsgroup alist mew-path-separator))
      (mew-addq ret (nth 0 ent)))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by default
;;;

(defun mew-refile-guess-by-default (&optional addr)
  ;; Concat +from and user.
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-default-body addr))

(defun mew-refile-guess-by-default-body (&optional addr)
  (let* ((proto mew-inherit-refile-proto)
	 (case mew-inherit-refile-case)
	 (fld (mew-proto-friend-folder proto case))
	 (from (downcase (or addr (mew-header-parse-address mew-from:) ""))))
    (if mew-refile-guess-strip-domainpart
	(setq from (mew-addrstr-extract-user from)))
    (mew-concat-folder2 fld from case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Learning functions
;;;

;; mew-refile-guess-learn (buf result)
;;
;; buf is message buffer.
;;
;; result is ((chosen1 chosen2 ...)
;;           (guess-func-name1 guess1 guess2...)
;;           (guess-func-name2 guess1 guess2...))
;;
;; that is, 'car' is a list of user chosen folders.
;;          'cdr' is a list of opinions by guess functions.
;;

(defun mew-refile-guess-learn (buf result)
  (let ((chosen (car result))  ;; (folder1 folder2 ...)
	(info (cdr result))) ;; (guess-func-name guess1 guess2...)
    (with-current-buffer buf
      (if (member 'mew-refile-guess-by-from mew-refile-guess-control)
	  (mew-refile-guess-by-from-learn chosen info))
      (if (member 'mew-refile-guess-by-thread mew-refile-guess-control)
	  (mew-refile-guess-by-thread-learn chosen info)))))

(defun mew-refile-guess-by-thread-learn (chosen info)
  ;; Create mew-refile-msgid-alist for mew-refile-guess-by-thread.
  (let* ((msgid (mew-idstr-get-first-id
		 (mew-header-get-value mew-message-id:)))
	 (folder (car chosen))
	 ;; other people's honest opinion and my honest opinion.
	 (oho info)
	 (mho (cdr (assoc 'mew-refile-guess-by-thread info))))
    (when (and msgid chosen)
      ;; if my opinion was right, I learn it.
      ;; or a folder was not in other people's opinion,
      ;; I accept it.
      (catch 'match
	(dolist (csn chosen)
	  (if (or (member csn mho)
		  (not (catch 'find
			 (while oho
			   (and (member csn (car oho)) (throw 'find t))
			   (setq oho (cdr oho))))))
	      (throw 'match (setq folder csn)))))
      (setq mew-refile-msgid-alist
	    (cons (list msgid folder "??")
		  (delq (assoc msgid mew-refile-msgid-alist) ;; delq is right
			mew-refile-msgid-alist))))))

(defun mew-refile-guess-by-from-learn (chosen info)
  ;; Create mew-refile-from-alist for mew-refile-guess-by-from.
  (let* ((from (downcase (or (mew-header-parse-address mew-from:) "")))
	 ;; 'my honest opinion' guessed by mew-refile-guess-by-from.
	 (mho (nth 1 (assoc 'mew-refile-guess-by-from info)))
	 folder to-cc)
    ;; default leaning key X is the address derived from From: header,
    ;; but only when
    ;;    mew-refile-guess-from-me-is-special is t
    ;;    X is my address.
    ;;    All addresses derived from To: Cc: values
    ;;      point to the only one address Y.
    ;; the learning key is set to Y instead of X.
    (if (and mew-refile-guess-from-me-is-special
	     (mew-is-my-address mew-regex-my-address-list from)
	     (setq to-cc
		   (mew-header-parse-address-list mew-refile-guess-key-list))
	     (= (length to-cc) 1))
	(setq from (car to-cc)))
    (unless (or (or (null from) (null chosen)) (and mho (member mho chosen)))
      ;; I decide which folder is most important among the user chosen
      ;; folders.
      (catch 'match
	(dolist (csn chosen)
	  ;; searching a folder anyone couldn't predict.
	  (unless (mew-member* csn info)
	    (throw 'match (setq folder csn)))))
      (run-hooks 'mew-refile-guess-by-from-learn-hook)
      ;; If candidate was found, I memorize it.
      (when folder
	(setq mew-refile-from-alist
	      (cons (cons from folder)
		    (delq (assoc from mew-refile-from-alist) ;; delq is right
			  mew-refile-from-alist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode
;;;

(defun mew-refile-get (msg)
  (assoc msg (mew-sinfo-get-refile)))

(defun mew-refile-set (msg folders)
  (mew-sinfo-set-refile (cons (cons msg folders) (mew-sinfo-get-refile))))

(defun mew-refile-reset (msg)
  (mew-sinfo-set-refile
   (delq (mew-refile-get msg) (mew-sinfo-get-refile))))

(defun mew-refile-change (src dst)
  (let ((ref (mew-refile-get src)))
    (when ref (setcar ref dst))))

(defun mew-refile-folder-check (folder &optional force-to-create)
  "A function to see if FOLDER exists.
Return t if exists or created. Otherwise, return nil."
  (when (stringp folder)
    (cond
     ((mew-folder-popp  folder) nil)
     ((mew-folder-nntpp folder) nil)
     ((mew-folder-virtualp folder) nil)
     ((mew-folder-imapp folder)
      (let* ((case mew-inherit-refile-case)
	     (mailboxes (mew-imap-folder-alist case)))
	(cond
	 ((or (assoc folder mailboxes)
	      (assoc (mew-imap-file-name-as-directory folder case) mailboxes))
	  t)
	 ;; This is very ad-hoc but no right way...
	 ((and (string= (mew-imap-separator case) ".")
	       (string-match mew-path-separator folder))
	  (message "%s should not contain \"%s\"" folder mew-path-separator)
	  nil)
	 ((and (y-or-n-p (format "%s does not exist. Create it? " folder))
	       (mew-imap-folder-insert case folder))
	  (message "%s will be created" folder)
	  t))))
     (t ;; local
      (let ((absdir (mew-expand-folder folder))  ;; /home/Mail/foo
	    (create-it force-to-create))
	(when absdir
	  (if (file-exists-p absdir)
	      (if (file-directory-p absdir)
		  t ;; exists
		(message "%s is a file" folder)
		nil) ;; xxx exists but a file
	    (unless create-it
	      (if (y-or-n-p (format "%s does not exist. Create it? " folder))
		  (setq create-it t)))
	    (if (not create-it)
		nil ;; not created
	      (mew-make-directory absdir)
	      (mew-local-folder-insert folder)
	      (message "%s has been created" folder)
	      t)))))))) ;; created

(defun mew-refile-decide-folders (buf msg cur-folders &optional auto exfld)
  ;; This functions returns
  ;;  ((folder1 folder2...)
  ;;   (func1 guess11 guess12...)  ;; for learning
  ;;   (func2 guess12 guess22...))
  (let ((proto mew-inherit-refile-proto)
	(case mew-inherit-refile-case)
	learn-info folders ret cands singlep lst-lst)
    (with-current-buffer buf
      (setq learn-info (mew-refile-guess auto)))
    (if auto
	;; if auto is set, simply use the guess.
	(setq folders (car learn-info))
      (cond
       (cur-folders
	;; add new folder
	(setq cands cur-folders))
       ((nth 1 (car learn-info))
	;; multi guess
	(setq cands (car learn-info)))
       (t
	;; single guess
	(setq singlep t)
	(setq cands (list (nth 0 (car learn-info))))))
      (setq cands (delete nil cands))
      (when exfld
	;; copying, two folders are necessary
	(setq singlep nil)
	(if (or (null cands) (equal (list exfld) cands))
	    (setq cands (list exfld proto))
	  (setq cands (cons exfld cands))))
      (setq cands (mew-uniq-list cands)) ;; unavoidable
      (unless cands
	(setq cands (list proto)))
      (setq folders (mew-input-refile-folders cands singlep case proto)))
    ;; check folder existence.
    (setq lst-lst (mapcar (lambda (x) (mew-split x ?,)) folders))
    (dolist (lst lst-lst)
      (dolist (fld lst)
	(if (and fld (not (member fld ret)) (mew-refile-folder-check fld))
	    (setq ret (cons fld ret)))))
    (cons (nreverse ret) (cdr learn-info)))) ;; return value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copy
;;;

(defun mew-summary-copy ()
  "Put the refile mark(default is 'o') on this message with
the current folder as a candidate in addition to guessed folders."
 (interactive)
 (mew-summary-msg-or-part
  (mew-summary-refilable
   (mew-summary-refile-body
    nil nil nil nil (mew-summary-folder-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refile (aka move)
;;;

(defun mew-refile-log-buffer (fld)
  (concat mew-buffer-prefix "refile " fld))

(defun mew-summary-refile (&optional report)
 "Put the refile mark(default is 'o') on this message. If already
marked with 'o', it prints where this message will be refiled. This
can overlay other marks. When it overlays, the cursor stays on the
message. If it marks newly, displays the next message. If executed
with '\\[universal-argument]', it displays how the refile rules work in Message mode."
 (interactive "P")
 (mew-summary-msg-or-part
  (mew-summary-refilable
   (if report (mew-summary-refile-report) (mew-summary-refile-body)))))

(defvar mew-override-body-open "<")
(defvar mew-override-body-close "> ")
(defconst mew-regex-override-body
  (concat (regexp-quote mew-override-body-open)
	  "[^>\r\n]*"
	  (regexp-quote mew-override-body-close)))

(defun mew-summary-refile-override-body (folders-str &optional force)
  (let* ((open mew-override-body-open)
	 (close mew-override-body-close)
	 (len (+ (length folders-str) (length open) (length close))))
    (save-excursion
      (when (mew-summary-goto-body 'after)
	(when (and (not force) (looking-at mew-regex-override-body))
	  (mew-elet
	   (delete-region (point) (match-end 0))))
	(mew-elet
	 (insert-and-inherit open folders-str close))))
    len))

(defun mew-summary-refile-remove-body ()
  (save-excursion
    (when (and (mew-summary-goto-body 'after)
	       (looking-at mew-regex-override-body))
      (mew-elet
       (delete-region (point) (match-end 0))))))

(defun mew-summary-refile-log (fld folders-str)
  (save-excursion
    (beginning-of-line)
    (let ((beg (point))
	  (cbuf (current-buffer))
	  (msg-id (mew-summary-my-id))
	  end)
      (forward-line)
      (when (search-backward "\r" nil t)
	(setq end (point))
	(set-buffer (get-buffer-create (mew-refile-log-buffer fld)))
	(goto-char (point-max))
	(mew-insert-buffer-substring cbuf beg end)
	(insert "== src=" fld " dst=" folders-str " id=" msg-id)
	(insert " date=" (mew-time-ctz-to-logtime (current-time)) "\n")))))

(defun mew-summary-refile-unlog (fld)
  (save-excursion
    (let ((msg-id (mew-summary-my-id))
	  start)
      (set-buffer (get-buffer-create (mew-refile-log-buffer fld)))
      (goto-char (point-min))
      (when (search-forward (concat " id=" msg-id) nil t)
	(beginning-of-line)
	(setq start (point))
	(forward-line)
	(delete-region start (point))))))

(defun mew-refile-decode-subject ()
  (save-excursion
    (goto-char (point-min))
    (if (and (re-search-forward (concat "^$\\|^" mew-subj:) nil t)
	     (not (looking-at "^$")))
	(let ((med (point)))
	  (forward-line)
	  (mew-header-goto-next)
	  (mew-header-decode-region mew-subj: med (point))))))

(defun mew-summary-refile-body (&optional exp-flds auto no-msg no-mark exfld)
  (let ((pos (point))
	fld msg folders cur-folders mark
	buf learn-info tmp delbuf invalidp folders-str
	mew-inherit-refile-proto mew-inherit-refile-case)
    (mew-summary-goto-message)
    (when (mew-sumsyn-match mew-regex-sumsyn-short)
      (setq fld (mew-sumsyn-folder-name))
      (setq msg (mew-sumsyn-message-number)) ;; msg is never nil
      (setq invalidp (not (mew-msg-validp msg))))
    (setq mew-inherit-refile-case (or (mew-case:folder-case fld)
				      mew-case))
    (setq mew-inherit-refile-proto (substring (mew-case:folder-folder fld) 0 1))
    (setq mark (mew-summary-get-mark)) ;; any mark
    (cond
     ((and mark (> (mew-markdb-level mark) (mew-markdb-level mew-mark-refile)))
      (or no-msg
	  (message "Cannot mark here because '%s' is stronger than '%s'"
		   (mew-markdb-name mark) (mew-markdb-name mew-mark-refile)))
      nil)
     (invalidp
      (or no-msg (message "Cannot refile this invalid message"))
      nil)
     (t
      (if exp-flds
	  (setq folders exp-flds)
	(unless (or auto (mew-sinfo-get-disp-msg))
	  ;; need to make a cache or a message buffer.
	  (mew-summary-display))
	(setq buf (mew-cache-hit fld msg))
	(unless buf
	  (save-excursion
	    (setq buf (generate-new-buffer mew-buffer-prefix))
	    (setq delbuf t)
	    (set-buffer buf)
	    (mew-erase-buffer)
	    (mew-insert-message
	     fld msg mew-cs-text-for-read mew-header-reasonable-size)
	    (mew-refile-decode-subject)))
	(when (and (eq mew-mark-refile mark) (get-buffer fld))
	  (with-current-buffer fld
	    (setq cur-folders (cdr (mew-refile-get msg)))))
	(condition-case nil
	    (setq learn-info (mew-refile-decide-folders
			      buf msg cur-folders auto exfld))
	  (quit (goto-char pos)))
	(setq folders (car learn-info)))
      ;; we must prevent refiling a message to +queue
      (dolist (folder folders)
	(unless (mew-folder-queuep folder)
	  (setq tmp (cons folder tmp))))
      (setq folders (nreverse tmp))
      (setq folders (delete mew-draft-folder folders))
      ;; mark refile
      (unless no-mark
	(when folders
	  (or exp-flds auto (mew-refile-guess-learn buf learn-info))
	  (setq folders-str (mew-join "," folders))
	  (cond
	   ((mew-virtual-p)
	    (with-current-buffer fld
	      (save-excursion
		(when (mew-summary-search-msg msg)
		  (mew-refile-reset msg)
		  (mew-refile-set msg folders)
		  (if cur-folders (mew-summary-refile-unlog fld))
		  (mew-summary-refile-log fld folders-str)
		  (mew-summary-refile-override-body folders-str)
		  ;; marked in mew-mark-put-mark
		  (set-buffer-modified-p nil)))))
	   (t
	    (mew-refile-reset msg)
	    (mew-refile-set msg folders)
	    (if cur-folders (mew-summary-refile-unlog fld))
	    (mew-summary-refile-log fld folders-str)))
	  (mew-summary-refile-override-body folders-str)
	  (mew-mark-put-mark mew-mark-refile no-msg)
	  (setq pos (point))))
      (if delbuf (mew-remove-buffer buf))
      ;; memorize last-folder
      (setq mew-refile-last-folder folders)
      (set-buffer-modified-p nil)))
    (goto-char pos)
    folders)) ;; return value

(defun mew-summary-refile-report ()
  (let ((win (selected-window))
	(customize-var '(mew-refile-ctrl-multi
			 mew-refile-guess-key-list
			 mew-refile-guess-strip-domainpart
			 mew-refile-guess-from-me-is-special))
	mew-inherit-refile-case
	mew-inherit-refile-proto
	fld msg guess)
    (save-excursion
      (mew-summary-goto-message)
      (when (mew-sumsyn-match mew-regex-sumsyn-short)
	(setq fld (mew-sumsyn-folder-name))
	(setq msg (mew-sumsyn-message-number))))
    (setq mew-inherit-refile-case (or (mew-case:folder-case fld)
				      mew-case))
    (setq mew-inherit-refile-proto (substring (mew-case:folder-folder fld) 0 1))
    (with-temp-buffer
      (mew-insert-message
       fld msg mew-cs-text-for-read mew-header-reasonable-size)
      (mew-refile-decode-subject)
      (setq guess (mew-refile-guess nil t)))
    (mew-window-configure 'message)
    ;; message buffer
    (mew-summary-display-preamble)
    (mew-elet
     (save-excursion
       ;; report result of guess.
       (insert (format "** Guess result: %s\n" (car guess)))
       ;; report status of customize variables.
       (insert "\n** Current Configurations:\n\n")
       (dolist (cvar customize-var)
	 (insert (format "%-40s:  " cvar))
	 (insert (format "%s\n"     (eval cvar))))
       (insert "\n** Each function's opinion:\n\n")
       ;; report how each functions guessed.
       (setq guess (cdr guess))
       (dolist (gs guess)
	 (insert (format "%-32s  " (car gs)))
	 (insert (format "return: %s\n"
			 (mapconcat 'identity (cdr gs) ","))))))
    (mew-summary-display-postscript 'no-hook)
    (select-window win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refile again
;;;

(defun mew-summary-refile-again ()
  "Put a refile mark on this message according to the previous
refile folder."
  (interactive)
  (mew-summary-refilable
   (save-excursion
     (mew-summary-goto-message)
     (let ((msg (mew-summary-message-number)))
       (when (mew-msg-validp msg)
	 (message "%s to %s" msg (mew-join ", " mew-refile-last-folder)))))
   (mew-summary-refile-body mew-refile-last-folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auto refile
;;;

(defun mew-summary-auto-refile (&optional mew-mark-review-only)
  "Refile each message in the folder automatically. If
'mew-refile-auto-refile-skip-any-mark' is non-nil,
any previously marked message will be skipped.
If '\\[universal-argument]' is specified, only messages marked with
'mew-mark-review' will be concerned."
  (interactive "P")
  (mew-summary-refilable
   (mew-decode-syntax-delete)
   (let ((mew-use-highlight-x-face nil)
	 (lines (mew-sinfo-get-ttl-line))
	 (case-fold-search nil)
	 (line 1) (mark nil) msg)
     (cond
      (mew-mark-review-only
       (setq msg (format "Refile all messages marked with '%c'? "
			 mew-mark-review)))
      (mew-refile-auto-refile-skip-any-mark
       (setq msg "Refile all non-marked messages? "))
      (t
       (setq msg "Refile messages including marked with weak marks?")))
     (if (and mew-refile-auto-refile-confirm (not (y-or-n-p msg)))
	 (message "Not refiled")
       (message "Auto refiling...")
       (save-window-excursion
	 (goto-char (point-min))
	 (while (re-search-forward mew-regex-sumsyn-valid nil t)
	   (setq mark (mew-summary-get-mark))
	   (if mew-mark-review-only
	       (and mark
		    (char-equal mark mew-mark-review)
		    (mew-summary-refile-body nil t 'no-msg))
	     (or (and mark
		      (or mew-refile-auto-refile-skip-any-mark
			  (>= (mew-markdb-level mark)
			      (mew-markdb-level mew-mark-refile))))
		 (mew-summary-refile-body nil t 'no-msg)))
	   (if (= (% (/ (* 100 line) lines) 10) 0)
	       (message "Auto refiling...%s%%" (/ (* 100 line) lines)))
	   (setq line (1+ line))
	   (forward-line)))
       (message "Auto refiling...done")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; "mx" extension
;;;

(defun mew-summary-mark-copy ()
  "\\<mew-summary-mode-map>
Put the refile mark onto all messages marked with '*',
with the current folder as a candidate in addition to guessed folders.
This is very convenient to refile all messages  picked by '\\[mew-summary-pick]'."
  (interactive)
  (mew-summary-refilable
   (let ((mew-use-highlight-x-face nil)
	 last)
     (message "Mark copying...")
     (save-excursion
       (save-window-excursion
	 (goto-char (point-min))
	 (catch 'loop
	   (while (re-search-forward mew-regex-msg-review nil t)
	     (setq last (mew-summary-refile-body
			 last nil 'no-msg nil (mew-summary-folder-name 'ext)))
	     (unless last (throw 'loop t))
	     (forward-line)))
	 (message "Mark copying...done"))))))

(defun mew-summary-mark-refile ()
  "\\<mew-summary-mode-map>
Put the refile mark onto all messages marked with '*'.
This is very convenient to refile all messages picked by '\\[mew-summary-pick]'."
  (interactive)
  (mew-summary-refilable
   (let ((mew-use-highlight-x-face nil)
	 last)
     (message "Mark refiling...")
     (save-excursion
       (save-window-excursion
	 (catch 'loop
	   (while (or (re-search-forward  mew-regex-msg-review nil t)
		      (re-search-backward mew-regex-msg-review nil t))
	     (setq last (mew-summary-refile-body last nil 'no-msg))
	     (unless last (throw 'loop t))
	     (forward-line)))
	 (message "Mark refiling...done"))))))

(provide 'mew-refile)

;;; Copyright Notice:

;; Copyright (C) 1994-2010 Mew developing team.
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

;;; mew-refile.el ends here
