;;; mew-complete.el --- Completion magic for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: May 30, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low level functions
;;;

(defun mew-draft-on-field-p ()
  (if (bolp)
      (if (bobp)
	  t
	(save-excursion
	  (forward-line -1)
	  (if (looking-at ".*,[ \t]?$") nil t)))
    (let ((pos (point)))
      (save-excursion
	(beginning-of-line)
	(if (looking-at mew-lwsp)
	    nil
	  (if (search-forward ":" pos t) nil t))))))

(defun mew-draft-on-value-p (switch)
  (save-excursion
    (beginning-of-line)
    (while (and (< (point-min) (point))	(looking-at mew-lwsp))
      (forward-line -1))
    (if (looking-at "\\([^:]*:\\)")
	(mew-field-get-func (match-string 1) switch)
      nil))) ;; what a case reaches here?

;;
;; Window management for completion candidates
;;

(defvar mew-complete-candidates nil)

(defun mew-complete-window-delete (&optional force)
  (when (mew-ainfo-get-win-cfg)
    ;; (mew-ainfo-get-win-cfg) remains when the last completion
    ;; finished with multiple candidates.
    ;; (e.g. foo<RET> when foo and foobar are displayed.)
    ;; In this case, this function is called in another
    ;; completion thread but setting window configuration is not
    ;; desired. If we set window configuration with the old
    ;; (mew-ainfo-get-win-cfg), the cursor jumps to mini buffer.
    ;; This was a stupid bug of Mew. So, let's see if the complete
    ;; buffer is displayed or not.
    (if (or force (get-buffer-window mew-buffer-completions))
	(set-window-configuration (mew-ainfo-get-win-cfg)))
    (mew-ainfo-set-win-cfg nil))
  (mew-remove-buffer mew-buffer-completions)
  (setq mew-complete-candidates nil))

(defun mew-complete-insert-folder-function (choice buffer mini-p base-size)
  (let ((start (mew-minibuf-point-min))
	(proto (substring choice 0 1))
	(pos (point)))
    (while (not (or (= start (point))
		    (not (char-before))
		    (char-equal (char-before) ?,)))
      (forward-char -1))
    (if (and (member proto mew-folder-prefixes)
	     (looking-at (concat "\\("
				 (regexp-opt mew-config-cases t)
				 ":\\)"
				 (regexp-quote proto))))
	(progn
	  (delete-region (match-end 1) pos)
	  (goto-char (match-end 1)))
      (delete-region (point) pos))
    (insert choice)
    (remove-text-properties start (point-max) '(mouse-face nil))
    (mew-complete-window-delete 'force)
    t))

(defun mew-complete-window-show (all)
  (unless (mew-ainfo-get-win-cfg)
    (mew-ainfo-set-win-cfg (current-window-configuration)))
  (if (and (get-buffer-window mew-buffer-completions)
	   (equal mew-complete-candidates all))
      (let ((win (get-buffer-window mew-buffer-completions)))
	(with-current-buffer mew-buffer-completions
	  (if (pos-visible-in-window-p (point-max) win)
	      (set-window-start win 1)
	    (scroll-other-window))))
    (setq mew-complete-candidates all)
    (with-output-to-temp-buffer mew-buffer-completions
      (when mew-inherit-complete-folder
	(make-local-variable 'choose-completion-string-functions)
	(add-hook 'choose-completion-string-functions
		  'mew-complete-insert-folder-function))
      (display-completion-list all))))

(defun mew-complete-backscroll ()
  "Backscroll the *Completion* buffer."
  (interactive)
  (let* ((win (get-buffer-window mew-buffer-completions))
	 (height (and win (window-height win))))
    (and win (scroll-other-window (- 3 height)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion function for a draft only
;;;

(defun mew-draft-set-completion-ignore-case (case)
  ;; Need to set the global variable "completion-ignore-case",
  ;; since clicking a candidate on a completion buffer checks
  ;; the global variable.
  ;; Yes, this has side-effect.
  (when (mew-draft-or-header-p)
    (setq completion-ignore-case case)))

(defun mew-draft-header-comp ()
  "Complete and expand address short names.
First, a short name is completed. When completed solely or the @ character
is inserted before the cursor, the short name is expanded to its address."
  (interactive)
  (if (mew-draft-on-field-p)
      (mew-complete-field)
    (let ((func (mew-draft-on-value-p mew-field-completion-switch)))
      (if func
	  (funcall func)
	(tab-to-tab-stop))))) ;; default keybinding

(defun mew-complete-field ()
  "Field complete function."
  (interactive)
  (let ((word (mew-delete-key))) ;; capitalized
    (if (null word)
	(mew-complete-window-show mew-fields)
      (mew-complete
       word
       (mapcar (lambda (x) (list (concat (mew-capitalize x) " "))) mew-fields)
       "field"
       nil))))

(defun mew-complete-newsgroups ()
  "Newsgroup complete function."
  (interactive)
  (let ((word (mew-delete-backward-char)))
    (if (null word)
	(tab-to-tab-stop)
      (mew-complete
       word
       (mew-nntp-folder-alist2 (mew-tinfo-get-case))
       "newsgroup"
       nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion function for both a draft and the minibuffer
;;;

(defun mew-complete-address ()
  "Complete and expand an address short name.
First alias key is completed. When completed solely or the @ character
is inserted before the cursor, the short name is expanded to its address."
  (interactive)
  (mew-draft-set-completion-ignore-case mew-complete-address-ignore-case)
  (let ((word (mew-delete-backward-char))
	(completion-ignore-case mew-complete-address-ignore-case))
    (if (null word)
	(tab-to-tab-stop)
      (if mew-use-full-alias
	  (mew-complete
	   word mew-addrbook-alist "alias" nil nil nil
	   'mew-addrbook-alias-get
	   'mew-addrbook-alias-hit)
	(if (string-match "@." word)
	    (insert (or (mew-addrbook-alias-next word mew-addrbook-alist) word))
	  (mew-complete
	   word mew-addrbook-alist "alias" ?@ nil nil
	   'mew-addrbook-alias-get
	   'mew-addrbook-alias-hit))))))

(defun mew-draft-addrbook-expand ()
  (interactive)
  (mew-draft-set-completion-ignore-case mew-complete-address-ignore-case)
  (let ((word (mew-delete-backward-char))
	(completion-ignore-case mew-complete-address-ignore-case)
	try)
    (if (null word)
	(message "No expand key")
      (setq try (try-completion word mew-addrbook-alist))
      (if (or (eq try t)
	      (and (stringp try) (string= word try)))
	  (insert (mew-addrbook-alias-get word mew-addrbook-alist))
	(insert word)
	(message "'%s' cannot be expanded" word)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completing folders
;;;

(defmacro mew-complete-proto-folder (sym &rest body)
  ;; (declare (indent 1))
  `(if mew-input-folder-search-direction
       (mew-input-folder-search-complete)
     (mew-draft-set-completion-ignore-case mew-complete-folder-ignore-case)
     (let ((,sym (mew-delete-backward-char))
	   (completion-ignore-case mew-complete-folder-ignore-case)
	   (mew-inherit-complete-folder t))
       ,@body)))

(put 'mew-complete-proto-folder 'lisp-indent-function 1)

(defun mew-complete-local-folder ()
  "Local folder complete function."
  (interactive)
  (mew-complete-proto-folder word
    (if (null word)
       (mew-complete-window-show (list "+"))
     (if (and (mew-folder-absolutep word)
	      (not (mew-draft-or-header-p)))
	 (mew-complete word (mew-complete-directory-alist word) "directory" nil)
       (mew-complete word (mew-local-folder-alist) "folder" nil)))))

;; case is specified by mew-inherit-case.
(defun mew-complete-imap-folder ()
  "IMAP folder complete function."
  (interactive)
  (mew-complete-proto-folder word
    (if (null word)
	(mew-complete-window-show (list "%"))
      (mew-complete
       word
       (mew-imap-folder-alist mew-inherit-case) ;; ie mew-sinfo-get-case
       "mailbox"
       nil))))

(defun mew-complete-fcc-folder ()
  "Fcc: folder complete function."
  (interactive)
  (mew-complete-proto-folder word
    (if (null word)
	(mew-complete-window-show (list "+" "%"))
      (cond
       ((and (mew-folder-absolutep word) (not (mew-draft-or-header-p)))
	(mew-complete word (mew-complete-directory-alist word) "directory" nil))
       ((mew-folder-imapp word)
	(mew-complete word (mew-imap-folder-alist (mew-tinfo-get-case)) "mailbox" nil))
       (t
	(mew-complete word (mew-local-folder-alist) "folder" nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion function for the minibuffer only
;;;

(defun mew-complete-folder ()
  "Folder complete function."
  (interactive)
  (if mew-input-folder-search-direction
      (mew-input-folder-search-complete)
    (mew-complete-folder2)))

(defun mew-input-folder-search-complete ()
  (let ((mew-inherit-complete-folder t)
	keys)
    (with-current-buffer mew-input-folder-search-buf
      (save-excursion
	(goto-char (point-min))
	(while (search-forward (or mew-input-folder-search-key "\n") nil t)
	  (setq keys
		(cons (buffer-substring (progn (beginning-of-line) (point))
					(progn (end-of-line) (point)))
		      keys)))))
    (mew-complete-window-show (nreverse (delete "" keys)))
    (mew-highlight-folder-comp-search-window)))

(defun mew-complete-folder2 ()
  (let ((word (mew-delete-backward-char nil ", \t\n"))
	(completion-ignore-case mew-complete-folder-ignore-case)
	(mew-inherit-complete-folder t)
	case folder)
    (cond
     ((null word)
      (mew-complete-window-show mew-config-cases2))
     ((setq case (mew-case:folder-case word))
      (setq folder (mew-case:folder-folder word))
      (cond
       ((mew-folder-localp folder)
	(mew-complete2 folder (mew-local-folder-alist) case))
       ((mew-folder-popp folder)
	(mew-complete2 folder (mew-pop-folder-alist) case))
       ((mew-folder-nntpp folder)
	(mew-complete2 folder (mew-nntp-folder-alist case) case))
       ((mew-folder-imapp folder)
	(mew-complete2 folder (mew-imap-folder-alist case) case))
       ((mew-folder-virtualp folder)
	(mew-complete
	 word (mew-buffer-list "^\\*" t 'mew-virtual-mode) "folder" nil))
       ((string= folder "")
	(insert word)
	(mew-complete-window-show
	 (mapcar (lambda (x) (concat case ":" x)) mew-folder-prefixes)))
       (t
	(insert word)
	(if (window-minibuffer-p (get-buffer-window (current-buffer)))
	    (mew-temp-minibuffer-message " [No matching folder]")
	  (message "No matching folder")))))
     (t
      (cond
       ((mew-folder-localp word)
	(mew-complete word (mew-local-folder-alist) "folder" nil))
       ((mew-folder-popp word)
	(mew-complete word (mew-pop-folder-alist) "folder" nil))
       ((mew-folder-nntpp word)
	(mew-complete word (mew-nntp-folder-alist nil) "newsgroup" nil))
       ((mew-folder-imapp word)
	(mew-complete word (mew-imap-folder-alist nil) "mailbox" nil))
       ((mew-folder-virtualp word)
	(mew-complete
	 word (mew-buffer-list "^\\*" t 'mew-virtual-mode) "folder" nil))
       ((mew-folder-absolutep word)
	(mew-complete word (mew-complete-directory-alist word) "directory" nil))
       (t
	(mew-complete
	 word
	 (mapcar (lambda (x) (list (concat x ":")))  mew-config-cases)
	 "case"
	 nil)))))))

(defun mew-complete-case ()
  "Complete function for cases."
  (interactive)
  (let ((word (or (mew-delete-backward-char) ""))
	(completion-ignore-case mew-complete-case-ignore-case))
    (mew-complete
     word
     (mapcar 'list mew-config-cases)
     "case"
     nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Circular completion function for a draft only
;;;

(defun mew-draft-circular-comp ()
  "Switch function for circular complete functions."
  (interactive)
  (let ((func (mew-draft-on-value-p mew-field-circular-completion-switch)))
    (if func
	(funcall func)
      (message "No circular completion here"))))

(defun mew-circular-complete-domain ()
  "Circular completion of domains for To:, Cc:, etc.
If the @ character does not exist, the first value of
mew-mail-domain-list is inserted. If exists, the next value of
mew-mail-domain-list concerned with the string between @ and
the cursor is inserted."
  (interactive)
  (mew-draft-set-completion-ignore-case
   mew-circular-complete-domain-ignore-case)
  (let ((word (mew-delete-backward-char "@"))
	(completion-ignore-case mew-circular-complete-domain-ignore-case))
    (cond
     ((eq word nil) ;; @ does not exist.
      (if (null mew-mail-domain-list)
	  (message "For domain circular completion, set mew-mail-domain-list")
	(insert "@")
	(insert (car mew-mail-domain-list))
	(mew-complete-window-delete)))
     ((eq word t) ;; just after @
      (if (null mew-mail-domain-list)
	  (message "For domain circular completion, set mew-mail-domain-list")
	(insert (car mew-mail-domain-list))
	(mew-complete-window-delete)))
     (t
      ;; cannot use mew-get-next since completion is necessary sometime.
      (mew-complete
       word
       (mew-slide-pair mew-mail-domain-list)
       "domain"
       t))))) ;; use cdr

(defun mew-circular-complete (msg sym &optional minibuf) ;; xxx msg
  "General circular complete function."
  (interactive)
  (let ((name (symbol-name sym))
	(val (symbol-value sym))
	str alst match)
    (if (null val)
	(mew-temp-minibuffer-message (format "[Set '%s']" name))
      (setq str (mew-delete-value nil minibuf))
      (setq alst (mew-slide-pair val))
      (if (or (null str) ;; draft
	      (and (string= str "") (null (assoc "" alst)))) ;; minibuf
	  (insert (car val))
	(setq match (assoc str alst))
	(if match
	    (insert (cdr match))
	  (insert str)
	  (mew-temp-minibuffer-message (format "[No matching %s]" msg)))))))

(defun mew-circular-complete-from ()
  "Circular complete function for From:."
  (interactive)
  (mew-circular-complete "from" 'mew-from-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Circular completion function for the minibuffer only
;;;

(defvar mew-circular-complete-function nil)

(defun mew-circular-complete-switch ()
  "A switch function to call a function defined to
'mew-circular-complete-function'."
  (interactive)
  (if mew-circular-complete-function (funcall mew-circular-complete-function)))

(defun mew-circular-complete-pick-pattern ()
  (mew-circular-complete "pick pattern" 'mew-pick-pattern-list 'minibuf))

(defun mew-circular-complete-case ()
  (mew-circular-complete "case" 'mew-config-cases 'minibuf))

(defun mew-circular-complete-case: ()
  (cond
   ((eq mew-input-complete-function 'mew-complete-local-folder)
    ())
   (mew-input-folder-search-direction
    (mew-input-folder-self-insert))
   (t
    (let (cases oldcase newcase insert-:)
      (save-excursion
	(if (search-backward "," nil t)
	    (forward-char 1)
	  (beginning-of-line))
	(if (looking-at mew-regex-case2)
	    (progn
	      (setq oldcase (mew-match-string 1))
	      (delete-region (match-beginning 1) (match-end 1)))
	  (setq oldcase mew-case-default)
	  (setq insert-: t))
	(if (setq cases (member oldcase mew-config-cases))
	    (if (> (length cases) 1)
		(setq newcase (nth 1 cases))
	      (setq newcase (car mew-config-cases)))
	  (setq newcase mew-case-default))
	(if (string= newcase mew-case-default)
	    (unless insert-: (delete-char 1))
	  (insert newcase)
	  (if insert-: (insert ":"))))
      (if (or (= (point) (mew-minibuf-point-min))
	      (save-excursion
		(forward-char -1)
		(looking-at "[:,]")))
	  (if (search-forward "," nil t)
	      (forward-char -1)
	    (goto-char (point-max))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion for a draft only
;;;

(defun mew-draft-expand ()
  "Switch function for expand functions."
  (interactive)
  (let ((func (mew-draft-on-value-p mew-field-expansion-switch)))
    (if func
	(funcall func)
      (message "No expansion here"))))

(defun mew-expand-address ()
  "Address expansion function for To:, Cc:, etc.
'user@domain' will be expands 'name <user@domain>' if
the name exists."
  (interactive)
  (let ((word (mew-delete-backward-char)) func name)
    (if (null word)
	(message "No address here")
      (setq func (mew-addrbook-func mew-addrbook-for-address-expansion))
      (if (null func)
	  (insert word)
	(setq name (funcall func word))
	(insert (if name (format "%s <%s>" name word) word))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Other completion stuff
;;;

;; dummy
(defvar mew-ext-host "")
(defvar mew-ext-user "")

(defun mew-complete-rfile ()
  "Complete a remote file."
  (interactive)
  (let* ((path-file (mew-delete-file-name))
	 (path (car path-file))
	 (file (cdr path-file))
	 rpath)
    (setq rpath (format "/%s@%s:%s" mew-ext-user mew-ext-host path))
    (mew-complete
     file
     rpath
     "remote file"
     nil
     'mew-ext-file-name-completion
     'mew-ext-file-name-all-completions)))

(defun mew-complete-pick-pattern ()
  "Complete pick patterns."
  (interactive)
  (let* ((pat (mew-delete-pattern))
	 (clist (append '("(" "!")
			mew-pick-field-list
			(mapcar 'car mew-pick-macro-alist))))
    (if (null pat)
	(mew-complete-window-show clist)
      (mew-complete
       pat
       (mapcar 'list clist)
       "pick pattern"
       nil))))

(defun mew-complete-sort-key ()
  "Complete sort keys."
  (interactive)
  (let* ((word (mew-delete-line))
	 field alist)
    (if (string-match ":" word)
	(progn
	  ;; If WORD contains ':', change alist for completion.
	  (setq field (car (mew-split word ?:)))
	  (setq alist
		(mapcar (lambda (str) (list (concat field ":" str))) mew-sort-modes)))
      ;; Otherwise, alist is mew-sort-key-alist itself.
      (setq alist mew-sort-key-alist))
    (mew-complete word alist "sort key" nil)))

(defun mew-complete-directory-alist (dir)
  "Return alist of directories for completion."
  (let ((odir dir) odir1 dirs1 sub dirs2)
    (setq dir (mew-file-chase-links (expand-file-name dir)))
    (when (file-directory-p dir)
      (setq odir1 (file-name-as-directory odir))
      (setq dirs1 (mapcar
		   (lambda (x)
		     (when (file-directory-p (expand-file-name x dir))
		       (cons (concat odir1 (file-name-as-directory x)) x)))
		   (directory-files dir nil "[^.]" 'nosort))))
    (setq sub (file-name-nondirectory dir))
    (setq odir (file-name-directory odir))
    (setq dir (file-name-directory dir))
    (when (and dir odir sub (not (string= sub "")))
      (setq odir (file-name-as-directory odir))
      (setq dirs2 (mapcar
		   (lambda (x)
		     (when (file-directory-p (expand-file-name x dir))
		       (cons (concat odir (file-name-as-directory x)) x)))
		   (directory-files dir nil
				    (concat "^" (regexp-quote sub))
				    'nosort))))
    (sort (delq nil (append dirs2 dirs1))
	  (lambda (x y) (string< (car x) (car y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hart function for completions
;;;

(defalias 'mew-complete-hit 'assoc)

(defun mew-complete-get (key alist)
  (cdr (mew-complete-hit key alist)))

(defun mew-complete (WORD ALIST MSG EXPAND-CHAR &optional TRY ALL GET HIT)
  (let* ((ftry (or TRY 'try-completion))
	 (fall (or ALL 'all-completions))
	 (fget (or GET 'mew-complete-get))
	 (fhit (or HIT 'mew-complete-hit))
	 (cmp (funcall ftry WORD ALIST))
	 (all (funcall fall WORD ALIST))
	 (len (length WORD))
	 subkey)
    (cond
     ;; already completed
     ((eq cmp t)
      (if EXPAND-CHAR ;; may be "t"
	  (insert (funcall fget WORD ALIST)) ;; use cdr
	(insert WORD)) ;; use car
      (mew-complete-window-delete))
     ;; EXPAND
     ((and (mew-characterp EXPAND-CHAR)
	   (char-equal (aref WORD (1- len)) EXPAND-CHAR)
	   (setq subkey (substring WORD 0 (1- len)))
	   (funcall fhit subkey ALIST))
      (insert (funcall fget subkey ALIST)) ;; use cdr
      (mew-complete-window-delete))
     ;; just one candidate
     ((= 1 (length all))
      (insert cmp)
      (if (window-minibuffer-p (get-buffer-window (current-buffer)))
	  (mew-temp-minibuffer-message " [Sole completion]")
	(message "Sole completion"))
      (mew-complete-window-delete))
     ;; two or more candidates
     ((stringp cmp) ;; (length all) > 1
      (insert cmp)
      (mew-complete-window-show all)
      (if (and (mew-characterp EXPAND-CHAR) (funcall fhit cmp ALIST))
	  (message
	   "To expand '%s', type '%c' then '%s'"
	   cmp EXPAND-CHAR
	   (substitute-command-keys
	    "\\<mew-draft-header-map>\\[mew-draft-header-comp]"))))
     ;; no candidate
     (t
      (insert WORD)
      ;;(mew-complete-window-delete)
      (if (window-minibuffer-p (get-buffer-window (current-buffer)))
	  (mew-temp-minibuffer-message (format " [No matching %s]" MSG))
	(message "No matching %s" MSG))))))

(defun mew-complete2-insert (case word)
  (if case
      (insert case ":" word)
    (insert word)))

(defun mew-complete2 (word alist case)
  (let* ((cmp (try-completion word alist))
	 (all (all-completions word alist)))
    (cond
     ;; already completed
     ((eq cmp t)
      (mew-complete2-insert case word) ;; use car
      (mew-complete-window-delete))
     ;; just one candidate
     ((= 1 (length all))
      (mew-complete2-insert case cmp)
      (if (window-minibuffer-p (get-buffer-window (current-buffer)))
	  (mew-temp-minibuffer-message " [Sole completion]")
	(message "Sole completion"))
      (mew-complete-window-delete))
     ;; two or more candidates
     ((stringp cmp) ;; (length all) > 1
      (mew-complete2-insert case cmp)
      (mew-complete-window-show all))
     ;; no candidate
     (t
      (mew-complete2-insert case word)
      ;;(mew-complete-window-delete)
      (if (window-minibuffer-p (get-buffer-window (current-buffer)))
	  (mew-temp-minibuffer-message " [No matching folder]")
	(message "No matching folder"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Minibuf magic
;;;

(defun mew-temp-minibuffer-message (m)
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (insert m))
    (let ((inhibit-quit t))
      (mew-let-user-read)
      (delete-region savemax (point-max))
      (when quit-flag
	(setq quit-flag nil)
	(setq unread-command-events (list 7)))))) ;; 7 == C-g

;;
;; Extracting completion key
;;

(defun mew-delete-backward-char (&optional here sep)
  "Delete appropriate preceding word and return it."
  (interactive)
  (let ((case-fold-search t)
        (start nil)
        (end (point))
        (regex (concat "[^" (or sep mew-address-separator) "]")))
    (save-excursion
      (while (and (not (bobp))
                  (string-match regex (mew-buffer-substring (1- (point)) (point))))
        (forward-char -1))
      (if (and here (not (re-search-forward (regexp-quote here) end t)))
          nil ;; "here" does not exist.
          (setq start (point))
          (if (= start end)
              (if here t nil) ;; just after "here",  just after separator
            (prog1
                (mew-buffer-substring start end)
              (delete-region start end)))))))

(defun mew-delete-file-name ()
  (if (search-backward mew-path-separator nil t)
      (forward-char 1)
    (beginning-of-line))
  (prog1
      (cons (mew-buffer-substring (mew-minibuf-point-min) (point))
	    (mew-buffer-substring (point) (point-max)))
    (delete-region (point) (point-max))))

(defun mew-delete-pattern ()
  (let ((pos (point)))
    (if (re-search-backward " \\|(\\|&\\||\\|!\\|," nil t)
	(forward-char 1)
      (beginning-of-line))
    (prog1
	(mew-buffer-substring (point) pos)
      (delete-region (point) pos))))

(defun mew-delete-line ()
  (let ((pos (point)))
    (beginning-of-line)
    (prog1
	(mew-buffer-substring (point) pos)
      (delete-region (point) pos))))

(defun mew-delete-key ()
  (let ((pos (point)))
    (beginning-of-line)
    (prog1
	(mew-capitalize (mew-buffer-substring (point) pos))
      (delete-region (point) pos))))

(defun mew-delete-value (&optional here minibuf)
  (beginning-of-line)
  (if minibuf
      (let ((start (point)) ret)
	(end-of-line)
	(setq ret (mew-buffer-substring start (point)))
	(delete-region start (point))
	ret)
    (when (looking-at "[^:]+:")
      (goto-char (match-end 0))
      (if (looking-at "[ \t]")
	  (forward-char 1)
	(insert " "))
      (if (eolp)
	  nil
	(let ((start (point)) ret)
	  (end-of-line)
	  (if (and here (re-search-backward (regexp-quote here) start t))
	      (progn
		(setq start (1+ (point)))
		(end-of-line)))
	  (setq ret (mew-buffer-substring start (point)))
	  (delete-region start (point))
	  ret)))))

;;
;; Making alist
;;

(defun mew-slide-pair (x)
  (let ((len (length x))
	(ret nil)
	(first (car x)))
    (cond
     ((= len 0) nil)
     ((= len 1) (list (cons first first)))
     (t
      (while (cdr x)
	(setq ret (cons (cons (nth 0 x) (nth 1 x)) ret))
	(setq x (cdr x)))
      (setq ret (cons (cons (car x) first) ret))
      (nreverse ret)))))

(provide 'mew-complete)

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

;;; mew-complete.el ends here
