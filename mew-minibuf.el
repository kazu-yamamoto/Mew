;;; mew-minibuf.el --- Minibuffer input methods for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion switch
;;;

(defvar mew-input-complete-function nil
  "A function to be called when TAB is typed in minibuffer.
This is used in 'mew-input-complete'.")

(defun mew-input-complete ()
  "Do completion according to the global variable
\"mew-input-complete-function\"."
  (interactive)
  (if (and mew-input-complete-function (fboundp mew-input-complete-function))
      (funcall mew-input-complete-function)))

(defvar mew-input-exit-minibuffer-function nil
  "A function to be called when RET is typed in minibuffer.
This function are used to check validity of 'case' and sort key.")

(defun mew-input-exit-minibuffer ()
  "Ensure the input meets a condition."
  (interactive)
  (if (or (not (and mew-input-exit-minibuffer-function
                    (fboundp mew-input-exit-minibuffer-function)))
          (funcall mew-input-exit-minibuffer-function))
      (exit-minibuffer)))

(defvar mew-input-comma-function nil
  "A function to be called when ',' is typed in minibuffer.
This function can be used to check validity of 'case'.")

(defun mew-input-comma ()
  "Ensure the input meets a condition."
  (interactive)
  (when (or (not (and mew-input-comma-function
		      (fboundp mew-input-comma-function)))
	    (funcall mew-input-comma-function))
    (insert ",")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clean up side effects
;;;

(defun mew-input-clear ()
  "A function to clean up side effects of window configuration
at completions."
  (with-current-buffer (window-buffer (minibuffer-window))
    ;; (mew-ainfo-get-win-cfg) is shared by many functions
    ;; because minibuffer is just one!
    (mew-ainfo-set-win-cfg nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder
;;;

(defvar mew-input-folder-search-buf " *Mew* folder search")

(defvar mew-input-folder-hist nil)

(defvar mew-input-folder-refile nil)

(defvar mew-input-folder-search-direction nil)
(defvar mew-input-folder-search-key nil)
(defvar mew-input-folder-search-match nil)
(defvar mew-input-folder-search-original nil)
(defvar mew-input-folder-search-point nil)
(defvar mew-input-folder-search-multi nil)

(defun mew-input-folder-clean-up ()
  (setq mew-input-folder-search-direction nil)
  (setq mew-input-folder-search-key nil)
  (setq mew-input-folder-search-match nil)
  (setq mew-input-folder-search-original nil)
  (setq mew-input-folder-search-point nil))

(defun mew-highlight-folder-comp-search-window ()
  (let* ((win (get-buffer-window mew-buffer-completions))
	 (match mew-input-folder-search-match)
	 face)
    (when (and win
	       mew-input-folder-search-key
	       mew-input-folder-search-match)
      (with-current-buffer mew-buffer-completions
	(mew-elet
	 (goto-char (point-min))
	 (when (looking-at "^Key: ")
	   (delete-region (point-min) (progn (forward-line) (point))))
	 (insert "Key: " mew-input-folder-search-key ",  "
		 "Match: " mew-input-folder-search-match "\n")
	 (put-text-property (point-min) (point-max) 'face nil)
	 (when (re-search-forward
		(format "\\(%s\\)\\([ \t]\\|$\\)" (regexp-quote match)) nil t)
	   (goto-char (match-beginning 1))
	   (setq face (if (not window-system)
			  'mew-face-header-from
			(if (facep 'isearch) 'isearch 'region)))
	   (put-text-property (point) (match-end 1) 'face face)
	   (unless (pos-visible-in-window-p (point) win)
	     (set-window-start win (progn (forward-line -2) (point))))))))))

(defun mew-input-folder-display (&optional msg)
  (mew-highlight-folder-comp-search-window)
  (mew-elet
   (delete-region (mew-minibuf-point-min) (point-max))
   (insert "(" (or mew-input-folder-search-match "") ") ")
   (insert (or mew-input-folder-search-key ""))
   (if msg (save-excursion (insert " [" msg "]")))
   (put-text-property (mew-minibuf-point-min) (point-max) 'read-only t)))

(defun mew-input-folder-search-setup (&optional min)
  (if mew-input-folder-refile
      (mew-input-folder-search-setup2 min)
    (mew-input-folder-search-setup1 min)))

(defun mew-input-folder-search-setup-buffer (alist min)
  (with-current-buffer (get-buffer-create mew-input-folder-search-buf)
    (setq buffer-read-only t)
    (mew-elet
     (mew-erase-buffer)
     (mapc (lambda (x) (if (stringp (car x)) (insert (car x) "\n"))) alist))
    (if min (goto-char (point-min)))))

(defun mew-input-folder-search-setup1 (&optional min)
  (let (case:folder case folder alist)
    (save-excursion
      (goto-char (point-max))
      (if (search-backward "," nil t)
	  (setq mew-input-folder-search-point
		(- (match-end 0) (mew-minibuf-point-min)))))
    (setq mew-input-folder-search-original
	  (mew-buffer-substring (mew-minibuf-point-min) (point-max)))
    (setq case:folder mew-input-folder-search-original)
    (if mew-input-folder-search-point
	(setq case:folder (substring case:folder mew-input-folder-search-point)))
    (setq case (mew-case:folder-case case:folder))
    (setq folder (mew-case:folder-folder case:folder))
    (when case
      (setq mew-input-folder-search-point
	    (+ (or mew-input-folder-search-point 0) (length case) 1))) ;; ":"
    (mew-input-folder-display)
    (if (or (null folder) (string= folder ""))
	(setq folder (mew-proto case)))
    (cond
     ((mew-folder-popp folder)
      (setq alist (mew-pop-folder-alist)))
     ((mew-folder-nntpp folder)
      (setq alist (mew-nntp-folder-alist case)))
     ((mew-folder-imapp folder)
      (setq alist (mew-imap-folder-alist case)))
     (t
      (setq alist (mew-local-folder-alist))))
    (mew-input-folder-search-setup-buffer alist min)))

(defun mew-input-folder-search-setup2 (&optional min)
  (let (do-search alist)
    (save-excursion
      (goto-char (point-max))
      (if (search-backward "," (minibuffer-prompt-end) t)
	  (progn
	    (goto-char (match-end 0))
	    (unless (looking-at mew-regex-case2)
	      (setq do-search t)
	      (setq mew-input-folder-search-point
		    (- (point) (mew-minibuf-point-min)))))
	(goto-char (mew-minibuf-point-min))
	(unless (looking-at mew-regex-case2)
	  (setq do-search t))))
    (if (not do-search)
	(progn
	  (mew-temp-minibuffer-message "Remove case!")
	  (mew-input-folder-clean-up))
      (setq mew-input-folder-search-original
	    (mew-buffer-substring (mew-minibuf-point-min) (point-max)))
      (mew-input-folder-display)
      (cond
       ((eq mew-input-complete-function 'mew-complete-local-folder)
	(setq alist (mew-local-folder-alist)))
       ((eq mew-input-complete-function 'mew-complete-imap-folder)
	(setq alist (mew-imap-folder-alist mew-inherit-case))))
      (mew-input-folder-search-setup-buffer alist min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder search forward
;;;

(defun mew-input-folder-search-forward-1 (&optional again)
  (let (no-match pos)
    (with-current-buffer mew-input-folder-search-buf
      (setq pos (point))
      (if again
	  (forward-line)
	(beginning-of-line))
      (if (search-forward mew-input-folder-search-key nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (setq mew-input-folder-search-match
		  (mew-buffer-substring
		   (save-excursion (beginning-of-line) (point))
		   (save-excursion (end-of-line) (point)))))
	(setq no-match t)
	(goto-char pos)))
    (if no-match
	(mew-input-folder-display "no match")
      (mew-input-folder-display))))

(defun mew-input-folder-search-forward ()
  "Search a folder forward."
  (interactive)
  (cond
   ((and mew-input-folder-search-direction (null mew-input-folder-search-key))
    (mew-input-folder-display "no match"))
   ((eq mew-input-folder-search-direction 'forward)
    (mew-input-folder-search-forward-1 'again))
   ((eq mew-input-folder-search-direction 'backward)
    (setq mew-input-folder-search-direction 'forward)
    (mew-input-folder-search-forward-1 'again))
   (t
    (setq mew-input-folder-search-direction 'forward)
    (mew-input-folder-search-setup 'min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder search backward
;;;

(defun mew-input-folder-search-backward-1 (&optional again)
  (let (no-match pos)
    (with-current-buffer mew-input-folder-search-buf
      (setq pos (point))
      (if again
	  (progn (forward-line -1) (end-of-line))
	(end-of-line))
      (if (search-backward mew-input-folder-search-key nil t)
	  (progn
	    (goto-char (match-end 0))
	    (setq mew-input-folder-search-match
		  (mew-buffer-substring
		   (save-excursion (beginning-of-line) (point))
		   (save-excursion (end-of-line) (point)))))
	(setq no-match t)
	(goto-char pos)))
    (if no-match
	(mew-input-folder-display "no match")
      (mew-input-folder-display))))

(defun mew-input-folder-search-backward ()
  "Search a folder backward."
  (interactive)
  (cond
   ((and mew-input-folder-search-direction (null mew-input-folder-search-key))
    (mew-input-folder-display "no match"))
   ((eq mew-input-folder-search-direction 'forward)
    (setq mew-input-folder-search-direction 'backward)
    (mew-input-folder-search-backward-1 'again))
   ((eq mew-input-folder-search-direction 'backward)
    (mew-input-folder-search-backward-1 'again))
   (t
    (setq mew-input-folder-search-direction 'backward)
    (mew-input-folder-search-setup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder commands
;;;

(defun mew-input-folder-exit-minibuffer ()
  "If called in folder search, this gets back to minibuffer to input
folders. If called in minibuffer to input folders, this
gets out of minibuffer."
  (interactive)
  (if mew-input-folder-search-direction
      (mew-elet
       (delete-region (mew-minibuf-point-min) (point-max))
       (cond
	(mew-input-folder-search-match
	 (when mew-input-folder-search-point
	   (insert (substring mew-input-folder-search-original
			      0 mew-input-folder-search-point)))
	 (insert mew-input-folder-search-match))
	(mew-input-folder-search-original
	 (insert mew-input-folder-search-original)))
       (mew-input-folder-clean-up)
       (mew-complete-window-delete))
    (exit-minibuffer)))

(defun mew-input-folder-abort-minibuffer ()
  "This function exits folder search mode if in folder search mode.
Otherwise, it exits minibuffer."
  (interactive)
  (if mew-input-folder-search-direction
      (mew-elet
       (delete-region (mew-minibuf-point-min) (point-max))
       (when mew-input-folder-search-original
	 (insert mew-input-folder-search-original))
       (mew-input-folder-clean-up)
       (mew-complete-window-delete))
    (abort-recursive-edit)))

(defun mew-input-folder-comma ()
  "This function inserts ',' on multiple folder input
and does not insert anything on single folder input."
  (interactive)
  (unless mew-input-folder-search-direction
    (insert ",")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Self insert
;;;

(defun mew-input-folder-self-insert ()
  "This function normally inserts its bound key to minibuffer.
When in folder search mode, this function searches a candidate
folder and displays it in addition to its bound key."
  (interactive)
  (let ((key (this-command-keys))
	last-str gfunc)
    (cond
     ((stringp key)
      (setq last-str key)
      (setq gfunc (lookup-key (current-global-map) key)))
     ((vectorp key)
      (setq gfunc (lookup-key (current-global-map) key))
      (unless gfunc
	(setq key (lookup-key function-key-map key))
	(cond
	 ((vectorp key) ;; normal Emacs
	  (setq gfunc (lookup-key (current-global-map) key)))
	 ((stringp key) ;; Meadow
	  (setq last-str key)
	  (setq gfunc (lookup-key (current-global-map) key)))))))
    (if mew-input-folder-search-direction
	(cond
	 ((or (equal key "\177") (equal key [127])
	      (equal key 'delete) (equal key 'backspace))
	  (if (null mew-input-folder-search-key)
	      (mew-input-folder-display "not allowed")
	    (setq mew-input-folder-search-key
		  (substring mew-input-folder-search-key 0 -1))
	    (when (string= mew-input-folder-search-key "")
	      (setq mew-input-folder-search-key nil)
	      (setq mew-input-folder-search-match nil)
	      (with-current-buffer mew-input-folder-search-buf
		(cond
		 ((eq mew-input-folder-search-direction 'forward)
		  (goto-char (point-min)))
		 ((eq mew-input-folder-search-direction 'backward)
		  (goto-char (point-max))))))
	    (mew-input-folder-display)))
	 ((not (string-match "self-insert-command" (symbol-name gfunc)))
	  (mew-input-folder-display "not allowed"))
	 ((eq mew-input-folder-search-direction 'forward)
	  (setq mew-input-folder-search-key
		(concat mew-input-folder-search-key last-str))
	  (mew-input-folder-search-forward-1))
	 ((eq mew-input-folder-search-direction 'backward)
	  (setq mew-input-folder-search-key
		(concat mew-input-folder-search-key last-str))
	  (mew-input-folder-search-backward-1)))
      (cond
       ((null gfunc)
	())
       ((string-match "self-insert-command" (symbol-name gfunc))
	(insert last-command-event))
       ((and (fboundp gfunc) (commandp gfunc))
	(call-interactively gfunc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Prefix hack
;;;

(defun mew-input-folder-complete-case (prefix-char)
  (when (and mew-use-case-completion
	     (eq mew-input-complete-function 'mew-complete-folder)
	     (mew-folder-remotep (char-to-string prefix-char))
	     (and mew-case (not (string= mew-case mew-case-default))))
    (insert mew-case ":")))

(defun mew-input-folder-prefix ()
  "A function to insert a folder prefix in minibuffer.
If the previous character is another folder prefix,
it is deleted automatically."
  (interactive)
  (cond
   (mew-input-folder-search-direction
    (mew-input-folder-self-insert))
   ;;
   ((or (eq mew-input-complete-function 'mew-complete-local-folder)
	(eq mew-input-complete-function 'mew-complete-imap-folder))
    ;; no case, not allow other prefixes.
    (let* ((pos (- (point) (mew-minibuf-point-min)))
	   (prefix (if (eq mew-input-complete-function
			   'mew-complete-local-folder)
		       mew-folder-local mew-folder-imap))
	   (insert-ok (string= (char-to-string last-command-event) prefix)))
      (cond
       ((or (= pos 0)
	    (save-excursion (forward-char -1) (looking-at ",")))
	(if insert-ok (insert prefix)))
       ((or (and (= pos 1)
		 (save-excursion (forward-char -1) (looking-at "[-+%$]")))
	    ;; excluding *
	    (save-excursion (forward-char -2) (looking-at ",[-+%$]")))
	;; A wrong prefix might accidentally be here.
	;; So, replace it just in case
	(when insert-ok
	  (forward-char -1)
	  (delete-char 1)
	  (insert prefix)))
       (t
	(insert last-command-event)))))
   (t
    (let ((pos (- (point) (mew-minibuf-point-min))))
      (cond
       ((or (= pos 0)
	    (save-excursion (forward-char -1) (looking-at ",")))
	(mew-input-folder-complete-case last-command-event)
	(insert last-command-event))
       ;;
       ((or (and (= pos 1)
		 (save-excursion (forward-char -1) (looking-at "[-+%$]")))
	    ;; excluding *
	    (save-excursion (forward-char -2) (looking-at ",[-+%$]")))
	(forward-char -1)
	(delete-char 1)
	(mew-input-folder-complete-case last-command-event)
	(insert last-command-event))
       ;;
       ((save-excursion
	  (forward-char -2)
	  (looking-at ":[-+%$*]"))
	(forward-char -1)
	(delete-char 1)
	(when (memq last-command-event '(?+ ?*))
	  ;; delete case:
	  (delete-region (save-excursion (or (and (search-backward "," nil t)
						  (match-end 0))
					     (mew-minibuf-point-min)))
			 (point)))
	(insert last-command-event))
       ;;
       (t
	(insert last-command-event)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder
;;;

(defun mew-input-folder-check (folders)
  (let (folder ret)
    (dolist (case:folder folders)
      (catch 'continue
	(if (string-match mew-regex-case case:folder)
	    (progn
	      (setq folder (mew-match-string 2 case:folder))
	      (if (= (length folder) 0) (throw 'continue nil))) ;; "case:" only
	  (setq folder case:folder))
	(if (and (= (length folder) 1)
		 (member folder mew-folder-prefixes)) ;; "prefix" only
	    (throw 'continue nil))
	(if (and (mew-folder-popp folder) ;; not $inbox
		 (not (string= mew-pop-inbox-folder folder)))
	    (throw 'continue nil))
	(setq case:folder (mew-canonicalize-case-folder case:folder))
	(setq ret (cons case:folder ret))))
    (setq ret (delete nil ret))
    (nreverse ret)))

(defun mew-input-local-folder (folder)
  "Input a local folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((default folder)
	 (init (mew-folder-prefix folder))
	 (mew-input-complete-function 'mew-complete-local-folder)
	 (mew-circular-complete-function 'mew-circular-complete-case:)
	 (ret (read-from-minibuffer (format "Folder name (%s): " default)
				    init mew-input-folder-map nil
				    'mew-input-folder-hist)))
    (when (or (string= ret "") (string= ret init))
      (setq ret default))
    (car (mew-input-folder-check (list ret)))))

(defun mew-input-folder (case folder)
  "Input a folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((default (mew-case-folder case folder))
	 (init (mew-case-folder case (mew-folder-prefix folder)))
	 (mew-input-complete-function 'mew-complete-folder)
	 (mew-circular-complete-function 'mew-circular-complete-case:)
	 ;; mew-inherit-case must be nil
	 (ret (read-from-minibuffer (format "Folder name (%s): " default)
				    init mew-input-folder-map nil
				    'mew-input-folder-hist))
	 (new-folder))
    (when (or (string= ret "") (string= ret init))
      (setq ret default))
    ;; mew-input-folder-check requires an IMAP separator,
    ;; which requires C-uZ. C-uZ needs "g". So, chicken and egg.
    ;; Skip the check for inbox to avoid the problem.
    (setq new-folder (mew-case:folder-folder ret))
    (if (member new-folder (list mew-inbox-folder
				 mew-pop-inbox-folder
				 mew-imap-inbox-folder
				 mew-nntp-newsgroup))
	ret
      (car (mew-input-folder-check (list ret))))))

(defun mew-input-folders (case:folder)
  "Input a folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((init (mew-canonicalize-case-folder case:folder))
	 (mew-input-complete-function 'mew-complete-folder)
	 (mew-circular-complete-function 'mew-circular-complete-case:)
	 (mew-input-folder-search-multi t)
	 ;; mew-inherit-case must be nil
	 (ret (read-from-minibuffer "Folder name: "
				    init mew-input-folder-map nil
				    'mew-input-folder-hist)))
    (when (string= ret "")
      (setq ret init))
    (setq ret (mapcar 'mew-chop (mew-split ret ?,)))
    (mew-input-folder-check ret)))

(defun mew-input-refile-folder-check (folders proto)
  (let (folder ret)
    (dolist (case:folder folders)
      (catch 'continue
	;; If case is specified, just remove it at this moment...
	(if (string-match mew-regex-case case:folder)
	    (progn
	      (setq folder (mew-match-string 2 case:folder))
	      (if (= (length folder) 0) (throw 'continue nil))) ;; "case:" only
	  (setq folder case:folder))
	(cond
	 ((eq proto 'imap)
	  (unless (mew-folder-imapp folder)
	    (throw 'continue nil)))
	 ((eq proto 'local)
	  (unless (mew-folder-localp folder)
	    (throw 'continue nil))))
	(if (and (= (length folder) 1)
		 (member folder mew-folder-prefixes)) ;; "prefix" only
	    (throw 'continue nil))
	(setq folder (mew-canonicalize-case-folder folder))
	(setq ret (cons folder ret))))
    (nreverse ret)))

(defun mew-input-refile-folders (folder-list singlep case proto)
  "Input refile folders from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let ((mew-input-complete-function (if (mew-folder-imapp proto)
					 'mew-complete-imap-folder
				       'mew-complete-local-folder))
	(mew-inherit-case case)
	(mew-input-folder-search-multi t)
	(mew-input-folder-refile t)
	;; Emacs 21.1, 21.2 and 21.3 has a bug of inhibit-quit.
	;; Set inhibit-quit to nil so that C-g can be used
	(inhibit-quit nil)
	default prompt init ret)
    (cond
     (singlep
      (setq default (car folder-list))
      (setq init (mew-folder-prefix default))
      (if case
	  (setq prompt (format "Folder name <%s:> (%s): " case default))
	(setq prompt (format "Folder name (%s): " default))))
     (t
      (if case
	  (setq prompt (format "Folder name <%s:>: " case))
	(setq prompt "Folder name: "))
      (setq init (mew-join "," folder-list))))
    (setq ret (read-from-minibuffer prompt
				    init mew-input-folder-map nil
				    'mew-input-folder-hist))
    (when (and singlep (string= ret init))
      (setq ret default))
    (setq ret (mapcar 'mew-chop (mew-split ret ?,)))
    (mew-input-refile-folder-check
     ret (if (mew-folder-imapp proto) 'imap 'local))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Address
;;;

(defvar mew-input-address-hist nil)

(defun mew-input-address (prompt &optional default)
  (mew-input-clear)
  (let ((mew-input-complete-function 'mew-complete-address)
	val vals addrs ret)
    (setq val (read-from-minibuffer
	       (if default (format prompt default) prompt)
	       ""
	       mew-input-map
	       nil
	       'mew-input-address-hist))
    (if (and default (string= val ""))
	(setq val default))
    (setq vals (mapcar 'mew-chop (mew-split-quoted val ?,)))
    (dolist (val vals)
      (setq addrs (mew-alias-expand val mew-addrbook-alist 0))
      (setq addrs (mapcar 'mew-addrstr-append-domain addrs))
      (setq ret (nconc ret addrs)))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick pattern
;;;

(defvar mew-input-pick-pattern-hist nil)

(defun mew-input-pick-pattern (prompt)
  (mew-input-clear)
  (let ((mew-input-complete-function 'mew-complete-pick-pattern)
	(mew-circular-complete-function 'mew-circular-complete-pick-pattern)
	(keymap (copy-keymap mew-input-map)))
    (define-key keymap " " nil)
    (mew-pick-macro-expand-string
     (read-from-minibuffer (concat prompt " pattern: ")
			   (car mew-pick-pattern-list)
			   keymap
			   nil
			   'mew-input-pick-pattern-hist))))

(defvar mew-input-pick-command-hist nil)

(defun mew-input-pick-command (prog opts)
  (mew-input-clear)
  (let ((init (mapconcat 'identity (cons prog opts) " "))
	(prompt "Cmd opts pattern: ")
	input lst cmd opts len pat)
    (unless (string-match " $" init)
      (setq init (concat init " ")))
    (setq input (read-string prompt init 'mew-input-pick-command-hist))
    (setq mew-input-pick-command-hist
	  (mew-uniq-list mew-input-pick-command-hist))
    (setq lst (mew-split-quoted input mew-sp))
    (setq cmd (car lst))
    (setq opts (cdr lst))
    (setq len (length opts))
    (cond
     ((= len 0)
      )
     ((= len 1)
      (setq pat (car opts))
      (setq opts nil))
     (t
      (setq pat (nth (1- len) opts))
      (setcdr (nthcdr (- len 2) opts) nil)))
    (list cmd opts pat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort key
;;;
;;; mew-sort-default-key-alist

(defvar mew-input-sort-key-hist nil)

(defun mew-input-sort-key-check ()
  (let* ((field:mode (mew-buffer-substring (mew-minibuf-point-min) (point-max)))
	 (mode (car (cdr (mew-split field:mode ?:))))
	 err)
    (if mode
	(unless (member mode mew-sort-modes)
	  (setq err mode)))
    (if err
        (progn
          (mew-temp-minibuffer-message (format " [No match: %s]" err))
          nil)
      t)))

(defun mew-input-sort-key (key)
  (mew-input-clear)
  (let* ((mew-input-complete-function 'mew-complete-sort-key)
	 (mew-input-exit-minibuffer-function 'mew-input-sort-key-check)
	 (field:mode (read-from-minibuffer
		      (format "Sort by (%s)?: " key)
		      ""
		      mew-input-map
		      nil
		      'mew-input-sort-key-hist))
	 field mode)
    (if (or (null field:mode) (string= field:mode ""))
	(setq field:mode key))
    (setq field (car (mew-split field:mode ?:)))
    (setq mode (or (car (cdr (mew-split field:mode ?:)))
		   (mew-alist-get-value (assoc field mew-sort-key-alist))
		   "text"))
    (list field mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remote file
;;;

(defvar mew-input-rfile-hist nil)

(defun mew-input-rfile (prompt) ;; prompt="To:"
  (mew-input-clear)
  (let ((mew-input-complete-function 'mew-complete-rfile))
    (read-from-minibuffer
     (concat prompt " ")
     ""
     mew-input-map
     nil
     'mew-input-rfile-hist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Range
;;;

(defun mew-range-update (folder)
  (when (get-buffer folder)
    (with-current-buffer folder
      (goto-char (point-max))
      (if (bobp)
	  mew-range-all ;; buffer is empty.
	(forward-line -1)
	(mew-summary-goto-message)
	(concat (number-to-string
		 (1+ (string-to-number (mew-summary-message-number))))
		"-")))))

(defun mew-input-range (folder askp)
  (let ((default (or (car (mew-folder-spec folder mew-range-list
					   mew-range-list-string-type
					   mew-range-list-list-type))
		     mew-range-str-update))
	comp range ret)
    (when askp
      (setq comp (mapcar 'list mew-input-range-list))
      (setq range (completing-read (format "Range (%s): " default) comp)))
    (if (or (string= range "") (null range))
	(setq range default))
    (cond
     ((string= range mew-range-str-all)
      (setq ret (list mew-range-all 'erase)))
     ((string= range mew-range-str-update)
      (setq ret (list (mew-range-update folder) nil)))
     ((string-match "^last:" range)
      (setq ret (list range 'erase)))
     ((or (string-match "^[0-9]+$" range)
	  (string-match "^[0-9]+-$" range)
	  (string-match "^-[0-9]+$" range)
	  (string-match "^[0-9]+-[0-9]+$" range))
      (setq ret (list range 'erase)))
     (t
      (setq ret nil))) ;; a wrong range
    ret))

(defun mew-input-range-remote (folder)
  ;; t   all
  ;; nil update
  ;; n   last:<n>
  ;; 'sync
  (let* ((default (or (car (mew-folder-spec folder mew-range-list
					    mew-range-list-string-type
					    mew-range-list-list-type))))
	 (comp (mapcar 'list mew-input-range-remote-list))
	 (range (completing-read (format "Range (%s): " default) comp)))
    (if (or (string= range "") (null range))
	(setq range default))
    (cond
     ((string= range mew-range-str-sync)   'sync)
     ((string= range mew-range-str-all)      t)
     ((string= range mew-range-str-update) nil)
     ((string-match "^last:\\([0-9]+\\)$" range)
      (string-to-number (match-string 1 range)))
     (t nil)))) ;; update just in case

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer
;;;

(defun mew-input-draft-buffer (default)
  (let* ((regex (mew-folder-regex (file-name-as-directory mew-draft-folder)))
	 (comp (mew-buffer-list regex t))
	 buf)
    (if (and (= (length comp) 1)
	     (string= default (car (car comp))))
	default
      (setq buf (completing-read (format "Buffer (%s): " default) comp))
      (if (string= buf "")
	  default
	buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File
;;;

(defun mew-input-file-name (&optional prompt default)
  (let ((msg (or prompt "File: "))
	(use-dialog-box nil)
	file)
    (cond
     ((null default)
      (setq file mew-save-dir))
     ((mew-folder-absolutep default)
      (setq file default))
     (t
      (setq file (concat mew-save-dir default))))
    (expand-file-name (read-file-name msg file file))))

(defun mew-input-directory-name (&optional default)
  (let ((dir (expand-file-name
	      (read-file-name "Directory: " default default t))))
    (if (file-directory-p dir)
	dir
      (mew-warn "%s is not directory" dir)
      (mew-input-directory-name default))))

(defun mew-convert-to-home-dir (dir)
  (let* ((chome (file-name-as-directory mew-home))
	 (ehome (concat "^" (regexp-quote (expand-file-name chome)))))
    (if (string-match ehome dir)
	(concat chome (substring dir (match-end 0)))
      dir)))

(defvar mew-summary-previous-directory nil)
(defvar mew-draft-previous-directory nil)

(defmacro mew-mode-input-file-name (prompt file preservep previous modedir)
  `(let (dir ret def)
     (if (and ,file (file-name-absolute-p ,file))
	 (setq def (mew-convert-to-home-dir ,file))
       (if ,preservep
	   (setq dir (or ,previous ,modedir))
	 (setq dir , modedir))
       (setq dir (and dir (file-name-as-directory dir)))
       (setq def (concat dir ,file)))
     (setq ret (mew-input-file-name ,prompt def))
     (if ,preservep
	 (setq ,previous (file-name-directory
			  (mew-convert-to-home-dir
			   (if (file-directory-p ret)
			       (file-name-as-directory ret)
			     ret)))))
     (if (and ,file (file-directory-p ret))
	 (setq ret (expand-file-name (file-name-nondirectory ,file) ret)))
     ret))

(defun mew-summary-input-file-name (&optional prompt file)
  (mew-mode-input-file-name prompt file mew-summary-preserve-dir
			    mew-summary-previous-directory mew-save-dir))

(defun mew-draft-input-file-name (&optional prompt file)
  (mew-mode-input-file-name prompt file mew-draft-preserve-dir
			    mew-draft-previous-directory mew-copy-dir))

(defmacro mew-mode-input-directory-name (preservep previous modedir)
  `(if ,preservep
       (let (dir ret)
	 (setq dir (file-name-as-directory (or ,previous ,modedir)))
	 (setq ret (mew-input-directory-name dir))
	 (setq ,previous (mew-convert-to-home-dir ret))
	 ret)
     (mew-input-directory-name default-directory)))

(defun mew-summary-input-directory-name ()
  (mew-mode-input-directory-name
   mew-summary-preserve-dir mew-summary-previous-directory mew-save-dir))

(defun mew-draft-input-directory-name ()
  (mew-mode-input-directory-name
   mew-draft-preserve-dir mew-draft-previous-directory mew-copy-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; String
;;;

(defun mew-input-string (prompt subdir default)
  (let ((input (read-string (format prompt subdir default) "")))
    (if (string= input "") default input)))

(defun mew-input-general (prompt alist &optional require-match initial)
  (let* ((completion-ignore-case t)
	 (question (if initial (format "%s (%s): " prompt initial)
		     (format "(%s): " prompt)))
	 (value (completing-read question alist nil require-match nil)))
    (if (and initial (string= value "")) initial value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type
;;;

(defun mew-input-type (prompt filename default type-list)
  (let ((completion-ignore-case t)
	(type))
    (setq type (completing-read
		(format prompt filename default)
		(mapcar 'list type-list)
		nil
		t
		""))
    (if (string= type "") default type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Case
;;;

(defun mew-input-case-check ()
  (let ((case (mew-buffer-substring (mew-minibuf-point-min) (point-max)))
	err)
    (catch 'nomatch
      (dolist (cs (mew-split case ?,))
	(unless (member cs mew-config-cases)
	  (throw 'nomatch (setq err cs)))))
    (if err
        (progn
          (mew-temp-minibuffer-message (format " [No match: %s]" err))
          nil)
      t)))

(defvar mew-input-case-hist nil)

(defun mew-input-case (default &optional edit)
  (mew-input-clear)
  (unless default (setq default mew-case-default))
  (let ((mew-input-complete-function 'mew-complete-case)
	(mew-circular-complete-function 'mew-circular-complete-case)
	(mew-input-exit-minibuffer-function 'mew-input-case-check)
	(mew-input-comma-function 'mew-input-case-check)
	case ret)
    (if edit
	(setq case (read-from-minibuffer
		    "Case value: "
		    default
		    mew-input-map
                    nil
                    'mew-input-case-hist))
      (setq case (read-from-minibuffer
		  (format "Case value (%s): " default)
		  ""
		  mew-input-map
                  nil
                  'mew-input-case-hist)))
    (if (string= case "")
	default
      (dolist (cs (mew-split case ?,))
	(if (member cs mew-config-cases)
	    (setq ret (cons cs ret))))
      (mapconcat 'identity (nreverse ret) ","))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mark
;;;

(defun mew-input-mark (&optional msg)
  (let ((ociea cursor-in-echo-area) char)
    (unwind-protect
	(progn
	  (message (or msg "Input mark: "))
	  (setq cursor-in-echo-area t)
	  (setq char (read-char))
	  (unless (char-equal char ?\r)
	    (message "%s%c" (or msg "Input mark: ") char)))
      (setq cursor-in-echo-area ociea))
    (cond
     ((mew-markdb-by-mark char) char)
     (t (message "Mark %c is not supported" char)
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encoding
;;;

(defun mew-input-encoding ()
  (let* ((encoding-list (list mew-b64 mew-qp))
	 (encoding-alist (mapcar (lambda (x) (list x)) encoding-list))
	 (prompt (format "Lines are too long. Input encoding (%s): "
			 mew-default-encoding))
	 (cte (completing-read prompt encoding-alist)))
    (if (string= cte "") (setq cte mew-default-encoding))
    (setq cte (downcase cte))
    (while (not (member cte encoding-list))
      (setq prompt (format "'%s' is unknown. Input encoding (%s): "
			   cte mew-default-encoding))
      (setq cte (completing-read prompt encoding-alist))
      (if (string= cte "") (setq cte mew-default-encoding))
      (setq cte (downcase cte)))
    cte))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command
;;;

(defun mew-input-command (&optional default)
  (let ((program (read-string "Command args: " default)))
    (setq program (mew-split-quoted program mew-sp))
    (setq program (delete "" program))
    (while (not (mew-which-exec (car program)))
      (setq program (read-string "Command args: " default))
      (setq program (mew-split-quoted program mew-sp))
      (setq program (delete "" program)))
    program))

(provide 'mew-minibuf)

;;; Copyright Notice:

;; Copyright (C) 1997-2011 Mew developing team.
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

;;; mew-minibuf.el ends here
