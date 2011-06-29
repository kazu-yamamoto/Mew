;;; mew-search.el --- Index Search

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug 24, 2005

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-prog-spotlight "mdfind")
(defvar mew-prog-wds       "wdsgrep.exe")
(defvar mew-prog-google    "gdgrep.rb")
(defvar mew-prog-est       "estcmd")

(mew-defstruct search
	       key name prog
	       func-search func-virtual
	       func-index-folder func-index-all
	       func-canonicalize-pattern
	       func-register func-unregister func-filter)

(defvar mew-search-switch
  `((spotlight "Spotlight" ,mew-prog-spotlight
     mew-search-with-spotlight mew-search-virtual-with-spotlight
     mew-spotlight-index-folder mew-spotlight-index-all
     mew-pick-canonicalize-pattern-spotlight
     nil nil)
    (wds "WDS" ,mew-prog-wds
     mew-search-with-wds mew-search-virtual-with-wds
     mew-wds-index-folder mew-wds-index-all
     mew-pick-canonicalize-pattern-wds
     mew-wds-register mew-wds-unregister)
    (google "Google" ,mew-prog-google
     mew-search-with-google mew-search-virtual-with-google
     mew-google-index-folder mew-google-index-all
     mew-pick-canonicalize-pattern-google
     mew-google-register mew-google-unregister)
    (est "Hyper Estraier" ,mew-prog-est
     mew-search-with-est mew-search-virtual-with-est
     mew-est-index-folder mew-est-index-all
     mew-pick-canonicalize-pattern-est
     nil nil
     mew-est-input-filter)))

(defun mew-search-get-list (func)
  (let ((sw mew-search-switch)
	ent ret)
    (while sw
      (setq ent (car sw))
      (setq sw (cdr sw))
      (if (mew-which-exec (mew-search-get-prog ent))
	  (setq ret (cons (funcall func ent) ret))))
    (nreverse ret)))

(defun mew-search-get-ent (method)
  (assoc method mew-search-switch))

(defvar mew-search-method (car (mew-search-get-list 'mew-search-get-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-summary-search-change-method ()
  "Change a search method."
  (interactive)
  (let* ((names (mew-search-get-list 'mew-search-get-name))
	 (ent (mew-search-get-ent mew-search-method))
	 (default (mew-search-get-name ent))
	 name)
    (if (not names)
	(message "No search method")
      (setq names (mapcar (lambda (x) (list x)) names))
      (setq name (completing-read (format "Search method (%s): " default) names nil t))
      (if (string= name "") (setq name default))
      (setq mew-search-method (nth 0 (mew-assoc-equal name mew-search-switch 1))))))

(defun mew-summary-search ()
  "Pick messages according to a specified pick pattern
with a search method. Then put the '*' mark onto them. "
  (interactive)
  (mew-pickable
   (if (not mew-search-method)
       (message "No search method")
     (let* ((ent (mew-search-get-ent mew-search-method))
	    (func (mew-search-get-func-search ent))
	    (name (mew-search-get-name ent))
	    (canon-func (mew-search-get-func-canonicalize-pattern ent))
	    (flt-func (mew-search-get-func-filter ent))
	    (folder (mew-summary-physical-folder))
	    pattern msgs filter)
       (if (not (fboundp func))
	   (message "This command cannot be used")
	 (setq pattern (if flt-func
			   (read-string (concat name " pick pattern: "))
			 (mew-input-pick-pattern (concat name " pick"))))
	 (if (and (string= pattern "") (not (fboundp flt-func)))
	     (message (mew-substitute-for-summary "Keyword must be specified. You may use '\\[mew-summary-pick]' instead"))
	   (when (and canon-func (fboundp canon-func))
	     (setq pattern (funcall canon-func pattern)))
	   (if (fboundp flt-func)
	       (setq filter (funcall flt-func)))
	   (setq msgs (funcall func pattern folder filter))
	   (mew-summary-pick-ls folder msgs)))))))

(defun mew-summary-selection-by-search (&optional ask-folder)
  "Making selection according to a specified pick pattern
with a search method."
  (interactive "P")
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ofolder (mew-summary-folder-name 'ext))
	   (ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-virtual ent))
	   (name (mew-search-get-name ent))
	   (canon-func (mew-search-get-func-canonicalize-pattern ent))
	   (flt-func (mew-search-get-func-filter ent))
	   vfolder opattern pattern dfunc file opts rttl file-rttl flds filter)
      (if (not (fboundp func))
	  (message "This command cannot be used")
	(if ask-folder
	    (setq flds (mew-input-folders
			(or (mew-summary-folder-name)
			    (mew-case-folder
			     mew-case
			     (mew-proto-inbox-folder (mew-proto mew-case)))))))
	(setq opattern (if flt-func
			   (read-string (concat name " virtual pattern: "))
			 (mew-input-pick-pattern (concat name " virtual"))))
	(if (and (string= opattern "") (not (fboundp flt-func)))
	    (message (mew-substitute-for-summary "Keyword must be specified"))
	  (if (string= opattern "") (setq opattern " "))
	  (if (and canon-func (fboundp canon-func))
	      (setq pattern (funcall canon-func opattern))
	    (setq pattern opattern))
	  (when (fboundp flt-func)
	    (setq filter (funcall flt-func))
	    (if (string= opattern " ") (setq opattern ""))
	    (setq opattern (concat opattern filter)))
	  (setq vfolder (mew-folder-to-selection opattern))
	  (mew-summary-switch-to-folder vfolder)
	  (mew-vinfo-set-mode 'selection)
	  (mew-vinfo-set-physical-folder nil)
	  (mew-vinfo-set-original-folder ofolder)
	  (mew-sinfo-set-find-key opattern)
	  (make-local-variable 'mew-summary-form-mark-delete)
	  (setq mew-summary-form-mark-delete nil)
	  (make-local-variable 'mew-summary-form-mark-spam)
	  (setq mew-summary-form-mark-spam nil)
	  (when (mew-summary-exclusive-p)
	    (with-temp-buffer
	      (mew-set-buffer-multibyte t)
	      (mew-piolet mew-cs-text-for-read mew-cs-text-for-write
		(setq file-rttl (funcall func pattern flds filter)))))
	  (mew-set '(file rttl) file-rttl)
	  (setq dfunc `(lambda () (mew-delete-file ,file)))
	  (setq opts (list "-i" file))
	  (mew-local-retrieve 'vir opts dfunc nil nil rttl))))))

(defun mew-summary-make-index-folder ()
  "Make index for this folder."
  (interactive)
  (mew-summary-only
   (if (not mew-search-method)
       (message "No search method")
     (let* ((ent (mew-search-get-ent mew-search-method))
	    (func (mew-search-get-func-index-folder ent))
	    (folder (mew-summary-folder-name 'ext)))
       (if (not (fboundp func))
	   (message "This command cannot be used")
	 (funcall func folder))))))

(defun mew-summary-make-index-all ()
  "Make index for all folders."
  (interactive)
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-index-all ent)))
      (if (fboundp func)
	  (funcall func)
	(message "This command cannot be used")))))

(defun mew-summary-search-register ()
  (interactive)
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-register ent)))
       (if (not (fboundp func))
	   (message "This command cannot be used")
	 (funcall func)))))

(defun mew-summary-search-unregister ()
  (interactive)
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-unregister ent)))
       (if (not (fboundp func))
	   (message "This command cannot be used")
	 (funcall func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Spotlight
;;;

(defun mew-assoc-match-list (key alist nth)
  (let ((case-fold-search t) a n ret)
    (while alist
      (setq a (car alist))
      (setq n (nth nth a))
      (if (or (and (stringp n) (string-match key n))
	      (equal n key) (eq n t))
	  (setq ret (cons a ret)))
      (setq alist (cdr alist)))
    ret)) ;; not reversed

(defun mew-expand-wildcard-folder (wlist)
  (let (case fld ent ret flds regex alist)
    (while wlist
      (setq ent (car wlist))
      (setq wlist (cdr wlist))
      (setq case (mew-case:folder-case ent))
      (setq fld (mew-case:folder-folder ent))
      (if (not (string-match "\\*$" fld))
	  (setq ret (cons ent ret))
	(setq regex (substring fld 0 -1))
	(setq regex (concat "^" (regexp-quote regex)))
	(cond
	 ((mew-folder-popp fld)
	  (setq alist (mew-pop-folder-alist)))
	 ((mew-folder-nntpp fld)
	  (setq alist (mew-nntp-folder-alist case)))
	 ((mew-folder-imapp fld)
	  (setq alist (mew-imap-folder-alist case)))
	 (t
	  (setq alist (mew-local-folder-alist))))
	(setq flds (mew-assoc-match-list regex alist 0))
	(setq flds (mapcar (lambda (x) (mew-case-folder case (nth 0 x))) flds))
	(setq ret (nconc flds ret))))
    (nreverse ret)))

(defun mew-search-spotlight (pattern path)
  (setq pattern (mew-cs-encode-string pattern 'utf-8))
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (mew-plet
     (mew-alet
      (call-process prog nil t nil "-onlyin" path pattern)))))

(defun mew-search-with-spotlight (pattern folder &optional dummy)
  (let ((path (mew-expand-folder folder))
	msgs)
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (mew-search-spotlight pattern path)
      (goto-char (point-min))
      (while (re-search-forward mew-regex-message-files5 nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line))
      (setq msgs (nreverse msgs))
      (setq msgs (sort (mapcar 'string-to-number msgs) '<))
      (mapcar 'number-to-string msgs))))

(defun mew-search-virtual-with-spotlight (pattern flds &optional dummy)
  (let* ((mpath (mew-expand-folder mew-folder-local))
	 (mail-regex (regexp-quote (file-name-as-directory mpath)))
	 (regex (format "^%s%s\\([0-9]+\\)\\(%s\\)?$" mail-regex (file-name-as-directory "\\(.*\\)") mew-suffix))
	 (file (mew-make-temp-name))
	 (prev "") (rttl 0) (n 1) crnt path)
    (unless flds (setq flds (list mew-folder-local)))
    (setq flds (mew-expand-wildcard-folder flds))
    (while flds
      (setq path (mew-expand-folder (car flds)))
      (setq flds (cdr flds))
      (mew-search-spotlight pattern path)
      (goto-char (point-min))
      (while (not (eobp))
	(when (looking-at regex)
	  (setq rttl (1+ rttl))
	  (setq crnt (match-string 1))
	  (delete-region (match-beginning 0) (match-beginning 2))
	  (when (not (string= crnt prev))
	    (beginning-of-line)
	    (insert "CD:" mew-folder-local crnt "\n"))
	  (setq prev crnt)
	  (forward-line)))
      (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
	(write-region (point-min) (point-max) file (> n 1) 'no-msg))
      (mew-erase-buffer)
      (setq n (1+ n)))
    (list file rttl)))

(defun mew-spotlight-index (dir)
  (let* ((default-directory dir)
	 (msgs (mew-dir-messages dir mew-regex-message-files3 'full))
	 dirent subdir)
    (while msgs
      (mew-set-file-type (car msgs))
      (setq msgs (cdr msgs)))
    (when (/= (mew-file-get-links dir) 2)
      (setq dirent (directory-files "." nil mew-regex-folder-candidate))
      (while dirent
	(setq subdir (car dirent))
	(setq dirent (cdr dirent))
	(when (and (file-directory-p subdir)
		   (not (file-symlink-p subdir)))
	  (mew-spotlight-index (expand-file-name subdir dir)))))))

(defun mew-spotlight-index-folder (folder)
  "Making spotlight index for this folder."
  (let* ((dir (mew-expand-folder folder))
	 (msgs (mew-dir-messages dir mew-regex-message-files3 'full)))
    (message "Spotlight indexing for %s..." folder)
    (while msgs
      (mew-set-file-type (car msgs))
      (setq msgs (cdr msgs)))
    (message "Spotlight indexing for %s...done" folder)))

(defun mew-spotlight-index-all ()
  "Making spotlight index for all messages."
  (interactive)
  (when (y-or-n-p "Make Spotlight index for all folders? ")
    (message "Spotlight indexing for all folders...")
    (let ((flds '("+" "+#imap" "+#pop" "+#nntp"))
	  dir)
      (while flds
	(setq dir (mew-expand-folder (car flds)))
	(setq flds (cdr flds))
	(if (file-directory-p dir) (mew-spotlight-index dir))))
    (message "Spotlight indexing for all folders...done")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Google Desktop
;;;

(defun mew-search-google (pattern path)
  (setq pattern (mew-cs-encode-string pattern 'shift_jis))
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (mew-plet
     (mew-alet
      (call-process prog nil t nil "-m" "-p" path "-s" "-q" pattern)))))

(defun mew-search-with-google (pattern folder &optional dummy)
  (let* ((path (mew-expand-folder folder))
	 msgs)
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (mew-search-google pattern path)
      (goto-char (point-min))
      (while (re-search-forward mew-regex-message-files5 nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line))
      (setq msgs (nreverse msgs))
      (setq msgs (sort (mapcar 'string-to-number msgs) '<))
      (mapcar 'number-to-string msgs))))

;; xxx flds are not used at this moment
(defun mew-search-virtual-with-google (pattern flds &optional dummy)
  (let* ((path (mew-expand-folder mew-folder-local))
	 (mail-regex (regexp-quote (file-name-as-directory path)))
	 (regex (concat "^" mail-regex "\\(.*\\)/" "\\([0-9]+\\)"))
	 (file (mew-make-temp-name))
	 (prev "") (rttl 0)
	 crnt)
    (mew-search-google pattern path)
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at regex)
	(setq rttl (1+ rttl))
	(setq crnt (match-string 1))
	(delete-region (match-beginning 0) (match-beginning 2))
	(when (not (string= crnt prev))
	  (beginning-of-line)
	  (insert "CD:" mew-folder-local crnt "\n"))
	(setq prev crnt)
	(forward-line)))
    (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
      (write-region (point-min) (point-max) file nil 'no-msg))
    (list file rttl)))

(defun mew-google-index-folder (folder)
  "Make Google index for this folder."
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (start-process prog nil prog "-m" "-i" (mew-expand-folder folder))
    (message "Google indexing for %s in background..." folder)))

(defun mew-google-index-all ()
  "Make Google index for all folders."
  (interactive)
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (start-process prog nil prog "-m" "-r" "-i" (mew-expand-folder "+"))
    (message "Google indexing for all folders in background...")))

(defun mew-google-register ()
  "Register Google component"
  (interactive)
  (message "Registering Google component...")
  (call-process mew-prog-google nil nil nil "-R")
  (message "Registering Google component...done"))

(defun mew-google-unregister ()
  "Unregister Google component"
  (interactive)
  (message "Unregistering Google component...")
  (call-process mew-prog-google nil nil nil "-U")
  (message "Unregistering Google component...done"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WDS Desktop
;;;

(defun mew-search-wds (pattern path)
  (setq pattern (mew-cs-encode-string pattern default-file-name-coding-system))
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (mew-plet
     (mew-alet
      (call-process prog nil t nil "-e" mew-suffix "-p" path "-s" pattern)))))

(defun mew-search-with-wds (pattern folder &optional dummy)
  (let* ((path (mew-expand-folder folder))
	 msgs)
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (mew-search-wds pattern path)
      (goto-char (point-min))
      (while (re-search-forward mew-regex-message-files5 nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line))
      (setq msgs (nreverse msgs))
      (setq msgs (sort (mapcar 'string-to-number msgs) '<))
      (mapcar 'number-to-string msgs))))

;; xxx flds are not used at this moment
(defun mew-search-virtual-with-wds (pattern flds &optional dummy)
  (let* ((path (mew-expand-folder mew-folder-local))
	 (mail-regex (regexp-quote (file-name-as-directory path)))
	 (regex (concat "^" mail-regex "\\(.*\\)/" "\\([0-9]+\\)"))
	 (file (mew-make-temp-name))
	 (prev "") (rttl 0)
	 crnt)
    (mew-search-wds pattern path)
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at regex)
	(setq rttl (1+ rttl))
	(setq crnt (match-string 1))
	(delete-region (match-beginning 0) (match-beginning 2))
	(when (not (string= crnt prev))
	  (beginning-of-line)
	  (insert "CD:" mew-folder-local crnt "\n"))
	(setq prev crnt))
      (forward-line))
    (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
      (write-region (point-min) (point-max) file nil 'no-msg))
    (list file rttl)))

(defun mew-wds-index-folder (folder)
  "Make WDS index for all folders."
  (mew-wds-index-all))

(defun mew-wds-index-all ()
  "Make WDS index for all folders."
  (interactive)
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent)))
    (start-process prog nil prog "-R" (format "%s=%s" mew-suffix ".eml"))
    (message "WDS indexing for all folders in background...")))

(defun mew-wds-register ()
  "Register '.mew' to WDS"
  (interactive)
  (message "Registering suffix '%s' to WDS..." mew-suffix)
  (call-process mew-prog-wds nil nil nil "-R" (format "%s=%s" mew-suffix ".eml"))
  (message "Registering suffix '%s' to WDS...done" mew-suffix))

(defun mew-wds-unregister ()
  "Unregister '.mew' from WDS"
  (interactive)
  (message "This command cannot be used"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hyper Estraier
;;;

(defun mew-est-input-filter ()
  (mew-input-pick-pattern "Hyper Estraier filter"))

(defvar mew-search-est-db "casket")
(defvar mew-prog-est-update "mewest")

(defun mew-search-est (pattern path filter)
  (setq pattern (mew-cs-encode-string pattern mew-cs-est))
  (if (string= filter "")
      (setq filter nil)
    (setq filter (mew-cs-encode-string filter mew-cs-est))
    (setq filter (let ((mew-inherit-pick-omit-and t))
		   (mew-pick-parse (mew-pick-lex filter))))
    (setq filter (mapcar 'mew-pick-filter-est-kyvl filter)))
  (let* ((ent (mew-search-get-ent mew-search-method))
	 (prog (mew-search-get-prog ent))
	 (casket (expand-file-name mew-search-est-db mew-conf-path))
	 ;; mew-home is not good for Zaurus
	 (pregex (concat "^"
			 (regexp-quote
			  (file-name-directory
			   (directory-file-name
			    (file-truename
			     (expand-file-name mew-mail-path)))))))
	 attr args)
    (setq path (file-truename (file-name-as-directory path)))
    (when (string-match pregex path)
      (setq path (substring path (match-end 0)))
      (mew-plet
       (mew-alet
	(setq attr (format "@uri STRINC %s" (mew-q-encode-string path ?%)))
	(cond
	 ((string-match "^ *ANDNOT " pattern)
	  (setq pattern (concat "[UVSET] " pattern)))
	 ((string-match "^ *$" pattern)
	  (setq pattern "[UVSET]")))
	(setq args (list "-vu" "-max" "-1" "-ord" "@cdate NUMA" casket pattern))
	(unless (eq mew-cs-est 'utf-8)
	  (setq args (cons (mew-cs-to-charset mew-cs-est) args))
	  (setq args (cons "-ic" args)))
	(setq filter (nreverse filter))
	(while filter
	  (setq args (cons "-attr" (cons (car filter) args)))
	  (setq filter (cdr filter)))
	(setq args (cons "-attr" (cons attr args)))
	(apply 'call-process prog nil t nil "search" args))))))

(defun mew-search-with-est (pattern folder filter)
  (let* ((path (mew-expand-folder folder))
	 (regex (format "file://.*/%s/.*/\\([0-9]+\\)\\(%s\\)?$"
			(file-name-nondirectory mew-mail-path)
			(regexp-quote mew-suffix)))
	 msgs)
    (with-temp-buffer
      (mew-set-buffer-multibyte t)
      (mew-search-est pattern path filter)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(setq msgs (cons (mew-match-string 1) msgs))
	(forward-line))
      (setq msgs (nreverse msgs))
      (setq msgs (sort (mapcar 'string-to-number msgs) '<))
      (mapcar 'number-to-string msgs))))

(defun mew-search-virtual-with-est (pattern flds filter)
  (let* ((regex (format "file://.*/%s/\\(.*\\)/\\([0-9]+\\)\\(%s\\)?$"
			(file-name-nondirectory mew-mail-path)
			(regexp-quote mew-suffix)))
	 (file (mew-make-temp-name))
	 (prev "") (rttl 0) (n 1)
	 path crnt start)
    (unless flds (setq flds (list mew-folder-local)))
    (setq flds (mew-expand-wildcard-folder flds))
    (while flds
      (setq path (mew-expand-folder (car flds)))
      (setq flds (cdr flds))
      (mew-search-est pattern path filter)
      (goto-char (point-min))
      (setq start (point))
      (while (re-search-forward regex nil t)
	(setq rttl (1+ rttl))
	(setq crnt (match-string 1))
	(delete-region start (match-beginning 2))
	(when (not (string= crnt prev))
	  (beginning-of-line)
	  (insert "CD:" mew-folder-local (mew-q-decode-string crnt ?%) "\n"))
	(setq prev crnt)
	(forward-line)
	(setq start (point)))
      (delete-region start (point-max))
      (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
	(write-region (point-min) (point-max) file (> n 1) 'no-msg))
      (mew-erase-buffer)
      (setq n (1+ n)))
    (list file rttl)))

(defun mew-est-index-folder (folder)
  "Make Hyper Estraier index for this folder."
  (interactive)
  (if (not (mew-which-exec mew-prog-est-update))
      (message "\"%s\" does not exist" mew-prog-est-update)
    (message "Hyper Estraier indexing for %s..." folder)
    (let* ((path (file-truename (mew-expand-folder folder)))
	   (pro (start-process "*Mew EST*" nil mew-prog-est-update "-s" mew-suffix "-b" (mew-expand-folder mew-mail-path) path)))
      (set-process-filter pro 'mew-est-index-filter)
      (set-process-sentinel pro 'mew-est-index-sentinel))))

(defun mew-est-index-all ()
  "Make Hyper Estraier index for all folders."
  (interactive)
  (if (not (mew-which-exec mew-prog-est-update))
      (message "'%s' does not exist" mew-prog-est-update)
    (message "Hyper Estraier indexing...")
    (let ((pro (start-process "*Mew EST*" nil mew-prog-est-update "-s" mew-suffix "-b" (mew-expand-folder mew-mail-path))))
      (set-process-filter pro 'mew-est-index-filter)
      (set-process-sentinel pro 'mew-est-index-sentinel))))

(defun mew-est-index-filter (process string)
  (save-excursion
    (cond
     ((string-match "exists" string)
      (message "Another Hyper Estraier indexer is running"))
     ((string-match "\.\.\.failed" string)
      (message "Hyper Estraier indexing ...failed"))
     ((string-match "old messages\.\.\.done" string)
      (message "Hyper Estraier purging ...done"))
     ((string-match "new messages\.\.\.done" string)
      (message "Hyper Estraier indexing ...done")))))

(defun mew-est-index-sentinel (process event)
  (save-excursion ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View
;;;

(defvar mew-prog-smew "smew")
(defvar mew-prog-cmew "cmew")
(defvar mew-id-db-file "id.db")
(defvar mew-id-db-ignore-regex "^\\./casket$|^\\./casket/|/\\.")

(defmacro mew-msgid-check (&rest body)
  `(let ((db (mew-expand-file "+" mew-id-db-file)))
     (cond
      ((not (file-exists-p db))
       (message "%s not found" mew-id-db-file))
      ((not (mew-which-exec mew-prog-smew))
       (message "%s not found" mew-prog-smew))
      (t
      ,@body))))

(defun mew-summary-selection-by-msgid ()
  "Creating Virtual mode with messages relating to the current message"
  (interactive)
  (mew-msgid-check
   (mew-summary-msg
    (let* ((ofolder (mew-summary-folder-name 'ext))
	   (subj (or (mew-summary-get-subject) mew-error-no-subject))
	   (str (mew-subject-simplify2 subj))
	   (vfolder (mew-folder-to-selection
		     (if (string= str "") mew-error-no-subject subj)))
	   (regex (format "\\(.*\\)/\\([0-9]+\\)\\(%s\\)?$" (regexp-quote mew-suffix)))
	   (rttl 0)
	   (file (mew-make-temp-name))
	   (db (mew-expand-file "+" mew-id-db-file))
	   (mydir (substring (mew-path-to-folder (mew-expand-folder (mew-summary-folder-name))) 1))
	   crnt start prev opts dfunc myid)
      (mew-sumsyn-match mew-regex-sumsyn-long)
      (setq myid (mew-sumsyn-my-id))
      ;;
      (mew-summary-switch-to-folder vfolder)
      (mew-vinfo-set-mode 'selection)
      (mew-vinfo-set-physical-folder nil)
      (mew-vinfo-set-original-folder ofolder)
      (mew-erase-buffer)
      ;;
      (make-local-variable 'mew-summary-form-mark-delete)
      (setq mew-summary-form-mark-delete nil)
      (make-local-variable 'mew-summary-form-mark-spam)
      (setq mew-summary-form-mark-spam nil)
      (when (mew-summary-exclusive-p)
	(message "Creating selection by message-id...")
	(mew-redraw)
	(with-temp-buffer
	  (mew-set-buffer-multibyte t)
	  (mew-piolet mew-cs-text-for-read mew-cs-text-for-write
	    (call-process mew-prog-smew nil t nil myid db mydir)
	    (goto-char (point-min))
	    (setq start (point))
	    (while (re-search-forward regex nil t)
	      (setq rttl (1+ rttl))
	      (setq crnt (match-string 1))
	      (delete-region start (match-beginning 2))
	      (when (not (string= crnt prev))
		(beginning-of-line)
		(insert "CD:" mew-folder-local crnt "\n"))
	      (setq prev crnt)
	      (forward-line)
	      (setq start (point)))
	    (delete-region start (point-max))
	    (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
	      (write-region (point-min) (point-max) file nil 'no-msg))))
	(setq dfunc `(lambda () (mew-delete-file ,file)))
	(setq opts (list "-i" file))
	(mew-local-retrieve 'vir opts dfunc nil nil rttl))))))

(defun mew-summary-get-subject ()
  (let* ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number))
	 (cache (mew-cache-hit fld msg))
	 subj)
    (if (and cache (get-buffer cache))
	(with-current-buffer cache
	  (setq subj (mew-header-get-value mew-subj:)))
      (with-temp-buffer
	(mew-insert-message
	 fld msg mew-cs-text-for-read mew-header-reasonable-size)
	(mew-refile-decode-subject)
	(setq subj (mew-header-get-value mew-subj:))))
    subj))

(defun mew-summary-make-id-index-folder ()
  "Make ID database for this folder."
  (interactive)
  (mew-summary-only
   (cond
    ((not (file-exists-p (expand-file-name mew-id-db-file mew-mail-path)))
     (message (mew-substitute-for-summary "Type '\\[mew-summary-make-id-index-all]' to create ID database")))
    ((not (mew-which-exec mew-prog-cmew))
     (message "%s not found" mew-prog-cmew))
    (t
     (let* ((dbfile (expand-file-name mew-id-db-file mew-mail-path))
	    (path (expand-file-name mew-mail-path))
	    (regex mew-id-db-ignore-regex)
	    (target (mew-folder-string
		     (mew-path-to-folder
		      (mew-expand-folder (mew-summary-folder-name 'ext)))))
	    (options (list dbfile path regex target))
	    (buf (generate-new-buffer (concat mew-buffer-prefix "cmew")))
	    pro)
       (message "Updating ID database...")
       (setq pro (apply 'start-process "cmew" buf mew-prog-cmew options))
       (set-process-filter pro 'mew-summary-make-id-index-filter)
       (set-process-sentinel pro 'mew-summary-make-id-index-sentinel))))))

(defun mew-summary-make-id-index-all (&optional full-update)
  "Make ID database for all folders.
If called with '\\[universal-argument]', the database is created
from scratch."
  (interactive "P")
  (if (not (mew-which-exec mew-prog-cmew))
      (message "%s not found" mew-prog-cmew)
    (let* ((dbfile (expand-file-name mew-id-db-file mew-mail-path))
	   (path (expand-file-name mew-mail-path))
	   (options (list dbfile path mew-id-db-ignore-regex))
	   (buf (generate-new-buffer (concat mew-buffer-prefix "cmew")))
	   pro)
      (if full-update (setq options (cons "-f" options)))
      (message "Updating ID database...")
      (setq pro (apply 'start-process "cmew" buf mew-prog-cmew options))
      (set-process-filter pro 'mew-summary-make-id-index-filter)
      (set-process-sentinel pro 'mew-summary-make-id-index-sentinel))))

(defun mew-summary-make-id-index-filter (process string)
  (save-excursion
    (mew-filter
     (goto-char (point-max))
     (insert string))))

(defun mew-summary-make-id-index-sentinel (process event)
  (let ((buf (process-buffer process)))
    (with-current-buffer buf
      (goto-char (point-max))
      (forward-line -1)
      (let ((str (mew-buffer-substring (line-beginning-position)
				       (line-end-position))))
	(message "Updating ID database...done (%s)" str)))
    (mew-remove-buffer buf)))

(provide 'mew-search)

;;; Copyright Notice:

;; Copyright (C) 2005-2011 Mew developing team.
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

;;; mew-search.el ends here
