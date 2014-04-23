;;; mew-summary4.el --- Summary mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-last-shell-command "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Switching to folder
;;;

(defun mew-summary-switch-to-folder (case:folder &optional func)
  (let ((ofolder (mew-summary-folder-name 'ext)))
    (cond
     ((get-buffer case:folder)
      (switch-to-buffer case:folder)
      (mew-set-buffer-cs mew-cs-m17n)
      (unless (mew-folder-virtualp case:folder) (mew-summary-folder-cache-load))
      (unless (string= ofolder case:folder) (mew-window-configure 'summary))
      nil) ;; existing folder
     (t
      (funcall (or func 'switch-to-buffer) (get-buffer-create case:folder))
      (kill-all-local-variables)
      (mew-set-buffer-cs mew-cs-m17n)
      (mew-buffers-setup case:folder)
      (let* ((case (mew-case:folder-case case:folder))
	     (folder (mew-case:folder-folder case:folder))
	     (inboxp (string= folder (mew-proto-inbox-folder folder (or case mew-case)))))
	(mew-sinfo-set-case case)
	(mew-sinfo-set-folder folder)
	(mew-sinfo-set-inboxp inboxp))
      (if (mew-folder-virtualp case:folder)
	  (mew-virtual-mode)
	(mew-summary-mode)
	(if mew-summary-trace-directory (cd (mew-expand-folder case:folder)))
	(mew-summary-folder-cache-load))
      (mew-window-configure 'summary)
      t)))) ;; new-folder

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to folder
;;;

(defun mew-summary-goto-folder (&optional goend)
  "Go to the folder which you specify.  If executed with
'\\[universal-argument]', the cursor always goes to the bottom of
Summary mode."
  (interactive "P")
  (let* ((proto (mew-proto-to-go (mew-summary-folder-name 'ext)))
	 (case1 (car (mew-summary-case-proto)))
	 (inbox (mew-proto-inbox-folder	proto case1))
	 (case:folder (mew-input-folder case1 inbox))
	 case folder buf win frame folder-alist)
    (if (null case:folder)
	(message "The folder does not exist")
      (setq case (mew-case:folder-case case:folder))
      (setq folder (mew-case:folder-folder case:folder))
      (cond
       ((mew-folder-imapp folder)
	(setq folder-alist (mew-imap-folder-alist case))
	;;
	(unless (or (assoc folder folder-alist)
		    (assoc (concat folder (mew-imap-separator case)) folder-alist))
	  (message "\"%s\" does not exist. %s" case:folder
		   (mew-substitute-for-summary "Type '\\[universal-argument]\\[mew-status-update]' to collect IMAP folders"))
	  (setq case:folder nil)))
       ((mew-folder-nntpp folder)
	(unless (assoc folder (mew-nntp-folder-alist case))
	  (message "\"%s\" does not exist. %s" case:folder
		   (mew-substitute-for-summary "Type '\\[universal-argument]\\[mew-status-update]' to collect newsgroups"))
	  (setq case:folder nil))))
      (when case:folder
	(when mew-use-other-frame-for-summary
	  (if (setq buf (get-buffer case:folder))
	      (if (setq win (get-buffer-window buf 0))
		  (progn
		    (setq frame (window-frame win))
		    (raise-frame frame)
		    (select-frame frame)
		    ;; Ensure, if possible, that frame gets input focus.
		    (mew-focus-frame frame))
		(select-frame (make-frame)))
	    (select-frame (make-frame))))
	(mew-summary-visit-folder case:folder goend)))))

(defun mew-summary-visit-folder (folder &optional goend no-ls)
  (let ((dir (mew-expand-folder folder))
	new-folder scanp)
    (cond
     ((mew-folder-virtualp folder)
      (if (get-buffer folder)
	  (if (mew-virtual-thread-p folder)
	      (if (mew-thread-cache-valid-p folder)
		  (mew-summary-switch-to-folder folder)
		(message "%s is old" folder))
	    (mew-summary-switch-to-folder folder))
        (message "No such virtual folder: %s" folder)))
     (t ;; local/remote folders
      (if (null dir)
	  (message "Folder is wrong")
	(cond
	 ((mew-folder-remotep (mew-case:folder-folder folder))
	  (unless (file-directory-p dir)
	    (mew-make-directory dir))
	  (setq scanp t))
	 (t ;; +, ~, /, drive letter
	  (if (file-directory-p dir)
	      (setq scanp t)
	    (message "No such folder %s" folder))))
	(when scanp
	  (if no-ls
	      (progn
		(mew-summary-switch-to-folder folder)
		(if goend (goto-char (point-max)))
		(if (pos-visible-in-window-p (point-min))
		    (mew-summary-cook-window)))
	    (setq new-folder (mew-summary-switch-to-folder folder))
	    (if (and (mew-summary-ls nil (or goend new-folder))
		     (pos-visible-in-window-p (point-min)))
		(mew-summary-cook-window)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Going to a line
;;;

(defun mew-summary-goto-line (&optional arg)
  "Jump to a line according to the number which you input.
If 'mew-summary-goto-line-then-display' is non-nil,
the message is then displayed."
  (interactive "P")
  (if arg
      (let ((msg (read-string "Message No.: " "")))
	(while (not (string-match mew-regex-message-files3 msg))
	  (setq msg (read-string "Message No.: " "")))
	(when (mew-summary-search-msg msg)
	  (if mew-summary-goto-line-then-display
	      (mew-summary-display))))
    (let ((line (string-to-number (read-string "Line No.: " ""))))
      (when (/= line 0)
	(goto-char (point-min)) (forward-line (1- line))
	(if mew-summary-goto-line-then-display
	    (mew-summary-display))))))

(defun mew-summary-goto-original-message ()
  "Go to the original message in the original Summary mode"
  (interactive)
  (cond
   ((mew-summary-p)
    (message "This is the original"))
   ((mew-virtual-p)
    (let ((folder (mew-folder-path-to-folder (mew-summary-folder-name) 'has-proto))
	  (msg (mew-summary-message-number)))
      (when folder
	(mew-summary-visit-folder folder nil 'no-ls)
	(if (mew-summary-search-msg msg)
	    (mew-summary-display)
	  (message "No original message found")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to Message mode
;;;

(defun mew-summary-goto-msg-mode ()
  "Go to Header mode."
  (interactive)
  (let (msgp)
    (with-current-buffer (window-buffer (next-window))
      (if (mew-message-p) (setq msgp t)))
    (if msgp
	(other-window 1)
      (message "No Message mode displayed"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to +draft
;;;

(defun mew-max-draft-buffer ()
  (let* ((draft-dir (file-name-as-directory mew-draft-folder))
	 (regex (mew-folder-regex draft-dir))
         (bufs (mew-buffer-list regex))
	 (msgregex (format "\\([0-9]+\\)\\(%s\\)?$" mew-suffix))
         nums n)
    (setq nums (mapcar (lambda (x) (string-match msgregex x) (string-to-number (mew-match-string 1 x))) bufs))
    (dolist (num nums)
      (if (or (null n) (< n num))
	  (setq n num)))
    (and n (format "%s%d%s" draft-dir n (if mew-use-suffix mew-suffix "")))))

(defun mew-summary-jump-to-draft-buffer ()
  "Jump to one of drafts if exists."
  (interactive)
  (let* ((draft (mew-max-draft-buffer)) buf)
    (if (null draft)
	(message "No draft buffer exists!")
      (setq buf (mew-input-draft-buffer draft))
      (if (get-buffer buf)
	  (switch-to-buffer buf)
	(message "No such draft buffer!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to top and bottom
;;;

(defun mew-summary-jump-top ()
  "Go to the beginning of this Summary mode.
If 'mew-summary-jump-top-then-display' is non-nil,
the top message is then displayed."
  (interactive)
  (if mew-use-push-mark (mew-push-mark))
  (goto-char (point-min))
  (if mew-summary-jump-top-then-display
      (mew-summary-display)))

(defun mew-summary-jump-bottom ()
  "Go to the end of this Summary mode.
If 'mew-summary-jump-bottom-then-display' is non-nil,
the top message is then displayed."
  (interactive)
  (if mew-use-push-mark (mew-push-mark))
  (goto-char (point-max))
  (if (and (mew-summary-exclusive-p 'no-msg)
	   (not (bobp))
	   (mew-sinfo-get-disp-msg))
      (forward-line -1))
  (if mew-summary-jump-bottom-then-display
      (mew-summary-display)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to the marker
;;;

(defun mew-summary-exchange-point ()
  "\\<mew-summary-mode-map>Get back to the position before typing '\\[mew-summary-retrieve]' or '\\[mew-summary-ls]'."
  (interactive)
  (mew-summary-only
   (let ((pos (mew-sinfo-get-ret-pos)))
     (if pos (goto-char pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Searching
;;;

(defun mew-summary-isearch-forward (&optional arg)
  "Incremental search forward in Message mode."
  (interactive "P")
  (let ((cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (widen)
	    (isearch-forward arg)
	    (mew-message-narrow-to-page))
	(select-window cwin)))))

(defun mew-summary-isearch-backward (&optional arg)
  "Incremental search backward in Message mode."
  (interactive)
  (let ((cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (widen)
	    (isearch-backward arg)
	    (mew-message-narrow-to-page))
	(select-window cwin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pipe
;;;

(defun mew-summary-pipe-message (prefix command)
  "Send the content of Message buffer to a command via pipe.
If called with '\\[universal-argument]', the body of the message
\(excluding its header) is sent."
  (interactive
   (list current-prefix-arg
	 (read-string "Shell command on message: " mew-last-shell-command)))
  (mew-summary-display 'redisplay)
  (when (or (not mew-ask-pipe)
            (y-or-n-p "Send this message to pipe? "))
    (with-current-buffer (mew-buffer-message)
      (save-restriction
	(widen)
	(if (string= command "") (setq command mew-last-shell-command))
	(goto-char (point-min)) ; perhaps this line won't be necessary
	(if prefix (search-forward "\n\n"))
	(let ((max-mini-window-height 1))
	  (shell-command-on-region (point) (point-max) command nil))
	(setq mew-last-shell-command command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print
;;;

(defun mew-summary-print (&optional arg)
  "Print the content of Message mode according to 'mew-print-function'.
If called with '\\[universal-argument]', you can specify a printer name."
  (interactive "P")
  (mew-summary-display 'redisplay)
  (let ((printer-name (if arg
			  (read-string "Printer name: ")
			printer-name))
	have-header str)
    (with-current-buffer (mew-buffer-message)
      (save-restriction
	(widen)
	(setq have-header (mew-msghdr-p))
	(setq str (buffer-substring (point-min) (point-max)))))
    (when (y-or-n-p "Print this message? ")
      (with-temp-buffer
	(insert str)
	(if have-header
	    (mew-header-delete-other-lines mew-field-for-printing))
	(funcall mew-print-function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saving a message to a file with header decoded
;;;

(defun mew-summary-store (&optional askcs)
  "Saving a buffer of Message mode to a file.
If executed with '\\[universal-argument]', coding-system is asked."
  (interactive "P")
  (mew-summary-display 'redisplay)
  (let* ((file (mew-summary-input-file-name))
	 (doit (if (file-exists-p file)
		   (if (y-or-n-p (format "File exists. Override it? "))
		       t
		     nil)
		 t)))
    (if (not doit)
	(message "Not saved")
      (let ((writecs (if askcs
			 (read-coding-system "Coding-system: ")
		       (default-value 'buffer-file-coding-system))))
	(with-current-buffer (mew-buffer-message)
	  (mew-frwlet mew-cs-dummy writecs
	    ;; do not specify 'no-msg
	    (write-region (point-min) (point-max) file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X-Face:
;;;

(defun mew-summary-x-face ()
  "Display xface."
  (interactive)
  (mew-summary-msg
   (let ((file (mew-make-temp-name)) xface)
     (with-current-buffer (mew-buffer-message)
       (setq xface (mew-header-get-value mew-x-face:)))
     (when xface
       (with-temp-buffer
	 (insert xface)
	 (mew-x-face-compface-to-xbm)
	 (mew-frwlet mew-cs-dummy mew-cs-text-for-write
	   (write-region (point-min) (point-max) file nil 'no-msg)))
       (mew-mime-image-ext file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calling a command
;;;

(defun mew-summary-cmd-msg ()
  "Executing an external command specifying this message as an
argument."
  (interactive)
  (mew-summary-msg-or-part
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (file (mew-expand-msg fld msg))
	  (cmd-args (mew-input-command))
	  (cmd (car cmd-args))
	  (args (nconc (cdr cmd-args) (list file))))
     (message "Executing %s for %s..." cmd msg)
     (apply 'call-process cmd nil nil nil args)
     (message "Executing %s for %s...done" cmd msg))))

(defun mew-summary-cmd-msgs ()
  "Executing an external command specifying messages
marked with '*' as arguments."
  (interactive)
  (mew-summary-multi-msgs
   (let* ((cmd-args (mew-input-command))
	  (cmd (car cmd-args))
	  (args (nconc (cdr cmd-args) FILES)))
     (message "Executing %s ..." cmd)
     (apply 'call-process cmd nil nil nil args)
     (message "Executing %s ...done" cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Citation
;;;

(defun mew-summary-cite (&optional arg)
  "Cite this message to one of drafts."
  (interactive "P")
  (let ((draft (mew-max-draft-buffer)) buf)
    (if (null draft)
	(message "No draft buffer exists!")
      (setq buf (mew-input-draft-buffer draft))
      (if (get-buffer buf)
	  (with-current-buffer buf
	    (mew-draft-cite arg))
	(message "No such draft buffer!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Flushing the queue
;;;

(defun mew-summary-send-message (&optional arg)
  "If in +queue, send the messages in +queue.
If in +postq, post the messages in +postq.
If in %queue, process the jobs in %queue.
Otherwise, flush the default queue.
If executed with '\\[universal-argument]', you can set the sending case."
  (interactive "P")
  (if (mew-folder-imap-queuep)
      (progn
	(mew-window-configure 'summary)
	(mew-imap-flush-queue))
    (let* ((fld (mew-summary-folder-name 'ext))
	   (proto (mew-proto-to-flush fld))
	   qfld case)
      (if (or arg mew-ask-flush-case)
	  (setq case (mew-input-case mew-case "Queue"))
	(setq case mew-case))
      (if (or (mew-folder-queuep fld) (mew-folder-postqp fld))
	  (progn
	    (setq qfld fld)
	    (mew-window-configure 'summary))
	(setq qfld (mew-proto-queue-folder proto case)))
      (if (and mew-ask-flush-queue
	       (not (y-or-n-p (concat "Flush " qfld  "? "))))
	  (message "The queue is not flushed")
	(cond
	 ((mew-folder-postqp qfld)
	  (mew-nntp2-flush-queue case qfld))
	 (t
	  (mew-smtp-flush-queue case qfld)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unlocking
;;;

(defun mew-subprocess-init ()
  (add-hook 'kill-emacs-hook 'mew-subprocess-kill))

(defun mew-subprocess-clean-up ()
  (mew-summary-kill-subprocess t)
  (remove-hook 'kill-emacs-hook 'mew-subprocess-kil))

(defun mew-subprocess-kill ()
  (mew-summary-kill-subprocess t))

(defun mew-summary-kill-subprocess (&optional kill-ssx)
  "\\<mew-summary-mode-map>
Kill a process in Summary mode.
Sometime a process accidentally remains in Summary mode.
In this situation, you cannot execute '\\[mew-summary-retrieve]', '\\[mew-summary-ls]', nor '\\[mew-summary-exec]'.
Use this command to solve this problem.

If called with '\\[universal-argument]', all SSH processes, if
any, are killed."
  (interactive "P")
  (let ((process mew-summary-buffer-process)
	(disp-msg (mew-called-interactively-p))
	pros sentinel)
    (if kill-ssx
	(progn
	  (setq pros (process-list))
	  (dolist (pro pros)
	    (if (string-match "mew" (process-name pro))
		(delete-process pro)))
	  (if disp-msg (message "All SSH/SSL/TLS processes were killed")))
      (cond
       ((not (processp process))
	;; If you type "x" over SSH and type "C-g" to quit,
	;; refile information remains invisible.
	;; This command turns it visible.
	(mew-summary-visible-buffer (current-buffer))
	(if disp-msg (message "No process to kill. This buffer is unlocked anyway")))
       ((eq process t)
	;; Emacs is locking
	(if disp-msg (message "This buffer was unlocked")))
       ((memq (process-status process) '(open closed))
	;; Network process
	(setq sentinel (process-sentinel process))
	(if (fboundp sentinel)
	    (funcall sentinel process "killed by the user"))
	(delete-process process)
	(if disp-msg (message "The process was killed")))
       (t
	;; Local process or SSH/SSL/TLS
	(delete-process process)
	(if disp-msg (message "The process was killed")))))
    (mew-summary-unlock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; De-composing with the old style
;;;

(defun mew-summary-unshar ()
  "Apply 'unshar' on messages marked with '*'."
  (interactive)
  (mew-summary-multi-msgs
   (if (y-or-n-p (format "Execute %s for these messages? " mew-prog-unshar))
       (let* ((default-directory (mew-summary-input-directory-name)))
	 (message "Executing %s..." mew-prog-unshar)
	 (apply 'call-process mew-prog-unshar nil nil nil FILES)
	 (message "Executing %s...done" mew-prog-unshar)))))

(defun mew-summary-uudecode ()
  "Apply 'uudecode' on messages marked with '*'."
  (interactive)
  (mew-summary-multi-msgs
   (cond
    ((not (mew-which-exec mew-prog-mime-decode))
     ())
    ((not (y-or-n-p "Uudecode these messages? "))
     ())
    (t
     (let ((dir (mew-summary-input-directory-name))
	   (fn nil)
	   (tmp (mew-make-temp-name))
	   (case-fold-search nil)
	   (files FILES) start)
       (with-temp-buffer
	 (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
	   (dolist (file files)
	     (mew-erase-buffer)
	     (mew-insert-file-contents file)
	     (goto-char (point-min))
	     (if (re-search-forward mew-eoh nil t)
		 (forward-line))
	     (setq start (point))
	     (goto-char (point-max))
	     (unless (bolp) (insert "\n"))
	     (write-region start (point-max) tmp 'append 'no-msg)))
	 (mew-erase-buffer)
	 (cd dir)
	 ;; mew-folder-local is dummy to let mewencode use
	 ;; the embedded filename
	 (call-process mew-prog-mime-decode tmp t nil
		       "-d" "-u" "-" mew-folder-local)
	 (mew-delete-file tmp)
	 (goto-char (point-min))
	 (if (not (looking-at "^filename: \\(.*\\)"))
	     (message "Failed to execute %s" mew-prog-mime-decode)
	   (setq fn (mew-match-string 1))
	   (setq fn (mew-summary-prog-exec mew-prog-compress "-df" "Z" fn))
	   (setq fn (mew-summary-prog-exec mew-prog-gzip "-df" "gz" fn))
	   (when (and (string-match "^\\(.*\\)\\.tar$" fn)
		      (y-or-n-p (format "Execute %s for %s? " mew-prog-tar fn)))
	     (message "Executing %s for %s..." mew-prog-tar fn)
	     (call-process mew-prog-tar nil nil nil "-xf" fn)
	     (message "Executing %s for %s...done" mew-prog-tar fn)))))))))

(defun mew-summary-prog-exec (prog opts suffix tarfile)
  (if (string-match (format "^\\(.*\\)\\.%s$" suffix) tarfile)
      (let ((basename (match-string 1 tarfile)))
	(if (not (y-or-n-p (format "Execute %s for %s? " prog tarfile)))
	    tarfile
	  (message "Executing %s for %s..." prog tarfile)
	  (call-process prog nil nil nil opts tarfile)
	  (message "Executing %s for %s...done" prog tarfile)
	  basename))
    tarfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; De-composing with the new style
;;;

(defun mew-summary-join ()
  "Concat Message/Partial fragments marked with '*' to an original
message."
  (interactive)
  (mew-summary-only
   (mew-summary-with-mewl
    (mew-summary-multi-msgs
     (let ((cfld (mew-summary-folder-name))
	   (folder (mew-input-burst-folder))
	   (tfile (mew-make-temp-name))
	   (targets FLD-MSG-LIST)
	   num ct med dbl vec len idx ttl TTL fid FID i
	   beg regex)
       (message "Joining...")
       (with-temp-buffer
	 (mew-set-buffer-multibyte t)
	 (insert "CD: " (mew-expand-folder2 cfld) "\n")
	 (let ((default-directory (mew-expand-folder cfld)))
	   (dolist (fld-msg targets)
	     (insert (mew-msg-get-filename (cdr fld-msg)) "\n")))
	 (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
	   (write-region (point-min) (point-max) tfile nil 'no-msg))
	 (mew-erase-buffer)
	 (call-process mew-prog-mewl nil t nil
		       "-b" mew-mail-path "-l" "0"
		       "-x" mew-suffix
		       "-i" tfile "-d" "content-type")
	 (mew-delete-file tfile)
	 (goto-char (point-min))
	 (while (not (eobp))
	   (if (not (looking-at "^\\([0-9]+\\)[ \t]*:[ \t]*"))
	       (forward-line)
	     (setq num (mew-match-string 1))
	     (setq med (match-end 0))
	     (forward-line)
	     (mew-header-goto-next)
	     (setq ct (mew-param-decode
		       (mew-buffer-substring med (1- (point)))))
	     (setq dbl (cons (cons num ct) dbl))))
	 (setq len (length dbl))
	 (setq vec (make-vector len nil))
	 (dolist (ent dbl)
	   (setq num (car ent))
	   (setq ttl (mew-syntax-get-param ent "total"))
	   (setq fid (mew-syntax-get-param ent "id"))
	   (setq idx (mew-syntax-get-param ent "number"))
	   (if TTL
	       (if (and ttl (not (string= TTL ttl)))
		   (error "total mismatch"))
	     (setq TTL ttl))
	   (if FID
	       (if (or (null fid) (not (string= FID fid)))
		   (error "fragment id mismatch"))
	     (setq FID fid))
	   (unless idx (error "no number"))
	   (setq idx (1- (string-to-number idx)))
	   (if (and (>= idx 0) (< idx len))
	       (aset vec idx num)
	     (error "invalid number")))
	 (unless TTL (error "no total"))
	 (dotimes (i len)
	   (unless (stringp (aref vec i)) (error "Not enough fragments")))
	 ;; now reassemble
	 (mew-erase-buffer)
	 (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
	   ;; the first fragment
	   (goto-char (point-max))
	   (save-restriction
	     (narrow-to-region (point) (point))
	     (mew-insert-file-contents (mew-expand-msg cfld (aref vec 0)))
	     ;; Removing unnecessary fields from the encapsulating
	     ;; (outer) header.
	     (goto-char (point-min))
	     (mew-header-delete-lines mew-field-delete-for-joining)
	     ;; Concatenating the two headers.
	     (goto-char (point-min))
	     (re-search-forward mew-eoh nil t)
	     (setq beg (point))
	     (while (looking-at mew-eoh)
	       (forward-line)
	       (if (eobp) (error "invalid message (number 1)")))
	     (delete-region beg (point))
	     ;; Removing unnecessary fields from the encapsulated
	     ;; (inner) header.
	     (setq beg (point))
	     (when (re-search-forward mew-eoh nil t)
	       (setq regex (mew-make-field-regex mew-field-delete-for-joining))
	       (save-restriction
		 (narrow-to-region beg (point))
		 (goto-char (point-min))
		 (let ((case-fold-search t))
		   (while (not (eobp))
		     (if (looking-at regex)
			 (setq beg nil) ;; logic is reversed
		       (setq beg (point)))
		     (forward-line)
		     (mew-header-goto-next)
		     (if beg (delete-region beg (point))))))))
	   ;; the second and subsequent fragments.
	   (setq i 1)
	   (while (< i len)
	     (goto-char (point-max))
	     (save-restriction
	       (narrow-to-region (point) (point))
	       (mew-insert-file-contents (mew-expand-msg cfld (aref vec i)))
	       (goto-char (point-min))
	       (re-search-forward mew-eoh nil t)
	       (forward-line)
	       (if (eobp) (error "invalid message (number %d)" (1+ i)))
	       (delete-region (point-min) (point)))
	     (setq i (1+ i)))
	   (mew-header-delete-lines `(,mew-x-mew-uidl:))
	   (write-region (point-min) (point-max)
			 (mew-folder-new-message folder)
			 nil 'no-msg)))
       (mew-touch-folder folder)
       (message "Joining...done")
       (when mew-ask-folder-after-join
	 (if (y-or-n-p (format "Go to %s? " folder))
	     (mew-summary-visit-folder folder 'goend))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleaning +trash
;;;

(defun mew-summary-clean-trash ()
  "Remove all messages in +trash."
  (interactive)
  (let* ((case (mew-sinfo-get-case))
	 (folder (mew-sinfo-get-folder))
	 case:trash trash trashdir msgs)
    (cond
     ((mew-folder-imapp folder)
      (setq trash (mew-imap-trash-folder case)))
     (t
      (setq trash mew-trash-folder)))
    (unless trash
      (setq trash folder))
    (setq case:trash (mew-input-folder case trash))
    (if (null case:trash)
	(message "Nothing was processed")
      (setq case (mew-case:folder-case case:trash))
      (setq trash (mew-case:folder-folder case:trash))
      (setq trashdir (mew-expand-folder case:trash))
      (setq msgs (mew-dir-messages trashdir))
      (if (and (mew-folder-localp trash) (null msgs))
	  (message "No messages to be removed in %s" trash)
	(if (not (y-or-n-p (format "Remove all messages in %s? " case:trash)))
	    (message "No messages removed in %s" trash)
	  (if (string= folder trash) (mew-summary-reset))
	  (message "Removing all messages in %s..." case:trash)
	  (when (get-buffer case:trash)
	    (with-current-buffer case:trash
	      (mew-summary-kill-subprocess)))
	  (mew-summary-unlink-msgs case:trash msgs)
	  (mew-summary-folder-cache-clean case:trash)
	  (message "Removing all messages in %s...done" case:trash)
	  (when (mew-folder-imapp trash)
	    (if (get-buffer case:trash)
		(set-buffer case:trash)
	      (mew-summary-switch-to-folder case:trash 'set-buffer))
	    (mew-imap-retrieve case 'exec case:trash
			       (mew-make-execinfo :remove '("1:*")
						  :rttl   0
						  :dttl   -1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copy local
;;;

(defun mew-summary-local-copy ()
  "Copy a message to a local folder."
  (interactive)
  (mew-summary-msg-or-part
   (let ((fld (mew-summary-folder-name))
	 (exclusivep t)
	 msg dstmsg dstfld file dstfile)
     (save-excursion
       (mew-summary-goto-message)
       (setq msg (mew-summary-message-number)))
     (setq file (mew-expand-msg fld msg))
     (setq dstfld (mew-input-local-folder mew-inbox-folder))
     (when (and dstfld (mew-local-folder-check dstfld 'ask))
       (setq dstmsg (mew-folder-new-message dstfld 'numonly))
       (setq dstfile (mew-expand-msg dstfld dstmsg))
       (when (get-buffer dstfld)
	 (set-buffer dstfld)
	 (setq exclusivep (mew-summary-exclusive-p)))
       (cond
	((not exclusivep)
	 (message "Try again later"))
	((string= file dstfile)
	 (message "You cannot copy this onto itself"))
	(t
	 (mew-summary-local-copy-one file dstfile)
	 (mew-touch-folder dstfld)
	 (message "Copied to %s/%s" dstfld dstmsg)))))))

(defun mew-summary-mark-local-copy (&optional arg)
  "Copy messages marked with '*' to a local folder.  If called
with '\\[universal-argument]', only messages marked with '*' in
the region are handled."
 (interactive "P")
  (mew-summary-not-in-draft
   (let ((mew-use-highlight-x-face nil)
	 (fld (mew-summary-folder-name))
	 (exclusivep t)
	 msgs file dstmsg dstfld dstfile
	 beg end region total i)
     (if (mew-mark-active-p) (setq arg t))
     (cond
      (arg
       (setq region (mew-summary-get-region))
       (setq beg (car region))
       (setq end (cdr region)))
      (t
       (setq beg (point-min))
       (setq end (point-max))))
     (setq dstfld (mew-input-local-folder mew-inbox-folder))
     (when (and dstfld (mew-local-folder-check dstfld 'ask))
       (let ((mew-use-suffix nil))
	 (setq dstmsg (mew-folder-new-message dstfld 'numonly)))
       (setq msgs (mew-summary-mark-collect mew-mark-review beg end))
       (when (get-buffer dstfld)
	 (with-current-buffer dstfld
	   (setq exclusivep (mew-summary-exclusive-p))))
       (cond
	((null msgs)
	 (message "No mark"))
	((not exclusivep)
	 (message "Try again later"))
	(t
	 (message "Copying...")
	 (setq total (length msgs))
	 (setq i 1)
	 (dolist (msg msgs)
	   (setq file (mew-expand-msg fld msg))
	   (setq dstfile (mew-expand-new-msg dstfld dstmsg))
	   (message "Copying (%d/%d)..." i total)
	   (mew-summary-local-copy-one file dstfile)
	   (setq dstmsg (number-to-string (1+ (string-to-number dstmsg))))
	   (setq i (1+ i)))
	 (mew-touch-folder dstfld)
	 (message "Copying...done")))))))

(defun mew-summary-local-copy-one (srcfile dstfile)
  (with-temp-buffer
    (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
      (mew-insert-file-contents srcfile)
      (goto-char (point-min))
      (mew-header-delete-lines (list mew-x-mew-uidl:))
      (write-region (point-min) (point-max) dstfile nil 'no-msg)
      (mew-set-file-modes dstfile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Deleting and moving a folder
;;;

(defun mew-summary-delete-folder ()
  "Delete this folder."
  (interactive)
  (mew-summary-only
   (let* ((folder (or (mew-sinfo-get-folder)
		      (mew-summary-folder-name 'ext)))
	  (case (mew-sinfo-get-case))
	  (case:folder (mew-summary-folder-name 'ext))
	  (dir (mew-expand-folder case:folder))
	  (buf (current-buffer)))
     (cond
      ((mew-folder-localp folder)
       (if (mew-folder-node-p folder)
	   (message "%s cannot be deleted" folder)
	 (when (y-or-n-p "Delete this folder? ")
	   (mew-delete-directory-recursively dir)
	   (mew-local-folder-delete folder)
	   (run-hook-with-args 'mew-summary-delete-folder-hook folder)
	   (mew-summary-visit-folder mew-inbox-folder)
	   (mew-kill-buffer buf))))
      ((mew-folder-imapp folder)
       (if (mew-folder-node-p folder case)
	   (message "%s cannot be deleted" folder)
	 (when (y-or-n-p "Delete this folder? ")
	   (setq mew-summary-buffer-process-error nil)
	   (mew-imap-retrieve case 'delete case:folder)
	   (mew-timing)
	   (mew-rendezvous mew-summary-buffer-process)
	   (if mew-summary-buffer-process-error
	       (progn
		 (message "%s cannot be deleted" folder)
		 (setq mew-summary-buffer-process-error nil))
	     (mew-imap-folder-delete case folder)
	     (mew-delete-directory-recursively dir) ;; cache
	     (run-hook-with-args 'mew-summary-delete-folder-hook case:folder)
	     (mew-summary-visit-folder mew-imap-inbox-folder)
	     (mew-kill-buffer buf)))))))))

(defun mew-summary-rename-folder ()
  "Rename this folder."
  (interactive)
  (mew-summary-only
   (let* ((folder (or (mew-sinfo-get-folder)
		      (mew-summary-folder-name 'ext)))
	  (case (mew-sinfo-get-case))
	  (case:folder (mew-summary-folder-name 'ext))
	  (dir (mew-expand-folder case:folder))
	  (buf (current-buffer))
	  new-folder new-dir case:new-folder)
     (cond
      ((mew-folder-localp folder)
       (setq new-folder (mew-input-local-folder folder))
       (setq new-dir (mew-expand-folder new-folder))
       (cond
	((string= folder new-folder)
	 (message "%s was not moved" folder))
	((file-exists-p new-dir)
	 (message "%s was not moved since %s exists" folder new-folder))
	(t
	 (mew-rename-directory dir new-dir)
	 (mew-summary-switch-to-folder new-folder)
	 (mew-kill-buffer buf)
	 (if (mew-folder-node-p folder)
	     ;; xxx remove all related buffer?
	     (mew-local-update 'force)
	   (mew-local-folder-delete folder)
	   (mew-local-folder-insert new-folder))
	 (run-hook-with-args 'mew-summary-rename-folder-hook folder new-folder))))
      ((mew-folder-imapp folder)
       (setq new-folder (car (mew-input-refile-folders (list folder) t case mew-folder-imap)))
       (setq case:new-folder (mew-case-folder case new-folder))
       (setq new-dir (mew-expand-folder case:new-folder))
       (cond
	((string= folder new-folder)
	 (message "%s was not moved" folder))
	((file-exists-p new-dir)
	 (message "%s was not moved since %s exists" folder new-folder))
	(t
	 (mew-imap-retrieve case 'rename case:folder case:new-folder)
	 (mew-timing)
	 (mew-rendezvous mew-summary-buffer-process)
	 (if mew-summary-buffer-process-error
	     (progn
	       (message "%s cannot be renamed" folder)
	       (setq mew-summary-buffer-process-error nil))
	   (rename-file dir new-dir t) ;; cache
	   (mew-summary-switch-to-folder case:new-folder)
	   (mew-kill-buffer buf)
	   (if (mew-folder-node-p folder case)
	       (if (y-or-n-p "Collect folders now? ")
		   (mew-imap-update case)
		 (mew-message-for-summary "Type '\\[universal-argument]\\[mew-status-update]' to collect folders"))
	     (mew-imap-folder-delete case folder)
	     (mew-imap-folder-insert case new-folder))
	   (run-hook-with-args 'mew-summary-rename-folder-hook
			       case:folder case:new-folder)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary only vs Summary/Message
;;;

(defun mew-summary-toggle-disp-msg (&optional arg)
  "Toggle 'Summary mode only' and 'Summary & Message mode'. If
you choose 'Summary mode only', you can quickly put the delete
	marks since the next message is not displayed."
  (interactive)
  (cond
   ((eq arg 'on)
    (mew-sinfo-set-disp-msg t))
   ((eq arg 'off)
    (mew-sinfo-set-disp-msg nil)
    (mew-summary-reset-mode-line))
   (t
    (mew-sinfo-set-disp-msg (not (mew-sinfo-get-disp-msg)))
    (if (mew-sinfo-get-disp-msg)
	(mew-summary-display 'redisplay)
      (mew-summary-goto-message)
      (mew-summary-reset)
      (mew-summary-cook-window))))
  (run-hooks 'mew-summary-toggle-disp-msg-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 8bit clean
;;;

(defun mew-summary-toggle-8bit ()
  "Toggle 8bit mode(i.e. 'mew-use-8bit')."
  (interactive)
  (setq mew-use-8bit (not mew-use-8bit))
  (if mew-use-8bit
      (message "mew-use-8bit has been set to 't'")
    (message "mew-use-8bit has been set to 'nil'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mew cache clean up
;;;

(defun mew-summary-cache-clean-up ()
  "Clean-up caches of analyzed messages."
  (interactive)
  (mew-cache-clean-up)
  (message "Mew cache clean up...done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle warning
;;;

(defun mew-summary-toggle-warning ()
  "Toggle warning level.
If 'mew-warning-field-level' is 2, set it to 1.
If 'mew-warning-field-level' is 1, set it to 2."
  (interactive)
  (if (= mew-warning-field-level 2)
      (setq mew-warning-field-level 1)
    (setq mew-warning-field-level 2))
  (mew-cache-clean-up)
  (mew-summary-analyze-again)
  (message "Mew warning level %d" mew-warning-field-level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle decode policy
;;;

(defun mew-summary-toggle-policy (&optional arg)
  "Toggle decode policy(i.e. 'mew-decode-broken')."
  (interactive "P")
  (if arg
      (setq mew-decode-broken nil
	    mew-use-name-parameter nil)
    (setq mew-decode-broken (not mew-decode-broken))
    (setq mew-use-name-parameter (not mew-use-name-parameter)))
  (mew-cache-clean-up)
  (mew-summary-analyze-again)
  (message "Mew \"%s\" mode" (if mew-decode-broken "TOLERANT" "STRICT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle debug
;;;

(defun mew-summary-toggle-debug ()
  "Toggle 'mew-debug'."
  (interactive)
  (setq mew-debug (not mew-debug))
  (if mew-debug
      (message "'mew-debug' is now 't'")
    (message "'mew-debug' is now 'nil'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle PGP/MIME and old PGP
;;;

(defun mew-summary-toggle-pgp ()
  "Toggle PGP/MIME and old PGP (i.e. 'mew-use-old-pgp')."
  (interactive)
  (setq mew-use-old-pgp (not mew-use-old-pgp))
  (if mew-use-old-pgp
      (message "mew-use-old-pgp has been set to 't'")
    (message "mew-use-old-pgp has been set to 'nil'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Learning spam and ham
;;;

(defun mew-summary-learn-spam ()
  "Learn that this message is a spam.
See also 'mew-spam-prog' and 'mew-spam-prog-args."
  (interactive)
  (mew-summary-msg
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (file (mew-expand-msg fld msg))
	  (case (mew-sinfo-get-case))
	  (prog (mew-spam-prog case))
	  (args (mew-spam-prog-args case))
	  beg end)
     (message "Learning as spam...")
     (when (mew-which-exec prog)
       (with-temp-buffer
	 (apply 'call-process prog file (current-buffer) nil args)
	 (setq beg (point-min))
	 (goto-char beg)
	 (forward-line)
	 (if (= (point) beg)
	     (message "Learning as spam...done")
	   (setq end (1- (point)))
	   (message "%s" (concat "Learned as spam: " (mew-buffer-substring beg end)))))))))

(defun mew-summary-learn-ham ()
  "Learn that this message is a ham.
See also 'mew-ham-prog' and 'mew-ham-prog-args."
  (interactive)
  (mew-summary-msg
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (file (mew-expand-msg fld msg))
	  (case (mew-sinfo-get-case))
	  (prog (mew-ham-prog case))
	  (args (mew-ham-prog-args case))
	  beg end)
     (message "Learning as ham...")
     (when (mew-which-exec prog)
       (with-temp-buffer
	 (apply 'call-process prog file (current-buffer) nil args)
	 (setq beg (point-min))
	 (goto-char beg)
	 (forward-line)
	 (if (= (point) beg)
	     (message "Learning as ham...done")
	   (setq end (1- (point)))
	   (message "%s" (concat "Learned as ham: " (mew-buffer-substring beg end)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Search invisible
;;;

(defun mew-summary-toggle-invisible ()
  "Toggle the value of search-invisible.
Invisible information in Summary mode cannot be searched by default.
Executing this command enables searching such information."
  (interactive)
  (if (eq search-invisible t)
      (setq search-invisible 'open)
    (setq search-invisible t))
  (message "'search-invisible' is set to '%s'" search-invisible))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lines
;;;

(defun mew-summary-line ()
  "Toggle normal lines, wrapped lines and long lines."
  (interactive)
  (let ((fld (mew-summary-folder-name))
	(msg (mew-summary-message-number))
	(cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (mew-message-line)
	    (mew-message-mode-line fld msg))
	(select-window cwin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Horizontal Scrolling
;;;

(defun mew-summary-scroll-left ()
  "Scroll the Message window to the right."
  (interactive)
  (let ((cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed")
      (select-window mwin)
      (unwind-protect
	  (mew-message-scroll-left)
	(select-window cwin)))))

(defun mew-summary-scroll-right ()
  "Scroll the Message window to the left."
  (interactive)
  (let ((cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed")
      (select-window mwin)
      (unwind-protect
	  (mew-message-scroll-right)
	(select-window cwin)))))

(provide 'mew-summary4)

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

;;; mew-summary4.el ends here
