;;; mew-attach.el --- attachments for Mew Draft mode

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; goodies
;;

(defun mew-draft-prepare-attachments (&optional no-scroll)
  "Prepare an attachment region in the bottom of the draft.
To compose a multipart message, you should execute this command first."
  (interactive)
  (if (mew-attach-p)
      (message "Attachments region has already been prepared")
    (let* ((attachdir (mew-attachdir))
	   (cp (expand-file-name mew-draft-coverpage attachdir))
	   (dir (file-name-nondirectory attachdir)))
      (if (mew-attach-p)
	  (message "Attachments already exist")
	(mew-attach-set)
	(mew-check-directory attachdir)
	(unless (file-exists-p cp) (write-region "" nil cp 'append 'no-msg))
	(unless mew-encode-syntax
	  (setq mew-encode-syntax (mew-encode-syntax-initial dir)))
	(mew-encode-syntax-print mew-encode-syntax)
	(mew-attach-goto-number 'here '(2))
	(if (and (not no-scroll)
		 (not (pos-visible-in-window-p
		       (point-max)
		       (get-buffer-window (current-buffer)))))
	    (set-window-start (get-buffer-window (current-buffer))
			      (mew-attach-begin)))
	(setq buffer-undo-list nil)))))

(defun mew-attach-expand-path (syntax nums)
  ;; Ignore the first dir
  (let ((path ""))
    (while (cdr nums)
      (setq syntax (aref syntax (+ mew-syntax-magic (1- (car nums)))))
      (setq path (concat path (mew-syntax-get-file syntax)))
      (setq nums (cdr nums)))
    path))

(defun mew-attach-line ()
  "Return the line number for the cursor from the beginning of the
attachments region.
0 if on the beginning boundary.
-1 if on the ending boundary."
  (let (ret max)
    (save-excursion
      (beginning-of-line)
      (setq ret (count-lines (1+ (mew-attach-begin)) (point)))
      (goto-char (point-max))
      (beginning-of-line)
      (setq max (count-lines (1+ (mew-attach-begin)) (point)))
      (if (or (= ret max) (= ret (1- max))) -1 ret))))

(defun mew-attach-line-lastp ()
  (let ((here (point)))
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line)
      (= (count-lines here (point)) 2))))

(defun mew-attach-move-onto-file ()
  (end-of-line)
  (re-search-backward " [^ ]" nil t)
  (forward-char))

(defun mew-attach-move-onto-body ()
  (goto-char (1+ (mew-attach-begin)))
  (forward-char -1))

(defun mew-attach-directory-p ()
  (save-excursion
    (mew-attach-move-onto-file)
    (looking-at (file-name-as-directory "[^ ]+"))))

(defun mew-attach-dot-p ()
  (save-excursion
    (mew-attach-move-onto-file)
    (looking-at "\\. *$")))

;; cursor only
(defun mew-attach-line0-1 ()
  (when (mew-attach-p)
    (let ((line (mew-attach-line)))
      (or (= line 0) (= line -1)))))

;; cursor only
(defun mew-attach-line01-1 ()
  (when (mew-attach-p)
    (let ((line (mew-attach-line)))
      (or (= line 0) (= line 1) (= line -1)))))

;; insert commands
(defun mew-attach-not-line012-1 ()
  (when (mew-attach-p)
    (let ((line (mew-attach-line)))
      (not (or (= line 0) (= line 1) (= line 2) (= line -1))))))

;; find commands
(defun mew-attach-not-line012-1-dot ()
  (when (mew-attach-p)
    (let ((line (mew-attach-line)))
      (not (or (mew-attach-dot-p)
	       (= line 0) (= line 1) (= line 2) (= line -1))))))

;; delete commands
(defun mew-attach-not-line02-1-dot ()
  (when (mew-attach-p)
    (let ((line (mew-attach-line)))
      (not (or (mew-attach-dot-p) (= line 0) (= line 2) (= line -1))))))


;; modify commands
(defun mew-attach-not-line0-1-dot ()
  (when (mew-attach-p)
    (let ((line (mew-attach-line)))
      (not (or (mew-attach-dot-p) (= line 0) (= line -1))))))

;; cursor
(defun mew-attach-line1-dot ()
  (when (mew-attach-p)
    (let ((line (mew-attach-line)))
      (or (mew-attach-dot-p) (= line 1)))))

(defun mew-attach-goto-number (direction nums)
  (let (numreg)
    (setq nums (nreverse nums))
    (cond
     ((eq direction 'next)
      (setq nums (cons (1+ (car nums)) (cdr nums))))
     ((eq direction 'prev)
      (setq nums (cons (1- (car nums)) (cdr nums))))
     ((eq direction 'up)
      (setq nums (cdr nums)))
     ((eq direction 'down)
      (setq nums (cons 1 nums)))
     (t ()))
    (if (null nums)
	(progn
	  (goto-char (1- (mew-attach-begin)))
	  (re-search-forward "^..... " nil t)
	  (mew-attach-move-onto-file))
      (setq numreg (number-to-string (car nums)))
      (setq nums (cdr nums))
      (dolist (num nums)
	(setq numreg (concat (number-to-string num) "." numreg)))
      (setq numreg (regexp-quote numreg))
      (goto-char (1+ (mew-attach-begin)))
      (re-search-forward (concat "^....." numreg " ") nil t)
      (mew-attach-move-onto-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dialog
;;;

(defun mew-substitute-for-attach (msg)
  (substitute-command-keys (concat "\\<mew-draft-attach-map>" msg)))

(defun mew-message-for-attach (msg)
  (message "%s" (mew-substitute-for-attach msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; commands
;;

;;
;; cursor commands
;;

(defun mew-attach-newline ()
  "Insert RET before the attachments region."
  (interactive)
  (when (= (point) (1+ (mew-attach-begin)))
    (insert "\n")
    (mew-draft-attach-keymap)))

(defun mew-attach-forward ()
  "Go to the first subdirectory in attachments."
  (interactive)
  (if (mew-attach-line0-1)
      (mew-attach-goto-number 'here nil)
    (mew-attach-move-onto-file)
    (if (mew-attach-directory-p)
	(mew-attach-goto-number 'down (mew-syntax-nums))
      (mew-attach-goto-number 'here (mew-syntax-nums)))))

(defun mew-attach-backforward ()
  "Go to the parent directory in attachments."
  (interactive)
  (if (mew-attach-line0-1)
      (mew-attach-move-onto-body)
    (let ((nums (mew-syntax-nums)))
      (if (= (length nums) 1)
	  (mew-attach-goto-number 'here nil)
	(mew-attach-goto-number 'up (mew-syntax-nums))))))

(defun mew-attach-previous ()
  "Go to the previous file in the current directory in attachments."
  (interactive)
  (if (mew-attach-line01-1)
      (mew-attach-move-onto-body)
    (if mew-attach-move-by-line
	(progn
	  (forward-line -1)
	  (mew-attach-move-onto-file))
      (let* ((nums (mew-syntax-nums))
	     (last (nth (1- (length nums)) nums)))
	(if (= last 1)
	    (mew-attach-goto-number 'here (mew-syntax-nums))
	  (mew-attach-goto-number 'prev (mew-syntax-nums)))))))

(defun mew-attach-next ()
  "Go to the next file in the current directory in attachments."
  (interactive)
  (if (mew-attach-line0-1)
      (mew-attach-goto-number 'here nil)
    (if mew-attach-move-by-line
	(if (mew-attach-line-lastp)
	    ()
	  (forward-line)
	  (mew-attach-move-onto-file))
      (if (mew-attach-line1-dot)
	  (mew-attach-goto-number 'here (mew-syntax-nums))
	(mew-attach-goto-number 'next (mew-syntax-nums))))))

;;
;; delete commands
;;

(defun mew-attach-delete ()
  "Delete this file or this directory in attachments."
  (interactive)
  (if (not (mew-attach-not-line02-1-dot))
      (message "Cannot delete here")
    (if (= (mew-attach-line) 1)
	(if (not (y-or-n-p "Delete entire attachments? "))
	    (message "Attachments were not deleted")
	  (mew-delete-directory-recursively (mew-attachdir))
	  (setq mew-encode-syntax nil)
	  (mew-attach-clear)
	  (message "Attachments were deleted"))
      (let* ((nums (mew-syntax-nums))
	     (subdir (mew-attach-expand-path mew-encode-syntax nums))
	     (attachdir (mew-attachdir))
	     (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	     (name (mew-syntax-get-file syntax))
	     (ename (if (string= subdir "") name (concat subdir name)))
	     (fullname (expand-file-name ename attachdir))
	     (dirp (string-match (concat mew-path-separator "$") name))
	     (msg (if dirp
		      "Delete %s with contained files? "
		    "Delete %s? ")))
	;; attachdir / {subdir/} name
	(when (y-or-n-p (format msg ename))
	  (message "Deleting %s..." ename)
          (if (file-exists-p fullname)
              (if dirp
                  (mew-delete-directory-recursively fullname)
                (mew-delete-file fullname)))
	  (message "Deleting %s...done" ename)
	  (setq mew-encode-syntax
		(mew-syntax-remove-entry mew-encode-syntax nums))
	  (mew-encode-syntax-print mew-encode-syntax))))))

;;
;; insert commands
;;

(defun mew-attach-multipart ()
  "Create a subdirectory(i.e. multipart) on '.' in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Cannot create a sub-multipart here")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (insert-default-directory nil)
	   (attachdir (mew-attachdir))
	   dir dirpath)
      ;; attachdir / {subdir/} dir
      (unless (string= subdir "")
	(setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / dir
      (setq dirpath (mew-random-filename attachdir 1 nil))
      (if (file-exists-p dirpath)
	  (message "Cannot make a directory, sorry")
	(setq dir (file-name-nondirectory dirpath))
	(mew-make-directory dirpath)
	(setq mew-encode-syntax
	      (mew-syntax-insert-entry
	       mew-encode-syntax
	       nums
	       (mew-encode-syntax-multi dir mew-type-mlm)))
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-duplicate (func action setmode &optional from to default)
  (if (not (mew-attach-not-line012-1))
      (message "Cannot %s here" action)
    (let* ((completion-ignored-extensions nil)
	   (nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   (frompath from)
	   (efile (and to (concat subdir to)))
	   (tofile to)
	   ct msg FILE)
      (while (or (null frompath)
		 (file-directory-p (file-chase-links frompath))
		 (not (file-exists-p (file-chase-links frompath))))
	(setq frompath (mew-draft-input-file-name
			(concat (mew-capitalize action) " from: ") default)))
      (while (or (null efile)
		 (file-exists-p (expand-file-name efile attachdir)))
	(setq tofile (mew-input-string
		      (concat (mew-capitalize action) " to %s(%s): ")
		      subdir
		      (file-name-nondirectory frompath)))
	(setq efile (concat subdir tofile)))
      (setq FILE (expand-file-name efile attachdir))
      (funcall func frompath FILE)
      (if setmode (mew-set-file-modes FILE))
      (setq ct (mew-ctdb-ct (mew-ctdb-by-file tofile)))
      (unless ct
	(setq ct (mew-content-type (mew-tinfo-get-case)))
	(cond
	 ((string= ct mew-ct-txt)
	  (setq msg (mew-substitute-for-attach
		     "Attached as text. Type '\\[mew-attach-toggle]' to toggle text to binary")))
	 ((string= ct mew-ct-apo)
	  (setq msg (mew-substitute-for-attach
		     "Attached as binary. Type '\\[mew-attach-toggle]' to toggle binary to text")))))
      (setq mew-encode-syntax
	    (mew-syntax-insert-entry
	     mew-encode-syntax
	     nums
	     (mew-encode-syntax-single tofile (list ct) nil nil nil 'cdp)))
      (mew-encode-syntax-print mew-encode-syntax)
      (if mew-attach-move-next-after-copy (mew-attach-next))
      (if msg (message "%s" msg)))))

(defun mew-attach-link (&optional from to)
  "Link a file with a symbolic link on '.' in attachments.
FROM must be absolute path. TO must be a file name."
  (interactive)
  (mew-attach-duplicate 'mew-symbolic-link "link" nil from to))

(defun mew-attach-copy (&optional from to)
  "Copy a file (via networks) on '.' in attachments.
FROM must be absolute path. TO must be a file name.
To copy a remote file, use the \"/[user@]hostname:/filepath\" syntax."
  (interactive)
  (mew-attach-duplicate 'copy-file "copy" t from to))

(defun mew-attach-toggle ()
  "Toggle text/binary in attachments."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot change %s here" mew-ct:)
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (file (mew-syntax-get-file syntax))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap))
	   cte)
      (if (not (or (string= ct mew-ct-txt) (string= ct mew-ct-apo)))
	  (message "Cannot toggle %s" ct)
	(if (string= ct mew-ct-txt)
	    (setq ct mew-ct-apo)
	  (setq ct mew-ct-txt))
	(setq ctl (list ct))
	(mew-syntax-set-ct syntax ctl)
	(setq cte (mew-ctdb-cte (mew-ctdb-by-ct ct)))
	(mew-syntax-set-cte syntax cte)
	(mew-syntax-set-cdp syntax (mew-syntax-cdp-format ct file))
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-link-message ()
  "Link the message displayed in Message mode with a symbolic link
on '.' in attachments."
  (interactive)
  (let* ((fid (mew-frame-id))
	 (fld (mew-current-get-fld fid))
	 (msg (mew-current-get-msg fid))
	 default)
    (if fld (setq default (mew-expand-msg fld msg)))
    (if mew-use-symbolic-link-for-forwarding
	(mew-attach-duplicate 'mew-symbolic-link "link" nil nil nil default)
      (mew-attach-duplicate 'copy-file "copy" nil nil nil default))))

(defun mew-attach-find-new-file ()
  "Open a new file into a buffer on '.' in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Cannot find a new file here")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   file filepath)
      ;; attachdir / {subdir/} dir
      (unless (string= subdir "")
	(setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir 1 nil mew-text-suffix))
      (if (null filepath)
	  (message "Cannot make a text file, sorry")
	(setq file (file-name-nondirectory filepath))
	(setq mew-encode-syntax
	      (mew-syntax-insert-entry
	       mew-encode-syntax
	       nums
	       (mew-encode-syntax-single file (list mew-ct-txt) ;; for charset
					 nil nil nil nil)))
	(mew-encode-syntax-print mew-encode-syntax)
	;;
	(switch-to-buffer (mew-find-file-noselect2 filepath))
	(text-mode)
	;; buffer switched
	(local-set-key "\C-c\C-q" 'mew-kill-buffer)))))

(defun mew-attach-audio ()
  "Sampling voice and insert as audio file on '.' in attachments"
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Cannot attach audio data here")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   file filepath recorded)
      ;; attachdir / {subdir/} dir
      (unless (string= subdir "")
	(setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir 1 nil mew-audio-suffix))
      (if (null filepath)
	  (message "Cannot make a file for audio, sorry")
	(setq file (file-name-nondirectory filepath))
	(with-temp-buffer
	  (condition-case nil
	      (progn
		(if (not (y-or-n-p "Are you ready to record audio? "))
		    (message "Nothing is recoded")
		  (mew-message-for-attach
		   "Type '\\[keyboard-quit]' to finish recording...")
		  (mew-plet
		   (apply 'call-process
			  (nth 0 mew-prog-audio2)
			  nil t nil
			  (nth 1 mew-prog-audio2)))))
	    (quit
	     (mew-message-for-attach
	      "Type '\\[keyboard-quit]' to finish recording...done")
	     (setq recorded t)
	     (mew-flet
	      (write-region (point-min) (point-max) filepath nil 'no-msg)))))
	(if recorded
	    (setq mew-encode-syntax ;; buffer local
		  (mew-syntax-insert-entry
		   mew-encode-syntax
		   nums
		   (mew-encode-syntax-single file mew-type-ado
					     nil nil nil 'cdp))))
	(mew-encode-syntax-print mew-encode-syntax)))))

;;
;; modify commands
;;

(defun mew-attach-undo ()
  "Unmark. The original mark appears."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot undo encoding here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums)))
      (mew-syntax-set-privacy syntax nil)
      (mew-syntax-set-decrypters syntax nil)
      ;;
      (mew-syntax-set-cte syntax
			  (mew-ctdb-cte
			   (mew-ctdb-by-ct
			    (car (mew-syntax-get-ct syntax)))))
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-type ()
  "Change the data type(Content-Type:) in attachments."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot change %s here" mew-ct:)
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (file (mew-syntax-get-file syntax))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap))
	   cte)
      (setq ct (mew-input-type "Type for %s (%s): " file ct
			       (if (mew-syntax-multipart-p syntax)
				   mew-mime-content-type-multipart-list
				 mew-mime-content-type-list)))
      (setq ctl (list ct))
      (mew-syntax-set-ct syntax ctl)
      (setq cte (mew-ctdb-cte (mew-ctdb-by-ct ct)))
      (mew-syntax-set-cte syntax cte)
      (mew-syntax-set-cdp syntax (mew-syntax-cdp-format ct file))
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-charset ()
  "Specify the charset parameter for a Text/* object in attachments."
  (interactive)
  (mew-attach-charset-base "charset"))

(defun mew-attach-icharset ()
  "Specify a input coding-system for a text file in attachments."
  (interactive)
  (mew-attach-charset-base "icharset"))

(defun mew-attach-charset-base (param)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot specify character set here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (file (mew-syntax-get-file syntax))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap))
	   (params (mew-syntax-get-params ctl))
	   (char (mew-syntax-get-param ctl param))
	   charset)
      (if (not (mew-ct-textp ct))
	  (message "Cannot use character set %s" ct)
	(unless char (setq char "guess"))
	(setq charset (mew-input-type
		       (if (string= param "charset")
			   "Output message charset for %s (%s): "
			 "Input file charset for %s (%s): ")
		       file char
		       (cons "guess" mew-charset-list)))
	(setq params (mew-delete param params))
	(if (string= charset "guess")
	    (setq ctl (cons ct params))
	  (setq ctl (cons ct (cons (list param charset) params))))
	(mew-syntax-set-ct syntax ctl)
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-description ()
  "Input a description(Content-Description:) in attachments."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot describe here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ocd (or (mew-syntax-get-cd syntax) (mew-syntax-get-file syntax)))
	   (cd (read-string "Description: " ocd)))
      (if (string= cd "")
	  (mew-syntax-set-cd syntax nil)
	(mew-syntax-set-cd syntax cd))
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-disposition ()
  "Input Content-Disposition: in attachments."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot set disposition here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap))
	   (oldfile (or (mew-syntax-get-param (mew-syntax-get-cdp syntax)
					      "filename")
			(mew-syntax-get-file syntax)))
	   (file (read-string "Filename: " oldfile)))
      (if (string= file "")
	  (mew-syntax-set-cdp syntax nil)
	(mew-syntax-set-cdp syntax (mew-syntax-cdp-format ct file nil 'force-file)))
      (mew-encode-syntax-print mew-encode-syntax))))

(defun mew-attach-base64 ()
  "Put the 'B' mark to encode with Base64 in attachments."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot encode with base64 here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap)))
      (if (or (mew-ct-messagep ct) (mew-ct-multipartp ct))
	  (message "Cannot encode with base64 here")
	(mew-syntax-set-cte syntax mew-b64)
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-quoted-printable ()
  "Put the 'Q' mark to encode with Quoted-Printable in attachments"
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot encode with quoted-printable here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap)))
      (if (or (mew-ct-messagep ct) (mew-ct-multipartp ct))
	  (message "Cannot encode with quoted-printable here")
	(mew-syntax-set-cte syntax mew-qp)
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-gzip64 ()
  "Put the 'G' mark to encode with Gzip64 in attachments. This is
applicable only to Text/Plain and Application/Postscript since compression
is not effective other objects. For example, JPEG is already compressed."
  (interactive)
  (if (not (mew-attach-not-line0-1-dot))
      (message "Cannot apply gzip here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap)))
      (if (or (mew-ct-messagep ct)
	      (mew-ct-multipartp ct)
	      (not (mew-ct-linebasep ct)))
	  ;; never use compress to types other than text/plain or
	  ;; application/postscript. Indeed, compression is not
	  ;; effective to compressed data such as jpeg.
	  (message "Cannot apply gzip to %s" ct)
	(mew-syntax-set-cte syntax mew-xg)
	(mew-encode-syntax-print mew-encode-syntax)))))

(defun mew-attach-zip ()
  "Put the 'Z' mark and encrypt it with \"zip\" in attachments."
  (interactive)
  (if (not (mew-attach-not-line02-1-dot))
      (message "Cannot encrypt with zip here")
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (ctl (mew-syntax-get-ct syntax))
	   (ct (mew-syntax-get-value ctl 'cap)))
      (if (or (mew-ct-messagep ct) (mew-ct-multipartp ct))
	  (message "Cannot encrypt with zip here")
	(if (not (mew-which-exec mew-prog-zip))
	    (message "\"zip\" not found")
	  (let ((password (mew-read-passwd "Zip password: "))
		(password1 (mew-read-passwd "Zip password again: ")))
	    (if (not (string= password password1))
		(message "Password mismatch!")
	      (let* ((subdir (mew-attach-expand-path mew-encode-syntax nums))
		     (attachdir (mew-attachdir))
		     (name (mew-syntax-get-file syntax))
		     (ename (if (string= subdir "") name (concat subdir name)))
		     (fullname (expand-file-name ename attachdir))
		     (zname (concat name ".zip"))
		     (zfullname (concat fullname ".zip"))
		     (zct "application/zip")
		     (default-directory (file-name-directory fullname)))
		;; must not specify the "-e" option due to
		;; variety of zip versions.
		(call-process mew-prog-zip nil nil nil "-P" password zname name)
		(if (not (file-exists-p zfullname))
                    (if (string= password "")
                        (message "\"zip\" does not allow zero length password")
                      (message "\"zip\" does not support encryption"))
		  (mew-syntax-set-ct syntax (list zct))
		  (mew-syntax-set-cte syntax mew-b64)
		  (mew-syntax-set-file syntax zname)
		  (mew-syntax-set-cdp syntax (mew-syntax-cdp-format zct zname))
		  (mew-encode-syntax-print mew-encode-syntax))))))))))

(defun mew-attach-pgp-sign ()
  "Put the 'PS' mark to sign with PGP in attachments."
  (interactive)
  (cond
   ((not (mew-attach-not-line0-1-dot))
    (message "Cannot PGP sign here"))
   ((null mew-pgp-ver)
    (message "%s does not exist" mew-prog-pgp))
   (t
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (privacy (mew-syntax-get-privacy syntax))
	   (pgp-sign (mew-pcdb-ct (mew-pcdb-by-service 'pgp-signature))))
      (cond
       ((>= (length privacy) 2)
	(message "Too many marks"))
       ((member (car pgp-sign) privacy) ;; xxx hard coding
	(message "Already marked"))
       (t
	(mew-syntax-set-privacy
	 syntax (append privacy pgp-sign))
	(mew-encode-syntax-print mew-encode-syntax)))))))

(defun mew-attach-pgp-enc ()
  "Put the 'PE' mark to encrypt with PGP in attachment.
Input decrypters' addresses."
  (interactive)
  (cond
   ((not (mew-attach-not-line0-1-dot))
    (message "Cannot PGP encrypt here"))
   ((null mew-pgp-ver)
    (message "%s does not exist" mew-prog-pgp))
   (t
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (privacy (mew-syntax-get-privacy syntax))
	   (towhom (mew-header-parse-address-list mew-destination:-list))
	   (pgp-enc (mew-pcdb-ct (mew-pcdb-by-service 'pgp-encryption))))
      (cond
       ((>= (length privacy) 2)
	(message "Too many marks"))
       ((member (car pgp-enc) privacy) ;; xxx hard coding
	(message "Already marked"))
       (t
	;; ask towhom before set privacy for C-g
	(if towhom
	    (setq towhom (mew-input-address "To (%s): " (mew-join "," towhom)))
	  (setq towhom (mew-input-address "To: ")))
	;; towhom is already canonicalized.
	(mew-syntax-set-privacy
	 syntax (append privacy pgp-enc))
	(mew-syntax-set-decrypters syntax towhom)
	(mew-encode-syntax-print mew-encode-syntax)))))))

(defun mew-attach-smime-sign ()
  "Put the 'PS' mark to sign with S/MIME in attachments."
  (interactive)
  (cond
   ((not (mew-attach-not-line0-1-dot))
    (message "Cannot S/MIME sign here"))
   ((null mew-smime-ver)
    (message "%s does not exist" mew-prog-smime))
   (t
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (privacy (mew-syntax-get-privacy syntax))
	   (smime-sign (mew-pcdb-ct (mew-pcdb-by-service 'smime-signature))))
      (cond
       ((>= (length privacy) 2)
	(message "Too many marks"))
       ((member (car smime-sign) privacy) ;; xxx hard coding
	(message "Already marked"))
       (t
	(mew-syntax-set-privacy
	 syntax (append privacy smime-sign))
	(mew-encode-syntax-print mew-encode-syntax)))))))

(defun mew-attach-smime-enc ()
  "Put the 'SE' mark to encrypt with S/MIME in attachment.
Input decrypters' addresses."
  (interactive)
  (cond
   ((not (mew-attach-not-line0-1-dot))
    (message "Cannot S/MIME encrypt here"))
   ((null mew-smime-ver)
    (message "%s does not exist" mew-prog-smime))
   (t
    (let* ((nums (mew-syntax-nums))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	   (privacy (mew-syntax-get-privacy syntax))
	   (towhom (mew-header-parse-address-list mew-destination:-list))
	   (smime-enc (mew-pcdb-ct (mew-pcdb-by-service 'smime-encryption))))
      (cond
       ((>= (length privacy) 2)
	(message "Too many marks"))
       ((member (car smime-enc) privacy) ;; xxx hard coding
	(message "Already marked"))
       (t
	;; ask towhom before set privacy for C-g
	(if towhom
	    (setq towhom (mew-input-address "To (%s): " (mew-join "," towhom)))
	  (setq towhom (mew-input-address "To: ")))
	;; towhom is already canonicalized.
	(mew-syntax-set-privacy
	 syntax (append privacy smime-enc))
	(mew-syntax-set-decrypters syntax towhom)
	(mew-encode-syntax-print mew-encode-syntax)))))))
;;
;; find commands
;;

(defun mew-attach-find-file ()
  "Open this file into a buffer in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1-dot))
      (message "Cannot find a file here")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   (syntax (mew-syntax-get-entry mew-encode-syntax nums))	
	   (file (mew-syntax-get-file syntax))
	   efile FILE)
      (setq efile (if (string= subdir "") file (concat subdir file)))
      (setq FILE (expand-file-name efile attachdir))
      (switch-to-buffer (mew-find-file-noselect2 FILE))
      ;; buffer switched
      (local-set-key "\C-c\C-q" 'mew-kill-buffer)
      (if (file-symlink-p FILE)
	  (message "This file is a symbolic link. Take care")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Drag & Drop
;;

(defun mew-draft-dnd-handle-local-file (uri action)
  (let* ((from (dnd-get-local-file-name uri t))
	 (to (file-name-nondirectory from)))
    (when from
      (unless (mew-attach-p)
	(mew-draft-prepare-attachments))
      (goto-char (point-max))
      (forward-line -2)
      (mew-attach-next)
      (cond
       ((file-directory-p from)
	(message "Directory cannot be attached"))
       ((or (eq action 'copy)
	    (eq action 'private)
	    (eq action 'move))
	(mew-attach-copy from to))
       ((eq action 'link)
	(mew-attach-link from to))
       (t
	(message "Nothing happened"))))))

(defun mew-draft-dnd-handle-file (uri action)
  (let ((local-file (dnd-get-local-file-uri uri)))
    (if local-file
	(mew-draft-dnd-handle-local-file local-file action)
      nil)))

(provide 'mew-attach)

;;; Copyright Notice:

;; Copyright (C) 1996-2011 Mew developing team.
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

;;; mew-attach.el ends here
