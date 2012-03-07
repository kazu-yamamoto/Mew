;;; mew-exec.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  2, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exec-func
;;;

(defun mew-mark-kill-refile (src msg)
  (let ((refinfo (mew-refile-get msg))
	myself)
    (setq myself (or (null refinfo) (member src refinfo)))
    (if myself (mew-summary-refile-remove-body))
    (not myself)))

(defun mew-mark-exec-refile (src msgs)
  "Refile MSGs from the SRC folder."
  (let (dsts tmp msg-dsts dsts-msgses)
    (dolist (msg msgs)
      (setq msg-dsts (mew-refile-get msg))
      (setq dsts (cdr msg-dsts))
      (if dsts ;; sanity check
	  (if (setq tmp (assoc dsts dsts-msgses))
	      (nconc tmp (list msg))	
	    (setq dsts-msgses (cons (list dsts msg) dsts-msgses))))
      (mew-refile-reset msg))
    ;; refile at once
    (dolist (dsts-msgs dsts-msgses)
      (mew-summary-mv-msgs src dsts-msgs))))

(defun mew-mark-exec-unlink (src dels)
  "Unlink DELS from the SRC folder.
DELS represents the messages to be deleted."
  (when dels
    (mew-summary-unlink-msgs src dels)))

(defun mew-mark-exec-delete (src dels)
  "Delete messages from the SRC folder according to 'mew-trash-folder'
and 'mew-trash-folder-list'.
DELS represents the messages to be deleted."
  (when dels
    ;; see also mew-summary-exec-remote-get-execinfo.
    (cond
     ((null mew-trash-folder)
      (mew-summary-unlink-msgs src dels))
     (mew-trash-folder-list
      (if (or (member src mew-trash-folder-list)
	      (string= src mew-trash-folder))
	  (mew-summary-unlink-msgs src dels)
	(mew-summary-totrash-msgs src dels)))
     (t
      (if (string= src mew-trash-folder)
	  (mew-summary-unlink-msgs src dels)
	(mew-summary-totrash-msgs src dels))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sanity-func
;;;

(defun mew-mark-sanity-refile (msgs)
  "Check the destination folder for MSGS."
  (let (uniq-dsts udst err-dsts dir)
    (dolist (msg msgs)
      (dolist (dst (cdr (mew-refile-get msg)))
	(mew-addq uniq-dsts dst)))
    (mew-addq uniq-dsts mew-trash-folder)
    (dolist (uniq-dst uniq-dsts)
      (setq udst (mew-case-folder mew-inherit-exec-case uniq-dst))
      (setq dir (mew-expand-folder udst))
      (if (file-exists-p dir)
	  (if (file-directory-p dir)
	      (unless (file-writable-p dir)
		(set-file-modes dir mew-folder-mode))
	    (setq err-dsts (cons udst err-dsts))) ;; NG
	(mew-make-directory dir)))
    err-dsts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unlinking, moving, to-trash
;;;

(defun mew-folder-suffix (folder)
  (if (or (mew-folder-queuep folder) (mew-folder-postqp folder))
      mew-queue-info-suffix
    (if (mew-folder-draftp folder)
	mew-draft-info-suffix)))

(defun mew-summary-unlink-msgs (src dels)
  (let ((suffix (mew-folder-suffix src))
	file infofile)
    (dolist (del dels)
      (setq file (mew-expand-msg src del))
      ;; if file does not exist, the marked line will be deleted anyway.
      (mew-delete-file file)
      (when suffix
	(setq infofile (concat file suffix))
	(mew-delete-file infofile)))))

(defun mew-i2s (num cache)
  (if cache
      (concat "0" (number-to-string num)) ;; "0" is the mark for invalid messages
    (number-to-string num)))

(defun mew-summary-mv-msgs (src dsts-msgs)
  (let* ((dsts (car dsts-msgs)) ;; (+foo +bar)
	 (msgs (cdr dsts-msgs)) ;; (1 2 3)
	 (srcx (mew-case:folder-folder src))
	 (myselfp (member srcx dsts))
	 (cache mew-inherit-offline)
	 (suffix (mew-folder-suffix srcx))
	 msgs- srcfile dstfile dst num strnum infofile)
    (if myselfp
	;; msg stays in the src folder with the same number
	(progn
	  (setq dsts (delete srcx dsts))
	  (dolist (msg msgs)
	    (if (file-regular-p (mew-expand-msg src msg))
		(setq msgs- (cons msg msgs-))))
	  (setq msgs- (nreverse msgs-))
	  (setq msgs msgs-))
      (setq dst (mew-case-folder mew-inherit-exec-case (car dsts)))
      (setq dsts (cdr dsts))
      (setq num (string-to-number (mew-folder-new-message dst 'num-only cache)))
      (dolist (msg msgs)
	(setq srcfile (mew-expand-msg src msg))
	(when (and (file-exists-p srcfile) (file-writable-p srcfile))
	  (setq strnum (mew-i2s num cache))
	  (setq msgs- (cons strnum msgs-))
	  (setq dstfile (mew-expand-new-msg dst strnum))
	  (setq num (1+ num))
	  (rename-file srcfile dstfile 'override)
	  (mew-set-file-times dstfile)
	  (when suffix
	    (setq infofile (concat srcfile suffix))
	    (mew-delete-file infofile))))
      (mew-touch-folder dst)
      (setq msgs- (nreverse msgs-))
      (setq src dst)) ;; myselfp
    (dolist (ent dsts)
      (setq dst (mew-case-folder mew-inherit-exec-case ent))
      (setq num (string-to-number (mew-folder-new-message dst 'num-only cache)))
      (dolist (msg msgs-)
	(setq srcfile (mew-expand-msg src msg))
	(setq strnum (mew-i2s num cache))
	(setq dstfile (mew-expand-new-msg dst strnum))
	(setq num (1+ num))
	(mew-link srcfile dstfile))
      (mew-touch-folder dst))))

(defun mew-summary-totrash-msgs (src dels)
  (let* ((trash (mew-case-folder mew-inherit-exec-case mew-trash-folder))
	 (trashdir (mew-expand-folder trash))
	 (cache mew-inherit-offline)
	 (suffix (mew-folder-suffix src))
	 num srcfile dstfile infofile)
    (unless (file-directory-p trashdir) (make-directory trashdir))
    (setq num (string-to-number (mew-folder-new-message trash 'num-only cache)))
    ;; must be here after ensuring that +trash exists.
    (dolist (del dels)
      (setq srcfile (mew-expand-msg src del)) ;; xxx
      (setq dstfile (mew-expand-new-msg trash (mew-i2s num cache)))
      (setq num (1+ num))
      (when (file-regular-p srcfile)
	;; if not, the marked line will be deleted anyway.
	(rename-file srcfile dstfile 'override)
	(mew-set-file-times dstfile))
      (when suffix
	(setq infofile (concat srcfile suffix))
	(mew-delete-file infofile)))
    (mew-touch-folder trash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands for mark processing
;;;

(defun mew-summary-exec (&optional arg)
  "\\<mew-summary-mode-map> Process marked messages. When called with
'\\[universal-argument]', process marked messages in the specified
region. To cancel the '*' mark, use '\\[mew-summary-undo]' or
'\\[mew-summary-undo-all]'."
  (interactive "P")
  (let (beg end region)
    (if (mew-mark-active-p) (setq arg t))
    (cond
     (arg
      (setq region (mew-summary-get-region))
      (setq beg (car region))
      (setq end (cdr region)))
     (t
      (setq beg (point-min))
      (setq end (point-max))))
    (mew-summary-exec-region beg end)))

(defun mew-summary-exec-one (&optional arg)
  "Process the current marked message.
When called with '\\[universal-argument]', process
all marked messages before the current message."
  (interactive "P")
  (mew-summary-goto-message)
  (mew-decode-syntax-delete)
  (save-window-excursion
    (let (beg end)
      (save-excursion
	(beginning-of-line)
	(setq beg (if arg (point-min) (point)))
	(end-of-line)
	(setq end (1+ (point))))
      (if (> end (point-max))
	  (message "No message")
	(mew-summary-exec-region beg end)))))

(defun mew-summary-exec-delete ()
  "Process messages marked with 'D'."
  (interactive)
  (let* ((ent (assoc mew-mark-delete mew-mark-spec))
	 (mew-mark-spec (list ent)))
    (mew-summary-exec-region (point-min) (point-max))))

(defun mew-summary-exec-unlink ()
  "Process messages marked with 'X'."
  (interactive)
  (let* ((ent (assoc mew-mark-unlink mew-mark-spec))
	 (mew-mark-spec (list ent)))
    (mew-summary-exec-region (point-min) (point-max))))

(defun mew-summary-exec-refile (&optional arg)
  "Process messages marked with 'o'.
If called with '\\[universal-argument]', only messages whose
destination is the same as that of the current message are processed."
  (interactive "P")
  (let* ((ent (assoc mew-mark-refile mew-mark-spec))
	 (mew-mark-spec (list ent)))
    (if (not arg)
	(mew-summary-exec-region (point-min) (point-max))
      (let* ((msg (mew-summary-message-number))
	     (dst (cdr (mew-refile-get msg)))
	     func)
	(if (not dst)
	    (message "No message to be refiled here")
	  (setq func (lambda (num) (equal dst (cdr (mew-refile-get num)))))
	  (mew-summary-exec-region (point-min) (point-max) func))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exec region
;;;

(defun mew-summary-move-and-display (msg &optional redisplay)
  (when (mew-summary-search-msg msg)
    (mew-summary-display redisplay)))

(defun mew-summary-go-back-summary (key)
  (let ((msg (mew-summary-message-number))
	(fld (mew-summary-physical-folder)))
    (if (not (and fld (get-buffer fld)))
	(message "No physical folder")
      (mew-summary-visit-folder fld nil 'no-ls)
      (if msg
	  (mew-summary-move-and-display msg)
	(goto-char (point-max)))
      (message "Now in %s. Type '%s' again" fld key))))

(defun mew-summary-exec-region (beg end &optional func)
  (let ((folder (mew-sinfo-get-folder)))
    (cond
     ((mew-folder-nntpp folder)
      (message "This command cannot be used in this folder"))
     ((mew-folder-remotep folder)
      (message "Refiling and deleting...")
      (force-mode-line-update)
      (mew-summary-exec-remote beg end func))
     ((mew-virtual-p)
      (mew-summary-go-back-summary
       (mew-substitute-for-summary "\\[mew-summary-exec]")))
     (t
      (message "Refiling and deleting...")
      (force-mode-line-update)
      (mew-summary-exec-local beg end func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refile log
;;;

(defun mew-exec-refile-log (buf)
  (when (get-buffer buf)
    (with-current-buffer buf
      (mew-frwlet mew-cs-dummy mew-cs-m17n
	(write-region (point-min) (point-max)
		      (expand-file-name mew-refile-log-file mew-conf-path)
		      'append 'no-msg)))
    (mew-remove-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Processing marks for local
;;;

(defun mew-mark-kill-line ()
  (let (start)
    (beginning-of-line)
    (setq start (point))
    (forward-line)
    (mew-elet (delete-region start (point)))))

(defun mew-summary-exec-local (beg end &optional func-decide)
  (when (mew-summary-exclusive-p)
    (save-excursion
      (condition-case nil
	  (let* ((marks (mew-mark-get-all-marks))
		 (src (mew-summary-folder-name 'ext))
		 (lbuf (mew-refile-log-buffer src))
		 (m (copy-marker end))
		 (cnt 0)
		 (case-fold-search nil)
		 msg msgs err-folders func-exec func-sanity killp regex)
	    (dolist (mark marks)
	      (setq func-exec (mew-markdb-func-exec mark))
	      (when func-exec
		(setq killp (mew-markdb-killp mark))
		(setq regex (mew-mark-regex mark))
		(setq msgs nil)
		(goto-char beg)
		(while (re-search-forward regex m t)
		  (setq msg (mew-summary-message-number))
		  (when (or (not func-decide) (funcall func-decide msg))
		    (setq msgs (cons msg msgs))
		    (cond
		     ((fboundp killp)
		      (if (funcall killp src msg)
			  (mew-mark-kill-line)
			(mew-mark-remove)))
		     ((eq killp t)
		      (mew-mark-kill-line)))))
		(setq msgs (nreverse msgs))
		(when msgs
		  (when (= cnt 0)
		    ;; opening...
		    (mew-summary-lock t "Executing")
		    (mew-summary-reset))
		  (setq cnt (1+ cnt))
		  ;; refiling and deleting...
		  (setq func-sanity (mew-markdb-func-sanity mark))
		  (when (and func-sanity
			     (setq err-folders (funcall func-sanity msgs)))
		    (mew-summary-unlock)
		    (mew-warn
		     "Nothing proceeded. Folder(s) MUST be a file!: %s"
		     (mew-join "," err-folders)))
		  (funcall func-exec src msgs))))
	    (if (= cnt 0)
		(message "No marks")
	      ;; ending...
	      (mew-cache-clean-up)
	      (mew-summary-folder-cache-save)
	      (set-buffer-modified-p nil)
	      (mew-summary-unlock)
	      (when (and (eobp) (not (pos-visible-in-window-p)))
		(forward-line -1)
		;; This ensures that some messages are displayed in Summary.
		(recenter))
	      (run-hooks 'mew-summary-exec-hook)
	      (mew-exec-refile-log lbuf)
	      (message "Refiling and deleting...done")))
	(quit
	 (set-buffer-modified-p nil)
	 (mew-summary-unlock))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Processing marks for remote
;;;

(defun mew-mark-invisible-line ()
  (let (start)
    (beginning-of-line)
    (setq start (point))
    (forward-line)
    (mew-elet
     (put-text-property start (point) 'invisible t)
     ;; For Emacs only. To make invisible text act as we want
     (put-text-property start (point) 'front-sticky t))))

(mew-defstruct execinfo remove kill refile move rttl dttl)
(mew-defstruct refileinfo uid size delete folders)

(defun mew-summary-exec-remote-get-execinfo (beg end modify &optional func-decide)
  ;; for POP or IMAP
  ;; If modify, marked lines get invisible. They are removed
  ;; IMAP's sentinel.
  ;; Otherwise, marked lines retain and they are to be processed
  ;; by mew-summary-exec-local().
  (let* ((case (mew-sinfo-get-case))
	 (folder (mew-sinfo-get-folder))
	 (m (copy-marker end))
	 (case-fold-search nil)
	 (rttl 0) (dttl 0)
	 trash trash-list lomov
	 siz msg uid ref flds del regex trss rmvs kils refs movs
	 refinfo refile-back)
    (when (mew-folder-imapp folder)
      ;; see also mew-mark-exec-delete.
      (setq trash (mew-imap-trash-folder case))
      (setq trash-list (mew-imap-trash-folder-list case))
      (setq lomov t)
      (cond
       ((null trash)
	)
       (trash-list
	(if (or (member folder trash-list)
		(string= folder trash))
	    (setq trash nil)))
       (t
	(if (string= folder trash)
	    (setq trash nil)))))
    (save-excursion
      (mew-decode-syntax-delete)
      ;;
      ;; refile
      ;;
      (when (mew-markdb-by-mark mew-mark-refile)
	(setq regex (mew-mark-regex mew-mark-refile))
	(goto-char beg)
	(while (re-search-forward regex m t)
	  (re-search-forward mew-regex-sumsyn-long)
	  (setq msg (mew-sumsyn-message-number))
	  (setq uid (mew-sumsyn-message-uid))
	  (setq siz (mew-sumsyn-message-size))
	  (when (or (not func-decide) (funcall func-decide msg))	
	    (setq refinfo (mew-refile-get msg))
	    (setq refile-back (cons refinfo refile-back))
	    (setq flds (cdr refinfo))
	    ;;
	    (setq del nil)
	    (if (member folder flds)
		(setq flds (delete folder flds)) ;; destructive
	      (setq del t)
	      (when modify
		(if lomov
		    (setq movs (cons msg movs))
		  (setq kils (cons msg kils)))))
	    (when flds
	      (setq ref (mew-make-refileinfo :uid uid
					     :size siz
					     :delete del
					     :folders flds))
	      (setq refs (cons ref refs)))
	    (if modify
		(if del
		    (mew-mark-invisible-line)
		  (mew-summary-refile-remove-body)
		  (mew-mark-remove)))))
	(setq refs (nreverse refs))
	(setq rttl (length refs))
	(setq movs (nreverse movs))
	(if modify (mew-sinfo-set-refile-back refile-back))
	(when lomov
	  (let ((mew-inherit-exec-case case))
	    (mew-mark-sanity-refile movs))))
      ;;
      ;; delete
      ;;
      (when (mew-markdb-by-mark mew-mark-delete)
	(setq regex (mew-mark-regex mew-mark-delete))
	(goto-char beg)
	(while (re-search-forward regex m t)
	  (re-search-forward mew-regex-sumsyn-long)
	  (setq msg (mew-sumsyn-message-number))
	  (setq uid (mew-sumsyn-message-uid))
	  (setq siz (mew-sumsyn-message-size))
	  (if trash
	      (setq trss (cons (mew-make-refileinfo :uid uid
						    :size siz
						    :delete t
						    :folders (list trash))
			       trss))
	    (setq rmvs (cons uid rmvs)))
	  (when modify
	    (setq kils (cons msg kils))
	    (mew-mark-invisible-line))
	  (setq dttl (1+ dttl))))
      ;;
      ;; unlink
      ;;
      (when (mew-markdb-by-mark mew-mark-unlink)
	(setq regex (mew-mark-regex mew-mark-unlink))
	(goto-char beg)
	(while (re-search-forward regex m t)
	  (re-search-forward mew-regex-sumsyn-long)
	  (setq msg (mew-sumsyn-message-number))
	  (setq uid (mew-sumsyn-message-uid))
	  (setq siz (mew-sumsyn-message-size))
	  (setq rmvs (cons uid rmvs))
	  (when modify
	    (setq kils (cons msg kils))
	    (mew-mark-invisible-line))
	  (setq dttl (1+ dttl))))
      ;;
      (setq rmvs (nreverse rmvs))
      (setq trss (nreverse trss))
      (if trss (setq refs (nconc trss refs)))
      ;;
      (set-buffer-modified-p nil)
      (cond
       ((and (null rmvs) (null refs)) nil)
       (modify
	(force-mode-line-update) ;; xxx
	(mew-make-execinfo :remove rmvs
			   :kill   kils
			   :refile refs
			   :move   movs
			   :rttl   rttl
			   :dttl   dttl))
       (t
	(mew-make-execinfo :remove rmvs
			   :refile refs
			   :rttl   0
			   :dttl   0))))))

(defun mew-summary-exec-remote (beg end &optional func-decide)
  (if (mew-folder-nntpp (mew-sinfo-get-folder))
      (message "This command cannot be used in this folder")
    (when (mew-summary-exclusive-p)
      (let* ((case (mew-sinfo-get-case))
	     (folder (mew-sinfo-get-folder))
	     (bnm (mew-summary-folder-name 'ext))
	     (lbuf (mew-refile-log-buffer bnm))
	     (execinfo (mew-summary-exec-remote-get-execinfo beg end 'modify func-decide)))
	(if (not execinfo)
	    (message "No messages to be processed")
	  (mew-summary-reset)
	  (while (eq (get-text-property (point) 'invisible) t)
	    (forward-line))
	  (cond
	   ((mew-folder-popp folder)
	    (mew-pop-retrieve case 'exec bnm execinfo))
	   ((mew-folder-imapp folder)
	    (mew-imap-retrieve case 'exec bnm execinfo)))
	  (mew-exec-refile-log lbuf))))))

(defun mew-mark-kill-invisible ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (get-text-property (point) 'invisible)
	  (mew-mark-kill-line)
	(forward-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Processing marks for offline
;;;

(defun mew-summary-exec-offline (&optional arg)
  "\\<mew-summary-mode-map>Process marked messages in an IMAP folder *offline*. Messages to be refiled in
a remote folder are moved to the corresponding folder but they are
marked invalid. Invalid message are marked with '#'.
Invalid messages will be overridden when the remote folder is scanned online.
A job to delete or refile messages, which is created by this command,
is queued in a queue folder (%queue for IMAP). To flush jobs in
the queue, type '\\[mew-summary-send-message]' in the queue online."
  (interactive "P")
  (cond
   ((mew-virtual-p)
    (mew-summary-go-back-summary
     (mew-substitute-for-summary "\\[mew-summary-exec-offline]")))
   ((not (mew-folder-imapp (mew-sinfo-get-folder)))
    (message "This command cannot be used in this folder"))
   (t
    (if (mew-mark-active-p) (setq arg t))
    (let (beg end region)
      (cond
       (arg
	(setq region (mew-summary-get-region))
	(setq beg (car region))
	(setq end (cdr region)))
       (t
	(setq beg (point-min))
	(setq end (point-max))))
      (message "Refiling and deleting...")
      (force-mode-line-update)
      (mew-summary-exec-offline-region beg end)))))

(mew-defstruct jobinfo mailbox execinfo mark-hist)

(defun mew-summary-exec-offline-region (beg end)
  "Process the current marked message offline."
  (interactive)
  (let* ((execinfo (mew-summary-exec-remote-get-execinfo beg end nil))
	 (rmvs (mew-execinfo-get-remove execinfo))
	 (refs (mew-execinfo-get-refile execinfo))
	 job)
    (if (not execinfo)
	(message "No messages to be processed")
      (let* ((fld (mew-sinfo-get-folder))
	     (case (mew-sinfo-get-case))
	     (imapq (mew-case-folder case (mew-imap-queue-folder case)))
	     (imapq-dir (mew-expand-folder imapq))
	     file file-info)
	(unless (file-directory-p imapq-dir) (mew-make-directory imapq-dir))
	(setq file (mew-folder-new-message imapq))
	(setq file-info (concat file mew-imapq-info-suffix))
	(setq job (mew-make-jobinfo :mailbox fld
				    :execinfo execinfo
				    :mark-hist (mew-sinfo-get-mark-hist)))
	(mew-lisp-save file-info job 'nobackup 'unlimit)
	(with-temp-buffer
	  (mew-header-insert mew-subj: (format "IMAP jobs for %s" fld))
	  (mew-header-insert mew-date: (mew-time-ctz-to-rfc (current-time)))
	  (mew-header-insert mew-from: "Mew IMAP manager")
	  (insert "\n")
	  (insert "Messages to be refiled:\n")
	  (dolist (ref refs)
	    (insert (format (if (mew-refileinfo-get-delete ref)
				"\t%s will be copied to %s\n"
			      "\t%s will be moved to %s\n")
			    (mew-refileinfo-get-uid ref)
			    (mew-join "," (mew-refileinfo-get-folders ref)))))
	  (insert "\n")
	  (when rmvs
	    (insert "Messages to be deleted:\n")
	    (insert "\t" (mew-join "," rmvs) "\n"))
	  (mew-frwlet mew-cs-dummy nil
	    (write-region (point-min) (point-max) file nil 'no-msg)))
	(mew-touch-folder imapq)
	;; refile log is not necessary since mew-summary-exec-local does it.
	(let ((mew-inherit-exec-case case)
	      (mew-inherit-offline t)
	      (mew-trash-folder (mew-imap-trash-folder case)))
	  (mew-summary-exec-local beg end))))))

(defun mew-summary-exec-offline-one (&optional arg)
  "Process the message on the cursor offline."
  (interactive "P")
  (if (not (mew-folder-imapp (mew-sinfo-get-folder)))
      (message "This command cannot be used in this folder")
    (mew-summary-goto-message)
    (mew-decode-syntax-delete)
    (save-window-excursion
      (let (beg end)
        (save-excursion
          (beginning-of-line)
          (setq beg (if arg (point-min) (point)))
          (end-of-line)
          (setq end (1+ (point))))
        (if (> end (point-max))
            (message "No message")
	  (force-mode-line-update) ;; xxx
          (mew-summary-exec-offline-region beg end))))))

(provide 'mew-exec)

;;; Copyright Notice:

;; Copyright (C) 1997-2012 Mew developing team.
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

;;; mew-exec.el ends here
