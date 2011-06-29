;;; mew-net.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb 27, 2002

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Port DB
;;;

(defvar mew-port-db
  '(("smtp"        25)
    ;; This is officially assigned to another service (urd) by IANA.
    ;; IESG will never assign a port number to SMTP over SSL...
    ("smtps"      465)
    ("pop3"       110)
    ("pop3s"      995)
    ("nntp"       119)
    ("nntps"      563)
    ("imap"       143)
    ("imaps"      993)
    ("submission" 587)))

(defun mew-serv-to-port (serv)
  (cond
   ((integerp serv) serv)
   ((string-match "^[0-9]+$" serv)
    (string-to-number serv))
   (t
    (or (mew-alist-get-value (assoc serv mew-port-db)) 0))))

(defun mew-*-to-port (port)
  (cond
   ((integerp port) port)
   ((string-match "^[0-9]+$" port)
    (string-to-number port))
   (t port)))

(defun mew-*-to-string (any)
  (cond
   ((integerp any)
    (number-to-string any))
   (t any)))

(defun mew-port-equal (port1 port2)
  (string= (mew-*-to-string port1) (mew-*-to-string port2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Net folders
;;;

(defvar mew-pop-folder  "+#pop/%s@%s#%s")
(defvar mew-imap-folder "+#imap/%s@%s#%s")
(defvar mew-nntp-folder "+#nntp/%s@%s#%s")

(defun mew-pop-folder (&optional case)
  (format mew-pop-folder
	  (mew-pop-user case) (mew-pop-server case) (mew-pop-port case)))

(defun mew-imap-folder (&optional case)
  (format mew-imap-folder
	  (mew-imap-user case) (mew-imap-server case) (mew-imap-port case)))

(defun mew-nntp-folder (&optional case)
  (format mew-nntp-folder
	  (or (mew-nntp-user case) "") ;; a user may be nil
	  (mew-nntp-server case) (mew-nntp-port case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Net status
;;;

(defun mew-summary-lock (key status &optional secure)
  (setq mew-summary-buffer-process key)
  (setq mew-summary-buffer-process-status (format " %s" status))
  (setq mew-summary-buffer-secure-process secure))


(defun mew-summary-unlock ()
  (setq mew-summary-buffer-process nil)
  (setq mew-summary-buffer-process-status nil)
  (setq mew-summary-buffer-secure-process nil)
  (force-mode-line-update))

(defun mew-net-status-clear (buf)
  (when (and mew-use-net-status buf (get-buffer buf))
    (with-current-buffer buf
      (setq mew-summary-buffer-process-status nil)
      (setq mew-summary-buffer-secure-process nil))))

(defun mew-net-status (buf status &optional substatus secure)
  (when mew-use-net-status
    (with-current-buffer buf
      (if substatus
	  (setq mew-summary-buffer-process-status
		(format " %s:%s" status substatus))
	(setq mew-summary-buffer-process-status (format " %s" status)))
      (setq mew-summary-buffer-secure-process secure))))

(defun mew-net-status1 (buf msg ttl cnt &optional secure)
  (when mew-use-net-status
    (when (<= cnt ttl)
      (let ((substatus (format "%02d%%" (/ (* cnt 100) ttl))))
	(mew-net-status buf msg substatus secure)))))

(defvar mew-net-status-percent-size 51200)

(defun mew-net-status2 (buf rttl rcnt siz &optional zero secure)
  (when mew-use-net-status
    (when (<= rcnt rttl)
      (let ((msiz (string-to-number siz))
	    (bsiz (if zero 0 (buffer-size)))
	    percent substatus)
	(if (and (integerp mew-net-status-percent-size)
		 (< msiz mew-net-status-percent-size))
	    (progn
	      (setq substatus (format "%d/%d" rcnt rttl))
	      (mew-net-status buf "Retrieving" substatus secure))
	  (if (= msiz 0) (setq msiz 1))
	  (if (< 10000 msiz)
	      (setq percent (/ bsiz (/ msiz 100)))
	    (setq percent (/ (* bsiz 100) msiz)))
	  (setq substatus (format "%d/%d:%02d%%" rcnt rttl percent))
	  (mew-net-status buf "Retrieving" substatus secure))))))

(defvar mew-local-status-unit 10)
(defvar mew-local-status-threshold 100)

(defun mew-net-status3 (buf rttl rcnt)
  (when mew-use-net-status
    (if rttl
	(if (or (not (integerp mew-local-status-unit))
		(not (integerp mew-local-status-threshold))
		(< (- rttl rcnt) mew-local-status-threshold)
		(= 0 (% rcnt mew-local-status-unit)))
	    (mew-net-status buf "Scanning" (format "%d/%d" rcnt rttl)))
      (mew-net-status buf "Scanning" (format "%d" rcnt)))))

(defun mew-summary-visible-buffer (buf)
  (with-current-buffer buf
    (mew-elet (put-text-property (point-min) (point-max) 'invisible nil))
    (setq mew-summary-buffer-raw t)
    (if (eq (get-buffer-window buf) (selected-window))
	(mew-summary-cook-window))
    (set-buffer-modified-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Biff
;;;

(defvar mew-biff-string nil)
(defvar mew-biff-timer-id nil)

(defun mew-biff-bark (n)
  (if (= n 0)
      (setq mew-biff-string nil)
    (if (and mew-use-biff-bell (eq mew-biff-string nil))
	(beep))
    (setq mew-biff-string (format "Mail(%d)" n))))

(defun mew-biff-clear ()
  (setq mew-biff-string nil))

(defun mew-biff-setup ()
  (let ((inbox (mew-proto-inbox-folder nil mew-case))
	func)
    (if (not mew-use-biff)
	(mew-biff-clean-up)
      (if mew-biff-timer-id (cancel-timer mew-biff-timer-id))
      (cond
       ((mew-folder-localp inbox)
	(setq func 'mew-pop-biff))
       ((mew-folder-popp inbox)
	(setq func 'mew-pop-biff))
       ((mew-folder-imapp inbox)
	(setq func 'mew-imap-biff)))
      (if func
	  (setq mew-biff-timer-id (mew-timer (* 60 mew-biff-interval) func)))))
  (let ((ent '(mew-biff-string mew-biff-string)))
    (unless (member ent global-mode-string)
      (if global-mode-string
	  (setq global-mode-string
		(append global-mode-string (list "" ent)))
	(setq global-mode-string (list ent))))))

(defun mew-biff-clean-up ()
  (if mew-biff-timer-id (cancel-timer mew-biff-timer-id))
  (setq mew-biff-timer-id nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keep time
;;;

(defun mew-time-diff (t1 t2)
  (/ (+ (* (- (nth 0 t2) (nth 0 t1)) 65536)
	(- (nth 1 t2) (nth 1 t1)))
     86400.0)) ;; one day (* 60 60 24)

(defun mew-expired-p (time keep)
  (cond
   ((and (consp keep)
	 (stringp (nth 0 keep)) (file-exists-p (nth 0 keep))
	 (integerp (nth 1 keep)))
    (if (>= (mew-time-diff time (mew-file-get-time (nth 0 keep))) (nth 1 keep))
	t))
   ((integerp keep)
    (if (>= (mew-time-diff time (current-time)) keep) t))
   ;; ((eq keep t) t)
   ;; This case MUST not be included because messages marked with 'T'
   ;; will be deleted.
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UIDL
;;;

(defvar mew-net-uidl-file ".mew-uidl")
(defvar mew-net-uidl-db nil)

(defun mew-net-uidl-db-get (tag)
  (cdr (assoc tag mew-net-uidl-db)))

(defun mew-net-uidl-db-set (tag uidl)
  (let* ((ent (assoc tag mew-net-uidl-db)))
    (if ent
	(setcdr ent uidl)
      (setq mew-net-uidl-db (cons (cons tag uidl) mew-net-uidl-db)))
    (mew-lisp-save mew-net-uidl-file mew-net-uidl-db nil 'unlimit)))

(defun mew-net-setup ()
  (setq mew-net-uidl-db (mew-lisp-load mew-net-uidl-file))
  (add-hook 'kill-emacs-hook 'mew-net-clean-up))

(defun mew-net-clean-up ()
  (remove-hook 'kill-emacs-hook 'mew-net-clean-up)
  (mew-lisp-save mew-net-uidl-file mew-net-uidl-db nil 'unlimit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; New message
;;;

(defun mew-net-get-new-message (pnm bnm msg get set)
  (if msg
      (cons msg (mew-expand-msg bnm msg)) ;; 'get
    (let ((msg (funcall get pnm))
	  file nxt)
      (cond
       (msg
	(setq nxt (number-to-string (1+ (string-to-number msg))))
	(funcall set pnm nxt)
	(setq file (mew-expand-new-msg bnm msg)))
       (t
	(setq file (mew-folder-new-message bnm))
	;; 1.eml => 1
	(setq msg (file-name-sans-extension (file-name-nondirectory file)))
	(setq nxt (number-to-string (1+ (string-to-number msg))))
	(funcall set pnm nxt)))
      (cons msg file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Queue
;;;

(defun mew-queue-check-new-message (file)
  (if (not (file-exists-p (concat file mew-queue-work-suffix)))
      file
    (let* ((dir (file-name-directory file))
	   ;; 1.eml => 1
	   (num (file-name-sans-extension (file-name-nondirectory file)))
	   (n (1+ (string-to-number num))))
      (while (or (file-exists-p (format "%s%d" dir n))
		 (file-exists-p (format "%s%d%s" dir n mew-queue-work-suffix)))
	(setq n (1+ n)))
      (format "%s%d" dir n))))

(defun mew-queue-enqueue (work qfld) ;; 1.wrk or draft/1
  (let (orig info iwrk)
    (if (not (string-match (concat mew-queue-work-suffix "$") work))
	;; draft/1
	(setq orig (mew-queue-check-new-message (mew-folder-new-message qfld)))
      ;; 1.wrk
      (setq orig (file-name-sans-extension work)) ;; 1
      (setq iwrk (concat orig mew-queue-info-suffix mew-queue-work-suffix))
      (mew-delete-file iwrk) ;; 1.mqi.wrk
      (if (file-exists-p orig)
	  (setq orig (mew-queue-check-new-message (mew-folder-new-message qfld)))))
    (rename-file work orig) ;; An error is signaled if orig exists.
    (unless mew-use-nfs-hack
      ;; This cause an error when using NFS.
      (mew-set-file-modes orig))
    (setq info (concat orig mew-queue-info-suffix))
    (list orig info)))

(defun mew-queue-enqueue2 (work) ;; 1.wrk
  (let* ((dir (file-name-directory work))
	 (orig (file-name-sans-extension work)) ;; 1
	 (info (concat orig mew-imapq-info-suffix)) ;; 1.iqi
	 (iwrk (concat info mew-queue-work-suffix))) ;; 1.iqi.wrk
    (when (file-exists-p orig)
      (setq orig (mew-queue-check-new-message (mew-folder-new-message dir)))
      (setq info (concat orig mew-imapq-info-suffix)))
    (rename-file work orig) ;; An error is signaled if orig exists.
    (rename-file iwrk info 'override)))

(defun mew-queue-insert-file (pnm n qfld msg) ;; 1
  (let* ((file (mew-expand-new-msg qfld msg))
	 (work (concat file mew-queue-work-suffix)) ;; 1.wrk
	 (info (concat file mew-queue-info-suffix)) ;; 1.mqi
	 (iwrk (concat info mew-queue-work-suffix)) ;; 1.mqi.wrk
	 buf data)
    (when (and (file-readable-p file) (file-readable-p info))
      (rename-file file work 'override)
      ;; If an old buffer exists by accident, we MUST remove the buffer.
      (if (setq buf (get-file-buffer work)) (mew-remove-buffer buf))
      (mew-frwlet mew-cs-text-for-read mew-cs-dummy
	(set-buffer (mew-find-file-noselect work)))
      (setq data (mew-lisp-load info))
      (dotimes (i n)
	(aset (mew-info pnm) i (aref data i)))
      (rename-file info iwrk 'override)
      t)))

(defun mew-queue-get-next (pnm qfld msgs n func)
  (let (flushp)
    (catch 'loop
      (dolist (msg msgs)
	(if (mew-queue-insert-file pnm n qfld msg)
	    (throw 'loop (setq flushp t)))))
    (if (fboundp func) (funcall func pnm msgs)) ;; set-messages
    flushp))

(defun mew-queue-backup (work info-suffix) ;; 1.wrk
  (let* ((orig (file-name-sans-extension work)) ;; 1
	 (back (mew-prepend-prefix orig mew-backup-prefix)) ;; #1
	 (info (concat orig info-suffix)) ;; 1.sfx
	 (iwrk (concat info mew-queue-work-suffix)) ;; 1.sfx.wrk
	 (ibck (mew-prepend-prefix info mew-backup-prefix))) ;; #1.sfx
    (if (file-exists-p work) (rename-file work back 'override))
    (if (file-exists-p iwrk) (rename-file iwrk ibck 'override))
    back))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fcc:
;;;

(defun mew-net-fcc-message (case fcc file)
  (let (flds fcc-file link fld imapp)
    (dolist (fc fcc)
      (if (mew-folder-imapp fc)
	  (progn
	    (setq flds (cons (mew-folder-imap-to-fcc case fc) flds))
	    (setq imapp t))
	(setq flds (cons fc flds))))
    (catch 'loop
      (while flds ;; cannot use dolist
	(setq fld (car flds))
	(mew-local-folder-check fld)
	(setq flds (cdr flds))
	(setq fcc-file (mew-folder-new-message fld))
	(when fcc-file
	  (copy-file file fcc-file)
	  (mew-set-file-modes fcc-file)
	  (mew-touch-folder fld)
	  (throw 'loop nil))))
    (dolist (fld flds)
      (mew-local-folder-check fld)
      (setq link (mew-folder-new-message fld))
      (when link
	(mew-link fcc-file link)
	(mew-touch-folder fld)))
    imapp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syncing
;;;

(defun mew-net-folder-sync (bnm hlds)
  (let (uid msg ulks new-hlds)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq uid (mew-summary-message-uid))
	(if (setq new-hlds (member uid hlds))
	    (progn
	      (setq hlds (cdr new-hlds))
	      (forward-line))
	  (setq msg (mew-summary-message-number))
	  (setq ulks (cons msg ulks))
	  (mew-mark-kill-line))))
    (when ulks
      (setq ulks (nreverse ulks))
      (let ((mew-trash-folder nil))
	(mew-mark-exec-delete bnm ulks))
      (mew-summary-folder-cache-save))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleaning
;;;

;; See also mew-summary-folder-cache-clean.
(defun mew-net-folder-clean ()
  (let* ((bnm (mew-summary-folder-name 'ext))
	 (dir (mew-expand-folder bnm))
	 (msgs (mew-dir-messages dir mew-regex-message-files3)))
    (mew-summary-unlink-msgs bnm msgs)
    (mew-erase-buffer)
    (setq mew-summary-buffer-raw nil)
    (mew-summary-folder-cache-save)))

(defun mew-net-invalid-cache-start ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward mew-regex-sumsyn-invalid nil t)
      (beginning-of-line)
      (point))))

(defun mew-net-invalid-cache-invisible ()
  (let ((beg (mew-net-invalid-cache-start)))
    (when beg
      (mew-elet
       (put-text-property beg (point-max) 'invisible t)))))

(defun mew-net-invalid-cache-clean ()
  (let* ((bnm (mew-summary-folder-name 'ext))
	 (dir (mew-expand-folder bnm))
	 (msgs (mew-dir-messages dir mew-regex-message-files4)))
    (mew-summary-unlink-msgs bnm msgs)
    (mew-mark-kill-invisible)
    (mew-summary-folder-cache-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Retrieving messages
;;;

(defun mew-summary-retrieve-gap (folder)
  (when (mew-summary-folder-dir-newp)
    (goto-char (point-max))
    (let ((range (mew-range-update folder)))
      (mew-local-retrieve 'scan folder range)
      (mew-rendezvous mew-summary-buffer-process))))

(defun mew-summary-case-proto ()
  (let (case:folder case proto)
    (cond
     ((mew-virtual-for-one-summary)
      (setq case:folder (mew-summary-physical-folder))
      (setq case (or (mew-case:folder-case case:folder)
		     mew-case))
      (setq proto (mew-folder-prefix (mew-case:folder-folder case:folder))))
     ((mew-virtual-p)
      (setq case (or mew-case mew-case-default))
      (setq proto (mew-proto case)))
     (t ;; Summary
      (setq case (or (mew-sinfo-get-case)   ;; remote
		     mew-case))	            ;; local
      (setq proto (mew-folder-prefix (mew-sinfo-get-folder)))))
    (list case proto)))

(defun mew-summary-retrieve (&optional no-flush)
  "In local folders, retrieve messages to +inbox asynchronously
according to 'mew-mailbox-type' and 'mew-case'.
If 'mew-auto-flush-queue' is non-nil, +queue is flushed.
If called with '\\[universal-argument]', +queue is not flushed.

In remote folders, visit an inbox folder and scan with 'update."
  (interactive "P")
  (let (case proto inbox case:inbox mailbox)
    (mew-set '(case proto) (mew-summary-case-proto))
    (setq inbox (mew-proto-inbox-folder proto case))
    (cond
     ((mew-folder-remotep proto)
      (setq case:inbox (mew-case-folder case inbox)))
     (t ;; local
      (setq case:inbox inbox)))
    ;; for mew-summary-exchange-point.
    (mew-sinfo-set-ret-pos (point))
    (mew-summary-visit-folder case:inbox)
    ;; in the inbox
    (when (mew-summary-exclusive-p)
      (cond
       ((mew-folder-remotep inbox)
	(mew-summary-ls nil 'goend 'update))
       (t ;; local
	;;
	(mew-summary-reset)
	(mew-summary-retrieve-gap inbox)
	;;
	(goto-char (point-max))
	(mew-sinfo-set-start-point (point))
	(mew-sinfo-set-direction 'down)
	(setq mailbox (mew-mailbox-type case))
	(cond
	 ((eq mailbox 'pop)
	  (mew-pop-retrieve case 'inc inbox (not no-flush)))
	 ((eq mailbox 'imap)
	  (mew-imap-retrieve case 'inc inbox (not no-flush)))
	 ((eq mailbox 'mbox)
	  (mew-mbox-retrieve case 'inc inbox (not no-flush)))))))))

(defun mew-summary-scan-boot (proto case)
  ;; called by mew()
  (let* ((inbox (mew-proto-inbox-folder proto case))
	 (case:inbox (mew-case-folder case inbox))
	 (dir (mew-expand-folder case:inbox))
	 mailbox)
    (unless (file-directory-p dir)
      (mew-make-directory dir))
    (mew-summary-switch-to-folder case:inbox)
    ;; in the inbox
    ;; for mew-summary-exchange-point.
    (mew-sinfo-set-ret-pos (point))
    (when (mew-summary-exclusive-p)
      (mew-summary-reset)
      ;;
      (if (mew-folder-localp inbox) (mew-summary-retrieve-gap case:inbox))
      ;;
      (goto-char (point-max))
      (mew-sinfo-set-start-point (point))
      (mew-sinfo-set-direction 'down)
      (cond
       ((mew-folder-localp inbox)
	(setq mailbox (mew-mailbox-type case))
	(cond
	 ((eq mailbox 'pop)
	  (mew-pop-retrieve case 'inc inbox))
	 ((eq mailbox 'imap)
	  (mew-imap-retrieve case 'inc inbox))
	 ((eq mailbox 'mbox)
	  (mew-mbox-retrieve case 'inc inbox))))
       ((mew-folder-popp inbox)
	(let ((get-body (if (mew-pop-header-only case) nil t)))
	  (mew-pop-retrieve case 'scan case:inbox nil get-body)))
       ((mew-folder-imapp inbox)
	(let ((get-body (if (mew-imap-header-only case) nil t)))
	  (mew-imap-retrieve case 'scan case:inbox nil get-body)))
       ((mew-folder-nntpp inbox)
	(let ((get-body (if (mew-nntp-header-only case) nil t)))
	  (mew-nntp-retrieve case 'scan case:inbox nil get-body)))))))

(defun mew-mbox-retrieve (case directive inbox &optional flush)
  ;; directive is 'inc
  (let* ((mbox-command (mew-mbox-command case))
	 (mbox-command-arg (mew-mbox-command-arg case))
	 dir mode opts)
    (if (not (mew-which-exec mbox-command))
	(message "'%s' not found!" mbox-command)
      (if mbox-command-arg
	  (if (not (string= mbox-command "incm"))
	      (setq opts (list "-e" mbox-command "-m" mbox-command-arg))
	    (setq dir (mew-expand-folder inbox))
	    (setq mode (mew-get-file-modes dir))
	    (if mew-use-suffix
		(setq opts (list "-e" mbox-command
				 "-m" (format "%s -o -p %d" mbox-command-arg mode)))
	      (setq opts (list "-e" mbox-command
			       "-m" (format "%s -p %d" mbox-command-arg mode)))))
	(setq opts (list "-e" mbox-command)))
      (mew-local-retrieve 'inc inbox flush opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Retrieving truncated messages
;;;

(defun mew-summary-get-inherit-case (fld msg)
  (let ((buf (mew-cache-hit fld msg))
	uid)
    (if buf
	(with-current-buffer buf
	  (setq uid (mew-header-get-value mew-x-mew-uidl:)))
      (with-temp-buffer
	(mew-insert-message fld msg mew-cs-text-for-read mew-header-reasonable-size)
	(setq uid (mew-header-get-value mew-x-mew-uidl:))))
    (mew-scan-uid-case uid)))

(mew-defstruct net-virtual-info virtual summary msgs threadp)

(defun mew-net-virtual-info (msgs)
  (mew-make-net-virtual-info :virtual (mew-summary-folder-name 'ext)
			     :summary (mew-summary-folder-name)
			     :msgs    msgs
			     :threadp (mew-thread-p)))

(mew-defstruct net-disp-info list fid buf fld msg)

(defun mew-net-disp-info ()
  (let* ((fid (mew-frame-id))
	 (buf (current-buffer))
	 (fld (mew-current-get-fld fid))
	 (msg (mew-current-get-msg fid))
	 (disp (mew-sinfo-get-disp-msg)))
    (if disp (mew-make-net-disp-info :fid fid :buf buf :fld fld :msg msg))))

(defun mew-net-disp-info-display (disp-info)
  (when disp-info
    (let ((fid (mew-net-disp-info-get-fid disp-info))
	  (buf (mew-net-disp-info-get-buf disp-info))
	  (fld (mew-net-disp-info-get-fld disp-info))
	  (msg (mew-net-disp-info-get-msg disp-info))
	  (nfid (mew-frame-id))
	  m)
      (when (and (equal fid nfid)
		 (equal fld (mew-current-get-fld nfid)))
	(with-current-buffer buf
	  (setq m (point-marker))
	  (set-marker-insertion-type m t)
	  (when (and (mew-sinfo-get-disp-msg)
		     (equal msg (mew-current-get-msg nfid))
		     (mew-summary-search-msg msg))
	    ;; The message was truncated probaby because it's too large.
	    (mew-summary-analyze-again))
	  (goto-char m))))))

(defun mew-summary-retrieve-message (&optional rev-del)
  "Retrieve the rest of a truncated('T') message.

In a LOCAL folder: a method to retrieve the message is determined
by 'mew-case' and 'mew-mailbox-type'.
If 'mew-pop-delete'/'mew-imap-delete' is non-nil, delete the message
from the mailbox. When executed with '\\[universal-argument]',
'mew-pop-delete'/'mew-imap-delete' is considered reversed.

In a REMOTE folder: case and protocol are determined by the folder.
The message in the server side is always retained."
  (interactive "P")
  (mew-pickable
   (mew-summary-msg
    (when (mew-sumsyn-match mew-regex-sumsyn-long)
      (let* ((msg (mew-sumsyn-message-number))
	     (uid (mew-sumsyn-message-uid))
	     (siz (mew-sumsyn-message-size))
	     (bnm (mew-summary-folder-name))
	     (disp-info (mew-net-disp-info))
	     (virtual-info (if (mew-virtual-for-one-summary)
			       (mew-net-virtual-info (list msg))))
	     folder case del rtr rtrs mailbox)
	(with-current-buffer bnm
	  (when (mew-summary-exclusive-p)
	    (setq folder (mew-sinfo-get-folder))
	    (setq case (mew-sinfo-get-case))
	    (when (mew-folder-localp folder)
	      (setq case (or (mew-summary-get-inherit-case folder msg) mew-case))
	      (setq mailbox (mew-mailbox-type case))
	      (cond
	       ((eq mailbox 'pop)
		(setq del (eq (mew-pop-delete case) t))) ;; delete may be number
	       ((eq mailbox 'imap)
		(setq del (eq (mew-imap-delete case) t)))) ;; delete may be number
	      (if rev-del (setq del (not del))))
	    (when (and uid (not (string= uid "")))
	      (setq rtr (mew-make-refileinfo :uid uid
					     :size siz
					     :delete del
					     :folders (list bnm msg)))
	      (setq rtrs (list rtr)))
	    (cond
	     ((not rtrs)
	      (message "No message to be retrieved"))
	     ((not (mew-msg-validp msg))
	      (message "No valid message to be retrieved"))
	     ((mew-folder-localp folder)
	      (cond
	       ((eq mailbox 'pop)
		(mew-pop-retrieve case 'get bnm rtrs virtual-info disp-info))
	       ((eq mailbox 'imap)
		(mew-imap-retrieve case 'get bnm rtrs virtual-info disp-info))))
	     ((mew-folder-popp folder)
	      (mew-pop-retrieve case 'get bnm rtrs virtual-info disp-info))
	     ((mew-folder-imapp folder)
	      (mew-imap-retrieve case 'get bnm rtrs virtual-info disp-info))
	     ((mew-folder-nntpp folder)
	      (mew-nntp-retrieve case 'get bnm rtrs virtual-info disp-info))))))))))

(defun mew-summary-retrieve-message-for-virtual-summary (msg)
  (save-excursion
    (when (mew-summary-search-msg msg)
      (let ((start (point)))
	(forward-line)
	(buffer-substring start (point))))))

(defun mew-summary-retrieve-message-for-virtual-selection (msg str)
  (save-excursion
    (when (mew-summary-search-msg msg)
      (let ((start (point)))
	(forward-line)
	(mew-elet
	 (delete-region start (point))
	 (insert str))))))

(defun mew-summary-retrieve-message-for-virtual-thread (msg str)
  (save-excursion
    (when (mew-summary-search-msg msg)
      (let* ((start (point))
	     (beg (mew-thread-next-property start))
	     (level (mew-thread-get-property beg))
	     (length (* mew-thread-indent-length level))
	     (end (+ beg length))
	     (idt-str (buffer-substring beg end)))
	(forward-line)
	(mew-elet
	 (delete-region start (point))
	 (insert str)
	 (goto-char beg)
	 (if (= level 0) (setq end (1+ end)))
	 (insert idt-str)
	 (mew-thread-put-property beg end level))))))

(defun mew-summary-retrieve-message-for-virtual (virtual-info)
  (let ((virtual (mew-net-virtual-info-get-virtual virtual-info))
	(summary (mew-net-virtual-info-get-summary virtual-info))
	(msgs    (mew-net-virtual-info-get-msgs    virtual-info))
	(func    (if (mew-net-virtual-info-get-threadp virtual-info)
		     'mew-summary-retrieve-message-for-virtual-thread
		   'mew-summary-retrieve-message-for-virtual-selection))
	str)
    (save-excursion
      (dolist (msg msgs)
	(set-buffer summary)
	;; Summary
	(setq str (mew-summary-retrieve-message-for-virtual-summary msg))
	(set-buffer virtual)
	;; Virtual
	(funcall func msg str))
      (mew-summary-unlock))))

(defun mew-summary-mark-retrieve-message (&optional rev-del)
  "Retrieve the rest of truncated('T') messages marked with '*'.

In a LOCAL folder: a method to retrieve the messages is determined by
'mew-case' and 'mew-mailbox-type'.
If 'mew-pop-delete'/'mew-imap-delete' is non-nil, delete the messages
from the mailbox.  When executed with '\\[universal-argument]',
'mew-pop-delete'/'mew-imap-delete' is considered reversed.

In a REMOTE folder: case and protocol are determined by the folder.
The messages in the server side is always retained."
  (interactive "P")
  (mew-summary-only
   (let* ((bnm (mew-summary-folder-name 'ext))
	  (folder (mew-sinfo-get-folder))
	  (case (mew-sinfo-get-case))
	  msg uid siz del rtr rtrs case-rtrs mailbox)
     (save-excursion
       (goto-char (point-min))
       (if (mew-folder-localp folder)
	   (let (tmp)
	     (while (re-search-forward mew-regex-msg-review nil t)
	       (when (mew-sumsyn-match mew-regex-sumsyn-long)
		 (setq uid (mew-sumsyn-message-uid))
		 (setq msg (mew-sumsyn-message-number))
		 (setq siz (mew-sumsyn-message-size))
		 (setq case (or (mew-summary-get-inherit-case folder msg) mew-case))
		 (setq mailbox (mew-mailbox-type case))
		 (when (and (mew-msg-validp msg) (mew-msg-truncatedp siz))
		   (cond
		    ((eq mailbox 'pop)
		     (setq del (eq (mew-pop-delete case) t))) ;; delete may be number
		    ((eq mailbox 'imap)
		     (setq del (eq (mew-imap-delete case) t))))	;; delete may be number
		   (if rev-del (setq del (not del)))
		   (setq rtr (mew-make-refileinfo :uid uid
						  :size siz
						  :delete del
						  :folders (list bnm msg)))
		   (if (setq tmp (nth 2 (assoc case case-rtrs)))
		       (nconc tmp (list rtr))
		     (setq case-rtrs (cons (list case mailbox (list rtr)) case-rtrs))))
		 (forward-line)))
	     (setq case-rtrs (nreverse case-rtrs)))
	 (while (re-search-forward mew-regex-msg-review nil t)
	   (when (mew-sumsyn-match mew-regex-sumsyn-long)
	   (setq uid (mew-sumsyn-message-uid))
	   (setq msg (mew-sumsyn-message-number))
	   (setq siz (mew-sumsyn-message-size))
	   (when (and (mew-msg-validp msg) (mew-msg-truncatedp siz))
	     (setq rtr (mew-make-refileinfo :uid uid
					    :size siz
					    :delete del
					    :folders (list bnm msg)))
	     (setq rtrs (cons rtr rtrs))))
	   (forward-line))
	 (setq rtrs (nreverse rtrs))))
     (if (and (not rtrs) (not case-rtrs))
	 (message "No message to be retrieved")
       (cond
	((mew-folder-localp folder)
	 (while case-rtrs ;; cannot use dolist
	   (setq case (nth 0 (car case-rtrs)))
	   (setq mailbox (nth 1 (car case-rtrs)))
	   (setq rtrs (nth 2 (car case-rtrs)))
	   (setq case-rtrs (cdr case-rtrs))
	   (cond
	    ((eq mailbox 'pop)
	     (mew-pop-retrieve case 'get bnm rtrs))
	    ((eq mailbox 'imap)
	     (mew-imap-retrieve case 'get bnm rtrs)))
	   (when case-rtrs
	     (mew-rendezvous mew-summary-buffer-process))))
	((mew-folder-popp folder)
	 (mew-pop-retrieve case 'get bnm rtrs))
	((mew-folder-imapp folder)
	 (mew-imap-retrieve case 'get bnm rtrs))
	((mew-folder-nntpp folder)
	 (mew-nntp-retrieve case 'get bnm rtrs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder clean up
;;;

(defun mew-folder-clean-up ()
  (remove-hook 'kill-emacs-hook 'mew-folder-clean-up)
  (mew-local-folder-clean-up)
  (mew-imap-folder-clean-up)
  (mew-nntp-folder-clean-up))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message packing
;;;

(defun mew-net-msg-pack (msgs)
  ;; (mew-net-msg-pack '("1" "3" "4" "5" "7" "8" "10"))
  ;; => ("1" "3:5" "7:8" "10")
  (let (pack a b s)
    (when (setq a (car msgs))
      (setq a (string-to-number a))
      (setq msgs (cdr msgs))
      (while (setq b (car msgs))
	(setq b (string-to-number b))
	(setq msgs (cdr msgs))
	(if (= (1+ a) b)
	    (if (null s)
		(setq s a))
	  (if (null s)
	      (setq pack (cons (format "%d" a) pack))
	    (setq pack (cons (format "%d:%d" s a) pack))
	    (setq s nil)))
	(setq a b))
      (if s
	  (setq pack (cons (format "%d:%d" s a) pack))
	(setq pack (cons (format "%d" a) pack)))
      (nreverse pack))))

(defvar mew-imap-message-cat-size 10)

(defun mew-net-msg-cat (lst)
  (let ((N (1- mew-imap-message-cat-size))
	(crn lst)
	prv nxt ret)
    (while (setq prv (nthcdr N crn))
      (setq nxt (cdr prv))
      (setcdr prv nil)
      (setq ret (cons (mew-join "," crn) ret))
      (setq crn nxt))
    (if crn (setq ret (cons (mew-join "," crn) ret)))
    (nreverse ret)))

(defun mew-net-msg-group (lst)
  (if (= (length lst) 1)
      lst ;; "1:*"
    (mew-net-msg-cat (mew-net-msg-pack lst))))

(provide 'mew-net)

;;; Copyright Notice:

;; Copyright (C) 2002-2011 Mew developing team.
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

;;; mew-net.el ends here
