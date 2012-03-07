;;; mew-pop.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jun 28, 2000

;;; Code:

(require 'mew)

(defvar mew-pop-msgid-file ".mew-msgid")
(defvar mew-pop-folder-alist (list (mew-folder-func mew-pop-inbox-folder)))
(defun mew-pop-folder-alist ()
  mew-pop-folder-alist)

;; It is impossible to implement mew-pop-skip-uidl since
;; messages cannot be gained access by UID.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; POP info
;;;

(defvar mew-pop-info-list
  '("server" "port" "process" "ssh-process" "ssl-process" "status"
    "directive" "bnm" "mdb"
    "rtrs" "dels" "refs" "rmvs" "kils" "left" "uidl" "range"
    "rttl" "rcnt" "dttl" "dcnt" "hlds"
    "user" "auth" "auth-list" "key" "passwd" "account"
    "size" "truncated" "get-body"
    "flush" "no-msg" "msgdb" "done" "dispatched" "error"
    "body-lines" "delete" "case"
    "virtual-info" "disp-info" "status-buf"))

(mew-info-defun "mew-pop-" mew-pop-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FSM
;;;

(defvar mew-pop-fsm
  '(("greeting"      nil ("\\+OK" . "capa"))
    ("capa"          t   ("\\+OK" . "auth") ("-ERR" . "pswd"))
    ("auth-cram-md5" nil ("\\+OK" . "pwd-cram-md5") ("-ERR" . "wpwd"))
    ("pwd-cram-md5"  nil ("\\+OK" . "list") ("-ERR" . "wpwd"))
    ("auth-plain"    nil ("\\+OK" . "pwd-plain") ("-ERR" . "wpwd"))
    ("pwd-plain"     nil ("\\+OK" . "list") ("-ERR" . "wpwd"))
    ("apop"          nil ("\\+OK" . "list") ("-ERR" . "wpwd"))
    ("user"          nil ("\\+OK" . "pass") ("-ERR" . "wpwd2"))
    ("pass"          nil ("\\+OK" . "list") ("-ERR" . "wpwd"))
    ("list"          t   ("\\+OK" . "uidl"))
    ("uidl"          t   ("\\+OK" . "umsg") ("-ERR" . "nouidl"))
    ("dels"	     nil ("\\+OK" . "dels"))
    ("retr"          t   ("\\+OK" . "dele"))
    ("dele"          nil ("\\+OK" . "retr"))
    ("quit"          nil ("\\+OK" . "noop"))))

(defun mew-pop-fsm-by-status (status)
  (assoc status mew-pop-fsm))

(defun mew-pop-fsm-next (status code)
  (cdr (mew-assoc-match code (nthcdr 2 (mew-pop-fsm-by-status status)) 0)))

(defun mew-pop-fsm-reply (status)
  (nth 1 (mew-pop-fsm-by-status status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structure
;;;

(mew-defstruct popinfo num uid size delete fldmsg)

(defun mew-pop-refileinfo-to-popinfo (refileinfo num)
  (cons num refileinfo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filters
;;;

(defun mew-pop-secure-p (pnm)
  (or (mew-pop-get-ssh-process pnm) (mew-pop-get-ssl-process pnm)))

(defun mew-pop-command-capa (pro pnm)
  (mew-net-status (mew-pop-get-status-buf pnm)
		  "Auth'ing"
		  nil
		  (mew-pop-secure-p pnm))
  (if (re-search-forward "<[=!-;?-~]+@[=!-;?-~]+>" nil t)
      (mew-pop-set-key pnm (mew-match-string 0)))
  (mew-pop-process-send-string pro "CAPA"))

(defun mew-pop-command-auth (pro pnm)
  (cond
   ((eq (mew-pop-get-auth pnm) t) ;; t means SASL
    (let ((auth-list (mew-pop-get-auth-list pnm))
	  auth func)
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "SASL \\([^\r\n]+\\)" nil t)
	    (setq auth (mew-auth-select (mew-match-string 1) auth-list))))
      (if (and auth
	       (setq func (mew-pop-auth-get-func auth))
	       (fboundp func))
	  (progn
	    (mew-pop-set-auth pnm auth)
	    (funcall func pro pnm))
	(mew-pop-debug "<AUTH>" "No preferred POP AUTH.\n")
	(mew-pop-command-wpwd2 pro pnm))))
   (t
    (mew-pop-command-pswd pro pnm))))

(defun mew-pop-command-pswd (pro pnm)
  (let ((auth (mew-pop-get-auth pnm)))
    (cond
     ((or (eq auth 'pass) (eq auth 'user))
      (mew-pop-set-status pnm "user")
      (mew-pop-command-user pro pnm))
     (t
      (mew-pop-set-status pnm "apop")
      (mew-pop-command-apop pro pnm)))))

(defun mew-pop-command-user (pro pnm)
  (let ((user (mew-pop-get-user pnm)))
    (mew-pop-process-send-string pro "USER %s" user)))

(defun mew-pop-command-pass (pro pnm)
  (let* ((prompt (format "POP password (%s): "
                        (mew-pop-get-account pnm)))
          (passwd (mew-pop-input-passwd prompt pnm)))
    (mew-pop-message pnm "Sending your POP password to the POP server...")
    (mew-pop-process-send-string pro "PASS %s" passwd)))

(defun mew-pop-command-apop (pro pnm)
  (let ((user (mew-pop-get-user pnm))
        (prompt (format "APOP password (%s): " (mew-pop-get-account pnm)))
	(key (mew-pop-get-key pnm))
	passwd kmd5)
    (cond
     (key
      (setq passwd (mew-pop-input-passwd prompt pnm))
      (setq kmd5 (mew-keyed-md5 key passwd))
      (mew-pop-message pnm "Sending your APOP password to the POP server...")
      (mew-pop-process-send-string pro "APOP %s %s" user kmd5))
     (t
      (mew-passwd-set-passwd (mew-pop-passtag pnm) nil)
      (mew-pop-message pnm "APOP password is not supported by this server")
      (mew-pop-command-quit2 pro pnm)))))

(defun mew-pop-command-wpwd (pro pnm)
  (let ((directive (mew-pop-get-directive pnm))
	(bnm (mew-pop-get-bnm pnm))
	(auth (mew-pop-get-auth pnm))
	(clear-pass t))
    (goto-char (point-min))
    (cond
     ((or (looking-at "-ERR \\[IN-USE\\]") ;; RFC2449
	  (re-search-forward " lock" nil t)) ;; for old servers: very ad hoc
      (mew-pop-message pnm "The mailbox is locked!")
      (setq clear-pass nil))
     ((or (eq auth 'pass) (eq auth 'user))
      (mew-pop-message pnm "POP password is wrong!"))
     ((eq auth 'apop)
      (mew-pop-message pnm "APOP password is wrong!"))
     ((stringp auth)
      (mew-pop-message pnm "%s password is wrong!" (upcase auth)))
     (t
      ;; pnm may be cleared already
      (mew-pop-message pnm "POP password is wrong!")))
    (if clear-pass (mew-passwd-set-passwd (mew-pop-passtag pnm) nil))
    (if (eq directive 'exec)
	(mew-summary-visible-buffer bnm))
    (mew-pop-command-quit2 pro pnm)))

(defun mew-pop-command-wpwd2 (pro pnm)
  (let ((directive (mew-pop-get-directive pnm))
	(status (mew-pop-get-status pnm))
	(bnm (mew-pop-get-bnm pnm)))
    (if (string= status "auth")
	(mew-pop-message pnm "No POP AUTH available!")
      (mew-pop-message pnm "Stronger password scheme should be used!"))
    (mew-passwd-set-passwd (mew-pop-passtag pnm) nil)
    (if (eq directive 'exec)
	(mew-summary-visible-buffer bnm))
    (mew-pop-command-quit2 pro pnm)))

(defun mew-pop-command-list (pro pnm)
  (mew-net-status (mew-pop-get-status-buf pnm)
		  "Checking"
		  nil
		  (mew-pop-secure-p pnm))
  (let ((directive (mew-pop-get-directive pnm)))
    (cond
     ((or (eq directive 'get) (eq directive 'exec) (eq directive 'sync))
      (mew-pop-set-status pnm "uidl")
      (mew-pop-process-send-string pro "UIDL"))
     (t
      (mew-pop-process-send-string pro "LIST")))))

(defun mew-pop-command-uidl (pro pnm)
  (let (msgs num siz)
    (while (re-search-forward "^\\([0-9]+\\) +\\([0-9]+\\)" nil t)
      (setq num (mew-match-string 1))
      (setq siz (mew-match-string 2))
      (setq msgs (cons (mew-make-popinfo :num num :size siz) msgs)))
    (if msgs
	(progn
	  (setq msgs (nreverse msgs))
	  (mew-pop-set-rtrs pnm msgs)
	  (mew-pop-set-left pnm (length msgs))
	  (mew-pop-process-send-string pro "UIDL"))
      (mew-pop-set-status pnm "quit")
      (mew-pop-command-quit pro pnm))))

;; The POP server does not support UIDL.
(defun mew-pop-command-nouidl (pro pnm)
  (let ((directive (mew-pop-get-directive pnm)))
    (if (or (eq directive 'scan) (eq directive 'exec) (eq directive 'sync))
	(progn
	  (mew-pop-message pnm "POP server does not support this command")
	  (mew-pop-command-quit2 pro pnm))
      (mew-pop-set-size pnm 0) ;; To get the entire message
      (mew-pop-set-rttl pnm (mew-pop-get-left pnm))
      (when (mew-pop-get-delete pnm)
	(dolist (rtr (mew-pop-get-rtrs pnm))
	  (mew-popinfo-set-delete rtr t)))
      (mew-pop-command-pre-retr pro pnm))))

(defun mew-pop-command-umsg (pro pnm)
  (let* ((directive (mew-pop-get-directive pnm))
	 (popinfos (mew-pop-get-rtrs pnm)) ;; popinfo
	 (refs (mew-pop-get-refs pnm)) ;; refileinfo
	 (rmvs (mew-pop-get-rmvs pnm))
	 (del-time (mew-pop-get-delete pnm))
	 (bnm (mew-pop-get-bnm pnm))
	 (range (mew-pop-get-range pnm))
	 (ctime (current-time))
	 (left 0)
	 rtr rtrs dels num uid uidl old-uidl uid-time hlds)
    (cond
     ((eq directive 'inc)
      (setq old-uidl (mew-net-uidl-db-get (mew-pop-passtag pnm))))
     ((eq directive 'biff)
      (setq old-uidl (mew-net-uidl-db-get (mew-pop-passtag pnm))))
     ((eq directive 'scan)
      (if (eq range nil) ;; update
	  (setq old-uidl (mew-lisp-load
			  (mew-expand-file bnm mew-pop-msgid-file))))))
    (while (re-search-forward "^\\([0-9]+\\) +\\([!-~]*\\)" nil t)
      (setq left (1+ left))
      (setq num (mew-match-string 1))
      (setq uid (mew-match-string 2))
      (setq uid-time nil)
      ;; A broken POP server may return a null UID.
      (if (string= uid "") (setq uid nil))
      (cond
       ((eq directive 'get)
	(cond
	 ((setq rtr (assoc uid refs))
	  (setq rtr (mew-pop-refileinfo-to-popinfo rtr num))
	  (setq rtrs (cons rtr rtrs)))
	 ((member uid rmvs)
	  (setq dels (cons num dels)))))
       ((eq directive 'exec)
	(if (member uid rmvs)
	    (setq dels (cons num dels))))
       ((eq directive 'biff)
	(when (and uid (not (assoc uid old-uidl)))
	  (setq rtr (mew-make-popinfo :num num))
	  (setq rtrs (cons rtr rtrs))))
       ((eq directive 'scan)
	(when uid
	  (setq uidl (cons uid uidl))
	  (when (or range ;; all, last:n
		    (not (member uid old-uidl))) ;; update
	    (setq rtr (assoc num popinfos))
	    (mew-popinfo-set-uid rtr uid)
	    (setq rtrs (cons rtr rtrs)))))
       ((eq directive 'sync)
	(when uid
	  (setq hlds (cons uid hlds))))
       ((eq directive 'inc)
	(if uid (setq uid-time (cdr (assoc uid old-uidl))))
	(cond
	 (uid-time
	  (setq uidl (cons (cons uid uid-time) uidl))
	  (if (mew-expired-p uid-time del-time)
	      (setq dels (cons num dels))))
	 (t
	  (setq uidl (cons (cons uid ctime) uidl))
	  (setq rtr (assoc num popinfos))
	  (mew-popinfo-set-uid rtr uid)
	  (if (eq del-time t) (mew-popinfo-set-delete rtr t))
	  (setq rtrs (cons rtr rtrs)))))))
    (mew-pop-set-uidl pnm uidl)
    ;; last:n
    (when (and (eq directive 'scan) (integerp range))
      (mew-ntake range rtrs))
    (setq rtrs (nreverse rtrs))
    (setq dels (nreverse dels))
    (setq hlds (nreverse hlds))
    (mew-pop-set-rtrs pnm rtrs)
    (mew-pop-set-dels pnm dels)
    (mew-pop-set-hlds pnm hlds)
    (mew-pop-set-rttl pnm (length rtrs))
    (mew-pop-set-dttl pnm (length dels))
    (mew-pop-set-left pnm left)
    (mew-pop-set-dispatched pnm t)
    (cond
     ((or (eq directive 'sync) (eq directive 'biff))
      (mew-pop-set-status pnm "quit")
      (mew-pop-command-quit pro pnm))
     (dels
      (mew-pop-command-pre-dels pro pnm))
     ((eq directive 'exec) ;; no dels
      (mew-pop-set-status pnm "quit")
      (mew-pop-command-quit pro pnm))
     (t
      (mew-pop-command-pre-retr pro pnm)))))

(defun mew-pop-command-pre-dels (pro pnm)
  (let* ((directive (mew-pop-get-directive pnm))
	 (dttl (mew-pop-get-dttl pnm)))
    (cond
     ((= dttl 0) ;; should not occur
      (mew-pop-command-pre-retr pro pnm))
     ((= dttl 1)
      (if (eq directive 'exec)
	  (mew-pop-message pnm "Deleting 1 message in background..."))
      (mew-pop-set-status pnm "dels")
      (mew-pop-command-dels pro pnm))
     (t
      (if (eq directive 'exec)
	  (mew-pop-message pnm "Deleting %d messages in background..." dttl))
      (mew-pop-set-status pnm "dels")
      (mew-pop-command-dels pro pnm)))))

(defun mew-pop-command-dels (pro pnm)
  (mew-net-status1
   (mew-pop-get-status-buf pnm) "Deleting"
   (mew-pop-get-dttl pnm) (mew-pop-get-dcnt pnm) (mew-pop-secure-p pnm))
  (let ((directive (mew-pop-get-directive pnm))
	(dels (mew-pop-get-dels pnm))
	(left (mew-pop-get-left pnm))
	num)
    (if (null dels)
	(if (eq directive 'exec)
	    (progn
	      (mew-pop-set-status pnm "quit")
	      (mew-pop-command-quit pro pnm))
	  (mew-pop-command-pre-retr pro pnm))
      (mew-pop-set-dcnt pnm (1+ (mew-pop-get-dcnt pnm)))
      (setq num (car dels))
      (mew-pop-set-dels pnm (cdr dels))
      (mew-pop-set-left pnm (1- left))
      (mew-pop-process-send-string pro "DELE %s" num))))

(defun mew-pop-command-pre-retr (pro pnm)
  (let* ((rttl (mew-pop-get-rttl pnm)))
    (cond
     ((= rttl 0)
      (mew-pop-message pnm "No new messages")
      (mew-pop-set-status pnm "quit")
      (mew-pop-command-quit pro pnm))
     ((= rttl 1)
      (mew-pop-message pnm "Retrieving 1 message in background...")
      (mew-pop-set-status pnm "retr")
      (mew-pop-command-retr pro pnm))
     (t
      (mew-pop-message pnm "Retrieving %d messages in background..." rttl)
      (mew-pop-set-status pnm "retr")
      (mew-pop-command-retr pro pnm)))))

(defun mew-pop-command-retr (pro pnm)
  (mew-net-status2 (mew-pop-get-status-buf pnm)
		   (mew-pop-get-rttl pnm)
		   (mew-pop-get-rcnt pnm)
		   (mew-popinfo-get-size (car (mew-pop-get-rtrs pnm)))
		   'zero
		   (mew-pop-secure-p pnm))
  (let* ((directive (mew-pop-get-directive pnm))
	 (rtrs (mew-pop-get-rtrs pnm))
	 (rtr  (car rtrs))
	 (num  (mew-popinfo-get-num rtr))
	 (siz  (mew-popinfo-get-size rtr))
	 (lim (mew-pop-get-size pnm))
	 (lines (mew-pop-get-body-lines pnm))
	 (get-body (mew-pop-get-get-body pnm)))
    (cond
     ((or (null rtr) (eq directive 'biff))
      (mew-pop-set-truncated pnm nil)
      (mew-pop-set-status pnm "quit")
      (mew-pop-command-quit pro pnm))
     ((eq directive 'get)
      (mew-pop-set-truncated pnm nil)
      (mew-pop-process-send-string pro "RETR %s" num))
     ((and (eq directive 'scan) (not get-body))
      (mew-pop-set-truncated pnm t)
      (mew-pop-process-send-string pro "TOP %s 0" num))
     ((or (= lim 0) (< (string-to-number siz) lim))
      (mew-pop-set-truncated pnm nil)
      (mew-pop-process-send-string pro "RETR %s" num))
     (t
      (mew-pop-set-truncated pnm t)
      (mew-pop-process-send-string pro "TOP %s %d" num lines)))))

(defun mew-pop-command-dele (pro pnm)
  (let* ((directive (mew-pop-get-directive pnm))
	 (width (1- (mew-scan-width)))
	 (left (mew-pop-get-left pnm))
	 (rtrs (mew-pop-get-rtrs pnm))
	 (rtr  (car rtrs))
	 (num  (mew-popinfo-get-num rtr))
	 (uid  (mew-popinfo-get-uid rtr))
	 (siz  (mew-popinfo-get-size rtr))
	 (del  (mew-popinfo-get-delete rtr))
	 (fld-msg (mew-popinfo-get-fldmsg rtr))
	 (truncated (mew-pop-get-truncated pnm))
	 (case (mew-pop-get-case pnm))
	 fld msg vec file msg-file lmsg mark folder)
    (cond
     ((null fld-msg)
      (setq fld (mew-pop-get-bnm pnm)))
     ((stringp fld-msg)
      (setq fld fld-msg))
     ((listp fld-msg)
      (mew-set '(fld msg) fld-msg)
      (setq lmsg msg)))
    (setq folder (mew-case:folder-folder fld))
    ;; deleting +OK
    (goto-char (point-min))
    (forward-line)
    (delete-region (point-min) (point))
    ;; line delimiters
    (mew-eol-fix-for-read)
    (mew-dot-delete)
    (setq msg-file (mew-net-get-new-message
		    pnm fld msg 'mew-pop-get-msgdb 'mew-pop-set-msgdb))
    (setq msg (car msg-file) file (cdr msg-file))
    (goto-char (point-min))
    (cond
     (truncated
      (mew-header-insert-xmu uid siz t (and (eq directive 'inc)
					    (or (mew-pop-get-case pnm)
						mew-case-default))))
     ((eq directive 'scan)
      (mew-header-insert-xmu uid siz nil))
     ((and (eq directive 'get) (mew-folder-popp folder))
      (mew-header-insert-xmu uid siz nil)))
    (catch 'write-error
      (condition-case nil
	  (let ((write-region-inhibit-fsync mew-use-async-write))
	    (mew-frwlet mew-cs-dummy mew-cs-text-for-write
	      (write-region (point-min) (point-max) file nil 'no-msg)))
	(error
	 (mew-pop-set-status pnm "quit")
	 (mew-pop-command-quit pro pnm)
	 (throw 'write-error nil)))
      (mew-pop-set-rcnt pnm (1+ (mew-pop-get-rcnt pnm)))
      (when (file-exists-p file)
	(mew-set-file-modes file)
	(mew-set-file-type file)
	;;
	(mew-set-buffer-multibyte t)
	(setq vec (mew-scan-header))
	(mew-scan-set-folder vec fld)
	(mew-scan-set-message vec msg)
	(if (or (eq directive 'inc) (eq directive 'scan))
	    (setq mark (mew-scan-inbox-action vec case)))
	(if (and mark
		 (eq directive 'scan)
		 (stringp mark)) ;; in the case of refiling
	    (setq mark nil))
	(mew-scan-body vec)
	(mew-set-buffer-multibyte nil)
	(mew-scan-insert-line fld vec width lmsg mark))
      (mew-pop-set-rtrs pnm (cdr rtrs))
      (if (and del (not truncated))
	  (progn
	    (mew-pop-set-left pnm (1- left))
	    (mew-pop-process-send-string pro "DELE %s" num))
	(mew-pop-set-status pnm "retr")
	(mew-pop-command-retr pro pnm)))))

(defun mew-pop-command-quit (pro pnm)
  (mew-pop-set-done pnm t)
  (mew-pop-process-send-string pro "QUIT"))

(defun mew-pop-command-quit2 (pro pnm)
  (mew-pop-set-done pnm t)
  (mew-pop-set-error pnm t)
  (mew-pop-set-status pnm "quit")
  (mew-pop-process-send-string pro "QUIT"))

(defun mew-pop-command-noop (pro pnm)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AUTH
;;;

(defvar mew-pop-auth-alist
  '(("CRAM-MD5" mew-pop-command-auth-cram-md5)
    ("PLAIN"    mew-pop-command-auth-plain)))

(defun mew-pop-auth-get-func (auth)
  (nth 1 (mew-assoc-case-equal auth mew-pop-auth-alist 0)))

(defun mew-pop-command-auth-cram-md5 (pro pnm)
  (mew-pop-process-send-string pro "AUTH CRAM-MD5")
  (mew-pop-set-status pnm "auth-cram-md5"))

(defun mew-pop-command-pwd-cram-md5 (pro pnm)
  (let ((user (mew-pop-get-user pnm))
	challenge passwd cram-md5)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward" \\([a-zA-Z0-9+/]+=*\\)" nil t)
	  (setq challenge (mew-match-string 1))))
    (setq passwd (mew-pop-input-passwd "POP CRAM-MD5: " pnm))
    (setq cram-md5 (mew-cram-md5 user passwd challenge))
    (mew-pop-process-send-string pro cram-md5)))

(defun mew-pop-command-auth-plain (pro pnm)
  (mew-pop-process-send-string pro "AUTH PLAIN")
  (mew-pop-set-status pnm "auth-plain"))

(defun mew-pop-command-pwd-plain (pro pnm)
  (let* ((prompt (format "POP PLAIN password (%s): "
                         (mew-pop-get-account pnm)))
         (passwd (mew-pop-input-passwd prompt pnm))
	 (user (mew-pop-get-user pnm))
	 (plain (mew-base64-encode-string (format "\0%s\0%s" user passwd))))
    (mew-pop-process-send-string pro "%s" plain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defconst mew-pop-info-prefix "mew-pop-info-")

(defun mew-pop-info-name (case)
  (let ((server (mew-pop-server case))
	(port (mew-*-to-string (mew-pop-port case)))
	(user (mew-pop-user case))
	(sshsrv (mew-pop-ssh-server case))
	(name mew-pop-info-prefix))
    (setq name (concat name user "@" server))
    (unless (mew-port-equal port mew-pop-port)
      (setq name (concat name ":" port)))
    (if sshsrv
        (concat name "%" sshsrv)
      name)))

(defun mew-pop-buffer-name (pnm)
  (concat mew-buffer-prefix pnm))

(defun mew-pop-process-send-string (pro &rest args)
  (let ((str (apply 'format args)))
    (mew-pop-debug "=SEND=" str)
    (if (and (processp pro) (eq (process-status pro) 'open))
	(process-send-string pro (concat str mew-cs-eol))
      (message "POP time out")))) ;; not mew-pop-message

(defun mew-pop-message (pnm &rest args)
  (or (mew-pop-get-no-msg pnm) (apply 'message args)))

(defun mew-pop-message2 (pnm msg left)
  (let (msg2)
    (cond
     ((or (null left) (= left 0))
      )
     ((= left 1)
      (setq msg2 " (1 message left)"))
     (t
      (setq msg2 (format " (%d messages left)" left))))
    (if msg2 (setq msg (concat msg msg2)))
    (mew-pop-message pnm msg)))

(defun mew-pop-passtag (pnm)
  (let ((server (mew-pop-get-server pnm))
	(port (mew-pop-get-port pnm))
	(user (mew-pop-get-user pnm)))
    (concat user "@" server ":" port)))

(defun mew-pop-passtag2 (case)
  (let ((server (mew-pop-server case))
	(port (mew-pop-port case))
	(user (mew-pop-user case)))
    (concat user "@" server ":" port)))

(defun mew-pop-input-passwd (prompt pnm)
  (let ((directive (mew-pop-get-directive pnm))
	(pro (mew-pop-get-process pnm))
	(tag (mew-pop-passtag pnm))
	pass)
    (if (eq directive 'biff)
	(or (mew-pop-get-passwd pnm)       ;; mew-pop-biff
	    (mew-input-passwd prompt tag)) ;; mew-pop-check
      (setq pass (mew-input-passwd prompt tag))
      (unless (and (processp pro) (eq (process-status pro) 'open))
	(mew-passwd-set-passwd tag nil))
      pass)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening POP
;;;

(defun mew-pop-open (pnm server port no-msg)
  (let ((sprt (mew-*-to-port port))
	pro tm)
    (condition-case emsg
	(progn
	  (setq tm (run-at-time mew-pop-timeout-time nil 'mew-pop-timeout))
	  (or no-msg (message "Connecting to the POP server..."))
	  (setq pro (open-network-stream pnm nil server sprt))
	  (mew-process-silent-exit pro)
	  (mew-set-process-cs pro mew-cs-text-for-net mew-cs-text-for-net)
	  (or no-msg (message "Connecting to the POP server...done")))
      (quit
       (or no-msg (message "Cannot connect to the POP server"))
       (setq pro nil))
      (error
       (or no-msg (message "%s, %s" (nth 1 emsg) (nth 2 emsg)))
       (setq pro nil)))
    (if tm (cancel-timer tm))
    pro))

(defun mew-pop-timeout ()
  (signal 'quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-pop-retrieve (case directive bnm &rest args)
  ;; in +inbox
  (let* ((server (mew-pop-server case))
         (user (mew-pop-user case))
	 (port (mew-*-to-string (mew-pop-port case)))
	 (sshsrv (mew-pop-ssh-server case))
	 (sslp (mew-pop-ssl case))
	 (sslport (mew-pop-ssl-port case))
         (proxysrv (mew-pop-proxy-server case))
         (proxyport (mew-pop-proxy-port case))
	 (pnm (mew-pop-info-name case))
	 (buf (get-buffer-create (mew-pop-buffer-name pnm)))
	 (no-msg (eq directive 'biff))
	 process sshname sshpro sslname sslpro lport tls
	 virtual-info disp-info virtual)
    (if (mew-pop-get-process pnm)
	(message "Another POP process is running. Try later")
      (cond
       (sshsrv
	(setq sshpro (mew-open-ssh-stream case server port sshsrv))
	(when sshpro
	  (setq sshname (process-name sshpro))
	  (setq lport (mew-ssh-pnm-to-lport sshname))
	  (when lport
	    (setq process (mew-pop-open pnm "localhost" lport no-msg)))))
       (sslp
	(if (mew-port-equal port sslport) (setq tls mew-tls-pop))
	(setq sslpro (mew-open-ssl-stream case server sslport tls))
	(when sslpro
	  (setq sslname (process-name sslpro))
	  (setq lport (mew-ssl-pnm-to-lport sslname))
	  (when lport
	    (setq process (mew-pop-open pnm mew-ssl-localhost lport no-msg)))))
       (proxysrv
	(setq process (mew-pop-open pnm proxysrv proxyport no-msg)))
       (t
	(setq process (mew-pop-open pnm server port no-msg))))
      (if (null process)
	  (if (eq directive 'exec)
	      (mew-summary-visible-buffer bnm))
	(mew-summary-lock process "POPing" (or sshpro sslpro))
	(mew-sinfo-set-summary-form (mew-get-summary-form bnm))
	(mew-sinfo-set-summary-column (mew-get-summary-column bnm))
	(mew-sinfo-set-unread-mark nil)
	(mew-sinfo-set-scan-id nil)
	(mew-sinfo-set-scan-md5 nil)
	(mew-info-clean-up pnm)
	(mew-pop-set-no-msg pnm no-msg) ;; must come here
	(mew-pop-message pnm "Communicating with the POP server...")
	(mew-pop-set-process pnm process)
	(mew-pop-set-ssh-process pnm sshpro)
	(mew-pop-set-ssl-process pnm sslpro)
	(mew-pop-set-server pnm server)
	(mew-pop-set-port pnm port)
	(mew-pop-set-user pnm user)
        (mew-pop-set-account pnm (format "%s@%s" user server))
	(mew-pop-set-auth pnm (mew-pop-auth case))
	(mew-pop-set-auth-list pnm (mew-pop-auth-list case))
	(mew-pop-set-status pnm "greeting")
	(mew-pop-set-directive pnm directive)
	(mew-pop-set-bnm pnm bnm)
	(mew-pop-set-status-buf pnm bnm)
	(mew-pop-set-rcnt pnm 1)
	(mew-pop-set-dcnt pnm 1)
	(mew-pop-set-rttl pnm 0)
	(mew-pop-set-dttl pnm 0)
	(mew-pop-set-size pnm (mew-pop-size case))
	(mew-pop-set-case pnm case)
	(mew-pop-set-body-lines pnm (mew-pop-body-lines case))
	(cond
	 ((eq directive 'biff)
	  (mew-pop-set-passwd pnm (nth 0 args))) ;; password
	 ((eq directive 'inc)
	  (mew-sinfo-set-unread-mark (mew-get-unread-mark bnm))
	  (mew-pop-set-flush pnm (nth 0 args)) ;; no-flush
	  (mew-pop-set-delete pnm (mew-pop-delete case)))
	 ((eq directive 'get)
	  (mew-sinfo-set-unread-mark (mew-get-unread-mark bnm))
	  (mew-pop-set-refs pnm (nth 0 args))
	  (setq virtual-info (nth 1 args))
	  (mew-pop-set-virtual-info pnm virtual-info)
	  (setq disp-info (nth 2 args))
	  (mew-pop-set-disp-info pnm disp-info)
	  (setq virtual (mew-net-virtual-info-get-virtual virtual-info))
	  (when virtual
	    (mew-pop-set-status-buf pnm virtual)
	    (with-current-buffer virtual
	      (mew-summary-lock process "POPing" (or sshpro sslpro)))))
	 ((eq directive 'scan)
	  (mew-pop-set-range pnm (nth 0 args))
	  (mew-pop-set-get-body pnm (nth 1 args))
	  (if (mew-pop-get-range pnm)
	      (progn
		(mew-pop-set-mdb pnm (mew-summary-mark-collect4))
		(mew-net-folder-clean))
	    (mew-sinfo-set-unread-mark (mew-get-unread-mark bnm))))
	 ((eq directive 'sync)
	  )
	 ((eq directive 'exec)
	  (mew-pop-set-rmvs pnm (nth 0 (nth 0 args)))
	  (mew-pop-set-kils pnm (nth 1 (nth 0 args)))))
	(mew-sinfo-set-start-point (point)) ;; after erase-buffer
	;;
	(set-process-sentinel process 'mew-pop-sentinel)
	(set-process-filter process 'mew-pop-filter)
	(set-process-buffer process buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-pop-debug (label string)
  (when (mew-debug 'net)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-pop-filter (process string)
  (let* ((pnm (process-name process))
	 (status (mew-pop-get-status pnm))
	 (mulrep (mew-pop-fsm-reply status))
	 stay next func)
    (mew-pop-debug (upcase status) string)
    (mew-filter
     ;; Process's buffer
     (goto-char (point-max))
     (mew-set-buffer-multibyte nil)
     (insert string)
     (when (string= status "retr")
       (mew-net-status2 (mew-pop-get-status-buf pnm)
			(mew-pop-get-rttl pnm)
			(mew-pop-get-rcnt pnm)
			(mew-popinfo-get-size (car (mew-pop-get-rtrs pnm)))
			nil
			(mew-pop-secure-p pnm)))
     (cond
      ((and (and (goto-char (point-min)) (looking-at "-ERR"))
	    (and (goto-char (1- (point-max))) (looking-at "\n$")))
       (setq next (mew-pop-fsm-next status "-ERR")))
      ((and (and (goto-char (point-min)) (looking-at "\\+")) ;; +OK
	    (or (and mulrep
		     (goto-char (point-max))
		     (= (forward-line -1) 0)
		     (looking-at "^\\.\r?$"))
		(and (not mulrep)
		     (goto-char (1- (point-max)))
		     (looking-at "\n$"))))
       (setq next (mew-pop-fsm-next status "+OK")))
      (t
       (setq stay t)))
     (unless stay
       (unless next (setq next "quit"))
       (mew-pop-set-status pnm next)
       (setq func (intern-soft (concat "mew-pop-command-" next)))
       (goto-char (point-min))
       (if (fboundp func)
	   (funcall func process pnm)
	 (error "No function called %s" (symbol-name func)))
       (if (and process (equal (process-buffer process) (current-buffer)))
	   (mew-erase-buffer))))))

(defun mew-pop-sentinel (process event)
  (let* ((pnm (process-name process))
	 (directive (mew-pop-get-directive pnm))
	 (mdb (mew-pop-get-mdb pnm))
	 (sshpro (mew-pop-get-ssh-process pnm))
	 (sslpro (mew-pop-get-ssl-process pnm))
	 (rttl (mew-pop-get-rttl pnm))
	 (dttl (mew-pop-get-dttl pnm))
	 (left (mew-pop-get-left pnm))
	 (bnm (or (mew-pop-get-bnm pnm) (current-buffer)))
	 (flush (mew-pop-get-flush pnm))
	 (kils (mew-pop-get-kils pnm))
	 (hlds (mew-pop-get-hlds pnm))
	 (uidl (mew-pop-get-uidl pnm))
	 (done (mew-pop-get-done pnm))
	 (error (mew-pop-get-error pnm))
	 (file (mew-expand-file bnm mew-pop-msgid-file))
	 (buf (process-buffer process))
	 (virtual-info (mew-pop-get-virtual-info pnm))
	 (disp-info (mew-pop-get-disp-info pnm)))
    (save-excursion
      (mew-pop-debug "POP SENTINEL" event)
      (set-process-buffer process nil)
      (set-buffer bnm)
      (mew-summary-mark-recover mdb)
      (mew-remove-buffer buf)
      (if (not done)
	  (let* ((rtrs (mew-pop-get-rtrs pnm))
		 (lefts (length rtrs))
		 (uid (mew-popinfo-get-uid (car rtrs)))
		 recovered)
	    (mew-pop-message pnm "POP connection is lost")
	    (when (mew-pop-get-dispatched pnm)
	      (cond
	       ((eq directive 'scan)
		;; uidl is reversed.
		(setq uidl (cdr (member uid uidl)))
		(mew-lisp-save file uidl nil 'unlimit)
		(setq recovered t))
	       ((eq directive 'inc)
		;; uidl is reversed.
		(setq uid (assoc uid uidl))
		(setq uidl (cdr (member uid uidl)))
		(mew-net-uidl-db-set (mew-pop-passtag pnm) uidl)
		(setq recovered t)))
	      (when recovered
		(mew-pop-message
		 pnm
		 "%d message retrieved. %d messages are left due to an error"
		 (- rttl lefts) lefts)
		(mew-summary-folder-cache-save))))
	(if virtual-info (mew-summary-retrieve-message-for-virtual virtual-info))
	(cond
	 (error
	  ;; retain the error message
	  )
	 ((eq directive 'biff)
	  (and (functionp mew-biff-function)
	       (funcall mew-biff-function rttl)))
	 ((eq directive 'sync)
	  (mew-pop-message pnm "Synchronizing messages...")
	  (mew-net-folder-sync bnm hlds)
	  (mew-pop-message pnm "Synchronizing messages...done"))
	 ((eq directive 'inc)
	  (mew-biff-clear)
	  (mew-net-uidl-db-set (mew-pop-passtag pnm) uidl)
	  (cond
	   ((= rttl 0)
	    (mew-pop-message2 pnm "No new messages" left))
	   ((= rttl 1)
	    (mew-pop-message2 pnm "1 message retrieved" left)
	    (mew-summary-folder-cache-save))
	   (t
	    (mew-pop-message2 pnm (format "%d messages retrieved" rttl) left)
	    (mew-summary-folder-cache-save))))
	 ((eq directive 'get)
	  (cond
	   ((= rttl 0)
	    (mew-pop-message2 pnm "No new messages" left))
	   ((= rttl 1)
	    (mew-pop-message2 pnm "1 message retrieved" left)
	    (mew-summary-folder-cache-save))
	   (t
	    (mew-pop-message2 pnm (format "%d messages retrieved" rttl) left)
	    (mew-summary-folder-cache-save))))
	 ((eq directive 'scan)
	  (mew-biff-clear)
	  (cond
	   ((or (= rttl 0) (null uidl))
	    (mew-pop-message pnm "No messages scanned"))
	   ((= rttl 1)
	    (mew-pop-message pnm "1 message scanned")
	    (mew-lisp-save file uidl nil 'unlimit)
	    (mew-summary-folder-cache-save))
	   (t
	    (mew-pop-message pnm "%d messages scanned" rttl)
	    (mew-lisp-save file uidl nil 'unlimit)
	    (mew-summary-folder-cache-save))))
	 ((eq directive 'exec)
	  (when kils
	    (mew-mark-exec-unlink bnm kils)
	    (mew-mark-kill-invisible)
	    (mew-summary-folder-cache-save))
	  (cond
	   ((= dttl 0)
	    (mew-pop-message pnm "No messages deleted"))
	   ((= dttl 1)
	    (mew-pop-message pnm "1 message deleted"))
	   (t
	    (mew-pop-message pnm "%d messages deleted" dttl))))))
      ;;
      (and mew-use-async-write (mew-unix-sync))
      (mew-net-status-clear (mew-pop-get-status-buf pnm))
      (mew-info-clean-up pnm)
      (set-buffer-modified-p nil)
      (mew-summary-unlock)
      (if (and (processp sshpro) (not mew-ssh-keep-connection))
	  (process-send-string sshpro "exit\n"))
      (if (and (processp sslpro) (not mew-ssl-keep-connection))
	  (delete-process sslpro))
      (mew-net-disp-info-display disp-info)
      (unless (eq directive 'biff)
	(run-hooks 'mew-pop-sentinel-non-biff-hook))
      (run-hooks 'mew-pop-sentinel-hook)
      (when (and mew-auto-flush-queue flush)
	(mew-smtp-flush-queue mew-case)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Biff
;;;

(defun mew-pop-biff ()
  (let* ((case mew-case)
	 (inbox (mew-proto-inbox-folder nil case))
	 (case:inbox (mew-case-folder case inbox))
	 (tag (mew-pop-passtag2 case))
	 passwd)
    (when (get-buffer case:inbox)
      (with-current-buffer case:inbox
	(when (and (mew-summary-exclusive-p 'no-msg)
		   (and (or mew-use-cached-passwd mew-use-master-passwd)
			(setq passwd (mew-passwd-get-passwd tag))))
	  (mew-pop-retrieve case 'biff case:inbox passwd))))))

(defun mew-pop-check ()
  "See if messages are arrived by POP."
  (interactive)
  (let* ((case mew-case)
	 (inbox (mew-proto-inbox-folder nil case))
	 (case:inbox (mew-case-folder case inbox)))
    (when (get-buffer case:inbox)
      (with-current-buffer case:inbox
	(when (mew-summary-exclusive-p)
	  (mew-pop-retrieve case 'biff case:inbox))))))

(provide 'mew-pop)

;;; Copyright Notice:

;; Copyright (C) 1999-2012 Mew developing team.
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

;;; mew-pop.el ends here
