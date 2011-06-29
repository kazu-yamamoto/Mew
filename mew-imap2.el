;;; mew-imap2.el for appending

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: May 22, 2003

;;; Code:

(require 'mew)

(defvar mew-imap2-literal+ t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMAP2 info
;;;

(defvar mew-imap2-info-list
  '("case" "directive" "mailbox"
    ;;
    "src" "messages" "src-list" "src-list-orig"
    "server" "port" "ssh-server"
    "user" "auth" "auth-list" "account"
    "status" "process" "ssh-process" "ssl-process"
    "cnt" "ttl" "literal+"
    ;; parameters used internally and should be initialized
    "tag" "string" "error" "auth-selected" "authl" "aux" "done"))

(defvar mew-imap2-info-list-clean-length 20)

(mew-info-defun "mew-imap2-" mew-imap2-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FSM
;;;

(defvar mew-imap2-fsm
  '(("greeting"      ("OK" . "capability"))
    ("capability"    ("OK" . "post-capability"))
    ("auth-cram-md5" ("OK" . "pwd-cram-md5") ("NO" . "wpwd"))
    ("pwd-cram-md5"  ("OK" . "next") ("NO" . "wpwd"))
    ("auth-login"    ("OK" . "user-login") ("NO" . "wpwd"))
    ("user-login"    ("OK" . "pwd-login") ("NO" . "wpwd"))
    ("pwd-login"     ("OK" . "next") ("NO" . "wpwd"))
    ("login"         ("OK" . "next") ("NO" . "wpwd"))
    ("append"        ("OK" . "post-append") ("\\[TRYCREATE\\]" . "create"))
    ("create"        ("OK" . "append") ("NO" . "wmbx"))
    ("post-append"   ("OK" . "done") ("\\[TRYCREATE\\]" . "create"))
    ("logout"        ("OK" . "noop"))))

(defun mew-imap2-fsm-by-status (status)
  (assoc status mew-imap2-fsm))

(defun mew-imap2-fsm-next (status code)
  (cdr (mew-assoc-match2 code (nthcdr 1 (mew-imap2-fsm-by-status status)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filters 2
;;;

(defun mew-imap2-command-capability (pro pnm)
  (mew-imap2-process-send-string pro pnm "CAPABILITY"))

(defun mew-imap2-command-post-capability (pro pnm)
  (let ((case-fold-search t)
	(aux (mew-imap2-get-aux pnm))
	(start 0)
	authl literal+)
    (while (string-match "AUTH=\\([^ \r\n]+\\)" aux start)
      (setq authl (cons (mew-match-string 1 aux) authl))
      (setq start (match-end 0)))
    (if (and mew-imap2-literal+
	     (string-match "\\bliteral\\+\\b" aux))
	(setq literal+ t))
    (mew-imap2-set-authl pnm authl)
    (mew-imap2-set-literal+ pnm literal+)
    (mew-imap2-set-status pnm "authentication")
    (mew-imap2-command-authentication pro pnm)))

(defun mew-imap2-command-authentication (pro pnm)
  (cond
   ((eq (mew-imap2-get-auth pnm) t) ;; t means SASL
    (let ((auth-list (mew-imap2-get-auth-list pnm))
	  (authl (mew-imap2-get-authl pnm))
	  auth func)
      (if (and authl
	       (setq auth (mew-auth-select2 authl auth-list))
	       (setq func (mew-imap2-auth-get-func auth))
	       (fboundp func))
	  (progn
	    (mew-imap2-set-auth-selected pnm auth)
	    (funcall func pro pnm))
	(mew-imap2-debug "<AUTH>" "No preferred IMAP AUTH.\n")
	(mew-imap2-set-status pnm "login")
	(mew-imap2-command-login pro pnm))))
   (t
    (mew-imap2-set-status pnm "login")
    (mew-imap2-command-login pro pnm))))

(defun mew-imap2-command-login (pro pnm)
  (let* ((user (mew-imap2-get-user pnm))
         (prompt (format "IMAP password (%s): "
                         (mew-imap2-get-account pnm)))
         (pass (mew-input-passwd prompt (mew-imap2-passtag pnm))))
    (setq user (mew-quote-string user ?\\ '(?\\ ?\")))
    (setq pass (mew-quote-string pass ?\\ '(?\\ ?\")))
    (mew-imap2-process-send-string pro pnm "LOGIN \"%s\" \"%s\"" user pass)))

(defun mew-imap2-command-wpwd (pro pnm)
  (let ((auth (mew-imap2-get-auth-selected pnm)))
    (mew-passwd-set-passwd (mew-imap2-passtag pnm) nil)
    (mew-imap2-set-error pnm (format "IMAP %s password is wrong!" auth))
    (mew-imap2-command-logout2 pro pnm)))

(defun mew-imap2-command-next (pro pnm)
  (let ((directive (mew-imap2-get-directive pnm))
	(case (mew-imap2-get-case pnm))
	(src-list (mew-imap2-get-src-list pnm))
	(msgs (mew-imap2-get-messages pnm))
	src dst mailbox msg src-file)
    (if msgs
	(setq src (mew-imap2-get-src pnm))
      (if (or (eq directive 'copy) (eq directive 'move))
	  (progn
	    (setq msgs src-list)
	    (mew-imap2-set-src-list pnm nil)
	    (mew-imap2-set-ttl pnm (length msgs))
	    (mew-imap2-set-cnt pnm 0)
	    (setq dst (mew-imap2-get-mailbox pnm))
	    (setq mailbox (mew-imap-expand-mailbox case dst))
	    (setq mailbox (mew-imap-utf-7-encode-string mailbox))
	    (mew-imap2-set-mailbox pnm mailbox))
	(catch 'break
	  (while src-list ;; cannot use dolist
	    (setq src (car src-list))
	    (setq src-list (cdr src-list))
	    (mew-imap2-set-src pnm src)
	    (mew-imap2-set-src-list pnm src-list)
	    (setq msgs (mew-folder-messages (mew-case-folder case src)))
	    (when msgs
	      (mew-imap2-set-ttl pnm (length msgs))
	      (mew-imap2-set-cnt pnm 0)
	      (cond
	       ((eq directive 'cp-r)
		(setq dst (mew-imap-local-to-imap case src))
		(unless (assoc dst (mew-imap-folder-alist case))
		  (mew-imap-folder-insert case dst))
		(setq mailbox (mew-imap-expand-mailbox case dst)))
	       ((eq directive 'fcc)
		(setq mailbox (mew-imap-fcc-to-mailbox case src))))
	      (setq mailbox (mew-imap-utf-7-encode-string mailbox))
	      (mew-imap2-set-mailbox pnm mailbox)
	      (throw 'break nil))))))
    (if msgs
	(progn
	  (setq msg (car msgs))
	  (setq msgs (cdr msgs))
	  (cond
	   ((or (eq directive 'copy) (eq directive 'move))
	    (setq src-file msg))
	   (t
	    (setq src-file (mew-expand-msg (mew-case-folder case src) msg))))
	  (mew-frwlet mew-cs-text-for-read mew-cs-dummy
	    (set-buffer (mew-find-file-noselect src-file)))
	  (mew-set-buffer-multibyte nil)
	  (mew-info-clean-up pnm mew-imap2-info-list-clean-length)
	  (mew-imap2-set-messages pnm msgs)
	  (set-process-buffer pro (current-buffer))
	  (mew-imap2-set-status pnm "append")
	  (mew-imap2-command-append pro pnm))
      (mew-imap2-set-status pnm "logout")
      (mew-imap2-command-logout pro pnm))))

(defun mew-imap2-command-append (pro pnm)
  (let ((mailbox (mew-imap2-get-mailbox pnm))
	(cnt (mew-imap2-get-cnt pnm))
	(ttl (mew-imap2-get-ttl pnm))
	(mbx (mew-imap2-get-mailbox pnm))
	(literal+ (mew-imap2-get-literal+ pnm))
	bytes)
    (setq cnt (1+ cnt))
    (mew-imap2-set-cnt pnm cnt)
    (message "Copying to %s %d/%d (%02d%%)" mbx cnt ttl (/ (* cnt 100) ttl))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (if (mew-eol-fix-for-write)
	(setq bytes (- (point-max) (point-min)))
      (setq bytes (- (point-max) (point-min)))
      (setq bytes (+ bytes (count-lines (point-min) (point-max)))))
    (set-buffer-modified-p nil)
    (if (not literal+)
	(mew-imap2-process-send-string
	 pro pnm "APPEND \"%s\" {%d}" mailbox bytes)
      (mew-imap2-process-send-string
       pro pnm "APPEND \"%s\" {%d+}" mailbox bytes)
      (mew-imap2-set-status pnm "post-append")
      (mew-imap2-command-post-append pro pnm))))

(defun mew-imap2-command-post-append (pro pnm)
  (process-send-region pro (point-min) (point-max))
  (process-send-string pro mew-cs-eol)) ;; APPEND command ends

(defun mew-imap2-command-create (pro pnm)
  (let ((mailbox (mew-imap2-get-mailbox pnm))
	(cnt (mew-imap2-get-cnt pnm)))
    (setq cnt (1- cnt))
    (mew-imap2-set-cnt pnm cnt)
    (mew-imap2-process-send-string pro pnm "CREATE \"%s\"" mailbox)))

(defun mew-imap2-command-wmbx (pro pnm)
  (mew-imap2-set-error pnm "mailbox is wrong!")
  (mew-imap2-set-status pnm "logout")
  (mew-imap2-command-logout pro pnm))

(defun mew-imap2-command-done (pro pnm)
  (let ((buf (process-buffer pro)))
    (set-process-buffer pro nil)
    (mew-remove-buffer buf)
    (mew-imap2-set-status pro "next")
    (mew-imap2-command-next pro pnm)))

(defun mew-imap2-command-logout (pro pnm)
  (mew-imap2-set-done pnm t)
  (mew-imap2-process-send-string pro pnm "LOGOUT"))

(defun mew-imap2-command-logout2 (pro pnm)
  ;; error is set
  (mew-imap2-set-done pnm t)
  (when (and (processp pro) (eq (process-status pro) 'open))
    (mew-imap2-set-status pnm "logout")
    (mew-imap2-process-send-string pro pnm "LOGOUT")))

(defun mew-imap2-command-noop (pro pnm)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AUTH
;;;

(defvar mew-imap2-auth-alist
  '(("CRAM-MD5" mew-imap2-command-auth-cram-md5)
    ("LOGIN"    mew-imap2-command-auth-login)))

(defun mew-imap2-auth-get-func (auth)
  (nth 1 (mew-assoc-case-equal auth mew-imap2-auth-alist 0)))

(defun mew-imap2-command-auth-cram-md5 (pro pnm)
  (mew-imap2-process-send-string pro pnm "AUTHENTICATE CRAM-MD5")
  (mew-imap2-set-status pnm "auth-cram-md5"))

(defun mew-imap2-command-pwd-cram-md5 (pro pnm)
  (let ((user (mew-imap2-get-user pnm))
        (prompt (format "IMAP CRAM-MD5 password (%s): "
                        (mew-imap2-get-account pnm)))
	(aux (mew-imap2-get-aux pnm))
	challenge passwd cram-md5)
    (if (string-match " \\([a-zA-Z0-9+/]+=*\\)" aux)
	(setq challenge (mew-match-string 1 aux)))
    (setq passwd (mew-imap2-input-passwd prompt pnm))
    (setq cram-md5 (mew-cram-md5 user passwd challenge))
    (mew-imap2-process-send-string2 pro cram-md5)))

(defun mew-imap2-command-auth-login (pro pnm)
  (mew-imap2-process-send-string pro pnm "AUTHENTICATE LOGIN")
  (mew-imap2-set-status pnm "auth-login"))

(defun mew-imap2-command-user-login (pro pnm)
  (let* ((user (mew-imap2-get-user pnm))
	 (euser (mew-base64-encode-string user)))
    (mew-imap2-process-send-string2 pro euser)))

(defun mew-imap2-command-pwd-login (pro pnm)
  (let* ((prompt (format "IMAP LOGIN password (%s): "
                        (mew-imap2-get-account pnm)))
         (passwd (mew-imap2-input-passwd prompt pnm))
         (epasswd (mew-base64-encode-string passwd)))
    (mew-imap2-process-send-string2 pro epasswd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defconst mew-imap2-info-prefix "mew-imap2-info-")

(defun mew-imap2-info-name (case)
  (let ((server (mew-imap-server case))
	(port (mew-*-to-string (mew-imap-port case)))
	(sshsrv (mew-imap-ssh-server case))
	(name mew-imap2-info-prefix))
    (setq name (concat name server))
    (unless (mew-port-equal port mew-imap-port)
      (setq name (concat name ":" port)))
    (if sshsrv
	(concat name "%" sshsrv)
      name)))

(defun mew-imap2-buffer-name (pnm)
  (concat mew-buffer-prefix pnm))

(defun mew-imap2-process-send-string (pro pnm &rest args)
  (let ((str (apply 'format args))
	(tag (mew-imap2-tag)))
    (mew-imap2-debug "=SEND=" (concat tag " " str))
    (mew-imap2-set-tag pnm tag)
    (if (and (processp pro) (eq (process-status pro) 'open))
	(process-send-string pro (concat tag " " str mew-cs-eol))
      (message "IMAP time out"))))

(defun mew-imap2-process-send-string2 (pro &rest args)
  (let ((str (apply 'format args)))
    (if (and (processp pro) (eq (process-status pro) 'open))
	(process-send-string pro (concat str mew-cs-eol))
      (message "IMAP time out"))))

(defun mew-imap2-tag ()
  (format "%s%04d" (mew-random-string 4 nil) (% (mew-random) 10000)))

(defun mew-imap2-passtag (pnm)
  (let ((server (mew-imap2-get-server pnm))
	(port (mew-imap2-get-port pnm))
	(user (mew-imap2-get-user pnm)))
    (concat user "@" server ":" port)))

(defun mew-imap2-passtag2 (case)
  (let ((server (mew-imap-server case))
	(port (mew-imap-port case))
	(user (mew-imap-user case)))
    (concat user "@" server ":" port)))

(defun mew-imap2-input-passwd (prompt pnm)
  (let* ((tag (mew-imap2-passtag pnm))
	 (pro (mew-imap2-get-process pnm))
	 (pass (mew-input-passwd prompt tag)))
    (unless (and (processp pro) (eq (process-status pro) 'open))
      (mew-passwd-set-passwd tag nil))
    pass))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Namespace
;;;


(defvar mew-folder-imap-fcc-prefix "%.fcc")

(defun mew-folder-imap-to-fcc (case folder)
  (concat
   (file-name-as-directory
    (mew-case-folder case mew-folder-imap-fcc-prefix))
   (mew-folder-string folder)))

(defun mew-folder-imap-fcc-dir (case)
  (mew-expand-folder (mew-case-folder case mew-folder-imap-fcc-prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening IMAP
;;;

(defun mew-imap2-open (pnm server port)
  (let ((sprt (mew-*-to-port port))
	pro tm)
    (condition-case emsg
	(progn
	  (setq tm (run-at-time mew-imap-timeout-time nil 'mew-imap2-timeout))
	  (message "Connecting to the IMAP server...")
	  (setq pro (open-network-stream pnm nil server sprt))
	  (mew-process-silent-exit pro)
	  (mew-set-process-cs pro mew-cs-text-for-net mew-cs-text-for-net)
	  (message "Connecting to the IMAP server...done"))
      (quit
       (setq pro nil)
       (message "Cannot connect to the IMAP server"))
      (error
       (setq pro nil)
       (message "%s, %s" (nth 1 emsg) (nth 2 emsg))))
    (if tm (cancel-timer tm))
    pro))

(defun mew-imap2-timeout ()
  (signal 'quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-imap2-copy-message (case directive src-list &optional dst)
  (let* ((server (mew-imap-server case))
         (user (mew-imap-user case))
	 (port (mew-*-to-string (mew-imap-port case)))
	 (pnm (mew-imap2-info-name case))
	 (sshsrv (mew-imap-ssh-server case))
	 (sslp (mew-imap-ssl case))
	 (sslport (mew-imap-ssl-port case))
	 (proxysrv (mew-imap-proxy-server case))
	 (proxyport (mew-imap-proxy-port case))
	 process sshname sshpro sslname sslpro lport tls)
    (cond
     (sshsrv
      (setq sshpro (mew-open-ssh-stream case server port sshsrv))
      (when sshpro
	(setq sshname (process-name sshpro))
	(setq lport (mew-ssh-pnm-to-lport sshname))
	(when lport
	  (setq process (mew-imap2-open pnm "localhost" lport)))))
     (sslp
      (if (mew-port-equal port sslport) (setq tls mew-tls-imap))
      (setq sslpro (mew-open-ssl-stream case server sslport tls))
      (when sslpro
	(setq sslname (process-name sslpro))
	(setq lport (mew-ssl-pnm-to-lport sslname))
	(when lport
	  (setq process (mew-imap2-open pnm mew-ssl-localhost lport)))))
     (proxysrv
      (setq process (mew-imap2-open pnm proxysrv proxyport)))
     (t
      (setq process (mew-imap2-open pnm server port))))
    (if (null process)
	(cond
	 ((and sshsrv (null sshpro))
	  (message "Cannot create to the SSH connection"))
	 ((and sslp (null sslpro))
	  (message "Cannot create to the SSL/TLS connection"))
	 (t
	  (message "Cannot connect to the IMAP server")))
      (mew-info-clean-up pnm)
      (mew-imap2-set-case pnm case)
      (mew-imap2-set-directive pnm directive)
      (mew-imap2-set-server pnm server)
      (mew-imap2-set-port pnm port)
      (mew-imap2-set-process pnm process)
      (mew-imap2-set-ssh-server pnm sshsrv)
      (mew-imap2-set-ssh-process pnm sshpro)
      (mew-imap2-set-ssl-process pnm sslpro)
      (mew-imap2-set-user pnm user)
      (mew-imap2-set-account pnm (format "%s@%s" user server))
      (mew-imap2-set-auth pnm (mew-imap-auth case))
      (mew-imap2-set-auth-list pnm (mew-imap-auth-list case))
      (mew-imap2-set-status pnm "greeting")
      (mew-imap2-set-src-list pnm src-list)
      (mew-imap2-set-src-list-orig pnm src-list)
      (mew-imap2-set-mailbox pnm dst)
      ;;
      (set-process-buffer process nil)
      (set-process-sentinel process 'mew-imap2-sentinel)
      (set-process-filter process 'mew-imap2-filter)
      (message "Copying in background..."))))

(defun mew-summary-from-local-to-imap ()
  "Copy messages in local folders under specified folder prefix
to a IMAP server decided by specified case. This may take very long
time (even several days) if there are many local messages."
  (interactive)
  (let* ((prefix (mew-input-local-folder mew-folder-local))
	 (case (mew-input-case mew-case-default "IMAP"))
	 (alist (mew-local-folder-alist))
	 regex src-list fld)
    (unless prefix (setq prefix mew-folder-local))
    (setq regex (concat "^" (regexp-quote prefix)))
    (dolist (ent alist)
      (setq fld (car ent))
      (if (string-match regex fld)
	  (setq src-list (cons fld src-list))))
    (setq src-list (nreverse src-list))
    (mew-imap2-copy-message case 'cp-r src-list)))

(defun mew-imap2-fcc (case)
  (let* ((prefix mew-folder-imap-fcc-prefix)
	 (fcc-dir (mew-folder-imap-fcc-dir case))
	 (fcc-dirs (mew-dir-list fcc-dir))
	 (src-list (mew-local-folder-make-alist fcc-dirs prefix 'make-list)))
    ;; src-list
    (mew-imap2-copy-message case 'fcc src-list)))

(defun mew-summary-imap-copy ()
  "Copy a message to the server side of IMAP."
  (interactive)
  (mew-summary-not-in-draft
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (fld/msg (mew-expand-msg fld msg))
	  (src-list (list fld/msg))
	  (case:dst (mew-input-folder mew-case mew-imap-inbox-folder))
	  (case (mew-case:folder-case case:dst))
	  (dst (mew-case:folder-folder case:dst)))
     (when (mew-folder-imapp dst)
       (mew-imap2-copy-message case 'copy src-list dst)))))

(defun mew-summary-mark-imap-copy (&optional arg)
  "Copy messages marked with '*' to the server side of IMAP.
If called with '\\[universal-argument]', only messages marked
with '*' in the region are handled."
  (interactive "P")
  (mew-summary-not-in-draft
   (let* ((fld (mew-summary-folder-name))
	  (case:dst (mew-input-folder mew-case mew-imap-inbox-folder))
	  (case (mew-case:folder-case case:dst))
	  (dst (mew-case:folder-folder case:dst))
	  msgs beg end region src-list)
     (if (mew-mark-active-p) (setq arg t))
     (cond
      (arg
       (setq region (mew-summary-get-region))
       (setq beg (car region))
       (setq end (cdr region)))
      (t
       (setq beg (point-min))
       (setq end (point-max))))
     (setq msgs (mew-summary-mark-collect mew-mark-review beg end))
     (if (null msgs)
	 (message "No mark")
       (when (mew-folder-imapp dst)
	 (setq src-list (mapcar (lambda (x) (mew-expand-msg fld x)) msgs))
	 (mew-imap2-copy-message case 'copy src-list dst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-imap2-debug (label string)
  (when (mew-debug 'net)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-imap2-filter (process string)
  (let* ((pnm (process-name process))
	 (status (mew-imap2-get-status pnm))
	 (tag (mew-imap2-get-tag pnm))
	 (eos (format "^%s \\(OK\\|NO\\|BAD\\)\\(.*\\)" tag))
	 (str (concat (mew-imap2-get-string pnm) string))
	 (buf (process-buffer process))
	 aux stay next func code resp)
    (save-excursion
      (mew-imap2-debug (upcase status) string)
      (if (and buf (get-buffer buf)) (set-buffer buf))
      (while (string-match "^\\*[^\n]*\n" str)
	(setq aux (substring str 0 (match-end 0)))
	(setq str (substring str (match-end 0)))
	(cond
	 ((string= status "greeting")
	  (setq next (mew-imap2-fsm-next "greeting" "OK")))
	 ((string= status "capability")
	  (mew-imap2-set-aux pnm (concat (mew-imap2-get-aux pnm) aux)))))
      (mew-imap2-set-string pnm str)
      (cond
       (next
	nil)
       ((string-match "^\\+" str)
	(mew-imap2-set-aux pnm str)
	(setq next (mew-imap2-fsm-next status "OK")))
       ((string-match eos str)
	(mew-imap2-set-tag pnm nil)
	(setq code (mew-match-string 1 str))
	(setq resp (mew-match-string 2 str))
	(setq next (mew-imap2-fsm-next status code))
	(unless next (setq next (mew-imap2-fsm-next status resp))))
       (t
	(setq stay t)))
      (unless stay
	(unless next (setq next "logout"))
	(mew-imap2-set-status pnm next)
	(setq func (intern-soft (concat "mew-imap2-command-" next)))
	(if (fboundp func)
	    (funcall func process pnm)
	  (error "No function called %s" (symbol-name func)))
	(mew-imap2-set-string pnm nil)))))

(defun mew-imap2-sentinel (process event)
  (let* ((pnm (process-name process))
	 (directive (mew-imap2-get-directive pnm))
	 (case (mew-imap2-get-case pnm))
	 (done (mew-imap2-get-done pnm))
	 (error (mew-imap2-get-error pnm))
	 (sshpro (mew-imap2-get-ssh-process pnm))
	 (sslpro (mew-imap2-get-ssl-process pnm))
	 (srcflds (mew-imap2-get-src-list-orig pnm)))
    (save-excursion
      (mew-imap2-debug "IMAP SENTINEL" event)
      (cond
       (error
	(message "%s" error))
       (done
	(cond
	 ((eq directive 'fcc)
	  (dolist (srcfld srcflds)
	    (mew-imap2-copy-fcc-cache case srcfld))
	  (mew-delete-directory-recursively
	   (mew-folder-imap-fcc-dir case)))
	 ((eq directive 'move)
	  (let ((file (car (mew-imap2-get-src-list-orig pnm))))
	    (if (file-exists-p file) (delete-file file)))))
	(message "Copying in background...done"))
       (t
	(message "IMAP connection is lost")))
      (mew-info-clean-up pnm)
      (if (and (processp sshpro) (not mew-ssh-keep-connection))
	  (process-send-string sshpro "exit\n"))
      (if (and (processp sslpro) (not mew-ssl-keep-connection))
	  (delete-process sslpro))
      (run-hooks 'mew-imap2-sentinel-hook))))

(defun mew-imap2-copy-fcc-cache (case srcfld)
  (let* ((case:srcfld (mew-case-folder case srcfld))
	 (srcs (mew-folder-messages case:srcfld))
	 (prefix (file-name-as-directory mew-folder-imap-fcc-prefix))
	 (regex (concat "^" (regexp-quote prefix)))
	 dst dstfld case:dstfld i)
    (string-match regex srcfld)
    (setq dstfld (concat mew-folder-imap (substring srcfld (match-end 0))))
    (setq case:dstfld (mew-case-folder case dstfld))
    (mew-local-folder-check case:dstfld)
    (setq i (string-to-number (mew-folder-new-message case:dstfld 'num 'cache)))
    ;; to make code simple, we just copy the file.
    (dolist (src srcs)
      (setq dst (mew-i2s i 'cache))
      (copy-file (mew-expand-msg case:srcfld src)
		 (mew-expand-new-msg case:dstfld dst))
      (setq i (1+ i)))
    (mew-touch-folder case:dstfld)))

(provide 'mew-imap2)

;;; Copyright Notice:

;; Copyright (C) 2003-2011 Mew developing team.
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

;;; mew-imap2.el ends here
