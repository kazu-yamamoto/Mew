;;; mew-smtp.el

;; Author:  Mew developing team
;; Created: Dec  3, 1999

;;; Code:

(require 'mew)
(autoload 'puny-encode-domain "puny")
(when (and (fboundp 'gnutls-available-p)
	   (gnutls-available-p))
  (require 'gnutls)
  (require 'nsm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SMTP info
;;;

(defvar mew-smtp-info-list
  '(;; parameters to be saved
    "raw-header" "recipients" "orig-recipients"
    "bcc" "fcc" "msgid" "logtime"
    "case" ;; save for re-edit
    ;; parameters used internally
    "server" "port" "ssh-server"
    "user" "auth-user" "auth-list"
    "helo-domain"
    "status" "process" "ssh-process" "ssl-process" "ssl-p"
    "qfld" "messages"
    ;; parameters used internally and should be initialized
    "string" "error" "auth-selected" "timer" "cont" "from" "sender"
    "done" "imapp" "capa" "fallback"))

(mew-info-defun "mew-smtp-" mew-smtp-info-list)

(defvar mew-smtp-info-list-save-length 8)
(defvar mew-smtp-info-list-clean-length 21)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FSM
;;;

(defvar mew-smtp-fsm
  '(("greeting"      ("220" . "ehlo"))
    ("ehlo"          ("250" . "post-ehlo") (t . "helo"))
    ;;
    ("auth"          ("250" . "next"))
    ("auth-cram-md5" ("334" . "pwd-cram-md5") (t . "wpwd"))
    ("pwd-cram-md5"  ("235" . "next") (t . "wpwd"))
    ("auth-login"    ("334" . "user-login") (t . "wpwd"))
    ("user-login"    ("334" . "pwd-login") (t . "wpwd"))
    ("pwd-login"     ("235" . "next") (t . "wpwd"))
    ("auth-plain"    ("235" . "next") (t . "wpwd"))
    ;; This is checked only for Gmail https://developers.google.com/gmail/imap/xoauth2-protocol
    ;; XXX: MS Exchange Returns 334 like CRAM-MD5?
    ;;  https://docs.microsoft.com/en-us/exchange/client-developer/legacy-protocols/how-to-authenticate-an-imap-pop-smtp-application-by-using-oauth
    ("auth-xoauth2"  ("235" . "next") (t . "wpwd"))
;; See blow
;;    ("auth-plain"    ("334" . "pwd-plain") (t . "wpwd"))
;;    ("pwd-plain"     ("235" . "next") (t . "wpwd"))
    ;;
    ("helo"          ("250" . "next"))
    ("mail-from"     ("250" . "rcpt-to"))
    ("rcpt-to"       ("250" . "data"))
    ("data"          ("354" . "content"))
    ("content"       ("250" . "done"))
    ("quit"          (t     . "noop"))))

(defun mew-smtp-fsm-by-status (status)
  (assoc status mew-smtp-fsm))

(defun mew-smtp-fsm-next (status code)
  (cdr (mew-assoc-match2 code (cdr (mew-smtp-fsm-by-status status)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filters
;;;

(defun mew-smtp-command-helo (pro pnm)
  (let ((helo-domain (mew-smtp-get-helo-domain pnm)))
    (mew-smtp-process-send-string pro "HELO %s" helo-domain)))

(defun mew-smtp-command-ehlo (pro pnm)
  (let ((helo-domain (mew-smtp-get-helo-domain pnm)))
    (mew-smtp-process-send-string pro "EHLO %s" helo-domain)))

(defun mew-smtp-command-post-ehlo (pro pnm)
  (let ((str (mew-smtp-get-string pnm)) (start 0) capa)
    (catch 'loop
      (while (string-match "^250\\([- ]?\\)\\(.*\\)$" str start)
	(setq capa (cons (mew-split (match-string 2 str) 32) capa))
	(if (string= (match-string 1 str) " ")
	    (throw 'loop nil))
	(setq start (match-end 0))))
    (mew-smtp-set-capa pnm (nreverse capa))
    (mew-smtp-set-status pnm "auth")
    (mew-smtp-command-auth pro pnm)))

(defun mew-smtp-command-auth (pro pnm)
  (let* ((case (mew-smtp-get-case pnm))
	 (use-smtp-auth (mew-use-smtp-auth case))
	 (capa (mew-smtp-get-capa pnm))
	 (auths (assoc "AUTH" capa))
	 (auth-list (mew-smtp-get-auth-list pnm))
	 auth func)
    (cond
     ((and use-smtp-auth auths)
      (if (and (setq auth (mew-auth-select2 auths auth-list))
	       (setq func (mew-smtp-auth-get-func auth))
	       (fboundp func))
	  (progn
	    (mew-smtp-set-auth-selected pnm auth)
	    (funcall func pro pnm))
	(mew-smtp-debug "<AUTH>" "No preferred SMTP AUTH.\n")
	(mew-smtp-command-wpwd pro pnm)))
     (t
      (mew-smtp-set-status pnm "next")
      (mew-smtp-command-next pro pnm)))))

(defun mew-smtp-command-wpwd (pro pnm)
  (let ((auth (mew-smtp-get-auth-selected pnm)))
    (mew-passwd-set-passwd (mew-smtp-passtag pnm) nil)
    (if auth
	(mew-smtp-set-error pnm (format "SMTP %s password is wrong!" auth))
      (mew-smtp-set-error pnm "No SMTP AUTH available!"))
    (mew-smtp-command-quit2 pro pnm)))

(defun mew-smtp-command-next (pro pnm)
  (let ((msgs (mew-smtp-get-messages pnm))
	(qfld (mew-smtp-get-qfld pnm))
	(case (mew-smtp-get-case pnm))
	(buf (process-buffer pro))
	msg)
    (if msgs
	(progn
	  (setq msg (car msgs))
	  (setq msgs (cdr msgs))
	  (mew-queue-insert-file pnm mew-smtp-info-list-save-length qfld msg)
	  (mew-set-buffer-multibyte nil)
	  (mew-info-clean-up pnm mew-smtp-info-list-clean-length)
	  (mew-smtp-set-case pnm case) ;; override
	  (mew-smtp-set-messages pnm msgs)
	  (set-process-buffer pro (current-buffer))
	  (mew-remove-buffer buf)
	  (mew-smtp-set-status pnm "mail-from")
	  (mew-smtp-command-mail-from pro pnm))
      (mew-smtp-set-status pnm "quit")
      (mew-smtp-command-quit pro pnm))))

(defun mew-smtp-command-mail-from (pro pnm)
  (widen)
  (clear-visited-file-modtime)
  ;;
  (let* ((case (mew-smtp-get-case pnm))
	 (resentp (mew-header-existp mew-resent-from:))
	 (sender: (if resentp mew-resent-sender: mew-sender:))
	 (from: (if resentp mew-resent-from: mew-from:))
	 (from (mew-header-get-value from:))
	 (froms (mew-addrstr-parse-address-list from))
	 (nfrom (length froms))
	 (mail-from (mew-smtp-mail-from case)))
    (mew-smtp-set-from pnm from) ;; for Bcc:
    (unless mail-from
      ;; Which address is suitable for MAIL FROM if multiple?
      (setq mail-from (car froms)))
    (unless (mew-header-existp sender:)
      (if (= nfrom 1)
	  (if (and mew-use-sender (not (string= mail-from (car froms))))
	      (mew-smtp-set-sender pnm (cons sender: mail-from)))
	(mew-smtp-set-sender pnm (cons sender: mail-from))))
    ;;
    (mew-smtp-process-send-string pro "MAIL FROM:<%s>" mail-from)))

(defun mew-smtp-command-rcpt-to (pro pnm)
  (let* ((recipients (mew-smtp-get-recipients pnm))
	 (recipient (car recipients)))
    (setq recipients (cdr recipients))
    (mew-smtp-set-recipients pnm recipients)
    (if recipients (mew-smtp-set-status pnm "mail-from"))
    (mew-smtp-process-send-string pro "RCPT TO:<%s>" recipient)))

(defun mew-smtp-command-data (pro pnm)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (mew-dot-insert)
  (mew-eol-fix-for-write)
  (set-buffer-modified-p nil)
  (mew-smtp-set-cont pnm (point-min))
  (mew-smtp-set-timer pnm nil)
  (mew-smtp-process-send-string pro "DATA"))

(defun mew-smtp-command-content (pro pnm)
  (save-excursion
    (let ((cont (mew-smtp-get-cont pnm))
	  (sender (mew-smtp-get-sender pnm))
          (inc 1000) (i 0) (N 10))
      (set-buffer (process-buffer pro))
      ;; Sender:
      (when sender
	(mew-smtp-process-send-string pro "%s %s" (car sender) (cdr sender))
	(mew-smtp-set-sender pnm nil))
      ;;
      (while (and (< cont (point-max)) (not (input-pending-p)) (< i N))
        (let ((next (min (point-max) (+ cont inc))))
	  (if (and (processp pro) (eq (process-status pro) 'open))
	      (process-send-region pro cont next))
          (setq cont next)
          (setq i (1+ i))))
      (mew-smtp-set-cont pnm cont)
      (if (< cont (point-max))
          (let ((timer
                 (if (input-pending-p)
                     (run-with-idle-timer
		      0.01 nil 'mew-smtp-command-content pro pnm)
                   (run-at-time 0.05 nil 'mew-smtp-command-content pro pnm))))
            (mew-smtp-set-timer pnm timer))
        (mew-smtp-set-cont pnm nil)
        (mew-smtp-set-timer pnm nil)
        (mew-smtp-process-send-string pro ".")))))

(defun mew-smtp-command-done (pro pnm)
  (let ((fcc (mew-smtp-get-fcc pnm))
	(case (mew-smtp-get-case pnm))
	(back (mew-queue-backup (buffer-file-name) mew-queue-info-suffix))
	imapp)
    ;; mew-folder-new-message may be slow if the folder contains
    ;; a lot of messages. So, let's Fcc in background.
    (setq imapp (mew-net-fcc-message case fcc back))
    (mew-smtp-set-imapp pnm imapp)
    (mew-smtp-log pnm)
    (if (mew-smtp-get-bcc pnm)
	(mew-smtp-bcc pro pnm back)
      (mew-smtp-set-status pro "next")
      (mew-smtp-command-next pro pnm))))

(defun mew-smtp-command-quit (pro pnm)
  (mew-smtp-set-done pnm t)
  (mew-smtp-process-send-string pro "QUIT"))

(defun mew-smtp-command-quit2 (pro pnm)
  ;; error is set
  (mew-smtp-set-done pnm t)
  (when (and (processp pro) (eq (process-status pro) 'open))
    (mew-smtp-set-status pnm "quit")
    (mew-smtp-process-send-string pro "QUIT")))

(defun mew-smtp-command-noop (_pro _pnm)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SMTP AUTH (RFC 2554)
;;;

(defvar mew-smtp-auth-alist
  '(("CRAM-MD5" mew-smtp-command-auth-cram-md5)  ;; RFC 2195
    ("PLAIN"    mew-smtp-command-auth-plain)     ;; RFC 2595
    ("LOGIN"    mew-smtp-command-auth-login)     ;; No spec
    ("XOAUTH2"  mew-smtp-command-auth-xoauth2)))

(defun mew-smtp-auth-get-func (auth)
  (nth 1 (mew-assoc-case-equal auth mew-smtp-auth-alist 0)))

;; CRAM MD5

(defun mew-smtp-command-auth-cram-md5 (pro pnm)
  (mew-smtp-process-send-string pro "AUTH CRAM-MD5")
  (mew-smtp-set-status pnm "auth-cram-md5"))

(defun mew-smtp-command-pwd-cram-md5 (pro pnm)
  (let* ((str (mew-smtp-get-string pnm))
	 (user (mew-smtp-get-auth-user pnm))
	 (prompt (format "SMTP CRAM-MD5 password (%s): " user))
	 challenge passwd cram-md5)
    (if (string-match " \\([a-zA-Z0-9+/]+=*\\)" str) ;; xxx
	(setq challenge (match-string 1 str)))
    (setq passwd (mew-smtp-input-passwd prompt pnm))
    (setq cram-md5 (mew-cram-md5 user passwd challenge))
    (mew-smtp-process-send-string pro cram-md5)))

;; LOGIN

(defun mew-smtp-command-auth-login (pro pnm)
  (mew-smtp-process-send-string pro "AUTH LOGIN")
  (mew-smtp-set-status pnm "auth-login"))

(defun mew-smtp-command-user-login (pro pnm)
  (let* ((user (mew-smtp-get-auth-user pnm))
         (euser (mew-base64-encode-string user)))
     (mew-smtp-process-send-string pro "%s" euser)))

(defun mew-smtp-command-pwd-login (pro pnm)
  (let* ((user (mew-smtp-get-auth-user pnm))
	 (prompt (format "SMTP LOGIN password (%s): " user))
         (passwd (mew-smtp-input-passwd prompt pnm))
	 (epasswd (mew-base64-encode-string passwd)))
    (mew-smtp-process-send-string pro epasswd)))

;; There are two variations for SMTP AUTH PLAIN.
;; 1) AUTH PLAIN then id+password
;; 2) AUTH PLAIN id+password
;;
;; 1) is popular however a server of an ISP supports 2) only, sigh.

(defun mew-smtp-command-auth-plain (pro pnm)
  (let* ((case (mew-smtp-get-case pnm))
	 (user (mew-smtp-get-auth-user pnm))
	 (authorize-id (mew-smtp-auth-plain-authorize-id case))
	 (prompt (format "SMTP PLAIN password (%s): " user))
	 (passwd (mew-smtp-input-passwd prompt pnm))
	 (plain (mew-base64-encode-string
		 (if authorize-id
		     (format "%s\0%s\0%s" user user passwd)
		   (format "\0%s\0%s" user passwd)))))
    (mew-smtp-process-send-string pro "AUTH PLAIN %s" plain)
    (mew-smtp-set-status pnm "auth-plain")))

(defun mew-smtp-command-auth-xoauth2 (pro pnm)
  (let* ((user (mew-smtp-get-auth-user pnm))
         (token (mew-auth-oauth2-token-access-token))
         (auth-string (mew-auth-xoauth2-auth-string user token)))
    (mew-smtp-process-send-string pro "AUTH XOAUTH2 %s" auth-string)
    (mew-smtp-set-status pnm "auth-xoauth2")))

;; (defun mew-smtp-command-auth-plain (pro pnm)
;;   (mew-smtp-process-send-string pro "AUTH PLAIN")
;;   (mew-smtp-set-status pnm "auth-plain"))

;; (defun mew-smtp-command-pwd-plain (pro pnm)
;;   (let* ((case (mew-smtp-get-case pnm))
;;	 (user (mew-smtp-get-auth-user pnm))
;;	 (authorize-id (mew-smtp-auth-plain-authorize-id case))
;;	 (prompt (format "SMTP PLAIN password (%s): " user))
;;	 (passwd (mew-smtp-input-passwd prompt pnm))
;;	 (plain (mew-base64-encode-string
;;		 (if authorize-id
;;		     (format "%s\0%s\0%s" user user passwd)
;;		   (format "\0%s\0%s" user passwd)))))
;;     (mew-smtp-process-send-string pro "%s" plain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defconst mew-smtp-info-prefix "mew-smtp-info-")

(defun mew-smtp-info-name (case &optional fallbacked)
  (let ((server (mew-smtp-server case))
	(port (mew-*-to-string (mew-smtp-port case)))
	(user (mew-smtp-user case))
	(sshsrv (mew-smtp-ssh-server case))
	(name mew-smtp-info-prefix))
    (if user
	(setq name (concat name user "@" server))
      (setq name (concat name server)))
    (when (and (not fallbacked)
	       mew-use-submission ;; xxx to be deleted
	       (fboundp 'make-network-process)
	       (string= port "smtp"))
      (setq port "submission"))
    (unless (mew-port-equal port mew-smtp-port)
      (setq name (concat name ":" port)))
    (if sshsrv
	(concat name "%" sshsrv)
      name)))

(defun mew-smtp-process-send-string (pro &rest args)
  (let ((str (apply 'format args)))
    (mew-smtp-debug "=SEND=" str)
    (if (and (processp pro) (eq (process-status pro) 'open))
	(process-send-string pro (concat str mew-cs-eol))
      (message "SMTP time out"))))

(defun mew-smtp-passtag (pnm)
  (concat (mew-smtp-get-auth-user pnm)
	  "@" (mew-smtp-get-server pnm)
	  ":" (mew-smtp-get-port pnm)))

(defun mew-smtp-input-passwd (prompt pnm)
  (let ((tag (mew-smtp-passtag pnm))
	(pro (mew-smtp-get-process pnm))
	pass)
    (setq pass (mew-input-passwd prompt tag))
    (unless (and (processp pro) (eq (process-status pro) 'open))
      (mew-passwd-set-passwd tag nil))
    pass))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening SMTP
;;;

(defvar mew-use-submission nil)

(defvar mew-smtp-submission-timeout 10)
(defun mew-smtp-submission-timeout (pro)
  (when (and (processp pro) (eq (process-status pro) 'connect))
    (mew-smtp-sentinel pro "time out - failed\n")))

(defvar mew-smtp-submission-family 'ipv4)

;;; XXX: (mew-open-network-stream) always returns a list
;;       and is also used for non-SMTP protocols.
;;; XXX: port must be resolved by using mew-serv-to-port
;;       because some service names are not in /etc/services.
;;       mew-serv-to-port uses mew-port-db.
(setq mew--advice-tls-parameters-plist nil)
(defun mew--advice-filter-args-gnutls-negotiate (&rest args)
  (nconc (car args) mew--advice-tls-parameters-plist))
;;;
;;; Functions to handle tunneling by an external program.  The
;;; external program must be capable of bidirectional communications
;;; via stdin and stdout and forward the data stream from/to the final
;;; destination.  TLS and STARTTLS will be handled by GnuTLS.
;;;
(defun mew--tun-type (tun-plist)
  (plist-get tun-plist :type))
(setq mew--advice-tun-command nil)
(defun mew--advice-tun-command (case host port tun-plist)
  (cond
   ((eq (mew--tun-type tun-plist) 'ssh)
    (format "%s -W %s:%s -oPort=%s %s"
	    (mew-ssh-prog case)
	    (plist-get tun-plist :host)
	    (plist-get tun-plist :port)
	    port host))
   (t nil)))
;; Override function for TLS connection over tunnel.
(defun mew--advice-override-open-gnutls-stream
    (name buffer host service &optional nowait)
  ;; Must be equivalent to
  ;; (open-gnutls-stream name buffer host service &optional nowait)
  (make-process args
		:command mew--advice-tun-command
		:coding 'utf-8-unix
		:connection-type 'pipe
		:noquery t
		:stderr nil)
  ;; Do gnutls-negotiate here
  )
;; Override function for PLAIN or STARTTLS connection over tunnel.
(defun mew--advice-override-make-network-process (&rest args)
  ;; PLAIN: make-network-process
  ;; STARTTLS: make-network-process -> gnutls-negotiate
  (make-process args
		:command mew--advice-tun-command
		:coding 'utf-8-unix
		:connection-type 'pipe
		:noquery t
		:stderr nil))
;;;
;;; XXX: This conditional can be removed safely.
(if (fboundp 'make-network-process)
    (defun mew-open-network-stream (name buf server port proto sslnp
					 starttlsp case &optional tun-plist)
      (let* ((tun-type (mew--tun-type tun-plist))
	     (mew--advice-tun-command (mew--advice-tun-command
				       case server port tun-plist))
	     (status-msg (format "Opening a %s connection %s%s%s..."
				(if sslnp "TLS" "TCP")
				(if sslnp "(GnuTLS" "")
				(if sslnp
				    (if starttlsp ", STARTTLS)" ")")
				  "")
				(if (eq tun-type 'ssh) " over SSH"
				  "")))
	     family nowait pro tlsparams)
	;; SMTP-specific
	(when (and (eq proto 'smtp) mew-inherit-submission)
	  (setq family mew-smtp-submission-family)
	  (setq nowait t))
	;; TLS does not work for Unix-domain socket for now.
	(when (and (not sslnp)
		   (stringp port) (string-match "^/" port))
	  (setq family 'local)
	  (setq server 'local))
	(cond
	 ;; Both GnuTLS and NSM are mandatory for 'native.
	 ((and sslnp (or (not (fboundp 'gnutls-available-p))
			 (not (gnutls-available-p))
			 (not (fboundp 'gnutls-boot-parameters))
			 (not (fboundp 'nsm-level))))
	  (setq pro
		(list nil
		      :error t
		      :status-msg
		      (concat status-msg
			      "FAILED (GnuTLS or NSM not available)"))))
	 ((and sslnp)
	  (let ((hostname (puny-encode-domain server))
		;; Note: on Emacs 26.3 and prior GnuTLS always uses
		;; the system-wide default path first even if
		;; trustfiles is specified.
		(trustfiles (mew-ssl-trustfiles case))
		(nsm-noninteractive nil)
		(network-security-level network-security-level))
	    (when (eq (mew-ssl-verify-level case) 0)
	      ;; Forcibly disable verification.
	      (setq network-security-level 'low))
	    (setq tlsparams
		  (cons 'gnutls-x509pki
			;; XXX: (gnutls-boot-parameters) returns
			;; :priority key instead of :priority-string
			;; while (gnutls-negotiate) accepts
			;; :priority-string.  To handle this odd
			;; mismatch, create :priority-string in the
			;; result of (gnutls-boot-parameters) here.
			(let ((boot-params
			       (gnutls-boot-parameters
				:type 'gnutls-x509pki
				:keylist (mew-ssl-client-keycert-list case)
				:trustfiles (mew-ssl-trustfiles case)
				:priority-string (mew-ssl-algorithm-priority case)
				:min-prime-bits mew-ssl-min-prime-bits
				;;
				;; mew-ssl-verify-error should be nil
				;; to defer verification to NSM.  Note
				;; that gnutls-verify-error overrides
				;; verify-error when it is nil.
				;; Setting gnutls-verify-error to t is
				;; also discouraged.
				;;
				:verify-error mew-ssl-verify-error
				:hostname hostname)))
			  (plist-put boot-params
				     :priority-string
				     (plist-get boot-params :priority)))))
	    ;; debug output: TLS params
	    (funcall (intern (concat "mew-" (symbol-name proto) "-debug"))
		     (format "TLS proto=%s, server=%s:%s, starttlsp=%s"
			     proto hostname port starttlsp)
		     (format "verify-level=%s, network-security-level=%s, nowait=%s, tlsparams=%s"
			     (mew-ssl-verify-level case) network-security-level
			     nowait
			     (apply #'concat (mapcar (lambda (a) (format "%s " a)) tlsparams))))
	    (let ((type (if starttlsp 'starttls 'tls)))
	      (with-temp-message status-msg
		;; XXX: (open-network-stream) does not pass tlsparams
		;; to (gnutls-negotiate) to start STARTTLS.  As a
		;; workaround, add an advice to forcibly append the
		;; parameters.  This should be fixed in
		;; (open-network-stream).
		(setq mew--advice-tls-parameters-plist (cdr tlsparams))
		(advice-add 'gnutls-negotiate
			    :filter-args #'mew--advice-filter-args-gnutls-negotiate)
		;;
		;; Override key functions when using a tunnel.
		(cond
		 ((and mew--advice-tun-command (eq type 'tls))
		  (advice-add 'open-gnutls-stream
			      :override #'mew--advice-override-open-gnutls-stream))
		 (mew--advice-tun-command
		  (advice-add 'make-network-process
			      :override #'mew--advice-override-make-network-process)))
		(setq pro (open-network-stream
			   name buf server port
			   :type type
			   :return-list t
			   :nowait nowait
			   :always-query-capabilities
			   (mew-starttls-get-param proto :always-query-capabilities nil)
			   :capability-command
			   (mew-starttls-get-param proto :capability-command t)
			   :end-of-capability
			   (mew-starttls-get-param proto :end-of-capability t)
			   :end-of-command
			   (mew-starttls-get-param proto :end-of-command t)
			   :success
			   (mew-starttls-get-param proto :success t)
			   :starttls-function
			   (mew-starttls-get-param proto :starttls-function nil)))
		(cond
		 ((and mew--advice-tun-command (eq type 'tls))
		  (advice-remove 'open-gnutls-stream
				 #'mew--advice-override-open-gnutls-stream))
		 (mew--advice-tun-command
		  (advice-remove 'make-network-process
				 #'mew--advice-override-make-network-process)))
		(advice-remove 'gnutls-negotiate
			       #'mew--advice-filter-args-gnutls-negotiate)
		;;
		;; When a validation error occurs, (car pro) will be nil.
		;;
		(let ((plainp (eq 'plain (plist-get (cdr pro) :type)))
		      (greeting (plist-get (cdr pro) :greeting))
		      (openp  (and (car pro)
				   (eq 'open (process-status (car pro)))))
		      ;; Falling back to a plain connection is allowed
		      ;; only when verify-level < 2.
		      (needtlsp (and starttlsp
				     (> (mew-ssl-verify-level case) 1))))
		  (cond
		   ((not openp)
		    (let ((msg (plist-get (cdr pro) :error)))
		      (setq pro (list nil
				      :error t
				      :status-msg
				      (concat status-msg "FAILED: " msg)))))
		   ((and plainp needtlsp)
		    (delete-process (car pro))
		    (setq pro (list nil
				    :error t
				    :status-msg
				    (concat status-msg "FAILED"))))
		   (t
		    (setq pro (list
			       (car pro)
			       :error nil))
		    (cond
		     ((eq proto 'pop)
		      (setq mew--gnutls-pop-greeting greeting))
		     ((eq proto 'imap)
		      (setq mew--gnutls-imap-greeting greeting))))))))))
	 (t
	  (with-temp-message status-msg
	    (let ((params (list :name name :buffer buf
				:service port :family family
				:nowait nowait))
		  ;; :host will be ignored when family is 'local.
		  (host (if (not (eq family 'local))
			    (list :host server))))
	      (setq pro (list
			 (apply #'make-network-process (nconc params host))
			 :greeting nil
			 :capabilities nil
			 :type 'plain
			 :error nil))))))
	(if (and (eq proto 'smtp) nowait)
	    (run-at-time mew-smtp-submission-timeout nil 'mew-smtp-submission-timeout pro))
	(when (plist-get (cdr pro) :error)
	  (message (plist-get (cdr pro) :status-msg)))
	pro))
  (defun mew-open-network-stream (name buf server port proto sslnp starttlsp case)
    (open-network-stream name buf server port :return-list t)))

(defun mew-smtp-open (pnm case server port starttlsp)
  (let ((sprt (mew-*-to-port port))
	(sslnp (mew-ssl-native-p (mew-smtp-ssl case)))
	pro tm)
    ;; xxx some OSes do not define "submission", sigh.
    (when (and (stringp sprt) (string= sprt "submission"))
      (setq sprt (mew-serv-to-port sprt)))
    (condition-case emsg
	(progn
	  (setq tm (run-at-time mew-smtp-timeout-time nil 'mew-smtp-timeout))
	  (message "Connecting to the SMTP server...")
	  (setq pro (mew-open-network-stream pnm nil server sprt
					     'smtp sslnp starttlsp case))
	  (when (and sslnp starttlsp)
	    (mew-smtp-debug "*GREETING*"
			    (plist-get (cdr pro) :greeting))
	    (mew-smtp-debug "*CAPABILITIES*"
			    (plist-get (cdr pro) :capabilities)))
	  (setq pro (car pro))
	  (when (not (processp pro)) (signal 'quit nil))
	  (mew-process-silent-exit pro)
	  (mew-set-process-cs pro mew-cs-text-for-net mew-cs-text-for-net)
	  (message "Connecting to the SMTP server...done"))
      (quit
       (setq pro nil)
       (message "Cannot connect to the SMTP server"))
      (error
       (setq pro nil)
       (message "%s, %s" (nth 1 emsg) (nth 2 emsg))))
    (if tm (cancel-timer tm))
    pro))

(defun mew-smtp-timeout ()
  ;; Do not timeout if the NSM query pane is active.
  (unless (get-buffer "*Network Security Manager*")
    (message "SMTP connection timed out (%d seconds)"
	     mew-smtp-timeout-time)
    (signal 'quit nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-smtp-send-message (case qfld msgs &optional fallbacked)
  (let ((server (mew-smtp-server case))
        (user (mew-smtp-user-only case))
	(port (mew-*-to-string (mew-smtp-port case)))
	(pnm (mew-smtp-info-name case fallbacked))
	(sshsrv (mew-smtp-ssh-server case))
	(sslp (mew-smtp-ssl case))
	(sslport (mew-*-to-string (mew-smtp-ssl-port case)))
	(sslnp (mew-ssl-native-p (mew-smtp-ssl case)))
	(starttlsp
	 (mew-ssl-starttls-p (mew-smtp-ssl case)
			     (mew-*-to-string (mew-smtp-port case))
			     (mew-smtp-ssl-port case)))
	mew-inherit-submission
	process sshname sshpro sslname sslpro lport tlsp tls fallback)
    (cond
     ((and (not sslnp) sslp starttlsp)
      (setq tlsp t)
      ;; let stunnel know that a wrapper protocol is SMTP
      (setq tls mew-tls-smtp)))
    ;; a fallback: "submission" -> "smtp"
    ;; mew-smtp-port is "smtp" and mew-use-submission is t on Emacs 22
    (when (and (or (not sslp) starttlsp tlsp)
	       (not fallbacked)
	       mew-use-submission
	       (fboundp 'make-network-process) ;; Emacs 22 or later
	       (mew-port-equal port "smtp"))
      (setq port "submission")
      (setq fallback t)
      (when (and sslp (not sslnp) (not tlsp))
	;; TLS uses stunnel. So, we should not use non-blocking connect.
	;; Timeout should be carried out by stunnel.
	(setq mew-inherit-submission t))
      (if (mew-port-equal sslport "smtp")
	  (setq sslport "submission")))
    (cond
     (sslnp
      (let ((serv (if starttlsp port sslport)))
	(setq process (mew-smtp-open pnm case server serv starttlsp))))
     (sshsrv
      (setq sshpro (mew-open-ssh-stream case server port sshsrv))
      (when sshpro
	(setq sshname (process-name sshpro))
	(setq lport (mew-ssh-pnm-to-lport sshname))
	(when lport
	  (setq process (mew-smtp-open pnm case "localhost" lport nil)))))
     (sslp
      (setq sslpro (mew-open-ssl-stream case server sslport tls))
      (when sslpro
	(setq sslname (process-name sslpro))
	(setq lport (mew-ssl-pnm-to-lport sslname))
	(when lport
	  (setq process (mew-smtp-open pnm case mew-ssl-localhost lport nil)))))
     (t
      (setq process (mew-smtp-open pnm case server port nil))))
    (if (null process)
	(cond
	 ((and sshsrv (null sshpro))
	  (message "Cannot create to the SSH connection"))
	 (sslnp
	  (message "Cannot open an SSL/TLS (GnuTLS) connection"))
	 ((and sslp (null sslpro))
	  (message "Cannot create to the SSL/TLS connection"))
	 (t
	  (if (and (not (eq server 'local))
	           (or (not sslp) tlsp)
		   (not fallbacked)
		   mew-use-submission
		   (fboundp 'make-network-process)
		   (mew-port-equal port "submission"))
	      (progn
		;; make-network-process with :nowait t sometime
		;; returns nil, why?
		(mew-smtp-send-message case qfld msgs t)
		(mew-info-clean-up pnm))
	    (message "Cannot connect to the SMTP server"))))
      (mew-info-clean-up pnm mew-smtp-info-list-clean-length)
      (mew-smtp-set-case pnm case)
      (mew-smtp-set-qfld pnm qfld)
      (mew-smtp-set-messages pnm msgs)
      (mew-smtp-set-server pnm server)
      (if sslp
	  (mew-smtp-set-port pnm sslport)
	(mew-smtp-set-port pnm port))
      (mew-smtp-set-process pnm process)
      (mew-smtp-set-ssh-server pnm sshsrv)
      (mew-smtp-set-ssh-process pnm sshpro)
      (mew-smtp-set-ssl-process pnm sslpro)
      (mew-smtp-set-ssl-p pnm sslp)
      (mew-smtp-set-helo-domain pnm (mew-smtp-helo-domain case))
      (mew-smtp-set-user pnm user)
      (mew-smtp-set-auth-user pnm (mew-smtp-user case))
      (mew-smtp-set-auth-list pnm (mew-smtp-auth-list case))
      (mew-smtp-set-status pnm "greeting")
      (mew-smtp-set-fallback pnm fallback)
      ;;
      (set-process-buffer process nil)
      (set-process-sentinel process 'mew-smtp-sentinel)
      (set-process-filter process 'mew-smtp-filter)
      (message "Sending in background...")
      ;;
      (when sslnp
	;; GnuTLS requires a client-initiated command after the
	;; session is established or upgraded to use TLS because
	;; no additional greeting from the server.
	(mew-smtp-set-status pnm "ehlo")
	(mew-smtp-command-ehlo process pnm))
      )))

(defun mew-smtp-flush-queue (case &optional qfld)
  (let (msgs)
    (unless qfld (setq qfld (mew-queue-folder case)))
    (if (mew-smtp-get-server (mew-smtp-info-name case)) ;; lock
	(message "%s is locked" qfld)
      (setq msgs (mew-folder-messages qfld))
      (when msgs
	(mew-summary-folder-cache-clean qfld)
	(run-hooks 'mew-smtp-flush-hook)
	(message "Flushing %s..." qfld)
	(mew-smtp-send-message case qfld msgs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-smtp-debug (label string)
  (when (mew-debug 'net)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-smtp-filter (process string)
  (let* ((pnm (process-name process))
	 (status (mew-smtp-get-status pnm))
	 (str (concat (mew-smtp-get-string pnm) string))
	 (buf (process-buffer process))
	 next func code)
    (save-excursion
      (mew-smtp-debug (upcase status) string)
      (if (and buf (get-buffer buf)) (set-buffer buf))
      ;; SMTP server's strings should be short enough.
      (mew-smtp-set-string pnm str)
      (cond
       ((and (string-match "\n$" str)
	     (string-match "^\\([1-5][0-7][0-9]\\) " str))
	(setq code (match-string 1 str))
	(setq next (mew-smtp-fsm-next status code))
	(cond
	 (next
	  (mew-smtp-set-status pnm next)
	  (setq func (intern-soft (concat "mew-smtp-command-" next)))
	  (and func (funcall func process pnm))
	  (mew-smtp-set-string pnm nil))
	 (t
	  (if (string-match "^pwd-" status)
	      (mew-smtp-set-error pnm "SMTP password is wrong!")
	    (if (string-match "\n$" str)
		(setq str (substring str 0 -1)))
	    (unless (string-match "[.!]$" str)
	      (setq str (concat str "."))))
	  (mew-smtp-set-error pnm str)
	  (mew-smtp-command-quit2 process pnm))))
       (t ()))))) ;; stay

(defun mew-smtp-sentinel (process event)
  (let* ((pnm (process-name process))
	 (pro (mew-smtp-get-process pnm))
	 (case (mew-smtp-get-case pnm))
	 (qfld (mew-smtp-get-qfld pnm))
	 (msgs (mew-smtp-get-messages pnm))
	 (fallback (mew-smtp-get-fallback pnm))
	 (status (mew-smtp-get-status pnm)))
    (save-excursion
      (mew-smtp-debug "SMTP SENTINEL" event)
      (cond
       ((or (string-match "failed" event)
	    (and (string= status "greeting")
		 (string-match "connection broken by remote peer" event)))
	(if fallback
	    (progn
	      (mew-smtp-send-message case qfld msgs fallback)
	      ;; A failed process stays
	      (when (memq (process-status pro) '(failed connect))
		(delete-process pro)
		(mew-info-clean-up pnm)))
	  (mew-smtp-set-error pnm (substring event 0 -1))
	  (mew-smtp-sentinel2 process event)))
       ((string-match "open" event)
	;; OK connected
	)
       (t
	(mew-smtp-sentinel2 process event))))))

(defun mew-smtp-sentinel2 (process _event)
  (let* ((pnm (process-name process))
	 (buf (process-buffer process))
	 (qfld (mew-smtp-get-qfld pnm))
	 (case (mew-smtp-get-case pnm))
	 (done (mew-smtp-get-done pnm))
	 (imapp (mew-smtp-get-imapp pnm))
	 (error (mew-smtp-get-error pnm))
	 (sshpro (mew-smtp-get-ssh-process pnm))
	 (sslpro (mew-smtp-get-ssl-process pnm)))
    (save-excursion
      (cond
       (error
	(when buf
	  ;; A message file is not inserted at the beginning of the SMTP
	  ;; session.
	  (set-buffer buf)
	  (mew-smtp-queue case error pnm))
	(mew-smtp-log pnm error)

	(if (memq system-type '(windows-nt ms-dos cygwin))
	    (message (format "%s  This mail has been queued to %s" error qfld))
	  (message-box (format "%s  This mail has been queued to %s" error qfld))))
       (done
	(message "Sending in background...done"))
       (t
	(if (null buf)
	    (message "SMTP connection is lost")
	  (set-buffer buf)
	  (mew-smtp-queue case "SMTP connection is lost" pnm))))
      (mew-info-clean-up pnm)
      (if (and (processp sshpro) (not mew-ssh-keep-connection))
	  (process-send-string sshpro "exit\n"))
      (if (and (processp sslpro) (not mew-ssl-keep-connection))
	  (delete-process sslpro))
      (run-hooks 'mew-smtp-sentinel-hook)
      (if (and done imapp) (mew-imap2-fcc case))
      (when buf (mew-remove-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Queuing
;;;

(defun mew-smtp-queue (case err &optional apnm)
  ;; Must be in a buffer where a message is contained.
  (let* ((pnm (or apnm (mew-smtp-info-name case)))
	 (qfld (mew-queue-folder case))
	 (oname (buffer-name))
	 (work (buffer-file-name))
	 file-info file info nname)
    (mew-local-folder-check qfld)
    (setq file-info (mew-queue-enqueue work qfld))
    (mew-set '(file info) file-info)
    (setq file (file-name-nondirectory file))
    (setq nname (mew-concat-folder qfld file))
    (if (mew-draft-p)
	(mew-smtp-set-case pnm (mew-tinfo-get-case)))
    ;;
    (mew-smtp-set-recipients pnm (mew-smtp-get-orig-recipients pnm))
    (let* ((n mew-smtp-info-list-save-length)
	   (data (make-vector n nil)))
      (dotimes (i n)
	(aset data i (aref (mew-info pnm) i)))
      (mew-lisp-save info data))
    ;;
    (message "%s has been queued to %s (%s)" oname nname err)
    (mew-touch-folder qfld)
    (file-name-sans-extension file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bcc:
;;;

(defun mew-smtp-bcc (pro pnm back)
  (let* ((dir (file-name-directory back))
	 (msg (file-name-nondirectory back))
	 (case (mew-smtp-get-case pnm))
	 msgid logtime)
    (mew-elet
     (mew-erase-buffer)
     (mew-set-buffer-multibyte t)
     (mew-smtp-set-recipients pnm (mew-smtp-get-bcc pnm))
     (mew-smtp-set-orig-recipients pnm (mew-smtp-get-bcc pnm))
     (mew-smtp-set-bcc pnm nil)
     ;;
     (mew-set '(msgid logtime) (mew-encode-id-date pnm (mew-smtp-message-id case))) ;; save-excursion
     (mew-smtp-set-msgid pnm msgid)
     (mew-smtp-set-logtime pnm logtime)
     (goto-char (point-max))
     (mew-draft-header-insert mew-to: "Bcc-Receiver:;")
     (mew-draft-header-insert mew-subj: mew-bcc-subject)
     (mew-draft-header-insert mew-from: (mew-smtp-get-from pnm))
     (mew-draft-header-insert mew-organization: (mew-organization case))
     (mew-draft-header-insert-alist (mew-header-alist case))
     ;; X-Mailer: must be the last
     (if (mew-use-x-mailer case)
	 (mew-draft-header-insert mew-x-mailer: mew-x-mailer))
     (mew-header-set "\n")
     (insert mew-bcc-body)
     (goto-char (mew-header-end))
     (forward-line)
     (setq mew-encode-syntax (mew-encode-syntax-initial dir))
     (setq mew-encode-syntax
	   (mew-syntax-insert-entry
	    mew-encode-syntax
	    '(2)
	    (mew-encode-syntax-single msg mew-type-msg nil nil nil)))
     (mew-encode-multipart mew-encode-syntax dir 0 'buffered)
     (mew-encode-make-header)
     (mew-encode-save-draft)
     (mew-overlay-delete-buffer)
     (mew-set-buffer-multibyte nil)
     (mew-info-clean-up pnm mew-smtp-info-list-clean-length)
     (set-process-buffer pro (current-buffer))
     (mew-smtp-set-status pnm "mail-from")
     (mew-smtp-command-mail-from pro pnm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Logging
;;;

(defun mew-smtp-log (pnm &optional err)
  (let ((logtime (mew-smtp-get-logtime pnm))
	(msgid (mew-smtp-get-msgid pnm))
	(recipients (mew-smtp-get-orig-recipients pnm))
	(server (mew-smtp-get-server pnm))
	(port (mew-smtp-get-port pnm))
	(sshsrv (mew-smtp-get-ssh-server pnm))
	(sslp (mew-smtp-get-ssl-p pnm)))
    (with-temp-buffer
      (and logtime (insert logtime))
      (and msgid (insert " id=" msgid))
      (and server (insert " server=" server ":" port))
      (and sshsrv (insert " sshsrv=" sshsrv))
      (and sslp (insert " SSL/TLS"))
      (and recipients
	   (setq recipients (mapconcat 'identity recipients ",")))
      (and recipients (insert " recipients=" recipients))
      (if err
	  (insert " status=" "("
                  (substring err 0 (string-match "\n+$" err))
                  ")")
	(insert " status=sent"))
      (insert "\n")
      (write-region (point-min) (point-max)
		    (expand-file-name mew-smtp-log-file mew-conf-path)
		    'append 'no-msg))))

(provide 'mew-smtp)

;;; Copyright Notice:

;; Copyright (C) 1999-2023 Mew developing team.
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

;;; mew-smtp.el ends here
