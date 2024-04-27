;;; mew-ssl.el

;; Author:  Mew developing team
;; Created: Jul 25, 2002

;;; Code:

(require 'mew)

(defun mew-ssl-native-p (type)
  "Return if the type is native or not"
  (or (eq type 'native)
      (and (eq type t) (eq mew-ssl-default 'native))))
(defun mew-ssl-starttls-p (type port sslport)
  "Return if STARTTLS should be used or not"
  (and type (mew-port-equal port sslport)))
(defvar mew-ssl-min-prime-bits 2048
  "Default prime bits for GnuTLS connection.")
(defvar mew-ssl-verify-error nil
  "verify-error parameter passed to GnuTLS.  You might want to
keep this as nil.")
(defvar mew-ssl-native-starttls-plist
  '(
    ;; RFC 3207
    (smtp . (:capability-command
	     (format "EHLO %s\r\n" (mew-smtp-helo-domain case))
	     :always-query-capabilities t
	     :end-of-command
	     (format "^[0-9]+ .*\r?\n")
	     :end-of-capability
	     (format "^[0-9]+ .*\r?\n")
	     :success (format "^2.*\r?\n")
	     :starttls-function
	     (lambda (capabilities)
	       (and (string-match "[ -]STARTTLS" capabilities)
		    "STARTTLS\r\n"))))
    ;; RFC 2595
    (imap . (:capability-command
	     (format "1 CAPABILITY\r\n")
	     :always-query-capabilities t
	     :end-of-capability
	     (format "\r?\n")
	     :end-of-command
	     (format "\r?\n")
	     :success
	     (format "^1 OK ")
	     :starttls-function
	     (lambda (capabilities)
	       (when (string-match-p "STARTTLS" capabilities)
		 "1 STARTTLS\r\n"))))
    ;; RFC 2595
    (pop . (:capability-command
	    (format "CAPA\r\n")
	    :always-query-capabilities nil
	    :end-of-capability
	    (format "^\\.\r?\n\\|^-ERR")
	    :end-of-command
	    (format "^\\(-ERR\\|+OK\\).*\r?\n")
	    :success
	    (format "^\\+OK.*\n")
	    :starttls-function
	    (lambda (capabilities)
	      (and (string-match "\\bSTLS\\b" capabilities)
		   "STLS\r\n"))))
    ;; RFC 4642
    (nntp . (:capability-command
	     (format "CAPABILITIES\r?\n")
	     :always-query-capabilities t
	     :end-of-capability
	     (format "^\\.\r?\n")
	     :end-of-command
	     (format "^\\.\r?\n")
	     :success
	     (format "^2.+ \r?\n")
	     :starttls-function
	     (lambda (capabilities)
	       (when (string-match-p "STARTTLS" capabilities)
		   "STARTTLS\r\n"))))
    ))
(defun mew-starttls-get-param (proto key evalp)
  "Get parameter from mew-ssl-native-starttls-plist"
  (let ((p (plist-get (cdr (assq proto mew-ssl-native-starttls-plist)) key)))
    (if evalp (eval p) p)))

(defvar mew-prog-ssl "stunnel")
(defvar mew-ssl-cert-directory "~/.certs"
  "The directory where certificates of root servers are stored.
A file name of a certificate should be `cert-hash.0'.
`cert-hash' can be extracted by `openssl x509 -hash -noout -in cert.pem'.")

(defvar mew-ssl-verify-level 1
  "Verification level of server's certificate.
0 - no verification.
1 - verify server's certificate if present. If verification failed, an
    SSL/TLS connection is not created. If not present, an SSL/TLS connection
    is created.
2 - verify server's certificate. If verification failed, an SSL/TLS
    connection is not created. If not present, an SSL/TLS connection is
    not created.
3 - verify server's certificate which locally installed (not one from
    the server).")

(defvar mew-prog-ssl-arg nil
  "A string of extra text to place in the configuration file,
which should end with a newline (example: \"fips=no\\n\"); or nil to insert
no extra text.")

(defvar mew-ssl-proxy-server nil)
(defvar mew-ssl-proxy-port nil)

(defvar mew-ssl-ver nil)
(defvar mew-ssl-minor-ver nil)

(defvar mew-ssl-libwrap nil
  "Set to t when stunnel supports \"LIBWRAP\" feature.")
(defvar mew-ssl-foreground nil
  "Set to t when stunnel supports \"foreground\" option.")
(defvar mew-ssl-pid nil
  "Set to t when stunnel supports \"pid\" option.")
(defvar mew-ssl-syslog nil
  "Set to t when stunnel supports \"syslog\" option.")

(defconst mew-ssl-process-exec-cnt 3)

(defconst mew-ssl-min-major-ver 5
  "Minimum major version of stunnel that Mew supports.")
(defconst mew-ssl-min-minor-ver 15
  "Minimum minor version of stunnel that Mew supports.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Magic words
;;;

(defconst mew-tls-smtp "smtp")
(defconst mew-tls-pop  "pop3")
(defconst mew-tls-nntp "nntp")
(defconst mew-tls-imap "imap") ;; xxx stunnel does not support this.

(defconst mew-ssl-localhost "localhost")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSL/TLS info
;;;

(defvar mew-ssl-info-list '("status" "try" "file" "string"))

(mew-info-defun "mew-ssl-" mew-ssl-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process name
;;;

(defconst mew-ssl-info-prefix "mew-ssl-info-")

(defun mew-ssl-info-name (server remoteport localport)
  (format "%s:%s:%d:%d" mew-ssl-info-prefix server remoteport localport))

(defun mew-ssl-info-name-regex (server remoteport)
  (format "^%s:%s:%d" mew-ssl-info-prefix server remoteport))

(defun mew-ssl-pnm-to-lport (pnm)
  (if (string-match ":\\([0-9]+\\)$" pnm) (match-string 1 pnm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing SSL/TLS
;;;

(defun mew-ssl-server (server)
  (if (string= server "localhost")
      mew-ssl-localhost
    server))

(defun mew-ssl-options (case server remoteport localport tls)
  (setq server (mew-ssl-server server))
  (let ((file (mew-make-temp-name)))
    (with-temp-buffer
      (insert "client=yes\n")
      (if mew-ssl-pid
	  (insert "pid=\n"))
      (insert (format "verify=%d\n" (mew-ssl-verify-level case)))
      (if (> (mew-ssl-verify-level case) 0)
	  (insert (format "checkHost=%s\n" server)))
      (if mew-ssl-foreground
	  (insert "foreground=yes\n"))
      (insert "debug=debug\n")
      (if mew-ssl-libwrap
	  (insert "libwrap=no\n"))
      (if mew-ssl-syslog
	  (insert "syslog=no\n"))
      (insert "CApath=" (expand-file-name (mew-ssl-cert-directory case)) "\n")
      (if (mew-prog-ssl-arg case)
	  (insert (mew-prog-ssl-arg case)))
      (insert (format "[%d]\n" localport))
      (insert (format "accept=%s:%d\n" mew-ssl-localhost localport))
      (if (mew-ssl-proxy-server case)
	  (insert
	   (format "connect=%s:%s\nprotocol=connect\nprotocolHost=%s:%d\n"
		   (mew-ssl-proxy-server case) (mew-ssl-proxy-port case)
		   server remoteport))
	(insert (format "connect=%s:%d\n" server remoteport))
	(if tls (insert (format "protocol=%s\n" tls))))
      (mew-frwlet mew-cs-dummy mew-cs-text-for-write
		  ;; NEVER use call-process-region for privacy reasons
		  (write-region (point-min) (point-max) file nil 'no-msg))
      (list file))))

(defun mew-open-ssl-stream (case server serv tls)
  "Open an SSL/TLS stream for SERVER\\='s SERV.
This function returns a process when an SSL/TLS connection is created
successfully.
If TLS is nil, an SSL connection is created.
If TLS is a magic word for `stunnel', a TLS connection is created.
A local port number can be obtained the process name after `:'. "
  (cond
   ((or (null mew-ssl-ver) (not (mew-which-exec mew-prog-ssl)))
    (message "'%s' is not found" mew-prog-ssl)
    nil)
   ((or (< mew-ssl-ver mew-ssl-min-major-ver)
	(and (= mew-ssl-ver mew-ssl-min-major-ver)
	     (< mew-ssl-minor-ver mew-ssl-min-minor-ver)))
    (message "Version %d.%d of '%s' is installed, but Mew requires version %d.%d or later."
	     mew-ssl-ver mew-ssl-minor-ver mew-prog-ssl mew-ssl-min-major-ver
	     mew-ssl-min-minor-ver)
    nil)
   (t
    (let* ((remoteport (mew-serv-to-port serv))
	   (localport (+ 8000 (% (mew-random) 4000)))
	   (process-connection-type mew-connection-type2)
	   (N mew-ssl-process-exec-cnt)
	   (pros (process-list))
	   (regex (mew-ssl-info-name-regex server remoteport))
	   name pnm pro dummy bound opts)
      (catch 'find
	(dolist (pr pros)
	  (when (string-match regex (process-name pr))
	    (if (memq (process-status pr) '(run))
		(setq pro pr)
	      (delete-process pr))
	    (throw 'find nil))))
      (if pro
	  pro
	(message "Creating an SSL/TLS connection...")
	(setq pro nil)
	(catch 'loop
	  (dotimes (_i N) ;; prevent byte-compile warning
	    (setq name (mew-ssl-info-name server remoteport localport))
	    (setq opts (mew-ssl-options case server remoteport localport tls))
	    (setq pro (apply 'start-process name nil mew-prog-ssl opts))
	    ;; An error would occur. So, let's exit in the case.
	    (cond
	     ((not (processp pro))
	      (message "Creating an SSL/TLS connection...FAILED")
	      (throw 'loop nil))
	     ((not (memq (process-status pro) '(run)))
	      (delete-process pro)
	      (message "Creating an SSL/TLS connection...FAILED")
	      (throw 'loop nil)))
	    ;; stunnel is now running.
	    (mew-process-silent-exit pro)
	    (setq pnm (process-name pro))
	    (mew-info-clean-up pnm)
	    (mew-ssl-set-try pnm 0)
	    (mew-ssl-set-file pnm (car opts))
	    (mew-set-process-cs pro mew-cs-text-for-read mew-cs-text-for-write)
	    (set-process-filter pro 'mew-ssl-filter1)
	    (set-process-sentinel pro 'mew-ssl-sentinel)
	    (mew-rendezvous (null (mew-ssl-get-status pnm)))
	    (if (eq (mew-ssl-get-status pnm) t)
		(throw 'loop (setq bound t)))
	    ;; bind-failure
	    (setq localport (1+ localport))))
	(mew-ssl-set-status pnm nil)
	(if (not bound)
	    (progn
	      (message "Creating an SSL/TLS connection...FAILED")
	      nil)
	  ;; "stunnel" does not gain access to the remote port
	  ;; until a tunneled connection is created.
	  ;; So, we need to check the SSL/TLS tunnel with a dummy
	  ;; tunneled connection here.
	  (set-process-filter pro 'mew-ssl-filter2)
	  (setq dummy (open-network-stream " *Mew dummy*" nil mew-ssl-localhost localport))
	  (mew-rendezvous (null (mew-ssl-get-status pnm)))
	  (if (processp dummy) (delete-process dummy))
	  (if (eq (mew-ssl-get-status pnm) t)
	      (progn
		(message "Creating an SSL/TLS connection...done")
		(set-process-filter pro 'mew-ssl-filter3)
		pro)
	    ;; verify-failure
	    (delete-process pro)
	    (message "Creating an SSL/TLS connection...FAILED (cert verify failure)")
	    nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-ssl-debug (label string)
  (when (mew-debug 'net)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-ssl-filter1 (process string)
  (let* ((pnm (process-name process))
	 (prev-str (mew-ssl-get-string pnm)))
    (save-excursion
      (mew-ssl-debug "SSL/TLS: " string)
      (mew-ssl-set-string pnm string)
      (setq string (concat prev-str string))
      (cond
       ((string-match "bound \\(\\|FD=[0-9]+ \\)to" string)
	(mew-ssl-set-status pnm t))
       ((string-match "gethostbyname: Valid name, no data record of requested type" string)
	(mew-ssl-set-status pnm 'gethostbyname-failure))
       ((string-match "gethostbyname: Host not found" string)
	(mew-ssl-set-status pnm 'gethostbyname-failure))
       ((string-match "Local: bind: Address already in use" string)
	(mew-ssl-set-status pnm 'bind-failure))))))

(defun mew-ssl-filter2 (process string)
  (let* ((pnm (process-name process))
	 (prev-str (mew-ssl-get-string pnm)))
    (save-excursion
      (mew-ssl-debug "SSL/TLS: " string)
      (mew-ssl-set-string pnm string)
      (setq string (concat prev-str string))
      (cond
       ((string-match "Negotiated \\| ciphersuite:\\|opened with SSL" string)
	(mew-ssl-set-status pnm t))
       ((string-match "Failed to initialize" string)
	(mew-ssl-set-status pnm t)) ;; xxx
       ((string-match "verify failed" string)
	(mew-ssl-set-status pnm 'verify-failure))))))

(defun mew-ssl-filter3 (_process string)
  (save-excursion
    (mew-ssl-debug "SSL/TLS: " string)))

(defun mew-ssl-sentinel (process _event)
  (let* ((pnm (process-name process))
	 (file (mew-ssl-get-file pnm)))
    (save-excursion
      (mew-delete-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stunnel version check
;;;

(defun mew-ssl-setup ()
  (if (not (mew-which-exec mew-prog-ssl))
      (setq mew-ssl-ver nil)
    (with-temp-buffer
      (call-process mew-prog-ssl nil t nil "-help")
      (goto-char (point-min))
      (re-search-forward "^stunnel " nil t 1)
      (if (looking-at "\\([45]\\)\\.\\([0-9]+\\)")
	  (progn
	    (setq mew-ssl-ver (string-to-number (mew-match-string 1)))
	    (setq mew-ssl-minor-ver (string-to-number (mew-match-string 2))))
	(setq mew-ssl-ver 3))
      (when (re-search-forward "LIBWRAP" nil t)
	(setq mew-ssl-libwrap t))
      (when (re-search-forward "foreground" nil t)
	(setq mew-ssl-foreground t))
      (when (re-search-forward "pid" nil t)
	(setq mew-ssl-pid t))
      (when (re-search-forward "syslog" nil t)
	(setq mew-ssl-syslog t)))))

(provide 'mew-ssl)

;;; Copyright Notice:

;; Copyright (C) 2002-2023 Mew developing team.
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

;;; mew-ssl.el ends here
