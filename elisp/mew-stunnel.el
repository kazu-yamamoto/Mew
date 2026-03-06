;;; -*- lexical-binding: nil; -*-
;;; mew-stunnel.el

;; Author:  Mew developing team
;; Created: Jul 25, 2002

;;; Code:

(require 'mew)

(defvar mew-prog-ssl "stunnel")

(defconst mew-stunnel-localhost "localhost")

;; for mew-config.el
;; The prefix should be mew-ssl

(defvar mew-prog-ssl-arg nil
  "A string of extra text to place in the configuration file,
which should end with a newline (example: \"fips=no\\n\"); or nil to insert
no extra text.")

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

(defvar mew-ssl-proxy-server nil)

(defvar mew-ssl-proxy-port nil)

;;

(defvar mew-ssl-libwrap nil
  "Set to t when stunnel supports \"LIBWRAP\" feature.")
(defvar mew-ssl-foreground nil
  "Set to t when stunnel supports \"foreground\" option.")
(defvar mew-ssl-pid nil
  "Set to t when stunnel supports \"pid\" option.")
(defvar mew-ssl-syslog nil
  "Set to t when stunnel supports \"syslog\" option.")

;;

(defvar mew-stunnel-ver nil)
(defvar mew-stunnel-minor-ver nil)

(defconst mew-stunnel-min-major-ver 5
  "Minimum major version of stunnel that Mew supports.")

(defconst mew-stunnel-min-minor-ver 15
  "Minimum minor version of stunnel that Mew supports.")

(defconst mew-stunnel-process-exec-cnt 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSL/TLS info
;;;

(defvar mew-stunnel-info-list '("status" "try" "file" "string"))

(mew-info-defun "mew-stunnel-" mew-stunnel-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process name
;;;

(defconst mew-stunnel-info-prefix "mew-stunnel-info-")

(defun mew-stunnel-info-name (server remoteport localport)
  (format "%s:%s:%d:%d" mew-stunnel-info-prefix server remoteport localport))

(defun mew-stunnel-info-name-regex (server remoteport)
  (format "^%s:%s:%d" mew-stunnel-info-prefix server remoteport))

(defun mew-ssl-pnm-to-lport (pnm)
  (if (string-match ":\\([0-9]+\\)$" pnm) (match-string 1 pnm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing SSL/TLS
;;;

(defun mew-stunnel-server (server)
  (if (string= server "localhost")
      mew-stunnel-localhost
    server))

(defun mew-stunnel-options (case server remoteport localport tls)
  (setq server (mew-stunnel-server server))
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
      (insert (format "accept=%s:%d\n" mew-stunnel-localhost localport))
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

(defun mew-open-stunnel-stream (case server serv tls)
  "Open an SSL/TLS stream for SERVER\\='s SERV.
This function returns a process when an SSL/TLS connection is created
successfully.
If TLS is nil, an SSL connection is created.
If TLS is a magic word for `stunnel', a TLS connection is created.
A local port number can be obtained the process name after `:'. "
  (cond
   ((or (null mew-stunnel-ver) (not (mew-which-exec mew-prog-ssl)))
    (message "'%s' is not found" mew-prog-ssl)
    nil)
   ((or (< mew-stunnel-ver mew-stunnel-min-major-ver)
	(and (= mew-stunnel-ver mew-stunnel-min-major-ver)
	     (< mew-stunnel-minor-ver mew-stunnel-min-minor-ver)))
    (message "Version %d.%d of '%s' is installed, but Mew requires version %d.%d or later."
	     mew-stunnel-ver mew-stunnel-minor-ver mew-prog-ssl mew-stunnel-min-major-ver
	     mew-stunnel-min-minor-ver)
    nil)
   (t
    (let* ((remoteport (mew-serv-to-port serv))
	   (localport (+ 8000 (% (mew-random) 4000)))
	   (process-connection-type mew-connection-type2)
	   (N mew-stunnel-process-exec-cnt)
	   (pros (process-list))
	   (regex (mew-stunnel-info-name-regex server remoteport))
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
	    (setq name (mew-stunnel-info-name server remoteport localport))
	    (setq opts (mew-stunnel-options case server remoteport localport tls))
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
	    (mew-stunnel-set-try pnm 0)
	    (mew-stunnel-set-file pnm (car opts))
	    (mew-set-process-cs pro mew-cs-text-for-read mew-cs-text-for-write)
	    (set-process-filter pro 'mew-stunnel-filter1)
	    (set-process-sentinel pro 'mew-ssl-sentinel)
	    (mew-rendezvous (null (mew-stunnel-get-status pnm)))
	    (if (eq (mew-stunnel-get-status pnm) t)
		(throw 'loop (setq bound t)))
	    ;; bind-failure
	    (setq localport (1+ localport))))
	(mew-stunnel-set-status pnm nil)
	(if (not bound)
	    (progn
	      (message "Creating an SSL/TLS connection...FAILED")
	      nil)
	  ;; "stunnel" does not gain access to the remote port
	  ;; until a tunneled connection is created.
	  ;; So, we need to check the SSL/TLS tunnel with a dummy
	  ;; tunneled connection here.
	  (set-process-filter pro 'mew-stunnel-filter2)
	  (setq dummy (open-network-stream " *Mew dummy*" nil mew-stunnel-localhost localport))
	  (mew-rendezvous (null (mew-stunnel-get-status pnm)))
	  (if (processp dummy) (delete-process dummy))
	  (if (eq (mew-stunnel-get-status pnm) t)
	      (progn
		(message "Creating an SSL/TLS connection...done")
		(set-process-filter pro 'mew-stunnel-filter3)
		pro)
	    ;; verify-failure
	    (delete-process pro)
	    (message "Creating an SSL/TLS connection...FAILED (cert verify failure)")
	    nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-stunnel-debug (label string)
  (when (mew-debug 'net)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-stunnel-filter1 (process string)
  (let* ((pnm (process-name process))
	 (prev-str (mew-stunnel-get-string pnm)))
    (save-excursion
      (mew-stunnel-debug "SSL/TLS: " string)
      (mew-stunnel-set-string pnm string)
      (setq string (concat prev-str string))
      (cond
       ((string-match "bound \\(\\|FD=[0-9]+ \\)to" string)
	(mew-stunnel-set-status pnm t))
       ((string-match "gethostbyname: Valid name, no data record of requested type" string)
	(mew-stunnel-set-status pnm 'gethostbyname-failure))
       ((string-match "gethostbyname: Host not found" string)
	(mew-stunnel-set-status pnm 'gethostbyname-failure))
       ((string-match "Local: bind: Address already in use" string)
	(mew-stunnel-set-status pnm 'bind-failure))))))

(defun mew-stunnel-filter2 (process string)
  (let* ((pnm (process-name process))
	 (prev-str (mew-stunnel-get-string pnm)))
    (save-excursion
      (mew-stunnel-debug "SSL/TLS: " string)
      (mew-stunnel-set-string pnm string)
      (setq string (concat prev-str string))
      (cond
       ((string-match "Negotiated \\| ciphersuite:\\|opened with SSL" string)
	(mew-stunnel-set-status pnm t))
       ((string-match "Failed to initialize" string)
	(mew-stunnel-set-status pnm t)) ;; xxx
       ((string-match "verify failed" string)
	(mew-stunnel-set-status pnm 'verify-failure))))))

(defun mew-stunnel-filter3 (_process string)
  (save-excursion
    (mew-stunnel-debug "SSL/TLS: " string)))

(defun mew-ssl-sentinel (process _event)
  (let* ((pnm (process-name process))
	 (file (mew-stunnel-get-file pnm)))
    (save-excursion
      (mew-delete-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stunnel version check
;;;

(defun mew-stunnel-setup ()
  (if (not (mew-which-exec mew-prog-ssl))
      (setq mew-stunnel-ver nil)
    (with-temp-buffer
      (call-process mew-prog-ssl nil t nil "-help")
      (goto-char (point-min))
      (re-search-forward "^stunnel " nil t 1)
      (if (looking-at "\\([45]\\)\\.\\([0-9]+\\)")
	  (progn
	    (setq mew-stunnel-ver (string-to-number (mew-match-string 1)))
	    (setq mew-stunnel-minor-ver (string-to-number (mew-match-string 2))))
	(setq mew-stunnel-ver 3))
      (when (re-search-forward "LIBWRAP" nil t)
	(setq mew-ssl-libwrap t))
      (when (re-search-forward "foreground" nil t)
	(setq mew-ssl-foreground t))
      (when (re-search-forward "pid" nil t)
	(setq mew-ssl-pid t))
      (when (re-search-forward "syslog" nil t)
	(setq mew-ssl-syslog t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Should move them to mew-tls.el?
;;;

(defun mew-tls-native-p (type)
  "Return if the type is native or not"
  (or (eq type 'native)
      (and (eq type t) (eq mew-ssl-default 'native))))

(defun mew-starttls-p (type port sslport)
  "Return if STARTTLS should be used or not"
  (and type (mew-port-equal port sslport)))

(defconst mew-tls-smtp "smtp")
(defconst mew-tls-pop  "pop3")
(defconst mew-tls-nntp "nntp")
(defconst mew-tls-imap "imap") ;; xxx stunnel does not support this.


(provide 'mew-stunnel)

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

;;; mew-stunnel.el ends here
