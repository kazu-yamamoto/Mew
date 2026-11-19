;;; -*- lexical-binding: t; -*-
;;; mew-gnutls.el

;; Author:  Mew developing team
;; Created: Mar  6, 2026

;;; Code:

;; Mew drives the connection itself instead of `open-network-stream',
;; which does not pass Mew's TLS parameters to `gnutls-negotiate' for
;; STARTTLS.  Nothing global is changed, so other connections opened
;; meanwhile by timers or filters are not affected.

(require 'gnutls)
(require 'nsm)

(defvar mew-gnutls-min-prime-bits 2048
  "Default prime bits for GnuTLS connection.")

(defvar mew-gnutls-verify-error nil
  "verify-error parameter passed to GnuTLS.  You might want to
keep this as nil.")

(defun mew-gnutls-p (type)
  "Return if the type is gnutls or not"
  (or (eq type 'native)
      (and (eq type t) (eq mew-ssl-default 'native))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Protocol parameters
;;;

(defun mew-gnutls-protocol-params (proto case)
  "Return the commands and the regexps for the greeting and STARTTLS.
Each regexp matches the end of a reply."
  (cond
   ((eq proto 'smtp) ;; RFC 3207
    (let ((reply "^[0-9]+ .*\n"))
      (list :greeting reply
	    :capability (format "EHLO %s\n" (mew-smtp-helo-domain case))
	    :end-of-capability reply
	    :starttls "^250[ -]STARTTLS\\b"
	    :command "STARTTLS\n"
	    :end-of-command reply
	    :success "^220 ")))
   ((eq proto 'imap) ;; RFC 2595
    (list :greeting "\n"
	  :capability "1 CAPABILITY\n"
	  :end-of-capability "^1 \\(OK\\|NO\\|BAD\\).*\n"
	  :starttls "^\\* CAPABILITY .*\\bSTARTTLS\\b"
	  :command "2 STARTTLS\n"
	  :end-of-command "^2 \\(OK\\|NO\\|BAD\\).*\n"
	  :success "^2 OK"))
   ((eq proto 'pop) ;; RFC 2595
    (let ((reply "^\\(\\+OK\\|-ERR\\).*\n"))
      (list :greeting reply
	    :capability "CAPA\n"
	    :end-of-capability "^\\.\n\\|^-ERR.*\n"
	    :starttls "^STLS\\b"
	    :command "STLS\n"
	    :end-of-command reply
	    :success "^\\+OK")))
   ((eq proto 'nntp) ;; RFC 4642
    (let ((reply "^[0-9]+ .*\n"))
      (list :greeting reply
	    :capability "CAPABILITIES\n"
	    :end-of-capability "^\\.\n\\|^[45][0-9][0-9] .*\n"
	    :starttls "^STARTTLS\\b"
	    :command "STARTTLS\n"
	    :end-of-command reply
	    :success "^382 ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversation
;;;

(defun mew-gnutls-receive (pro start regex)
  "Wait until REGEX matches after START in the buffer of PRO.
Return the text from START to the end of the match."
  (with-current-buffer (process-buffer pro)
    (let (end)
      (while (progn
	       (goto-char start)
	       (not (setq end (re-search-forward regex nil t))))
	(unless (process-live-p pro)
	  (error "Connection closed by the server"))
	(accept-process-output pro 0.05))
      (buffer-substring start end))))

(defun mew-gnutls-command (pro command regex)
  "Send COMMAND to PRO and return the reply which ends with REGEX."
  (let ((start (with-current-buffer (process-buffer pro) (point-max))))
    (process-send-string pro command)
    (mew-gnutls-receive pro start regex)))

(defun mew-gnutls-starttls (pro params)
  "Have the server of PRO start TLS and return its greeting."
  (let* ((greeting (mew-gnutls-receive pro 1 (plist-get params :greeting)))
	 (capa (mew-gnutls-command pro (plist-get params :capability)
				   (plist-get params :end-of-capability))))
    (unless (string-match (plist-get params :starttls) capa)
      (error "Server does not support STARTTLS"))
    (unless (string-match (plist-get params :success)
			  (mew-gnutls-command
			   pro (plist-get params :command)
			   (plist-get params :end-of-command)))
      (error "Server refused STARTTLS"))
    greeting))

(defun mew-gnutls-handshake (pro server port proto starttlsp case)
  "Make PRO a TLS connection and return the greeting of the server.
For STARTTLS, the greeting is the one before TLS."
  (let* ((params (mew-gnutls-protocol-params proto case))
	 (greeting (and starttlsp (mew-gnutls-starttls pro params))))
    (gnutls-negotiate :process pro
		      :type 'gnutls-x509pki
		      :hostname (puny-encode-domain server)
		      :keylist (mew-ssl-client-keycert-list case)
		      :trustfiles (mew-ssl-trustfiles case)
		      :priority-string (mew-ssl-algorithm-priority case)
		      :min-prime-bits mew-gnutls-min-prime-bits
		      ;; nil defers verification to NSM.
		      :verify-error mew-gnutls-verify-error)
    ;; Level 0 disables verification.
    (unless (or (eq (mew-ssl-verify-level case) 0)
		(nsm-verify-connection pro server port starttlsp))
      (error "Server certificate is not accepted"))
    (or greeting
	(mew-gnutls-receive pro 1 (plist-get params :greeting)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening
;;;

(defun mew-open-gnutls-stream (name server port proto starttlsp case status-msg)
  "Open a TLS connection and return (PROCESS :greeting GREETING).
On failure, return (nil :error MESSAGE)."
  (let ((buf (generate-new-buffer " *Mew GnuTLS*"))
	pro pro-plist)
    (funcall (intern (concat "mew-" (symbol-name proto) "-debug"))
	     (format "TLS proto=%s, server=%s:%s, starttlsp=%s"
		     proto server port starttlsp)
	     (format "verify-level=%s" (mew-ssl-verify-level case)))
    ;; A timeout or C-g must not leave a half-open process.
    (unwind-protect
	(condition-case err
	    (with-temp-message status-msg
	      (setq pro (make-network-process
			 :name name :buffer buf
			 :host (puny-encode-domain server) :service port
			 :coding mew-cs-text-for-net))
	      (setq pro-plist
		    (list pro :greeting
			  (mew-gnutls-handshake pro server port proto
						starttlsp case))))
	  (error
	   (setq pro-plist (list nil :error (error-message-string err)))))
      (when (processp pro)
	(if (car pro-plist)
	    (set-process-buffer pro nil)
	  (delete-process pro)))
      (kill-buffer buf))
    pro-plist))

(defun mew-check-process (pro-plist status-msg)
  (let* ((pro (car pro-plist))
	 (plist (cdr pro-plist))
	 (openp (and (processp pro) (eq 'open (process-status pro)))))
    (if openp
	pro-plist
      (if (processp pro) (delete-process pro))
      (let ((msg (plist-get plist :error)))
	(message "%s" (concat status-msg "FAILED: " msg)))
      (list nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defun mew-open-network-stream (name buf server port proto gnutlsp starttlsp case)
  "Open a connection and return (PROCESS . PLIST).
With GNUTLSP, PLIST has :greeting, which the caller must handle
since the server does not send it again."
  (let* ((status-msg (format "Opening a %s connection %s%s..."
			     (if gnutlsp "TLS" "TCP")
			     (if gnutlsp "(GnuTLS" "")
			     (if gnutlsp
				 (if starttlsp ", STARTTLS)" ")")
			       "")))
	 (pro-plist (list nil))
	 family)
    ;; TLS does not work for Unix-domain socket for now.
    (when (and (not gnutlsp) (stringp port) (string-match "^/" port))
      (setq family 'local)
      (setq server 'local))
    (cond
     ((and gnutlsp (not (gnutls-available-p)))
      (message "%s" (concat status-msg "FAILED (GnuTLS not available)")))
     (gnutlsp
      (setq pro-plist (mew-open-gnutls-stream name server port proto starttlsp case status-msg))
      (mew-check-process pro-plist status-msg))
     (t
      (with-temp-message status-msg
	(setq pro-plist (apply 'open-network-stream
			       name buf server port
			       :coding mew-cs-text-for-net
			       :return-list t
			       :family family)))
      (mew-check-process pro-plist status-msg)))))

(provide 'mew-gnutls)

;;; Copyright Notice:

;; Copyright (C) 2002-2026 Mew developing team.
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

;;; mew-gnutls.el ends here
