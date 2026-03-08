;;; -*- lexical-binding: nil; -*-
;;; mew-gnutls.el

;; Author:  Mew developing team
;; Created: Mar  6, 2026

;;; Code:

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
;;; Parameters
;;;

(defvar mew-gnutls-plist
  '(;; RFC 3207
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

(defun mew-gnutls-get-param (proto key evalp)
  "Get parameter from mew-gnutls-plist"
  (let ((p (plist-get (cdr (assq proto mew-gnutls-plist)) key)))
    (if evalp (eval p) p)))

;;; XXX: (mew-open-network-stream) always returns a list
;;       and is also used for non-SMTP protocols.
;;; XXX: port must be resolved by using mew-serv-to-port
;;       because some service names are not in /etc/services.
;;       mew-serv-to-port uses mew-port-db.
(defvar mew--advice-tls-parameters-plist nil)

(defun mew--advice-filter-args-gnutls-negotiate (&rest args)
  (nconc (car args) mew--advice-tls-parameters-plist))

(defun mew-gnutls-boot-parameters (case hostname)
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
		:min-prime-bits mew-gnutls-min-prime-bits
		;;
		;; mew-gnutls-verify-error should be nil
		;; to defer verification to NSM.  Note
		;; that gnutls-verify-error overrides
		;; verify-error when it is nil.
		;; Setting gnutls-verify-error to t is
		;; also discouraged.
		;;
		:verify-error mew-gnutls-verify-error
		:hostname hostname)))
	  (plist-put boot-params
		     :priority-string
		     (plist-get boot-params :priority)))))

(defun mew-gnutls-parameters (case proto starttlsp)
  (list
   :always-query-capabilities
   (and starttlsp
	(mew-gnutls-get-param
	 proto :always-query-capabilities nil))
   :capability-command
   (and starttlsp
	(mew-gnutls-get-param
	 proto :capability-command t))
   :end-of-capability
   (and starttlsp
	(mew-gnutls-get-param
	 proto :end-of-capability t))
   :end-of-command
   (and starttlsp
	(mew-gnutls-get-param
	 proto :end-of-command t))
   :success
   (and starttlsp
	(mew-gnutls-get-param
	 proto :success t))
   :starttls-function
   (and starttlsp
	(mew-gnutls-get-param
	 proto :starttls-function nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defun mew-open-gnutls-stream (name buf server port proto gnutlsp starttlsp case nowait status-msg)
  (let* ((hostname (puny-encode-domain server))
	 ;; Note: on Emacs 26.3 and prior GnuTLS always uses
	 ;; the system-wide default path first even if
	 ;; trustfiles is specified.
	 (trustfiles (mew-ssl-trustfiles case))
	 (nsm-noninteractive nil)
	 (network-security-level network-security-level)
	 (tlsparams (mew-gnutls-boot-parameters case hostname))
	 pro)
    (when (eq (mew-ssl-verify-level case) 0)
      ;; Forcibly disable verification.
      (setq network-security-level 'low))
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
	(setq pro (apply 
		   'open-network-stream
		   name buf server port
		   :type type
		   :return-list t
		   :nowait nowait
		   (mew-gnutls-parameters case proto starttlsp)))
	(advice-remove 'gnutls-negotiate
		       #'mew--advice-filter-args-gnutls-negotiate)
	;;
	;; When a validation error occurs, (car pro) will be nil.
	;;
	(let ((plainp (eq 'plain (plist-get (cdr pro) :type)))
	      (greeting (plist-get (cdr pro) :greeting))
	      (capabilities (plist-get (cdr pro) :capabilities))
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
	    (when (and gnutlsp starttlsp)
	      (mew-smtp-debug "*GREETING*"
			      (if greeting
				  (string-replace "\r\n" "\n"
						  greeting)))
	      (mew-smtp-debug "*CAPABILITIES*"
			      (if capabilities
				  (string-replace "\r\n" "\n"
						  capabilities))))
	    (setq pro (list
		       (car pro)
		       :error nil))
	    (cond
	     ((eq proto 'smtp)
	      (setq mew--gnutls-smtp-greeting greeting))
	     ((eq proto 'pop)
	      (setq mew--gnutls-pop-greeting greeting))
	     ((eq proto 'imap)
	      (setq mew--gnutls-imap-greeting greeting))))))))
    pro))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defun mew-open-network-stream (name buf server port proto gnutlsp starttlsp case)
  (let* ((tun-type nil)
	 (status-msg (format "Opening a %s connection %s%s%s..."
			     (if gnutlsp "TLS" "TCP")
			     (if gnutlsp "(GnuTLS" "")
			     (if gnutlsp
				 (if starttlsp ", STARTTLS)" ")")
			       "")
			     (if (eq tun-type 'ssh) " over SSH"
			       "")))
	 family nowait pro)
    ;; SMTP-specific
    (when (and (eq proto 'smtp) mew-inherit-submission)
      (setq family mew-smtp-submission-family)
      (setq nowait t))
    ;; TLS does not work for Unix-domain socket for now.
    (when (and (not gnutlsp)
	       (stringp port) (string-match "^/" port))
      (setq family 'local)
      (setq server 'local))
    (cond
     ;; Both GnuTLS and NSM are mandatory for 'native.
     ((and gnutlsp (or (not (fboundp 'gnutls-available-p))
		       (not (gnutls-available-p))
		       (not (fboundp 'gnutls-boot-parameters))
		       (not (fboundp 'nsm-level))))
      (setq pro
	    (list nil
		  :error t
		  :status-msg
		  (concat status-msg
			  "FAILED (GnuTLS or NSM not available)"))))
     (gnutlsp
      (setq pro (mew-open-gnutls-stream name buf server port proto gnutlsp starttlsp case nowait status-msg)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defvar mew-smtp-submission-family 'ipv4)
(defvar mew-smtp-submission-timeout 10)
(defun mew-smtp-submission-timeout (pro)
  (when (and (processp pro) (eq (process-status pro) 'connect))
    (mew-smtp-sentinel pro "time out - failed\n")))

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
