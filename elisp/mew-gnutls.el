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
	     (format "EHLO %s\n" (mew-smtp-helo-domain case))
	     :always-query-capabilities t
	     :end-of-command
	     (format "^[0-9]+ .*\n")
	     :end-of-capability
	     (format "^[0-9]+ .*\n")
	     :success (format "^2.*\n")
	     :starttls-function
	     (lambda (capabilities)
	       (and (string-match "[ -]STARTTLS" capabilities)
		    "STARTTLS\n"))))
    ;; RFC 2595
    (imap . (:capability-command
	     (format "1 CAPABILITY\n")
	     :always-query-capabilities t
	     :end-of-capability
	     (format "\n")
	     :end-of-command
	     (format "\n")
	     :success
	     (format "^1 OK ")
	     :starttls-function
	     (lambda (capabilities)
	       (when (string-match-p "STARTTLS" capabilities)
		 "1 STARTTLS\n"))))
    ;; RFC 2595
    (pop . (:capability-command
	    (format "CAPA\n")
	    :always-query-capabilities nil
	    :end-of-capability
	    (format "^\\.\n\\|^-ERR")
	    :end-of-command
	    (format "^\\(-ERR\\|+OK\\).*\n")
	    :success
	    (format "^\\+OK.*\n")
	    :starttls-function
	    (lambda (capabilities)
	      (and (string-match "\\bSTLS\\b" capabilities)
		   "STLS\n"))))
    ;; RFC 4642
    (nntp . (:capability-command
	     (format "CAPABILITIES\n")
	     :always-query-capabilities t
	     :end-of-capability
	     (format "^\\.\n")
	     :end-of-command
	     (format "^\\.\n")
	     :success
	     (format "^2.+ \n")
	     :starttls-function
	     (lambda (capabilities)
	       (when (string-match-p "STARTTLS" capabilities)
		 "STARTTLS\n"))))
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

(defun mew-open-gnutls-stream (name buf server port proto starttlsp case status-msg)
  (let* ((hostname (puny-encode-domain server))
	 ;; Note: on Emacs 26.3 and prior GnuTLS always uses
	 ;; the system-wide default path first even if
	 ;; trustfiles is specified.
	 (trustfiles (mew-ssl-trustfiles case))
	 (nsm-noninteractive nil)
	 (network-security-level network-security-level)
	 (tlsparams (mew-gnutls-boot-parameters case hostname))
	 (type (if starttlsp 'starttls 'tls))
	 pro-plist pro plist)
    (when (eq (mew-ssl-verify-level case) 0)
      ;; Forcibly disable verification.
      (setq network-security-level 'low))
    ;; debug output: TLS params
    (funcall (intern (concat "mew-" (symbol-name proto) "-debug"))
	     (format "TLS proto=%s, server=%s:%s, starttlsp=%s"
		     proto hostname port starttlsp)
	     (format "verify-level=%s, network-security-level=%s, tlsparams=%s"
		     (mew-ssl-verify-level case) network-security-level
		     (apply #'concat (mapcar (lambda (a) (format "%s " a)) tlsparams))))
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
    (with-temp-message status-msg
      (setq pro-plist (apply
		       'open-network-stream
		       name buf server port
		       :coding mew-cs-text-for-net
		       :type type
		       :return-list t
		       (mew-gnutls-parameters case proto starttlsp))))
    (advice-remove 'gnutls-negotiate
		   #'mew--advice-filter-args-gnutls-negotiate)
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
  (let* ((tun-type nil)
	 (status-msg (format "Opening a %s connection %s%s%s..."
			     (if gnutlsp "TLS" "TCP")
			     (if gnutlsp "(GnuTLS" "")
			     (if gnutlsp
				 (if starttlsp ", STARTTLS)" ")")
			       "")
			     (if (eq tun-type 'ssh) " over SSH"
			       "")))
	 (pro-plist (list nil))
	 family)
    ;; TLS does not work for Unix-domain socket for now.
    (when (and (not gnutlsp) (stringp port) (string-match "^/" port))
      (setq family 'local)
      (setq server 'local))
    (cond
     ;; Both GnuTLS and NSM are mandatory for 'native.
     ((and gnutlsp (or (not (fboundp 'gnutls-available-p))
		       (not (gnutls-available-p))
		       (not (fboundp 'gnutls-boot-parameters))
		       (not (fboundp 'nsm-level))))
      (message "%s" (concat status-msg "FAILED (GnuTLS or NSM not available)")))
     (gnutlsp
      (setq pro-plist (mew-open-gnutls-stream name buf server port proto starttlsp case status-msg))
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
