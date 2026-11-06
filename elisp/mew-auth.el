;;; -*- lexical-binding: t; -*-
;;; mew-auth.el

;; Author:  Mew developing team
;; Created: Aug 17, 1999

;;; Code:

(require 'mew)

(defun mew-auth-select (grt lst)
  (mew-auth-select2 (mew-split grt mew-sp) lst))

(defun mew-auth-select2 (auths lst)
  (let (n preference strongest)
    (dolist (auth auths)
      (setq n (mew-member-case-equal auth lst))
      (cond
       ((null n) ())
       ((null preference)
	(setq preference n)
	(setq strongest auth))
       ((< n preference)
	(setq preference n)
	(setq strongest auth))))
    strongest))

;;
;; Passwords and tokens over an unencrypted connection
;;

(defvar mew-auth-cleartext-list '("PLAIN" "LOGIN" "XOAUTH2" "AUTHINFO")
  "Methods which send a password or a token as it is.
\"LOGIN\" is also the IMAP command and \"AUTHINFO\" the NNTP one.")

(defun mew-auth-cleartext-refused-p (auth secure case)
  "Return t if AUTH may not be used since the connection is not SECURE."
  (and (not secure)
       (mew-member-case-equal auth mew-auth-cleartext-list)
       (not (mew-auth-cleartext-allowed case))))

(defun mew-auth-cleartext-refuse (proto auth case)
  "Explain in the *Warnings* buffer why PROTO stopped before AUTH.
Return a one-line message for the error of the session."
  (let ((ssl (format "%s-ssl" (downcase proto))))
    (display-warning
     'mew
     (format "%s: %s would send your password or token unencrypted%s,
so Mew stopped before sending it.

To protect it, connect to the server with TLS: set mew-%s
\(see its documentation), or add (%s ...) to the entry of the case
in mew-config-alist.

To send it as it is anyway, for example to a server on your own LAN,
add (auth-cleartext-allowed t) to the entry of the case in
mew-config-alist, or set mew-auth-cleartext-allowed to t for all cases."
	     proto auth
	     (if (mew-case-default-p case) "" (format " (case %s)" case))
	     ssl ssl)
     :error)
    (format "%s %s needs TLS: see the *Warnings* buffer" proto auth)))

(defun mew-md5-raw (str)
  (let* ((md5str (mew-md5 str))
	 (len (length md5str))
	 (md5raw (make-string (/ len 2) 0))
	 (i 0) (j 0))
    (while (< i len)
      (aset md5raw j (+ (* (mew-hexchar-to-int (aref md5str i)) 16)
			(mew-hexchar-to-int (aref md5str (1+ i)))))
      (setq i (+ i 2))
      (setq j (1+ j)))
    md5raw))

;; (mew-hmac-md5 "what do ya want for nothing?" "Jefe")
;; -> 0x750c783e6ab0b503eaa86e310a5db738

(defun mew-string-to-octets (str)
  "Convert STR into a unibyte string of octets.
MD5 and HMAC are defined on octets, so a multibyte string has to be
encoded first.  UTF-8 is used because SASL says so."
  (if (mew-multibyte-string-p str)
      (mew-cs-encode-string str 'utf-8)
    str))

(defun mew-hmac-md5 (message key)
  "HMAC-MD5 defined in RFC 2104"
  (let* ((ipad 54) ;; 0x36
	 (opad 92) ;; 0x5c
	 (blocksize 64) ;; B in RFC 2104
	 (ikey (make-string blocksize 0))
	 keylen okey digest)
    (setq message (mew-string-to-octets message))
    (setq key (mew-string-to-octets key))
    ;; RFC 2104 says that a key longer than the block size is
    ;; hashed first.
    (if (> (length key) blocksize)
	(setq key (mew-md5-raw key)))
    (setq keylen (length key))
    (dotimes (i keylen)
      (aset ikey i (aref key i)))
    (setq okey (copy-sequence ikey))
    (dotimes (i blocksize)
      (aset ikey i (logxor (aref ikey i) ipad))
      (aset okey i (logxor (aref okey i) opad)))
    (setq digest (mew-md5-raw (concat ikey message)))
    (mew-md5 (concat okey digest))))

(defun mew-cram-md5 (user passwd b64-challenge)
  "CRAM-MD5 defined in RFC 2195"
  (let* ((challenge (mew-base64-decode-string b64-challenge))
	 (response (mew-hmac-md5 challenge passwd)))
    (mew-base64-encode-string (format "%s %s" user response))))

;;

(defun mew-keyed-md5 (key passwd)
  (mew-md5 (concat key passwd)))

(provide 'mew-auth)

;;; Copyright Notice:

;; Copyright (C) 2000-2023 Mew developing team.
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

;;; mew-auth.el ends here
