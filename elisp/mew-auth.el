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

(defun mew-hmac-md5 (message key)
  "HMAC-MD5 defined in RFC 2104"
  (let* ((keylen (length key))
	 (ipad 54) ;; 0x36
	 (opad 92) ;; 0x5c
	 (ikey (make-string 64 0))
	 okey digest)
    (when (< keylen 64)
      (dotimes (i keylen)
	(aset ikey i (aref key i))))
    (setq okey (copy-sequence ikey))
    (dotimes (i 64)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XOAUTH2

(defun mew-auth-xoauth2-auth-string (user token)
  ;; base64(user=user@example.com^Aauth=Bearer ya29vF9dft4...^A^A)
  (base64-encode-string (format "user=%s\1auth=Bearer %s\1\1" user token) t))

(defun mew-auth-xoauth2-json-status (status-string)
  ;; https://developers.google.com/gmail/imap/xoauth2-protocol#error_response_2
  (require 'json)
  (let ((json-status
         (ignore-errors
           (json-read-from-string
            (base64-decode-string status-string)))))
    (if json-status
        (if (string-match "^2" (cdr (assoc 'status json-status)))
            "OK" ;; 2XX
          "NO") ;; XXX: Anyway NO?
      "OK"))) ;; XXX: Maybe OK if not JSON.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OAuth2

(defvar mew-auth-oauth2-client-id
  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.apps.googleusercontent.com")

(defvar mew-auth-oauth2-client-secret
  "xxxxxxxxxxxxxxxxxxxxxxxx")

(defvar mew-auth-oauth2-auth-url "https://accounts.google.com/o/oauth2/auth"
  "OAuth2 auth server URL.")

(defvar mew-auth-oauth2-token-url "https://accounts.google.com/o/oauth2/token"
  "OAuth2 token server URL.")

(defvar mew-auth-oauth2-resource-url "https://mail.google.com/"
  "URL used to request access to Mail Resources.")

(defvar mew-auth-oauth2-redirect-url nil
  "URL used to OAuth redirect url.")

(declare-function oauth2-auth-and-store "oauth2")
(declare-function oauth2-refresh-access "oauth2")
(declare-function oauth2-token-access-token "oauth2")

(defun mew-auth-oauth2-auth-and-store
    (resource-url client-id client-secret &optional redirect-url)
  "Request access to a mail resource and store it using `auth-source'."
  (require 'oauth2)
  (oauth2-auth-and-store
   mew-auth-oauth2-auth-url
   mew-auth-oauth2-token-url
   resource-url
   client-id
   client-secret
   redirect-url))

(defun mew-auth-oauth2-token ()
  "Get OAuth token for Mew to access mail service."
  (require 'oauth2)
  (let ((token (mew-auth-oauth2-auth-and-store
                mew-auth-oauth2-resource-url
                mew-auth-oauth2-client-id
                mew-auth-oauth2-client-secret
                mew-auth-oauth2-redirect-url)))
    (oauth2-refresh-access token)
    token))

(defun mew-auth-oauth2-token-access-token ()
  (require 'oauth2)
  (ignore-errors (oauth2-token-access-token (mew-auth-oauth2-token))))

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
