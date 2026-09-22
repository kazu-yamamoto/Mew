;;; -*- lexical-binding: nil; -*-
;;; mew-oauth2.el -- OAuth for Mew

;; Author:  Mew developing team
;; Created: Apri 28, 2024

;;; Code:

(require 'mew)
(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;
(defvar mew-oauth2-info-list
  '("code"
    "client-id" "client-secret"
    "redirect-url" "redirect-port"
    "auth-url" "token-url" "resource-url"
    ))

(mew-info-defun "mew-oauth2-" mew-oauth2-info-list)

(defvar mew-prog-curl "curl")

(defvar mew-oauth2-client-id nil)

(defvar mew-oauth2-client-secret nil)

;;;

(defvar mew-oauth2-redirect-url "http://localhost:8080")

(defvar mew-oauth2-redirect-port 8080)

;;; gmail

(defvar mew-oauth2-auth-url "https://accounts.google.com/o/oauth2/auth"
  "OAuth2 auth server URL.")

(defvar mew-oauth2-token-url "https://accounts.google.com/o/oauth2/token"
  "OAuth2 token server URL.")

(defvar mew-oauth2-resource-url "https://mail.google.com/"
  "URL used to request access to Mail Resources.")

;;; MS356

;;(setq mew-oauth2-auth-url "https://login.microsoftonline.com/organizations/oauth2/v2.0/authorize")
;;(setq mew-oauth2-token-url "https://login.microsoftonline.com/organizations/oauth2/v2.0/token")
;;(setq mew-oauth2-resource-url "https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access")

(defun mew-oauth2-debug (label string)
  (when (mew-debug 'oauth2)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (let ((start (point)))
	(insert (format "\n<%s>\n%s\n" label string))
	(goto-char start)
	(mew-crlf-to-lf))
      (goto-char (point-max)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HTTP/1.1 server to get "code"
;;;

(defvar mew-oauth2-code nil)

(defun mew-oauth2-setup-redirect-handler (port)
  "Setup OAuth2 redirect server bound to PORT.
It serves http://localhost:PORT"
  (setq mew-oauth2-code nil)
  (let ((proc-name (format "oauth2-redirect-handler:%d" port)))
    (unless (process-status proc-name)
      (make-network-process
       :name proc-name
       :buffer nil
       :host 'local
       :service port
       :sentinel 'mew-oauth2-redirect-handler-sentinel
       :filter 'mew-oauth2-redirect-handler-filter
       :server t))))

(defun mew-oauth2-cleanup-redirect-handler (port)
  "Clean up OAuth2 redirect servers both LISTEN and ACCEPT."
  (let ((proc-name (format "oauth2-redirect-handler:%d" port)))
    (mapcar (lambda (p)
              (if (string= (process-contact p :name) proc-name)
                  (delete-process p)))
            (process-list))))

(defun mew-oauth2-redirect-handler-sentinel (_proc _event)
  )

(defun mew-oauth2-query-decode (str)
  "Decode STR, one value taken from a query string.
\"+\" stands for a space there, which `url-unhex-string' does not know."
  (url-unhex-string (subst-char-in-string ?+ ?\s str)))

(defun mew-oauth2-redirect-handler-filter (proc string)
  (if (string-match "^GET .*[?&]code=\\([^& ]+\\)" string)
      (let ((code (mew-oauth2-query-decode (match-string 1 string))))
        (process-send-string
         proc
	 (concat "HTTP/1.1 200 OK\r\n"
                 "Content-Type: text/plain\r\n"
                 "\r\n"
                 "Mew gets the following authorization code:\n"
		 code "\n"))
	(setq mew-oauth2-code code)
	(delete-process proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Getting authorization code
;;;

(defun mew-oauth2-get-auth-code (url client-id resource-url redirect-url challenge port)
  (let ((url-params
	 (concat
	  url
	  "?response_type=code"
	  "&client_id=" client-id
	  "&scope=" (url-hexify-string resource-url)
	  "&redirect_uri=" (url-hexify-string redirect-url)
	  "&code_challenge=" challenge
	  "&code_challenge_method=S256")))
    (mew-oauth2-cleanup-redirect-handler port)
    ;; The listening socket has to go even when the user gives up, or
    ;; the port stays taken for the rest of the session.
    (unwind-protect
	(condition-case nil
	    (progn
	      (mew-oauth2-setup-redirect-handler port)
	      (browse-url url-params)
	      (mew-rendezvous (null mew-oauth2-code))
	      mew-oauth2-code)
	  (error "")
	  (quit ""))
      (mew-oauth2-cleanup-redirect-handler port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Getting access_token with authorization code
;;;

(defun mew-oauth2-params (alist)
  "Make a form-urlencoded body out of ALIST of (KEY . VALUE).
A VALUE of nil is sent as the empty string.  Without the encoding, a
client secret containing \"+\" or \"&\" would arrive mangled."
  (mapconcat (lambda (kv)
	       (concat (car kv) "=" (url-hexify-string (or (cdr kv) ""))))
	     alist "&"))

(defun mew-oauth2-post (url params)
  "POST PARAMS to URL with curl and return the parsed JSON.
Return nil if curl is missing or if the answer is not JSON, so that
the caller can ask for a new authorization instead of sending a token
which does not exist."
  (cond
   ((not (mew-which-exec mew-prog-curl))
    (message "%s does not exist" mew-prog-curl)
    nil)
   (t
    (with-temp-buffer
      (call-process mew-prog-curl nil t nil
		    "-XPOST" url "--data" params "--silent")
      (goto-char (point-min))
      (condition-case nil
	  (json-parse-buffer)
	(error
	 (message "OAuth2: the token server did not answer with JSON")
	 nil))))))

(defun mew-oauth2-get-access-token (url client-id client-secret redirect-url code verifier)
  (mew-oauth2-post
   url
   (mew-oauth2-params
    (list (cons "grant_type"    "authorization_code")
	  (cons "code"          code)
	  (cons "code_verifier" verifier)
	  (cons "client_id"     client-id)
	  (cons "client_secret" client-secret)
	  (cons "redirect_uri"  redirect-url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Getting access_token with refresh_token
;;;

(defun mew-oauth2-refresh-access-token (url client-id client-secret refresh-token)
  (mew-oauth2-post
   url
   (mew-oauth2-params
    (list (cons "grant_type"    "refresh_token")
	  (cons "client_id"     client-id)
	  (cons "client_secret" client-secret)
	  (cons "refresh_token" refresh-token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface
;;;

(defun mew-xoauth2-json-status (status-string)
  ;; https://developers.google.com/gmail/imap/xoauth2-protocol#error_response_2
  (let ((json-status
         (ignore-errors
           (json-read-from-string
            (base64-decode-string status-string)))))
    ;; The status may be a number as well as a string, and it may not
    ;; be there at all.
    (let ((status (and (listp json-status) (cdr (assoc 'status json-status)))))
      (cond
       ((null status) "OK") ;; XXX: Maybe OK if not JSON.
       ((string-match "^2" (format "%s" status)) "OK") ;; 2XX
       (t "NO"))))) ;; XXX: Anyway NO?

(defun mew-xoauth2-auth-string (user tag case)
  (mew-passwd-setup-master)
  (let* ((tk (mew-passwd-get-passwd tag))
         (token (if (hash-table-p tk) tk (make-hash-table)))
         (access-token (mew-xoauth2-get-access-token token case)))
    (mew-passwd-set-passwd tag token)
    (mew-passwd-set-counter tag 0)
    (when mew-passwd-master
      (mew-passwd-save))
    ;; base64(user=user@example.com^Aauth=Bearer ya29vF9dft4...^A^A)
    (if access-token
	(base64-encode-string
	 (format "user=%s\1auth=Bearer %s\1\1" user access-token) t)
      ;; Sending "Bearer nil" only looks like an attempt to the server.
      (message "OAuth2: no access token for %s" tag)
      "")))

(defun mew-xoauth2-store (token json)
  "Keep the tokens of JSON in TOKEN.
The refresh token is kept only when the answer carries one, since the
server does not repeat it on every refresh."
  (let ((access-token (gethash "access_token" json))
	(refresh-token (gethash "refresh_token" json))
	(expires-in (gethash "expires_in" json)))
    (puthash :access_token access-token token)
    (if refresh-token (puthash :refresh_token refresh-token token))
    (puthash :expire (if expires-in (time-add (- expires-in 100) nil)) token)
    access-token))

(defun mew-xoauth2-refresh (token case)
  "Get a new access token with the refresh token kept in TOKEN.
Return nil when there is none or when the server refused it, which
happens once it is revoked or has expired.  TOKEN is left untouched in
that case, so that the refresh token is not thrown away."
  (let ((refresh-token (gethash :refresh_token token))
	json)
    (when refresh-token
      (setq json (mew-oauth2-refresh-access-token
		  (mew-oauth2-token-url case)
		  (mew-oauth2-client-id case)
		  (mew-oauth2-client-secret case)
		  refresh-token))
      (if (and json (gethash "access_token" json))
	  (mew-xoauth2-store token json)
	(message "OAuth2: the refresh token was refused")
	nil))))

(defun mew-xoauth2-authorize (token case)
  "Run the authorization code flow and keep the result in TOKEN."
  (let* ((verifier (mew-oauth2-pkce-code-verifier))
	 (challenge (mew-oauth2-pkce-code-challenge verifier))
	 (auth-code (mew-oauth2-get-auth-code
		     (mew-oauth2-auth-url case)
		     (mew-oauth2-client-id case)
		     (mew-oauth2-resource-url case)
		     (mew-oauth2-redirect-url case)
		     challenge
		     (mew-oauth2-redirect-port case)))
	 (json (mew-oauth2-get-access-token
		(mew-oauth2-token-url case)
		(mew-oauth2-client-id case)
		(mew-oauth2-client-secret case)
		(mew-oauth2-redirect-url case)
		auth-code
		verifier)))
    (if (and json (gethash "access_token" json))
	(mew-xoauth2-store token json)
      (message "OAuth2: no access token was given")
      nil)))

(defun mew-xoauth2-get-access-token (token case)
  (let ((access-token (gethash :access_token token))
	(expire (gethash :expire token)))
    (if (and access-token expire (time-less-p nil expire))
	access-token
      ;; A refused refresh token is not the end: ask for a new
      ;; authorization instead of going on without a token.
      (or (mew-xoauth2-refresh token case)
	  (mew-xoauth2-authorize token case)))))
;;;

;; RFC 7636 Appendix A

(defun mew-numlist-to-string (nums)
  (apply 'concat (seq-map 'unibyte-string nums)))

;; (mew-base64-url-without-padding-encode
;;  (mew-numlist-to-string '(3 236 255 224 193)))
;; "A-z_4ME"
(defun mew-base64-url-without-padding-encode (str)
  (base64url-encode-string str t))

;; RFC 7636 Appendix B

;; (setq rfc7636-random32 '(116 24 223 180 151 153 224 37 79 250 96 125 216 173 187 186 22 212 37 77 105 214 191 240 91 88 5 88 83 132 141 121))
;; (setq rfc7636-verifier (mew-base64-url-without-padding-encode
;; 			(mew-numlist-to-string rfc7636-random32)))
;; "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
(defun mew-oauth2-pkce-code-verifier ()
  (mew-base64-url-without-padding-encode (mew-random-binary-string 32)))
;; (>= (length (mew-oauth2-pkce-code-verifier)) 43)

;; (mew-oauth2-pkce-code-challenge rfc7636-verifier)
;; "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
(defun mew-oauth2-pkce-code-challenge (verifier)
  (mew-base64-url-without-padding-encode
   (secure-hash 'sha256 verifier nil nil t)))

(provide 'mew-oauth2)

;;; Copyright Notice:

;; Copyright (C) 1996-2023 Mew developing team.
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

;;; mew-oauth2.el ends here
