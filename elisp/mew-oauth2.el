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

(defun mew-oauth2-redirect-handler-sentinel (proc event)
  )

(defun mew-oauth2-redirect-handler-filter (proc string)
  (if (string-match "^GET .*[?&]code=\\([^&]+\\)" string)
      (let ((code (match-string 1 string)))
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
    (condition-case nil
	(progn
	  (mew-oauth2-setup-redirect-handler port)
	  (browse-url url-params)
	  (mew-rendezvous (null mew-oauth2-code))
	  ;; fixme condition-case
	  (mew-oauth2-cleanup-redirect-handler port)
	  mew-oauth2-code)
      (error "")
      (quit ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Getting access_token with authorization code
;;;

(defun mew-oauth2-get-access-token (url client-id client-secret redirect-url code verifier)
  (let ((params (concat 
		 "grant_type=authorization_code"
		 "&code=" code
		 "&code_verifier=" verifier
		 "&client_id=" client-id
		 "&client_secret=" client-secret
		 "&redirect_uri=" (url-hexify-string redirect-url))))
    (with-temp-buffer
      (call-process "curl" nil t nil "-XPOST" url "--data" params "--silent")
      (goto-char (point-min))
      (json-parse-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Getting access_token with refresh_token
;;;

(defun mew-oauth2-refresh-access-token (url client-id client-secret refresh-token)
  (let ((params (concat 
		 "grant_type=refresh_token"
		 "&client_id=" client-id
		 "&client_secret=" client-secret
		 "&refresh_token=" refresh-token)))
    (with-temp-buffer
      (call-process "curl" nil t nil "-XPOST" url "--data" params "--silent")
      (goto-char (point-min))
      (json-parse-buffer))))

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
    (if json-status
        (if (string-match "^2" (cdr (assoc 'status json-status)))
            "OK" ;; 2XX
          "NO") ;; XXX: Anyway NO?
      "OK"))) ;; XXX: Maybe OK if not JSON.

(defun mew-xoauth2-auth-string (user tag)
  (mew-passwd-setup-master)
  (let* ((tk (mew-passwd-get-passwd tag))
         (token (if (hash-table-p tk) tk (make-hash-table)))
         (access-token (mew-xoauth2-get-access-token token)))
    (mew-passwd-set-passwd tag token)
    ;; base64(user=user@example.com^Aauth=Bearer ya29vF9dft4...^A^A)
    (base64-encode-string (format "user=%s\1auth=Bearer %s\1\1" user access-token) t)))

(defun mew-xoauth2-get-access-token (token)
  (let* ((expire (gethash :expire token))
	 (access-token (gethash :access_token token))
	 (refresh-token (gethash :refresh_token token)))
    (cond
     ((and access-token (time-less-p (current-time) expire))
      access-token)
     (refresh-token
      (let* ((json (mew-oauth2-refresh-access-token
		    mew-oauth2-token-url
		    mew-oauth2-client-id
		    mew-oauth2-client-secret
		    refresh-token))
	     (expires-in (gethash "expires_in" json))
	     (refresh-token (gethash "refresh_token" json)))
	(setq access-token (gethash "access_token" json))
	(setq expire (if expires-in (time-add (- expires-in 100) (current-time)) nil))
	(puthash :access_token access-token token)
	(if refresh-token (puthash :refresh_token refresh-token token))
	(puthash :expire expire token)
	access-token))
     (t
      (let* ((verifier (mew-oauth2-pkce-code-verifier))
	     (challenge (mew-oauth2-pkce-code-challenge verifier))
	     (auth-code (mew-oauth2-get-auth-code
			 mew-oauth2-auth-url
			 mew-oauth2-client-id
			 mew-oauth2-resource-url
			 mew-oauth2-redirect-url
			 challenge
			 8080))
	     (json (mew-oauth2-get-access-token 
		    mew-oauth2-token-url
		    mew-oauth2-client-id
		    mew-oauth2-client-secret
		    mew-oauth2-redirect-url
		    auth-code
		    verifier))
	     (expires-in (gethash "expires_in" json)))
	(setq access-token (gethash "access_token" json))
	(setq refresh-token (gethash "refresh_token" json))
	(setq expire (if expires-in (time-add (- expires-in 100) (current-time)) nil))
	(puthash :access_token access-token token)
	(puthash :refresh_token refresh-token token)
	(puthash :expire expire token)
	access-token)))))
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

;;; mew-summary.el ends here
