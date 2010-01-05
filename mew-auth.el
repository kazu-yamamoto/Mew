;;; mew-auth.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
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

(provide 'mew-auth)

;;; Copyright Notice:

;; Copyright (C) 2000-2010 Mew developing team.
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
