;;; mew-cache.el --- Cache management for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cache info
;;;

(defvar mew-cinfo-list '("fld" "msg" "time" "size" "decode-broken"))

(mew-blinfo-defun 'mew-cinfo mew-cinfo-list)

(defun mew-cinfo-set (fld msg time size decode-broken)
  (mew-cinfo-set-fld fld)
  (mew-cinfo-set-msg msg)
  (mew-cinfo-set-time time)
  (mew-cinfo-set-size size)
  (mew-cinfo-set-decode-broken decode-broken))

(defun mew-cinfo-equal (fld msg time size)
  (and (string= (mew-cinfo-get-fld) fld)
       (string= (mew-cinfo-get-msg) msg)
       (equal (mew-cinfo-get-time) time)
       (eq (mew-cinfo-get-size) size)))

(defun mew-cache-dinfo-get-decode-broken (buf)
  (when buf
    (with-current-buffer buf
      (mew-cinfo-get-decode-broken))))

(defvar mew-xinfo-list
  '("decode-err" "warning" "info" "action" "multi-form" "icon-spec"
    "pri-result" "not-decrypted" "text-body"))

(mew-blinfo-defun 'mew-xinfo mew-xinfo-list)

(defun mew-xinfo-copy (buf)
  (set 'mew-xinfo (with-current-buffer buf (symbol-value 'mew-xinfo))))

(defun mew-xinfo-clear ()
  (set 'mew-xinfo nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Prepare new message --- caching
;;

(defun mew-cache-decode-syntax (buf)
  (with-current-buffer buf mew-decode-syntax))

(defvar mew-cache nil
  "A list of decoded messages cache.
The (new ... old) order of ((\"+folder\" . \"message\") . cache-buffer)")

(defun mew-cache-key (fld msg)
  (cons fld msg))

(defun mew-cache-buffer-get (entry)
  (cdr entry))

(defun mew-cache-entry-make (fld msg buf)
  (cons (mew-cache-key fld msg) buf))

(defun mew-cache-get (fld msg)
  (assoc (mew-cache-key fld msg) mew-cache))

(defun mew-cache-hit (fld msg &optional must-hit)
  "Return the buffer associated with FLD and MSG.
If no cache entry is found, nil is returned.
If a cache entry is invalid, the entry is removed and nil is returned.
If MUST-HIT is non-nil and no valid cache entry is found, an error occurs."
  (let ((entry (mew-cache-get fld msg))
	cache file time size ok)
    (if (null entry)
	(if must-hit
	    (error "Cache not found")
	  nil)
      (setq cache (mew-cache-buffer-get entry))
      (setq file (mew-expand-msg fld msg))
      (unless (file-readable-p file)
	(error "%s does not exist" (mew-concat-folder fld msg)))
      (setq time (mew-file-get-time file))
      (setq size (mew-file-get-size file))
      (with-current-buffer cache
	(setq ok (mew-cinfo-equal fld msg time size)))
      (if ok
	  (progn
	    (mew-cache-sort entry)
	    cache)
	(if must-hit
	    (error "Cache not found")
	  (mew-cache-delete2 fld msg)
	  nil)))))

(defun mew-cache-sort (entry)
  (setq mew-cache (cons entry (delq entry mew-cache))))

(defun mew-cache-add (fld msg)
  (let ((len (length mew-cache))
	buf)
    (if (< len mew-cache-size)
	(setq buf (get-buffer-create (format "%s%d" mew-buffer-cache len)))
      (setq buf (mew-cache-buffer-get (nth (1- len) mew-cache)))
      (setcdr (nthcdr (- len 2) mew-cache) nil))
    (setq mew-cache (cons (mew-cache-entry-make fld msg buf) mew-cache))
    buf))

(defun mew-cache-delete ()
  "Delete the most recent cache entry."
  (let ((buf (mew-cache-buffer-get (car mew-cache))))
    ;; must preserve the buffer itself because the buffer creation
    ;; depends on the length of mew-cache.
    (setq mew-cache (nconc (cdr mew-cache)
			   (list (mew-cache-entry-make nil nil buf))))))

(defun mew-cache-delete2 (fld msg)
  "Delete the specific cache entry."
  (let ((entry (mew-cache-get fld msg)) buf)
    (if (null entry)
	()
      (setq buf (mew-cache-buffer-get entry))
      (setq mew-cache (delq entry mew-cache))
      (setq mew-cache (nconc mew-cache
			     (list (mew-cache-entry-make nil nil buf)))))))

(defvar mew-fields-xcc
  (list mew-fcc: mew-dcc: mew-bcc:
	mew-resent-fcc: mew-resent-bcc: mew-resent-dcc:))

(defun mew-xinfo-get-xcc (fld msg)
  (when (or (mew-folder-queuep fld) (mew-folder-postqp fld))
    (let ((info (concat (mew-expand-msg fld msg) mew-queue-info-suffix))
	  (headers mew-fields-xcc)
	  data addr xinfo)
      (when (and (file-readable-p info)
		 (setq data (aref (mew-lisp-load info) 0)))
	(with-temp-buffer
	  (insert data)
	  (goto-char (point-min))
	  (dolist (header headers)
	    (when (setq addr (mew-header-get-value header))
	      (setq xinfo (cons (format "%s %s\n" header addr) xinfo)))))
	(nreverse xinfo)))))

(defun mew-disable-alternative-check ()
  (let ((xmailer (mew-header-get-value mew-x-mailer:))
	(regexs mew-disable-alternative-regex-list))
    (if (null xmailer)
	t
      (catch 'loop
	(dolist (regex regexs t)
	  (if (string-match regex xmailer)
	      (throw 'loop nil)))))))

(defun mew-cache-message (fld msg &optional unlimit no-err)
  "Cache the message specified by FLD and MSG.
If an invalid message are cached, deletes it and caches the message again.
If UNLIMIT is non-nil, decodes the message to be cached without
the limitations. If NO-ERR is non-nil, an error is caused
if decode fails."
  (let* ((cbuf (current-buffer))
	 (cache (mew-cache-hit fld msg))
	 (use-alternative mew-use-alternative)
	 tim-siz decode errormsg)
    (catch 'return
      (if cache
	  (progn
	    (set-buffer cache)
	    ;; Decryption may fail if password is wrong. So, try
	    ;; to decode this again.
	    (if (or (and unlimit (mew-xinfo-get-not-decrypted))
		    (and unlimit (mew-xinfo-get-decode-err)))
		;; cache is invalid
		(setq decode t)))
	(setq cache (mew-cache-add fld msg))
	(setq decode t))
      (if (not decode) (throw 'return nil))
      ;;
      (set-buffer cache)
      ;; in cache buffer
      (mew-erase-buffer)
      (condition-case errmsg
	  (setq tim-siz	(mew-insert-message fld msg mew-cs-text-for-read nil))
	(error
	 ;; file not exist
	 (mew-cache-delete)
	 (setq errormsg (nth 1 errmsg))
	 (throw 'return (setq cache nil))))
      (mew-cinfo-set fld msg (car tim-siz) (cdr tim-siz) mew-decode-broken)
      (if (and use-alternative mew-disable-alternative-regex-list)
	  (setq use-alternative (mew-disable-alternative-check)))
      (mew-dinfo-set nil t t use-alternative)
      (mew-decode-syntax-clear)
      (mew-xinfo-set-text-body mew-use-text-body)
      (condition-case nil
	  (if unlimit
	      (let ((mew-header-max-length nil)
		    (mew-header-max-depth nil))
		(mew-decode))
	    (mew-decode))
	;; Don't put error handing here. Because (mew-decode) would
	;; set debug-on-error to t.
	(quit
	 ;; prefetching an encrypted message
	 (mew-cache-delete)
	 ;; The following message is not friendly to users.
	 ;; (message "MIME decoding for %s/%s aborted" fld msg)
	 (throw 'return (setq cache nil))))
      (mew-ainfo-set-icon msg)
      (mew-xinfo-set-info (append (mew-xinfo-get-info) (mew-xinfo-get-xcc fld msg)))
      (mew-decode-syntax-set))
    ;;
    (set-buffer cbuf)
    (if errormsg (if no-err (message "%s" errormsg) (error "%s" errormsg)))
    cache)) ;; return value

(defun mew-cache-clean-up ()
  "A function to flush all decoded messages in cache list."
  (interactive)
  (dotimes (n mew-cache-size)
    (mew-kill-buffer (format "%s%d" mew-buffer-cache n)))
  (mew-summary-reset)
  (setq mew-cache nil))

(defalias 'mew-cache-flush 'mew-cache-clean-up)

(provide 'mew-cache)

;;; Copyright Notice:

;; Copyright (C) 1997-2012 Mew developing team.
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

;;; mew-cache.el ends here
