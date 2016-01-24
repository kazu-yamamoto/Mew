;;; mew-passwd.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: May 23, 2006

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-prog-passwd "gpg")
(defvar mew-passwd-file ".mew-passwd.gpg")
(defvar mew-passwd-cipher "AES")
(defvar mew-passwd-repeat 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal variables
;;;

(defvar mew-passwd-encryption-name "GPG Encryption")
(defvar mew-passwd-decryption-name "GPG Decryption")

(defvar mew-passwd-master nil)
(defvar mew-passwd-alist nil)
(defvar mew-passwd-timer-id nil)
(defvar mew-passwd-rendezvous nil)
(defvar mew-passwd-agent-hack nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; External functions
;;;

(defun mew-passwd-get-passwd (key)
  (nth 1 (assoc key mew-passwd-alist)))

(defun mew-passwd-set-passwd (key val)
  (if (assoc key mew-passwd-alist)
      (setcar (nthcdr 1 (assoc key mew-passwd-alist)) val)
    (setq mew-passwd-alist (cons (list key val 0) mew-passwd-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions
;;;

(defun mew-passwd-get-counter (key)
  (nth 2 (assoc key mew-passwd-alist)))

(defun mew-passwd-set-counter (key val)
  (if (assoc key mew-passwd-alist)
      (setcar (nthcdr 2 (assoc key mew-passwd-alist)) val)))

(defun mew-passwd-get-keys ()
  (mapcar 'car mew-passwd-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GPG version
;;;

(defun mew-passwd-gpg-version ()
  (when (mew-which mew-prog-passwd exec-path)
    (with-temp-buffer
      (call-process mew-prog-passwd nil t nil "--version")
      (goto-char (point-min))
      (re-search-forward "(GnuPG) " nil t)
      (if (looking-at "\\([0-9]+\\)\\.\\([0-9]+\\)")
	  (list
	   (string-to-number (match-string 1))
	   (string-to-number (match-string 2)))))))

(defun mew-passwd-check-agent-hack ()
  (let ((ver (mew-passwd-gpg-version)))
    (when ver
      (let ((major (nth 0 ver))
	    (minor (nth 1 ver)))
	(cond
	 ((= major 1) nil)
	 ((= major 2) (>= minor 1))
	 (t t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setup and cleanup
;;;

(defun mew-passwd-setup ()
  (when (and mew-use-cached-passwd (not mew-use-master-passwd))
    (if mew-passwd-timer-id (cancel-timer mew-passwd-timer-id))
    (setq mew-passwd-timer-id
	  (mew-timer (* mew-passwd-timer-unit 60) 'mew-passwd-timer))))

(defun mew-passwd-setup-master ()
  (when (and (not mew-passwd-master) mew-use-master-passwd)
    (setq mew-passwd-agent-hack (mew-passwd-check-agent-hack))
    (let ((file (expand-file-name mew-passwd-file mew-conf-path)))
      (if (file-exists-p file)
	  (setq mew-passwd-alist (mew-passwd-load))
	;; save nil and ask master twice
	(mew-passwd-save)))
    (add-hook 'kill-emacs-hook 'mew-passwd-clean-up)))

(defun mew-passwd-clean-up ()
  (remove-hook 'kill-emacs-hook 'mew-passwd-clean-up)
  (when mew-passwd-master
    (mew-passwd-save))
  (setq mew-passwd-master nil)
  (when (and mew-use-cached-passwd (not mew-use-master-passwd))
    (setq mew-passwd-alist nil)
    (if mew-passwd-timer-id (cancel-timer mew-passwd-timer-id))
    (setq mew-passwd-timer-id nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Timer
;;;

(defun mew-passwd-timer ()
  (let ((keys (mew-passwd-get-keys)))
    (dolist (key keys)
      (if (< (mew-passwd-get-counter key) mew-passwd-lifetime)
	  (mew-passwd-set-counter key (1+ (mew-passwd-get-counter key)))
	;; time out
	(mew-passwd-set-passwd key nil)
	(mew-passwd-set-counter key 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input
;;;

(defun mew-input-passwd (prompt key)
  (if (and key (or mew-use-cached-passwd mew-use-master-passwd))
      (progn
	(mew-passwd-setup-master)
	(if (mew-passwd-get-passwd key)
	    (progn
	      (mew-timing)
	      (if mew-passwd-reset-timer (mew-passwd-set-counter key 0))
	      (mew-passwd-get-passwd key))
	  (let ((pass (mew-read-passwd prompt)))
	    (mew-passwd-set-passwd key pass)
	    (mew-passwd-set-counter key 0)
	    pass)))
    (mew-read-passwd prompt)))

(defun mew-read-passwd (prompt)
  (let ((inhibit-input-event-recording t)
	;; A process filter sets inhibit-quit to t to prevent quitting.
	;; Set inhibit-quit to nil so that C-g can be used
	(inhibit-quit nil))
    (condition-case nil
	(read-passwd prompt)
      ;; If read-passwd causes an error, let's return "" so that
      ;; the password process will safely fail.
      (quit "")
      (error ""))))

(defun mew-passwd-read-passwd (prompt &optional encrypt-p)
  (if mew-passwd-master
      (progn
	(mew-timing)
	mew-passwd-master)
    (let ((pass (mew-read-passwd prompt)))
      (unless encrypt-p (setq mew-passwd-master pass))
      pass)))

(defun mew-passwd-change ()
  "Change the master password."
  (interactive)
  (setq mew-passwd-master nil)
  (mew-passwd-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load and save
;;;

(defmacro mew-passwd-rendezvous (pro)
  `(condition-case nil
       (let ((inhibit-quit nil))
	 (setq mew-passwd-rendezvous t)
	 (while mew-passwd-rendezvous
	   (accept-process-output ,pro 0.1 nil t)))
     (quit
      (setq mew-passwd-rendezvous nil))))

(defun mew-passwd-load ()
  (let* ((process-connection-type mew-connection-type2)
	 (file (expand-file-name mew-passwd-file mew-conf-path))
	 (tfile (mew-make-temp-name "gpg-load"))
	 (args (mew-passwd-adjust-args (list "-d" "--yes" "--output" tfile file)))
	 (N mew-passwd-repeat)
	 pwds pro)
    (unwind-protect
	(with-temp-buffer
	  (catch 'loop
	    (dotimes (i N)
	      (when mew-passwd-agent-hack (mew-passwd-clear-passphrase file))
	      (setq pro (apply 'mew-start-process-lang
			       mew-passwd-decryption-name
			       (current-buffer)
			       mew-prog-passwd
			       args))
	      (set-process-filter   pro 'mew-passwd-filter)
	      (set-process-sentinel pro 'mew-passwd-sentinel)
	      (mew-passwd-rendezvous pro)
	      (unless (file-exists-p tfile)
		(setq mew-passwd-master nil))
	      (when mew-passwd-master
		(let ((coding-system-for-read 'undecided))
		  (insert-file-contents tfile))
		(condition-case nil
		    (setq pwds (read (current-buffer)))
		  (error ()))
		(throw 'loop nil)))
	    (message "Master password is wrong!")
	    (mew-let-user-read)))
      (mew-passwd-delete-file tfile))
    pwds))

(defun mew-passwd-save ()
  (let* ((process-connection-type mew-connection-type2)
	 (file (expand-file-name mew-passwd-file mew-conf-path))
	 (tfile (mew-make-temp-name "gpg-save"))
	 (args (mew-passwd-adjust-args (list "-c"
					     "--cipher-algo" mew-passwd-cipher
					     "--yes" "--output" file tfile)))
	 (N mew-passwd-repeat)
	 pro)
    (if (file-exists-p file)
	(rename-file file (concat file mew-backup-suffix) 'override))
    (unwind-protect
	(with-temp-buffer
	  (pp mew-passwd-alist (current-buffer))
	  (write-region (point-min) (point-max) tfile nil 'no-msg)
	  (catch 'loop
	    (dotimes (i N)
	      (setq pro (apply
			 'mew-start-process-lang
			 mew-passwd-encryption-name
			 (current-buffer)
			 mew-prog-passwd
			 args))
	      (set-process-filter   pro 'mew-passwd-filter)
	      (set-process-sentinel pro 'mew-passwd-sentinel)
	      (mew-passwd-rendezvous pro)
	      (if (file-exists-p file) (throw 'loop nil)))
	    (message "Master password is wrong! Passwords not saved")
	    (mew-let-user-read)))
      (unless (file-exists-p file)
	(rename-file (concat file mew-backup-suffix) file))
      (mew-passwd-delete-file tfile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-passwd-filter (process string)
  (let* ((name (process-name process))
	 (regex (concat "^" (regexp-quote mew-passwd-encryption-name)))
	 (encrypt-p (string-match regex name)))
    (with-current-buffer (process-buffer process)
      (cond
       ((string-match "invalid passphrase" string)
	(mew-warn "Master password mismatch!")
	(setq mew-passwd-master nil))
       ((string-match "[bB]ad \\(?:session \\)?key" string)
	(mew-warn "Master password is wrong!")
	(setq mew-passwd-master nil))
       ((string-match "Enter passphrase:" string)
	(process-send-string process (mew-passwd-read-passwd (if encrypt-p "New master password: " "Master password: ") encrypt-p))
	(process-send-string process "\n"))
       ((string-match "Repeat passphrase:" string)
	(process-send-string process (mew-passwd-read-passwd "New master password again: "))
	(process-send-string process "\n"))
       ((string-match "exiting" string)
	(setq mew-passwd-rendezvous nil))))))

(defun mew-passwd-sentinel (process event)
  (setq mew-passwd-rendezvous nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defun mew-passwd-delete-file (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (let ((coding-system-for-write 'binary)
	    (size (mew-file-get-size file)))
	(dotimes (i size)
	  (insert 0))
	(write-region (point-min) (point-max) file nil 'no-msg)))
    (delete-file file)))

(defun mew-passwd-get-cache-id (file)
  (with-temp-buffer
    (call-process mew-prog-passwd nil t nil "--list-packets" file)
    (goto-char (point-min))
    (when (re-search-forward "salt \\([^ ,]+\\)," nil t)
      (concat "S" (match-string 1)))))

(defun mew-passwd-clear-passphrase (file)
  (when (file-exists-p file)
    (let ((cache-id (mew-passwd-get-cache-id file)))
      (with-temp-buffer
	(insert "CLEAR_PASSPHRASE " cache-id "\n")
	(call-process-region (point-min) (point-max) "gpg-connect-agent")))))

(defun mew-passwd-adjust-args (args)
  (if mew-passwd-agent-hack
      (cons "--pinentry-mode" (cons "loopback" args))
    args))

(provide 'mew-passwd)

;;; Copyright Notice:

;; Copyright (C) 1996-2015 Mew developing team.
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

;;; mew-passwd.el ends here
