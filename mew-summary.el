;;; mew-summary.el --- Summary mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary info
;;;

(defvar mew-sinfo-list
  '(;; parameters to be saved
    "refile"  "mark-hist"
    ;; parameters used internally
    "scan-id" "scan-md5" "find-key" "cursor-line" "direction" "start-point"
    "cache-time" "summary-form" "summary-column" "unread-mark" "refile-back"
    "disp-msg" "case" "folder" "proto" "mid-marker" "mid-line" "ttl-line"
    "mark-review" "ret-pos" "inboxp" "column"))

(mew-blinfo-defun 'mew-sinfo mew-sinfo-list)

(defvar mew-sinfo-list-save-length 2)
(defvar mew-sinfo-file ".mew-sinfo")

(defun mew-sinfo-save ()
  (when (mew-summary-p)
    (let* ((n mew-sinfo-list-save-length)
	   (data (make-vector n nil))
	   (bnm (mew-summary-folder-name 'ext))
	   (file (mew-expand-file bnm mew-sinfo-file)))
      (dotimes (i n)
	(aset data i (aref mew-sinfo i)))
      (mew-lisp-save file data nil 'unlimit))))

(defun mew-sinfo-load ()
  (when (mew-summary-p)
    (let* ((n mew-sinfo-list-save-length)
	   (bnm (mew-summary-folder-name 'ext))
	   (file (mew-expand-file bnm mew-sinfo-file))
	   data)
      (setq data (mew-lisp-load file))
      (when data
	(dotimes (i n)
	  (aset mew-sinfo i (aref data i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros
;;;

(defmacro mew-summary-msg-or-part (&rest body)
  "See if the cursor is on a message or a part."
  `(cond
    ((eobp) (message "No message"))
    ((not (or (mew-summary-message-number) (mew-syntax-number)))
     (message "No message"))
    (t ,@body)))

(defmacro mew-summary-msg (&rest body)
  "See if the cursor is on a message."
  `(cond
    ((eobp) (message "No message"))
    ((not (mew-summary-message-number))
     (message "Please use this command on a message, not a part"))
    (t ,@body)))

(defmacro mew-summary-part (&rest body)
  "See if the cursor is on a part."
  `(cond
    ((eobp) (message "No part"))
    ((not (mew-syntax-number))
     (message "Please use this command on a part, not a message"))
    (t ,@body)))

(defmacro mew-summary-multi-msgs (&rest body)
  "Collect messages marked with '*' and set their corresponding
files to FILES."
  `(let* ((FLD-MSGS (mew-summary-mark-collect2 mew-mark-review))
	  (FLD-MSG-LIST FLD-MSGS) ;; may be used in body
	  FILES) ;; may be used in body
     (cond
      ((null FLD-MSGS)
       (message "No %c marks" mew-mark-review))
      (t
       ;; a little bit complicated because of Virtual mode
       (dolist (fld-msg FLD-MSGS)
	 (setq FILES (cons (mew-expand-msg (car fld-msg) (cdr fld-msg))
			   FILES)))
       (setq FILES (nreverse FILES))
       ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subfunctions
;;;

(defun mew-summary-local-p ()
  (mew-folder-localp (mew-summary-folder-name 'ext)))

(defun mew-summary-draft-p ()
  (mew-folder-draftp (mew-summary-folder-name 'ext)))

(defun mew-summary-queue-p ()
  (or (mew-folder-queuep (mew-summary-folder-name 'ext))
      (mew-folder-postqp (mew-summary-folder-name 'ext))))

(defun mew-summary-pop-p ()
  (mew-folder-popp (mew-summary-folder-name 'ext)))

(defun mew-summary-case1 ()
  (and (mew-summary-or-virtual-p) (not (mew-summary-draft-p))))

(defun mew-summary-case2 ()
  (and (mew-summary-p) (not (mew-summary-queue-p))))

(defun mew-summary-case3 ()
  (and (mew-summary-p) mew-summary-buffer-process))

(defmacro mew-summary-only (&rest body)
  "See if the mode of this buffer is Summary mode.
This macro is used to prohibit using a command in Virtual mode."
  `(cond
    ((not (mew-summary-p))
     (message "This command can be used in Summary mode only"))
    (t ,@body)))

(defmacro mew-virtual-only (&rest body)
  "See if the mode of this buffer is Virtual mode.
This macro is used to prohibit using a command in Summary mode."
  `(cond
    ((not (mew-virtual-p))
     (message "This command can be used in Virtual mode only"))
    (t ,@body)))

(defmacro mew-thread-only (&rest body)
  "See if this buffer is Thread folder.
This macro is used to prohibit using a command in Summary mode."
  `(cond
    ((not (mew-thread-p))
     (message "This command can be used in Thread folder only"))
    (t ,@body)))

(defmacro mew-pickable (&rest body)
  "See if pick can be used for this folder."
  `(cond
    ((not (mew-pickable-p))
     (message "This command cannot be used in this folder"))
    (t ,@body)))

(defmacro mew-summary-not-in-queue (&rest body)
  "See if this folder is not +queue."
  `(cond
    ((not (mew-summary-or-virtual-p))
     (message "This command cannot be used in this mode"))
    ((mew-summary-queue-p)
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body)))

(defmacro mew-summary-not-in-draft (&rest body)
  "See if this folder is not +draft."
  `(cond
    ((not (mew-summary-or-virtual-p))
     (message "This command cannot be used in this mode"))
    ((mew-summary-draft-p)
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body)))

(defmacro mew-summary-not-in-nntp (&rest body)
  "See if this folder is not NNTP."
  `(cond
    ((mew-folder-nntpp (mew-sinfo-get-folder))
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body)))

(defmacro mew-summary-local-or-imap (&rest body)
  "See if this folder is either local or IMAP."
  `(cond
    ((not (mew-summary-or-virtual-p))
     (message "This command cannot be used in this mode"))
    ((or (mew-folder-nntpp (mew-sinfo-get-folder))
	 (mew-folder-popp (mew-sinfo-get-folder)))
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body))) ;; local or IMAP

(defmacro mew-summary-local-only (&rest body)
  "See if this folder is local."
  `(cond
    ((not (mew-summary-or-virtual-p))
     (message "This command cannot be used in this mode"))
    ((not (mew-summary-local-p))
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body)))

(defmacro mew-summary-with-mewl (&rest body)
  `(cond
    ((not (mew-which-exec mew-prog-mewl))
     (message "'%s' not found!" mew-prog-mewl))
    (t ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Region
;;;

(defun mew-summary-get-region ()
  "Get a region according to 'mew-summary-region-include-cursor-line'
and return (beg . end)."
  (save-excursion
    (let ((beg (region-beginning))
	  (end (region-end)))
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (goto-char end)
      (cond
       ((eq mew-summary-region-include-cursor-line t)
	(forward-line))
       ((eq mew-summary-region-include-cursor-line 'end)
	(if (eq (char-after) ?\n)
	    (forward-line)
	  (beginning-of-line)))
       (t
	(if (> (current-column) 0)
	    (forward-line)
	  (beginning-of-line))))
      (cons beg (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dialog
;;;

(defun mew-substitute-for-summary (msg)
  (substitute-command-keys (concat "\\<mew-summary-mode-map>" msg)))

(defun mew-message-for-summary (msg)
  (message "%s" (mew-substitute-for-summary msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modeline
;;;

(defvar mew-mode-line-target "%p")
(defvar mew-mode-line-format
  `(""
    (mew-summary-buffer-left-msgs mew-summary-buffer-left-msgs "L%l")
    (mew-summary-buffer-raw "*")
    (mew-summary-buffer-secure-process ,mew-secure-format2)))

(defvar mew-mode-line-process
  '((mew-summary-buffer-secure-process mew-secure-format)
    (mew-summary-buffer-process mew-summary-buffer-process-status)))

(defun mew-summary-setup-mode-line ()
  (let ((tgt mew-mode-line-target)
	target prev pos)
    (if (boundp 'mode-line-position)
	(progn
	  (make-local-variable 'mode-line-position) ;; Emacs 21.3.50
	  (setq mode-line-position
		(copy-sequence (default-value 'mode-line-position)))
	  (setq prev mode-line-position))
      (setq mode-line-format (copy-sequence (default-value 'mode-line-format)))
      (setq prev mode-line-format))
    (setq target (or (rassoc (list tgt) prev) ;; Emacs 21.3.50
		     (rassoc tgt prev)
		     (car (member tgt prev))))
    (when target
      (setq pos (- (length prev) (length (member target prev))))
      (setcar (nthcdr pos prev) mew-mode-line-format))
    (when (boundp 'line-number-mode)
      (make-local-variable 'line-number-mode)
      (setq line-number-mode nil))
    (or (assq 'mew-summary-buffer-process mode-line-process)
	(setq mode-line-process
	      (append mew-mode-line-process mode-line-process)))))

(defun mew-summary-reset-mode-line ()
  (setq mew-summary-buffer-left-msgs nil))

(defun mew-summary-mode-name (name)
  (let ((case (if (mew-case-default-p mew-case) "" mew-case)))
    (if (string= case "")
	(setq mode-name name)
      (setq mode-name (format "%s %s" name case)))
    (force-mode-line-update)))

(defun mew-summary-mode-line ()
  (unless mew-summary-buffer-process
    (let ((pos (point))
	  (mid-point (marker-position (mew-sinfo-get-mid-marker)))
	  (mid-line (mew-sinfo-get-mid-line))
	  (ttl (mew-sinfo-get-ttl-line))
	  cur left)
      (if (< pos mid-point)
	  (if (< pos (/ mid-point 2))
	      (setq cur (mew-count-lines (point-min) pos))
	    (setq cur (- mid-line (mew-count-lines pos mid-point))))
	(if (< pos (+ mid-point (/ mid-point 2)))
	    (setq cur (+ mid-line (mew-count-lines mid-point pos)))
	  (setq cur (- ttl (mew-count-lines pos (point-max))))))
      (unless (and (mew-decode-syntax-p)
		   (equal (mew-decode-syntax-buffer) (current-buffer))
		   (>= pos (mew-decode-syntax-begin))
		   (<= pos (mew-decode-syntax-end)))
	(setq cur (1+ cur)))
      (setq left (- ttl cur))
      (setq mew-summary-buffer-left-msgs (format "[%d/%d{%d}]" cur ttl left)))
    (force-mode-line-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Locking
;;;

(defun mew-summary-exclusive-p (&optional no-msg)
  (cond
   ((eq mew-summary-buffer-process t)
    (or no-msg (message "Try again later"))
    nil) ;; not exclusive
   ((processp mew-summary-buffer-process)
    (or no-msg
	(mew-message-for-summary
	 "Another process is running. Try later or type '\\[mew-summary-kill-subprocess]' to kill it"))
    nil) ;; not exclusive
   (t t))) ;; exclusive

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode
;;;

(defun mew-summary-mode ()
  "A major mode to show a list of messages in a folder.

\\{mew-summary-mode-map}"
  (interactive)
  (setq major-mode 'mew-summary-mode)
  (setq mode-line-buffer-identification (mew-mode-line-id))
  (use-local-map mew-summary-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;;
  (make-local-variable 'tab-width)
  (make-local-variable 'search-invisible)
  (setq search-invisible nil)
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (make-local-variable 'search-invisible)
  (jit-lock-register 'mew-summary-cook-region)
  (add-hook 'kill-buffer-hook 'mew-sinfo-save nil 'local)
  (mew-sinfo-set-disp-msg t)
  ;;
  (mew-summary-mode-name mew-mode-name-summary)
  (mew-summary-setup-mode-line)
  (mew-summary-setup-decoration)
  (mew-highlight-cursor-line)
  (mew-run-mode-hooks 'mew-summary-mode-hook))

(defun mew-summary-kill ()
  "Kill this Summary mode."
  (interactive)
  (mew-kill-buffer)
  (when (and mew-use-other-frame-for-summary (> (length (frame-list)) 1))
    (mew-remove-buffer (mew-buffer-message))
    (delete-frame)))

(defun mew-summary-reset ()
  (mew-unhighlight-cursor-line)
  (mew-window-configure 'summary)
  (mew-remove-buffer (mew-buffer-message))
  (mew-current-set nil nil nil)
  (mew-summary-reset-mode-line)
  (mew-decode-syntax-delete))

(provide 'mew-summary)

;;; Copyright Notice:

;; Copyright (C) 1996-2012 Mew developing team.
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
