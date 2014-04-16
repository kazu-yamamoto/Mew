;;; mew-message.el --- Message mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message info
;;;

(defvar mew-minfo-list '("summary" "eom" "veil-to" "veil-cc" "reob"))

(mew-blinfo-defun 'mew-minfo mew-minfo-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message mode
;;;

(defun mew-message-mode ()
  "A  major mode for display a message.

\\{mew-message-mode-map}"
  (interactive)
  (setq major-mode 'mew-message-mode)
  (setq mode-name mew-mode-name-message)
  (setq mode-line-buffer-identification (mew-mode-line-id))
  (use-local-map mew-message-mode-map)
  (setq buffer-read-only t)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter mew-page-delimiter)
  (mew-message-setup-decoration)
  (mew-message-toolbar-update)
  (mew-run-mode-hooks 'mew-message-mode-hook))

(defun mew-message-mode-line (fld message)
  (let* ((msg (or message
		  (mew-current-get-msg (mew-frame-id))))
	 (status (mew-message-line-status))
	 (flag (nth status '("N" "W" "L"))))
    (when msg
      (setq mode-line-process (format " %s %s/%s" flag fld msg)))))

(defun mew-message-next-page (&optional lines)
  "Scroll up this message. Return 'nil' if more pages. Otherwise, return 't'."
  (interactive)
  (if (save-excursion
	(goto-char (window-end))
        (and (pos-visible-in-window-p) (eobp)))
      ;; Nothing in this page.
      (if (or (null mew-break-pages)
	      (save-excursion
		(goto-char (window-end))
		(save-restriction
		  (widen) (forward-line) (eobp)))) ;; Real end of buffer?
          (if mew-summary-show-pause
              (prog1
		  (mew-minfo-get-reob)
		(unless (mew-minfo-get-reob)
		  (message "End of buffer")
		  (mew-minfo-set-reob t)))
	    t)
	;; Go to the next page.
	(mew-message-narrow-to-page 1)
	nil)
    ;; More in this page.
    (condition-case nil
	(scroll-up lines)
      (end-of-buffer
       (goto-char (point-max))
       (message "End of buffer")))
    nil))

(defun mew-message-prev-page (&optional lines)
  "Back-scroll this message. Return 'nil' if more pages.
Otherwise, return 't'."
  (interactive)
  (move-to-window-line 0)
  (if (save-excursion
	(beginning-of-line)
	(and (pos-visible-in-window-p) (bobp)))
      ;; Nothing in this page.
      (if (or (null mew-break-pages)
	      (save-restriction
		(widen) (bobp))) ;; Real beginning of buffer?
	  t
	;; Go to the previous page.
	(mew-message-narrow-to-page -1)
	nil)
    ;; More in this page.
    (condition-case nil
	(scroll-down lines)
      (beginning-of-buffer
       (goto-char (point-min))
       (message "Beginning of buffer")))
    nil))

(defun mew-message-narrow-to-page (&optional arg)
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (condition-case nil
	(forward-page -1) ;; Beginning of the current page.
      (beginning-of-buffer ()))
    (widen)
    (cond
     ((> arg 0)	(forward-page arg))
     ((< arg 0) (forward-page (1- arg))))
    (forward-page)
    (let* ((end (point)))
      (forward-page -1)
      (if (and (eolp) (not (bobp)))
	  (forward-line))
      (narrow-to-region (point) end))))

(defun mew-message-goto-summary ()
  "Get back to Summary mode."
  (interactive)
  (let* ((fld (mew-minfo-get-summary))
	 (fid (mew-frame-id))
	 (msg (mew-current-get-msg fid))
	 (part (mew-current-get-part fid)))
    (if (not (get-buffer fld))
	(message "No Summary mode for %s" fld)
      (if (get-buffer-window fld)
	  (select-window (get-buffer-window fld))
	(mew-summary-switch-to-folder fld))
      (cond
       ((and msg part (mew-decode-syntax-p))
	(setq part (concat "^.. +"
			   (regexp-quote (mapconcat 'number-to-string part "."))
			   " "))
	(goto-char (mew-decode-syntax-begin))
	(re-search-forward part (mew-decode-syntax-end) t)
	(beginning-of-line)
	(mew-summary-display))
       (msg
	(mew-summary-move-and-display msg))))))

(defun mew-message-reply ()
  "Answer to this message. A new draft is prepared in Draft mode.
Mew automatically decides To: and Cc:."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-reply))

(defun mew-message-reply-with-citation ()
  "Answer to this message. A new draft is prepared in Draft mode.
Mew automatically decides To: and Cc: and cites the body."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-reply-with-citation))

(defun mew-message-forward ()
  "Forward this message to a third person. A new draft is prepared in
Draft mode and this message is automatically attached."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-forward))

(defun mew-message-resend ()
  "\\<mew-message-mode-map>
Resend this message with Resent-To:. It is strongly
discouraged to use this command since beginners are always
confused. Please use '\\[mew-message-forward]' instead."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-resend))

(defun mew-message-next-msg (&optional arg)
  "Display a message or a part below."
  (interactive "p")
  (let* ((swin (previous-window))
	 (mbuf (current-buffer)))
    (select-window swin) ;; for the next forward-line
    (when (mew-summary-or-virtual-p)
      (forward-line arg) ;; minus arg results in prev
      (mew-summary-display))
    ;; for window config
    (select-window (get-buffer-window mbuf))))

(defun mew-message-prev-msg (&optional arg)
  "Display a message or a part above."
  (interactive "p")
  (mew-message-next-msg (- arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; URLs
;;;

(defun mew-message-goto-next-url (&optional arg)
  (interactive "P")
  (let ((next (if arg
                   'previous-single-property-change
                 'next-single-property-change))
        pos)
    (setq pos (funcall next (point) 'mew-url))
    (if pos
        (if (not (get-text-property pos 'mew-url))
            (setq pos (funcall next pos 'mew-url))))
    (when pos
      (goto-char pos))))

(defun mew-browse-url-at-mouse (event)
  (interactive "e")
  (let ((buf (current-buffer)) url)
    (mouse-set-point event)
    (setq url (or (get-text-property (point) 'mew-url)
		  (get-text-property (point) 'w3m-href-anchor)))
    (if (and url (string-match mew-regex-nonascii url))
	(setq url (mew-puny-encode-url url)))
    (if url (browse-url url))
    (pop-to-buffer buf)))

(defun mew-browse-url-at-point ()
  (interactive)
  (let (url)
    (setq url (or (get-text-property (point) 'mew-url)
		  (get-text-property (point) 'w3m-href-anchor)))
    (if (and url (string-match mew-regex-nonascii url))
	(setq url (mew-puny-encode-url url)))
    (if url (browse-url url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lines
;;;

(defun mew-message-line-status ()
  (cond
   ((not (eq buffer-undo-list t)) 1) ;; wrapped
   (truncate-lines 2) ;; long
   (t 0))) ;; normal

(defun mew-message-line ()
  "Toggle normal lines, wrapped lines and long lines."
  (interactive)
  (let* ((N 3)
	 (status (% (1+ (mew-message-line-status)) N)))
    (mew-elet
     (cond
      ((= status 0)
       (setq truncate-lines nil)
       (message "Normal lines"))
      ((= status 1)
       (setq buffer-undo-list nil)
       (mew-wrap-lines (if (mew-header-end) (1+ (mew-header-end)) (point-min)) (point-max))
       (message "Wrapped lines"))
      ((= status 2)
       (save-excursion
	 (primitive-undo (length buffer-undo-list) buffer-undo-list))
       (buffer-disable-undo)
       (setq truncate-lines t)
       (message "Long lines")))
     (set-window-hscroll (selected-window) 0)
     (set-buffer-modified-p nil))))

(defun mew-normal-line ()
  (setq truncate-lines nil))

(defcustom mew-wrap-lines-column fill-column
  "*Number of fill column to wrap line."
  :group 'mew-message
  :type 'integer)

(defun mew-wrap-lines (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (save-excursion
      (goto-char (point-min))
      (let ((adaptive-fill-mode nil)
	    (winwidth (- (window-width) 2))
	    (fill-column mew-wrap-lines-column)
	    width start fill-prefix)
	(while (not (eobp))
	  (setq start (point))
	  (setq fill-prefix (mew-fill-match-adaptive-prefix))
	  (end-of-line)
	  (setq width (current-column))
	  (forward-line)
	  (if fill-prefix
	      (setq fill-column (+ mew-wrap-lines-column (string-width fill-prefix)))
	    (setq fill-column mew-wrap-lines-column))
	  (setq fill-column (min fill-column winwidth))
	  (when (> width fill-column)
	    (fill-region-as-paragraph start (1- (point)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Horizontal Scrolling
;;;

(defun mew-message-scroll-left ()
  "Scroll the Message window to the right."
  (interactive)
  (call-interactively 'scroll-left))

(defun mew-message-scroll-right ()
  "Scroll the Message window to the left."
  (interactive)
  (call-interactively 'scroll-right))

(provide 'mew-message)

;;; Copyright Notice:

;; Copyright (C) 1996-2014 Mew developing team.
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

;;; mew-message.el ends here
