;;; mew-highlight.el --- Highlight for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct 18, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cursor line
;;;

(defun mew-highlight-cursor-line ()
  "A function to highlight the cursor line in Summary and Virtual mode."
  (when mew-use-highlight-cursor-line
    (mew-elet
     (if (overlayp (mew-sinfo-get-cursor-line))
	 (move-overlay (mew-sinfo-get-cursor-line)
		       (save-excursion (beginning-of-line) (point))
		       (save-excursion (end-of-line) (point)))
       (mew-sinfo-set-cursor-line
	(mew-overlay-make
	 (save-excursion (beginning-of-line) (point))
	 (save-excursion (end-of-line) (point))))
       (overlay-put
	(mew-sinfo-get-cursor-line) 'face mew-highlight-cursor-line-face))))
  (when mew-use-cursor-mark
    (unless (markerp overlay-arrow-position)
      (make-local-variable 'overlay-arrow-position)
      (setq overlay-arrow-position (make-marker)))
    (unless (and overlay-arrow-string
		 (string=  overlay-arrow-string mew-cursor-mark))
      (make-local-variable 'overlay-arrow-string)
      (setq overlay-arrow-string mew-cursor-mark))
    (set-marker overlay-arrow-position
		(save-excursion (beginning-of-line) (point)))))

(defun mew-unhighlight-cursor-line ()
  (when (overlayp (mew-sinfo-get-cursor-line))
    (move-overlay (mew-sinfo-get-cursor-line) 1 1)
    ;; (goto-char (point-max)) works wrong if underline exists.
    ;; Redisplays it so that it works well.
    ;; (sit-for 0)
    ;; This code rocks Summary mode when typing "x", sigh.
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Marks
;;;

(defun mew-highlight-mark-line (mark)
  (when mew-use-highlight-mark
    (let ((face (mew-highlight-mark-get-face mark)))
      (unless face (setq face 'default))
      (put-text-property
       (save-excursion (beginning-of-line) (point))
       (save-excursion (end-of-line) (point))
       'face face))))

(defun mew-highlight-unmark-line ()
  (remove-text-properties
   (save-excursion (beginning-of-line) (point))
   (save-excursion (end-of-line) (point))
   '(face nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header
;;;

(defun mew-unhighlight-region (BEG END)
  (mew-overlay-delete-region BEG END))

(defun mew-unhighlight-header ()
  (save-restriction
    (widen)
    (mew-unhighlight-region (point-min) (mew-header-end))))

(defun mew-highlight-header-region (BEG END)
  "A function to highlight header in Message and Draft mode."
  (when mew-use-highlight-header
    (let (key beg med nspec overlay key-face val-face start)
      (save-excursion
	(mew-elet
	 (mew-unhighlight-region BEG END)
	 (save-restriction
	   (narrow-to-region BEG END)
	   (goto-char (point-min))
	   (while (not (eobp))
	     (if (not (looking-at mew-keyval))
		 (forward-line)
	       (setq start (point))
	       (setq key (mew-match-string 1))
	       (setq beg (match-beginning 0))
	       (setq med (match-end 0))
	       (forward-line)
	       (setq nspec (mew-nspec-by-key key))
	       (setq key-face (mew-key-face key nspec))
	       (setq val-face (mew-val-face key nspec))
	       (setq overlay (mew-overlay-make beg med))
	       (overlay-put overlay 'face key-face)
	       (setq overlay (mew-overlay-make med (1- (point))))
	       (overlay-put overlay 'face val-face)
	       (while (looking-at mew-lwsp+)
		 (forward-line)
		 (setq overlay (mew-overlay-make (match-end 0) (1- (point))))
		 (overlay-put overlay 'face val-face))
	       (if (member (mew-capitalize key)
			   mew-draft-address-warning-fields)
		   (mew-highlight-header-address-region start (point)))))))))))

(defun mew-highlight-header-address-region (beg end)
  (let ((case (mew-tinfo-get-case))
	addr domain overlay begovl endovl warnp)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "[^\n\t, <>:;]+@\\([^\n\t, <>:;]+\\)" end t)
	(setq addr (downcase (mew-match-string 0)))
	(setq domain (downcase (mew-match-string 1)))
	(setq begovl (match-beginning 0))
	(setq endovl (match-end 0))
	(cond
	 ((member addr (mew-safe-addresses case))
	  (setq warnp nil))
	 ((member addr (mew-warn-addresses case))
	  (setq warnp t))
	 ((member domain (mew-safe-domains case))
	  (setq warnp nil))
	 ((member domain (mew-warn-domains case))
	  (setq warnp t))
	 ((or (mew-warn-addresses case) (mew-safe-domains case))
	  (setq warnp t))
	 (t
	  (setq warnp nil)))
	(when warnp
	  (setq overlay (mew-overlay-make begovl endovl))
	  (overlay-put overlay 'face 'mew-face-header-warning)
	  (overlay-put overlay 'priority 2)))))) ;; a bug of Meadow

(defun mew-highlight-header ()
  (save-restriction
    (widen)
    (mew-highlight-header-region (point-min) (mew-header-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Body
;;;

(defun mew-highlight-body-region (BEG END &optional draft rehighlight)
  (when (and (or mew-use-highlight-body mew-use-highlight-url)
	     (or (= 0 mew-highlight-body-max-size)
		 (<= BEG mew-highlight-body-max-size)))
    (unless (or (= 0 mew-highlight-body-max-size)
		(<= END mew-highlight-body-max-size))
      (setq END mew-highlight-body-max-size))
    (let* ((inhibit-point-motion-hooks t)
	   (cite-regex mew-highlight-body-regex-cite)
	   (cmt-regex mew-highlight-body-regex-comment)
	   (url-regex mew-regex-url)
	   (fancy-num 0)
	   (fancy-length (length mew-highlight-body-cite-faces))
	   beg end face fancy-alst fancy-prefix idn url)
      (save-excursion
	(mew-elet
	 (when draft
	   (mew-rear-nonsticky BEG END)
	   (if rehighlight
	       (remove-text-properties BEG END '(face nil mouse-face nil))))
	 (when mew-use-highlight-body
	   (goto-char BEG)
	   (while (and (<= (point) END) (re-search-forward cite-regex END t))
	     (setq beg (match-beginning 0))
	     (setq end (match-end 0))
	     (if (match-beginning 1)
		 (setq fancy-prefix (mew-match-string 1))
	       (setq fancy-prefix nil))
	     (when (and fancy-prefix
			(< (string-width fancy-prefix)
			   mew-highlight-body-prefix-width))
	       (if (setq face (cdr (assoc fancy-prefix fancy-alst)))
		   (put-text-property beg end 'face face)
		 (setq face (nth fancy-num mew-highlight-body-cite-faces))
		 (setq fancy-alst (cons (cons fancy-prefix face) fancy-alst))
		 (setq fancy-num (1+ fancy-num))
		 (when (= fancy-length fancy-num)
		   (setq fancy-num 0))
		 (put-text-property beg end 'face face)))
	     (forward-line))
	   (goto-char BEG)
	   (while (and (<= (point) END) (re-search-forward cmt-regex END t))
	     (setq beg (match-beginning 0))
	     (setq end (match-end 0))
	     (put-text-property	beg end 'face 'mew-face-body-comment)))
	 (when mew-use-highlight-url
	   (goto-char BEG)
	   (while (and (<= (point) END) (re-search-forward url-regex END t))
	     (save-restriction
	       (setq beg (match-beginning 0))
	       (setq end (match-end 0))
	       (narrow-to-region beg end)
	       (goto-char (point-min))
	       (setq url (mew-buffer-substring (point-min) (point-max)))
	       (while (and mew-use-punycode (re-search-forward mew-regex-punycode nil t))
		 (setq idn (mew-puny-decode (mew-buffer-substring (match-beginning 0) (match-end 0))))
		 (when idn
		   (delete-region (match-beginning 0) (match-end 0))
		   (insert idn)))
	       (goto-char (point-max))
	       (put-text-property (point-min) (point-max) 'mew-url url)
	       (put-text-property (point-min) (point-max) 'face 'mew-face-body-url)
	       (put-text-property
		(point-min) (point-max) 'mouse-face mew-highlight-url-mouse-face)
	       (setq END (- END (- end (point-max))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X Face
;;;

(defun mew-highlight-x-face (beg end)
  "Display X-Face."
  (if (and mew-use-highlight-x-face
	   window-system
	   (fboundp mew-highlight-x-face-function))
      (funcall mew-highlight-x-face-function beg end)))

(defvar mew-highlight-x-face-function (if mew-icon-p 'mew-highlight-x-face-original)
  "*A function to display-Xface")

(defun mew-highlight-x-face-original (beg end)
  (save-excursion
    (goto-char beg)
    (mew-elet
     (let ((regex1 "^X-Face: *\\(.*\\(\n[ \t].*\\)*\\)\n")
	   (buf (current-buffer))
	   overlay xface beg0 end0 xbmp)
       (while (re-search-forward regex1 end t)
	 (setq beg0 (match-beginning 0))
	 (setq end0 (match-end 0))
	 (with-temp-buffer
	   (mew-insert-buffer-substring
	    buf (match-beginning 1) (match-end 1))
	   (setq xbmp (mew-x-face-compface-to-xbm))
	   (if xbmp (setq xface (mew-x-face-create))))
	 (when xface
	   (setq overlay (mew-overlay-make beg0 end0))
	   (overlay-put overlay 'invisible t)
	   (save-restriction
	     (narrow-to-region beg end)
	     (mew-x-face-display xface ))))))))

(defun mew-x-face-compface-to-xbm ()
  (when (and (mew-which-exec mew-prog-uncompface)
	     (mew-which-exec mew-prog-icontopbm)
	     (mew-which-exec mew-prog-pbmtoxbm))
    (mew-set-buffer-multibyte nil)
    (mew-flet
     (call-process-region (point-min) (point-max) mew-prog-uncompface t t nil)
     (goto-char (point-min))
     (insert "/* Format_version=1, Width=48, Height=48, Depth=1, Valid_bits_per_item=16 */\n")
     (call-process-region (point-min) (point-max) mew-prog-icontopbm t t nil)
     (if mew-use-highlight-x-face-inversion
	 (call-process-region (point-min) (point-max) mew-prog-pbminvert t t nil))
     (call-process-region (point-min) (point-max) mew-prog-pbmtoxbm t t nil))
    t)) ;; return value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cooking
;;;

(defun mew-summary-cook-window (&optional WIN BEG)
  (let* ((win (or WIN (selected-window)))
	 (beg (or BEG (window-start win)))
	 (end (window-end win t))
	 (buf (window-buffer win)))
    (with-current-buffer buf
      (funcall mew-summary-cook-function beg end))))

;; See also mew-scan-insert-line
(defun mew-summary-cook-region (beg end &optional interrupt)
  (when (and (mew-summary-or-virtual-p) mew-summary-buffer-raw)
    (let ((inhibit-point-motion-hooks t)
	  ret mark face start med)
      (catch 'loop
	(save-excursion
	  (mew-elet
	   (goto-char beg)
	   (if (mew-in-decode-syntax-p)
	       (goto-char (mew-decode-syntax-end)))
	   (if (and (mew-thread-p)
		    mew-use-thread-separator
		    (not (mew-summary-message-number)))
	       (forward-line))
	   (setq start (point))
	   (while (and (< (point) end) ;; we cannot trust end
		       (search-forward "\r" end t))
	     (if (and interrupt (input-pending-p))
		 (throw 'loop (setq ret t)))
	     (setq med (match-beginning 0))
	     (forward-line)
	     (if (and mew-use-highlight-mouse-line window-system)
		 (put-text-property
		  start med 'mouse-face mew-highlight-mouse-line-face))
	     (put-text-property med (1- (point)) 'invisible t)
	     (if (mew-in-decode-syntax-p)
		 (goto-char (mew-decode-syntax-end)))
	     (if (and (mew-thread-p)
		      mew-use-thread-separator
		      (not (mew-summary-message-number)))
		 (forward-line))
	     (setq start (point)))
	   (when mew-use-highlight-mark
	     (goto-char beg)
	     (while (and (< (point) end) ;; we cannot trust end
			 (re-search-forward mew-regex-mark end t))
	       (if (and interrupt (input-pending-p))
		   (throw 'loop (setq ret t)))
	       (setq mark (mew-sumsyn-mark))
	       (setq face (mew-highlight-mark-get-face mark))
	       (beginning-of-line)
	       (setq start (point))
	       (forward-line)
	       (if face (put-text-property start (1- (point)) 'face face)))))))
      (set-buffer-modified-p nil)
      ret)))

(defun mew-summary-cook-folders ()
  (save-excursion
    (dolist (buf mew-buffers)
      (when (and (get-buffer buf) (not (input-pending-p)))
	(set-buffer buf)
	(setq mew-summary-buffer-raw
	      (mew-summary-cook-region
	       (point-min) (point-max) 'interrupt))))))

(defvar mew-highlight-timer-id nil)

(defun mew-highlight-timer-setup ()
  (if mew-highlight-timer-id (cancel-timer mew-highlight-timer-id))
  (setq mew-highlight-timer-id
	(run-with-idle-timer mew-highlight-timer-interval
			     t 'mew-summary-cook-folders)))

(defun mew-highlight-timer-clean-up ()
  (if mew-highlight-timer-id (cancel-timer mew-highlight-timer-id))
  (setq mew-highlight-timer-id nil))

(provide 'mew-highlight)

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

;;; mew-highlight.el ends here
