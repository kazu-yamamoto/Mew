;;; mew-sort.el --- Sorting messages for Mew

;; Author:  Takashi P.KATOH <p-katoh@shiratori.riec.tohoku.ac.jp>
;;          Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  6, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort variety
;;;

(defvar mew-sort-switch
  '(("text"    mew-sort-key-text    mew-sort-string)
    ("ml"      mew-sort-key-ml      mew-sort-string)
    ("mlnum"   mew-sort-key-mlnum   mew-sort-string)
    ("date"    mew-sort-key-date    mew-sort-string)
    ("num"     mew-sort-key-num     mew-sort-number)
    ("postnum" mew-sort-key-postnum mew-sort-number)))

(defun mew-sort-key-text (key folder msg)
  (mew-subject-simplify key nil 'no-replace))

(defun mew-sort-key-ml (key folder msg)
  (mew-subject-simplify2 key))

(defun mew-sort-key-mlnum (key folder msg)
  (let (mlname mlnum)
    (cond
     ((string-match "^\\([[(][^])]+\\)[: ]+\\([0-9]+\\)[])]" key)
      (setq mlname (match-string 1 key))
      (setq mlnum (match-string 2 key)))
     ((string-match "^[0-9]+$" key)
      (setq mlname "")
      (setq mlnum (match-string 0 key)))
     (t
      (setq mlname "")
      (setq mlnum "0")))
    (concat mlname (format "\000%010d" (string-to-number mlnum)))))

(defun mew-sort-key-date (key folder msg)
  (if (string= key "")
      (let ((time (mew-file-get-time (mew-expand-msg folder msg))))
	(mew-time-ctz-to-sortkey time))
    (mew-time-rfc-to-sortkey key)))

(defun mew-sort-key-num (key folder msg)
  (string-to-number key))

(defun mew-sort-key-postnum (key folder msg)
  (if (string-match "[0-9]+$" key)
      (string-to-number (match-string 0 key))
    (string-to-number key)))

(defun mew-sort-key (x) (cdr x))

(defun mew-sort-string (x y)
  (or (string= (mew-sort-key x) (mew-sort-key y))
      (string< (mew-sort-key x) (mew-sort-key y))))

(defun mew-sort-number (x y)
  (<= (mew-sort-key x) (mew-sort-key y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defvar mew-sort-line nil)

(defun mew-sort-index (x) (car x))

(defun mew-sort-insert (line msg)
  (insert line)
  (when msg
    (save-excursion
      (forward-line -1)
      (mew-syntax-change-message-number msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving messages
;;;

(defun mew-summary-sort-move-rename (src dst pos &optional lastp)
  (rename-file (mew-msg-get-filename src) (mew-msg-new-filename dst))
  (mew-refile-change src dst)
  (mew-summary-sort-move src dst pos lastp))

(defun mew-summary-sort-move-for-selection (src dst pos &optional lastp)
  (mew-summary-sort-move src nil pos lastp))

;; If not found, returns nil.
(defun mew-summary-sort-move (src dst pos &optional lastp)
  (mew-elet
   (let (beg end line)
     (cond
      (lastp
       (when pos
	 (goto-char pos)
	 (mew-sort-insert mew-sort-line dst)
	 nil))
      ((null pos) ;; first
       (when (mew-summary-search-msg src)
	 (setq beg (point))
	 (forward-line)
	 (setq end (point))
	 ;; We need to keep properties in Summary mode.
	 ;; This must be "buffer-substring".
	 (setq mew-sort-line (buffer-substring beg end))
	 (delete-region beg end)
	 beg))
      (t
       (when (mew-summary-search-msg src)
	 (setq beg (point))
	 (forward-line)
	 (setq end (point))
	 (cond
	  ((< pos beg)
	   ;; We need to keep properties in Summary mode.
	   ;; This must be "buffer-substring".
	   (setq line (buffer-substring beg end))
	   (goto-char end)
	   (delete-region beg end)
	   (save-excursion
	     (goto-char pos)
	     (mew-sort-insert line dst))
	   (point))
	  ((= pos beg)
	   ;; We need to keep properties in Summary mode.
	   ;; This must be "buffer-substring".
	   (setq line (buffer-substring beg end))
	   (delete-region beg end)
	   (goto-char pos)
	   (mew-sort-insert line dst)
	   (point))
	  (t
	   ;; We need to keep properties in Summary mode.
	   ;; This must be "buffer-substring".
	   (setq line (buffer-substring beg end))
	   (goto-char pos)
	   (mew-sort-insert line dst)
	   (delete-region beg end)
	   beg))))))))

(defun mew-summary-sort-move-for-debug (src dst pos &optional lastp)
  (mew-elet
   (insert (format "move %s to %s\n" src dst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defun mew-sort-get-range (arg)
  (let (region beg end range rbeg rend)
    (if arg
	(progn
	  (setq region (mew-summary-get-region))
	  (setq beg (car region))
	  (setq end (cdr region))
	  (if (= beg end) (error "No region")))
      (setq beg (point-min))
      (setq end (point-max)))
    (save-excursion
      (goto-char beg)
      (setq rbeg (mew-summary-message-number))
      (goto-char end)
      (forward-line -1)
      (setq rend (mew-summary-message-number))
      (forward-line)
      (if (and rbeg rend)
	  (setq range (concat rbeg "-" rend))
	(error "No region")))
    (list range beg end)))

(defun mew-sort-ask-key (folder)
  (let* ((sort-key (or (mew-alist-get-value
			(assoc folder mew-sort-default-key-alist))
		       mew-sort-default-key))
	 key type funcs newkey)
    (mew-set '(key type) (mew-input-sort-key sort-key))
    (setq funcs (assoc type mew-sort-switch))
    (setq newkey (concat (capitalize key) ":"))
    (cons newkey (cdr funcs))))

(defun mew-sort-get-file-index (folder range key func1 func2)
  (let* ((i 0)
	 (fld (mew-expand-folder2 folder))
	 num med value ent files idx)
    (with-temp-buffer
      (apply 'call-process
	     mew-prog-mewl nil t nil
	     (append (list "-b" mew-mail-path "-l" "0"
			   "-x" mew-suffix
			   "-d" key)
		     (mew-scan-mewl-src fld range)))
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (looking-at "^\\([0-9]+\\)[ \t]*:[ \t]*"))
	    (forward-line)
	  (setq num (mew-match-string 1))
	  (setq med (match-end 0))
	  (forward-line)
	  (mew-header-goto-next)
	  (mew-header-decode-region key med (point))
	  (setq value (mew-buffer-substring med (1- (point))))
	  (setq files (cons num files))
	  (setq ent (cons (cons i (funcall func1 value folder num)) ent))
	  (setq i (1+ i)))))
    (setq files (vconcat (nreverse files)))
    (setq ent (sort ent func2))
    (setq idx (vconcat (mapcar 'mew-sort-index ent)))
    (list files idx)))

(defun mew-sort-files (folder files idx func)
  ;;
  ;;         sorted        sorted
  ;;   files    idx    ->  files
  ;; 0    10      1        (was 20)
  ;; 1    20      2        (was 30)
  ;; 2    30      0        (was 10)
  ;;      31(new)
  ;;
  ;;
  ;;     src                dst
  ;; 10  0 (*a)       31 (*b)
  ;; 20  1 idx[0]     10    0
  ;; 30  2 idx[1]     20    1
  ;; 31  0 idx[2]     30    2
  ;;     (*c)
  ;; *a: initial src is 0
  ;; *b: initial files[dst] is 31 (new tmp file)
  ;; *c: break condition, src is looped!
  ;;     files[src] is 31 (new tmp file)
  ;;
  (let* ((dir (mew-expand-folder folder))
	 (default-directory dir)
	 (len (length idx)) ;; not (length files)
	 (tmp (mew-folder-new-message folder 'num-only))
	 src dst pos)
    (dotimes (i len)
      (setq mew-sort-line nil)
      (unless (= i (aref idx i))
	(setq dst len)
	(setq src i)
	(setq pos (funcall func (aref files src) tmp nil))
	(catch 'loop
	  (while t
	    (setq dst src)
	    (setq src (aref idx dst))
	    (if (= src i) (throw 'loop nil))
	    (setq pos (funcall func (aref files src) (aref files dst) pos))
	    (aset idx dst dst)))
	(funcall func tmp (aref files dst) pos 'last)
	(aset idx dst dst)))))

(defun mew-sort-push-mark ()
  (unless (eobp)
    (beginning-of-line)
    (mew-elet
     (put-text-property (point) (1+ (point)) 'mew-sort-orig t))))

(defun mew-sort-pop-mark ()
  (let ((orig (next-single-property-change (point-min) 'mew-sort-orig)))
    ;; 'mew-sort-orig may start with bob.
    (if (null orig)
	(mew-push-mark)
      (save-excursion
	(goto-char orig)
	(beginning-of-line)
	(setq orig (point)))
      (mew-elet
       ;; 'mew-sort-orig is copied onto the entire message
       ;; number. (window-width) is long enough to remove
       ;; it.
       (remove-text-properties
	orig (+ orig (window-width)) '(mew-sort-orig nil)))
      (mew-push-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort bodies
;;;

(defun mew-summary-sort-body (folder arg)
  (let (key idx files range beg end func1 func2 diag)
    ;; Summary cache updates
    (mew-summary-reset)
    (mew-summary-retrieve-gap folder)
    ;;
    (mew-set '(range beg end) (mew-sort-get-range arg))
    (mew-set '(key func1 func2) (mew-sort-ask-key folder))
    (setq diag (if arg folder (format "%s: %s" folder range)))
    ;;
    (message "Sorting %s..." diag)
    (mew-summary-lock t "Sorting")
    (unwind-protect
	(progn
	  (mew-set '(files idx) (mew-sort-get-file-index folder range key func1 func2))
	  ;;
	  (mew-sort-push-mark)
	  (if arg (narrow-to-region beg end))
	  ;;
	  (mew-sort-files folder files idx 'mew-summary-sort-move-rename)
	  ;;
	  (goto-char (point-min))
	  (if arg (widen))
	  (mew-sort-pop-mark)
	  ;;
	  (run-hooks 'mew-sort-hook)
	  (message "Sorting %s...done. Type '%s' to update ID database" diag (mew-substitute-for-summary "\\[mew-summary-make-id-index-folder]")))
      (mew-summary-folder-cache-save)
      (set-buffer-modified-p nil)
      (mew-summary-unlock))))

(defun mew-summary-sort-body-for-debug (folder arg)
  (let ((win (selected-window))
	key idx files range func1 func2 diag)
    (mew-set '(range beg end) (mew-sort-get-range arg))
    (mew-set '(key func1 func2) (mew-sort-ask-key folder))
    (setq diag (if arg folder (format "%s: %s" folder range)))
    ;;
    (message "Sorting %s..." diag)
    (mew-summary-lock t "Sorting")
    (unwind-protect
	(mew-set '(files idx) (mew-sort-get-file-index folder range key func1 func2))
      (mew-summary-unlock))
    (when (and files idx)
      (mew-window-configure 'message)
      ;; message buffer
      (mew-elet
       (mew-erase-buffer)
       (insert "Sort as follows:\n"))
      ;;
      (unwind-protect
	  (progn
	    (mew-sort-files folder files idx 'mew-summary-sort-move-for-debug)
	    (message "Sorting %s...done" diag))
	(mew-message-clear-end-of)
	(set-buffer-modified-p nil)
	(goto-char (point-min))
	(select-window win)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort command
;;;

(defun mew-summary-sort (&optional arg)
  "Sort messages and list them up again.
If called with '\\[universal-argument]', sort the region.
After sorting, the cursor moves onto the beginning of the buffer
or the region. "
  (interactive "P")
  (mew-summary-only
   (mew-summary-local-only
    (mew-summary-not-in-queue
     (mew-summary-not-in-draft
      (mew-summary-with-mewl
       (when (mew-summary-exclusive-p)
	 (let ((folder (mew-summary-folder-name)))
	   (if (null folder)
	       (message "No message")
	     (if (mew-mark-active-p) (setq arg t))
	     (if (mew-debug 'sort)
		 (mew-summary-sort-body-for-debug folder arg)
	       (mew-summary-sort-body folder arg)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort for selection
;;;

(defun mew-summary-selection-by-sort-body (arg)
  (let* ((buf (current-buffer))
	 (ofolder (mew-summary-folder-name 'ext))
	 (vfolder (mew-folder-to-selection ofolder))
	 (pfolder (mew-summary-physical-folder))
	 key idx files range beg end func1 func2 diag)
    (mew-set '(range beg end) (mew-sort-get-range arg))
    (mew-set '(key func1 func2) (mew-sort-ask-key ofolder))
    (setq diag (if arg ofolder (format "%s: %s" ofolder range)))
    ;;
    (message "Sorting %s..." diag)
    (mew-summary-lock t "Sorting")
    (unwind-protect
	(mew-set '(files idx) (mew-sort-get-file-index ofolder range key func1 func2))
      (mew-summary-unlock))
    (when (and files idx)
      (mew-summary-switch-to-folder vfolder)
      (mew-vinfo-set-mode 'selection)
      (mew-vinfo-set-physical-folder pfolder)
      (mew-vinfo-set-original-folder ofolder)
      (unwind-protect
	  (progn
	    (mew-erase-buffer)
	    (mew-elet
	     (insert (with-current-buffer buf (buffer-substring beg end)))
	     (mew-sort-files ofolder files idx 'mew-summary-sort-move-for-selection)
	     (mew-summary-set-count-line)
	     (goto-char (point-min))
	     (message "Sorting %s...done" diag)))
	(set-buffer-modified-p nil)))))

(defun mew-summary-selection-by-sort (&optional arg)
  (interactive "P")
  (mew-summary-only
   (mew-summary-not-in-queue
    (mew-summary-not-in-draft
     (mew-summary-with-mewl
      (when (mew-summary-exclusive-p)
	(if (mew-mark-active-p) (setq arg t))
	(mew-summary-selection-by-sort-body arg)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Packing
;;;

(defun mew-summary-pack-rename (src dst)
  (mew-elet
   (rename-file (mew-msg-get-filename src) (mew-msg-new-filename dst))
   (mew-refile-change src dst)
   (when (re-search-forward (mew-regex-sumsyn-msg src) nil t)
     (mew-syntax-change-message-number2 dst)
     (forward-line))))

(defun mew-summary-pack (&optional force)
  "Pack messages and list them up again.
After packing, the cursor stays in the current message.
If this command is used in a remote folder,
local cache messages are packed."
  (interactive "P")
  (if (not force)
      (message (mew-substitute-for-summary
		"Pack breaks search index, so pack was obsoleted. Type '\\[universal-argument]\\[mew-summary-pack]' to force pack."))
    (mew-summary-only
     (mew-summary-local-only
      (mew-summary-not-in-queue
       (mew-summary-not-in-draft
	(when (mew-summary-exclusive-p)
	  (let ((folder (mew-summary-folder-name)))
	    (cond
	     ((null folder)
	      (message "No message"))
	     ((or (not mew-ask-pack) (y-or-n-p (format "Pack %s? " folder)))
	      (mew-summary-pack-body folder)))))))))))

(defun mew-summary-pack-body (folder)
  (let* ((dir (mew-expand-folder folder))
	 (default-directory dir)
	 (n 1)
	 msgs src dst)
    ;;
    (mew-summary-reset)
    (mew-summary-retrieve-gap folder)
    ;;
    (message "Packing %s..." folder)
    (mew-summary-lock t "Packing")
    (condition-case nil
	(progn
	  (setq msgs (mew-dir-messages "."))
	  (setq msgs (mapcar 'string-to-number msgs))
	  (setq msgs (sort msgs '<)) ;; sort is inevitable
	  ;; the cursor stays the current position.
	  (save-excursion
	    (goto-char (point-min))
	    (dolist (msg msgs)
	      (setq src (number-to-string msg))
	      (cond
	       ((= msg n);; including src is a directory
		(setq n (1+ n)))
	       ((file-directory-p src)
		)
	       (t
		(setq dst (number-to-string n))
		(while (file-exists-p dst)
		  (setq n (1+ n))
		  (setq dst (number-to-string n)))
		(mew-summary-pack-rename src dst)
		(setq n (1+ n))))))
	  (mew-summary-folder-cache-save)
	  (set-buffer-modified-p nil)
	  (mew-summary-unlock)
	  (run-hooks 'mew-pack-hook)
	  (message "Packing %s...done (the last is %d). Type '%s' to update ID database" folder (1- n) (mew-substitute-for-summary "\\[mew-summary-make-id-index-folder]")))
      (quit
       (set-buffer-modified-p nil)
       (mew-summary-unlock)))))

(provide 'mew-sort)

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

;;; mew-sort.el ends here
