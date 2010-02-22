;;; mew-summary3.el --- Summary mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subfunctions
;;;

(defmacro mew-summary-prepare-draft (&rest body)
  "Common procedure to prepare a draft."
  `(progn
     (unwind-protect
	 (let ((inhibit-quit t))
	   ,@body
	   (mew-redraw)
	   (if quit-flag (setq quit-flag nil)))
       (mew-draft-save-buffer)) ;; to make sure not to use this draft again
     (mew-set-file-modes (buffer-file-name))
     (mew-touch-folder mew-draft-folder)
     (message "Draft is prepared")))

(defun mew-summary-prepare-three-windows ()
  "Prepare three windows: Summary mode, Message mode, and Draft mode"
  (unless mew-use-other-frame-for-draft
    (if (get-buffer (mew-buffer-message))
	(delete-windows-on (mew-buffer-message)))
    (cond
     ((< (window-height) 25)
      (delete-other-windows))
     (mew-use-full-window
      (mew-delete-other-window)))
    (let ((split-window-keep-point t))
      (split-window-vertically))))

(defun mew-draft-multi-copy (draft files)
  (let* ((attach (mew-draft-to-attach draft))
	 (attachdir (mew-expand-folder attach)))
    (mew-check-directory attachdir)
    (dolist (file files)
      (if mew-use-symbolic-link-for-forwarding
	  (mew-symbolic-link file (mew-folder-new-message attach))
	(copy-file file (mew-folder-new-message attach))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sending
;;;

(defun mew-summary-set-message-buffer (fld msg)
  ;; need to make a cache or a message buffer.
  (mew-summary-toggle-disp-msg 'on)
  ;; Since we will switch to a draft buffer,
  ;; the window of the draft buffer must not be deleted.
  ;; So, binding mew-use-full-window to nil.
  (let ((mew-use-full-window nil))
    (mew-summary-display))
  (set-buffer (mew-buffer-message))
  ;; header exists only in cache if multipart
  (unless (mew-msghdr-p) (set-buffer (mew-cache-hit fld msg))))

(defun mew-summary-write (&optional from)
  "Write a message. A new draft is prepared in Draft mode.
If called with '\\[universal-argument]', the From: address of
the current message is copied to To: in a draft."
  (interactive "P")
  (if (null from)
      (mew-summary-send)
    (mew-summary-msg-or-part
     (let ((fld (mew-summary-folder-name))
	   (msg (mew-summary-message-number2))
	   to)
       (if (not (and fld msg))
	   (message "No message")
	 (save-excursion
	   (mew-summary-set-message-buffer fld msg)
	   (setq to (mew-addrstr-parse-address (mew-header-get-value mew-from:)))
	   (if (mew-is-my-address mew-regex-my-address-list to)
	       (setq to (mew-addrstr-parse-address (mew-header-get-value mew-to:)))))
	 (mew-summary-send to)
	 (goto-char (point-min))
	 (if (search-forward mew-subj: nil t)
	     (end-of-line)))))))

(defun mew-summary-send (&optional to cc subject newsgroups)
  "Write a message. A new draft is prepared in Draft mode."
  (interactive)
  (let ((draft (mew-folder-new-message mew-draft-folder))
	asked)
    (if (and (mew-summary-physical-folder)
	     (mew-folder-nntpp (mew-summary-physical-folder)))
	(if (null newsgroups) (setq newsgroups ""))
      (when (and mew-ask-to (null to))
        (setq to (mew-input-address (concat mew-to: " ")))
	(setq asked t))
      (when (and mew-ask-cc (null cc))
        (setq cc (mew-input-address (concat mew-cc: " ")))
	(setq asked t)))
    (mew-current-set-window-config)
    (mew-window-configure 'draft)
    (mew-summary-prepare-draft
     (mew-draft-find-and-switch draft)
     (mew-delete-directory-recursively (mew-attachdir draft))
     (mew-draft-header subject nil to cc newsgroups nil nil nil asked)
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-newdraft-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Replying
;;;

(defun mew-subject-simplify (str &optional act-lst no-replace)
  "A function to simplify a value of Subject: according to
'mew-subject-simplify-replace-alist'."
  (let ((case-fold-search t)
	(action-list (or act-lst mew-subject-simplify-replace-alist))
	(ret str)
	regexp replace)
    (dolist (action action-list)
      (setq regexp (mew-alist-get-key action))
      (setq replace (if no-replace nil (mew-alist-get-value action)))
      (if (string-match regexp ret)
	  (setq ret (replace-match (if replace (eval replace) "") nil t ret))))
    ret))

(defun mew-subject-simplify-ml (str)
  (if (string-match "^[[(][^])]+[])][ \t]*" str)
      (substring str (match-end 0))
    str))

(defun mew-subject-simplify2 (str)
  (mew-subject-simplify
   (mew-subject-simplify-ml
    (mew-subject-simplify str nil 'no-replace)) nil 'no-replace))

(defun mew-to-cc-newsgroups (replysender)
  (let (alist key tcn flds to cc newsgroups fromme func)
    (cond
     (replysender
      (setq alist mew-reply-sender-alist)
      (setq func 'mew-header-parse-address-list2))
     ((mew-is-my-address mew-regex-my-address-list
			 (mew-header-parse-address mew-from:))
      (setq fromme t)
      (setq alist mew-reply-fromme-alist)
      ;; If from me, let's leave the "anonymous:;" keyword.
      (setq func 'mew-header-parse-address-list))
     (t ;; reply all
      (setq alist mew-reply-all-alist)
      (setq func 'mew-header-parse-address-list2)))
    (catch 'loop
      (dolist (ent alist)
	(setq key (car ent))
	(setq ent (cdr ent))
	(when (or (eq key t)
		  (and (stringp key) (mew-header-get-value key))
		  (and (listp key)
		       (string= (mew-header-get-value (nth 0 key))
				(nth 1 key))))
	  (dolist (tcn-flds ent)
	    (setq tcn (car tcn-flds))
	    (setq flds (cdr tcn-flds))
	    (cond
	     ((mew-case-equal tcn mew-to:)
	      (setq to (funcall func flds)))
	     ((mew-case-equal tcn mew-cc:)
	      (setq cc (funcall func flds)))
	     ((mew-case-equal tcn mew-newsgroups:)
	      (setq newsgroups (funcall func flds)))))
	  (throw 'loop nil))))
    (list to cc newsgroups fromme)))

(defun mew-in-reply-to-references ()
  (let ((old-message-id  (mew-idstr-get-first-id
			  (mew-header-get-value mew-message-id:)))
	(old-in-reply-to (mew-idstr-get-first-id
			  (mew-header-get-value mew-in-reply-to:)))
	(old-references  (mew-idstr-to-id-list
			  (mew-header-get-value mew-references:) 'rev))
	id-list in-reply-to references)
    ;; Assuming that In-Reply-To: contains one ID.
    (when old-message-id
      ;; when old-message-id is nil,
      ;; we do not care even if old-references exist.
      (setq in-reply-to old-message-id)
      (cond
       (old-references
	(setq id-list old-references)
	(mew-addq id-list old-in-reply-to)
	(mew-addq id-list old-message-id)
	(setq id-list (nreverse id-list)))
       (t
	(setq id-list (list old-message-id))
	(mew-addq id-list old-in-reply-to)))
      (setq references (mew-id-list-to-idstr id-list)))
    (list in-reply-to references)))

(defun mew-summary-reply (&optional replysender)
  "Reply to this message. A new draft is prepared in Draft mode.
Values of To:, Cc:, and Newsgroups: are prepared according to
three alists.

\(1) If called with '\\[universal-argument]', replying to the
    sender/poster only. In this case, 'mew-reply-sender-alist' is used.

\(2) If this message is sent by ME, you probably intend to reply with
    the original header. In this case, 'mew-reply-fromme-alist' is used.

\(3) Otherwise, replying to all people listed. In this case,
    'mew-reply-all-alist' is used.

The default value of 'mew-reply-sender-alist' is as follows:

	'((\"Reply-To:\"
	   (\"To:\" \"Reply-To:\" \"From:\"))
	  (t
	   (\"To:\" \"From:\")))

This is read as follows:

	(1.1) If Reply-To: exists, copy the values of Reply-To:
              and From: to new To:.
	(1.2) Otherwise, copy the value of From: to To:.

If you would like to reply only to the address on Reply-To: (if any),
set 'mew-reply-sender-alist' to:

	'((\"Reply-To:\"
	   (\"To:\" \"Reply-To:\"))
	  (t
	   (\"To:\" \"From:\")))

The default value of 'mew-reply-fromme-alist' is as follows:

	'((t
	   (\"To:\" \"To:\")
	   (\"Cc:\" \"Cc:\")
	   (\"Newsgroups:\" \"Newsgroups:\"))))

This is read as follows:

	(2.1) Copying the value of To: to new To: and
              copying the value of Cc: to new Cc: and
              copying the value of Newsgroups: to new Newsgroups:.

The default value of 'mew-reply-all-alist' is as follows:

	'(((\"Followup-To:\" \"poster\")
	   (\"To:\" \"From:\"))
	  (\"Followup-To:\"
	   (\"Newsgroups:\" \"Followup-To:\" \"Newsgroups:\"))
	  (\"Newsgroups:\"
	   (\"Newsgroups:\" \"Newsgroups:\"))
	  (\"Reply-To:\"
	   (\"To:\" \"Reply-To:\" \"From:\")
	   (\"Cc:\" \"To:\" \"Cc:\" \"Apparently-To:\"))
	  (t
	   (\"To:\" \"From:\")
	   (\"Cc:\" \"To:\" \"Cc:\" \"Apparently-To:\")))

This is read as follows:

	(3.1) If the value of Followup-To: is \"poster\", copying the
              value of From: to new To:.
	(3.2) If Followup-To: exists, copying the values of
              Followup-To: and Newsgroups: to new Newsgroups:.
	(3.3) If Newsgroups: exists, copying the value of Newsgroups:
              to Newsgroups:.
	(3.4) If Reply-To: exists, copying the values of Reply-To: and
              From: to new To:. And copying the values of To:, Cc: and
              Apparently-To: to new Cc:.

	(3.5) Otherwise, copying the value of From: to new To:. And
              copying the values of To:, Cc: and Apparently-To: to
              new Cc:.

You may want to set 'mew-reply-all-alist' to:

	'(((\"Followup-To:\" \"poster\")
	   (\"To:\" \"From:\"))
	  (\"Followup-To:\"
	   (\"Newsgroups:\" \"Followup-To:\"))
	  (\"Newsgroups:\"
	   (\"Newsgroups:\" \"Newsgroups:\"))
	  (\"Reply-To:\"
	   (\"To:\" \"Reply-To:\"))
	  (t
	   (\"To:\" \"From:\")
	   (\"Cc:\" \"To:\" \"Cc:\" \"Apparently-To:\")))
"
  (interactive "P")
  (mew-summary-msg-or-part
   (mew-summary-not-in-draft
    (mew-current-set-window-config)
    (let ((owin (selected-window))
	  (fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number2))
	  (split-width-threshold nil)
	  cwin cbuf draft case
	  subject to cc newsgroups in-reply-to references
	  encrypted fromme ret)
      (if (string= (mew-summary-folder-name) mew-draft-folder)
	  (message "Cannot reply to draft message")
	(setq draft (mew-folder-new-message mew-draft-folder))
	(mew-summary-prepare-draft
	 (mew-summary-prepare-three-windows)
	 (mew-draft-find-and-switch draft t)
	 (mew-delete-directory-recursively (mew-attachdir draft))
	 (setq cwin (selected-window)) ;; draft
	 (setq cbuf (window-buffer cwin))
	 (select-window owin)
	 (mew-summary-set-message-buffer fld msg)
	 (when mew-case-guess-when-replied
	   (setq case (mew-draft-get-case-by-guess
		       mew-case-guess-when-replied-alist)))
	 (setq encrypted (mew-syntax-encrypted-p mew-decode-syntax))
	 (save-restriction
	   ;; if body contains ^L, header is not accessible.
	   ;; mew-header-* cannot widen essentially. So widen here.
	   (widen)
	   ;; now cache buffer
	   ;;
	   ;; Subject:
	   (setq subject (mew-header-get-value mew-subj:))
	   (when subject
	     (setq subject (concat mew-reply-string subject))
	     (setq subject (mew-subject-simplify subject)))
	   ;;
	   ;; To:, Cc:, Newsgroups:
	   (setq ret (mew-to-cc-newsgroups replysender))
	   (mew-set '(to cc newsgroups fromme) ret)
	   ;;
	   ;; In-Reply-To:, References:
	   (setq ret (mew-in-reply-to-references))
	   (mew-set '(in-reply-to references) ret))
	 ;;
	 (if (window-live-p cwin)
	     (select-window cwin) ;; draft
	   (pop-to-buffer cbuf))
	 (when case
	   (if mew-case-guess-addition
	       (setq case (mew-draft-add-case (mew-tinfo-get-case) case)))
	   (mew-tinfo-set-case case))
	 (mew-draft-header subject nil to cc newsgroups in-reply-to references
			   nil fromme)
	 (when (eq mew-summary-reply-position 'body)
	   (goto-char (mew-header-end))
	   (forward-line))
	 (mew-draft-mode encrypted)
	 (run-hooks 'mew-draft-mode-newdraft-hook)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Replaying with citation
;;;

(defun mew-summary-reply-with-citation (&optional replysender)
  "Answer to this message. A new draft is prepared in Draft mode.
And this message is automatically cited. See also 'mew-summary-reply'."
  (interactive "P")
  (mew-summary-msg-or-part
   (mew-summary-not-in-draft
    (let ((mew-summary-reply-position nil)
	  (mew-message-citation-frame-id (mew-frame-id)))
      (mew-summary-reply replysender)
      ;; mew-draft-mode-hook may insert text.
      (save-excursion
	(goto-char (point-max))
	(run-hooks 'mew-before-cite-hook)
	(mew-draft-cite)))
    ;; the cursor is after To:
    (cond
     ((eq mew-summary-reply-with-citation-position 'body)
      (goto-char (mew-header-end))
      (forward-line))
     ((eq mew-summary-reply-with-citation-position 'end)
      (goto-char (point-max)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Forwarding
;;;

(defun mew-summary-forward ()
  "Forward this message to a third person. A new draft is prepared in
Draft mode and this message is automatically attached."
  (interactive)
  (mew-summary-msg-or-part
   (mew-summary-not-in-draft
    (mew-current-set-window-config)
    (let* ((owin (selected-window))
	   (fld (mew-summary-folder-name))
	   (msg (mew-summary-message-number2))
	   (file (mew-expand-msg fld msg))
	   (draft (mew-folder-new-message mew-draft-folder))
	   (draftdir (file-name-nondirectory draft))
	   (to (and mew-ask-to (mew-input-address (concat mew-to: " "))))
	   (cc (and mew-ask-cc (mew-input-address (concat mew-cc: " "))))
	   (asked (or mew-ask-to mew-ask-cc))
	   subject fwsubject cwin)
      (mew-summary-prepare-draft
       (mew-summary-prepare-three-windows)
       (mew-draft-find-and-switch draft t)
       (mew-delete-directory-recursively (mew-attachdir draft))
       (setq cwin (selected-window)) ;; draft
       (select-window owin)
       (mew-summary-set-message-buffer fld msg)
       (setq subject (mew-header-get-value mew-subj:))
       (if subject
	   (setq fwsubject (mew-subject-simplify (concat mew-forward-string subject))))
       (select-window cwin) ;; draft
       ;;
       (mew-draft-header fwsubject 'nl to cc nil nil nil nil asked)
       (mew-draft-mode)
       (run-hooks 'mew-draft-mode-newdraft-hook)
       (mew-draft-multi-copy draft (list file))
       (setq mew-encode-syntax (mew-encode-syntax-initial-multi draftdir 1))
       (save-excursion
	 (mew-draft-prepare-attachments t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multi forwarding
;;;

(defun mew-summary-multi-forward ()
  "Forward messages marked with '*' to a third person. A new draft
is prepared in Draft mode and this message is automatically
attached."
  (interactive)
  (mew-summary-multi-msgs
   (mew-summary-not-in-draft
    (mew-current-set-window-config)
    (let* ((draft (mew-folder-new-message mew-draft-folder))
	   (draftdir (file-name-nondirectory draft))
	   (to (and mew-ask-to (mew-input-address (concat mew-to: " "))))
	   (cc (and mew-ask-cc (mew-input-address (concat mew-cc: " "))))
	   (asked (or mew-ask-to mew-ask-cc)))
      (mew-summary-prepare-draft
       (mew-summary-prepare-three-windows)
       (mew-draft-find-and-switch draft t)
       (mew-delete-directory-recursively (mew-attachdir draft))
       (mew-draft-header nil 'nl to cc nil nil nil nil asked)
       (mew-draft-mode)
       (run-hooks 'mew-draft-mode-newdraft-hook)
       (mew-draft-multi-copy draft FILES)
       (setq mew-encode-syntax
	     (mew-encode-syntax-initial-multi draftdir (length FILES)))
       (save-excursion
	 (mew-draft-prepare-attachments t)))))))

(provide 'mew-summary3)

;;; Copyright Notice:

;; Copyright (C) 1996-2010 Mew developing team.
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

;;; mew-summary3.el ends here
