;;; -*- lexical-binding: nil; -*-
;;; mew-env.el --- Environment setup for Mew

;; Author:  Mew developing team
;; Created: Mar  6, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

;; to avoid competition with mh-e.el, sigh.
(let ((ent (rassq 'mh-letter-mode auto-mode-alist)))
  (and ent (setq auto-mode-alist (delq ent auto-mode-alist))))

(defvar mew-connection-type1 nil
  "Connection type for many processes. `t' means PTY and `nil' means PIPE.
PIPE is usually recommended for speed but some OSes such as Linux
requires PTY.")

(defvar mew-connection-type2 t
  "Connection type for processes that requires a password.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacsen
;;;

(if window-system (require 'faces))

(defvar mew-icon-p (and (featurep 'tool-bar) window-system))

(require 'mew-key)
(require 'mew-gemacs)
(require 'mew-mule3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun mew-mark () (marker-position (mark-marker)))

(defun mew-md5 (str) (md5 str nil nil 'binary))

(defun mew-timer (sec func) (run-at-time sec sec func))

(defalias 'mew-characterp 'characterp)

(if (fboundp 'mouse-region-match)
    (defalias 'mew-mouse-region-p 'mouse-region-match)
  (defmacro mew-mouse-region-p (&rest _args) nil))

(defun mew-hscroll ()
  (set (make-local-variable 'auto-hscroll-mode) t))

(defalias 'mew-minibuf-point-min 'minibuffer-prompt-end)

(defun mew-process-silent-exit (pro)
  (set-process-query-on-exit-flag pro nil))

(defun mew-set-coding-priority (pri)
  (apply 'set-coding-system-priority
	 (mapcar (lambda (x) (symbol-value x)) pri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sticky
;;;

(defun mew-front-sticky (beg end)
  (put-text-property beg end 'front-sticky t))

(defun mew-front-nonsticky (beg end)
  (put-text-property beg end 'front-sticky nil))

(defun mew-rear-sticky (beg end)
  (put-text-property beg end 'rear-nonsticky nil))

(defun mew-rear-nonsticky (beg end)
  (put-text-property beg end 'rear-nonsticky t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Overlay
;;;

(defun mew-overlay-make (beg end)
  (let ((ovl (make-overlay beg end)))
    (overlay-put ovl 'mew t)
    ovl))

(defun mew-overlay-delete (ovl)
  (and (overlay-get ovl 'mew) (delete-overlay ovl)))

(defun mew-overlay-delete-region (beg end)
  "Delete overlays in the region."
  (interactive "r")
  (mapc 'mew-overlay-delete (overlays-in beg end))
  (if (fboundp 'remove-images) (remove-images beg end))) ;; xxx

(defun mew-overlay-delete-buffer ()
  (save-restriction
    (widen)
    (mew-overlay-delete-region (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File operations
;;;

(defun mew-symbolic-link (filename newname &optional OK-IF-ALREADY-EXISTS)
  (if (file-directory-p (file-chase-links filename))
      (error "Cannot make a symbolic link to directory")
    (make-symbolic-link filename newname OK-IF-ALREADY-EXISTS)))

(defun mew-link (filename newname &optional OK-IF-ALREADY-EXISTS)
  (if (file-directory-p (file-chase-links filename))
      (error "Cannot make a link to directory")
    (condition-case nil
	(add-name-to-file filename newname OK-IF-ALREADY-EXISTS)
      (file-error
       (copy-file filename newname OK-IF-ALREADY-EXISTS 'keepdate)))))

(if (memq system-type '(darwin windows-nt cygwin))
    (defalias 'mew-set-file-times 'set-file-times)
  (defmacro mew-set-file-times (&rest _args) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Property
;;;

(defalias 'mew-buffer-substring 'buffer-substring-no-properties)

(defalias 'mew-match-string 'match-string-no-properties)

(defun mew-insert-buffer-substring (buf beg end)
  (insert (with-current-buffer buf (mew-buffer-substring beg end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base64
;;;

(defun mew-base64-encode-string (str)
  (base64-encode-string str 'no-line-break))

(defun mew-base64-decode-string (str64)
  (condition-case nil
      (base64-decode-string str64)
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Language
;;;

;; to load the thread separator
(cond
 ((and (boundp 'current-language-environment)
       (string= current-language-environment "Japanese"))
  (require 'mew-lang-jp))
 ((and (boundp 'current-language-environment)
       (string= current-language-environment "Korean"))
  (require 'mew-lang-kr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unix and Mac
;;;

(if (fboundp 'unix-sync)
    (defalias 'mew-unix-sync 'unix-sync)
  (defmacro mew-unix-sync (&rest _args) nil))

;; mac-set-file-type only existed in the Carbon port.
(defmacro mew-mac-set-file-type (&rest _args) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Face
;;;

(defalias 'mew-face-spec-func 'cons)

(defun mew-face-spec-primitive (col bold)
  (if col
      (if bold
	  (list :foreground col :weight 'bold)
	(list :foreground col :weight 'normal))
    (if bold
	(list :weight 'bold)
      (list :weight 'normal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defalias 'mew-fill-match-adaptive-prefix 'fill-match-adaptive-prefix)

(if (fboundp 'create-animated-image)
    (defalias 'mew-create-image 'create-animated-image)
  (defalias 'mew-create-image 'create-image))

(defun mew-run-mode-hooks (&rest funcs)
  (apply 'run-mode-hooks funcs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unix/Mac/Win
;;;

(cond
 ((memq system-type '(windows-nt cygwin))
  (defun mew-set-file-type (_file) ())
  (defvar mew-cs-est 'shift_jis)
  (defun mew-focus-frame (frame)
    (if (fboundp 'w32-focus-frame) (w32-focus-frame frame))))
 ((eq system-type 'darwin)
  (defun mew-set-file-type (_file) ;; prevent byte-compile warning
    (unless mew-use-suffix
      (mew-mac-set-file-type _file mew-file-type)))
  (defvar mew-cs-est 'utf-8)
  (defun mew-focus-frame (_frame)
    (when focus-follows-mouse
      (set-mouse-position
       (selected-frame) (1- (frame-width)) 0))))
 (t
  (defun mew-set-file-type (_file) ())
  (defvar mew-cs-est 'utf-8)
  (defun mew-focus-frame (_frame)
    (when focus-follows-mouse
      (set-mouse-position
       (selected-frame) (1- (frame-width)) 0)))))


(provide 'mew-env)

;;; Copyright Notice:

;; Copyright (C) 1997-2023 Mew developing team.
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

;;; mew-env.el ends here
