;;; mew-win32.el --- Settings for Mew on Win32

;; Author:  Shuichi Kitaguchi <kit@Mew.org>
;; Created: Dec 05, 1997

;;; Code:

(eval-when-compile
  (mew-no-warning-defvar w32-start-process-show-window)
  (mew-no-warning-defun mw32script-init)
  (mew-no-warning-defun mw32script-make-pathext-regexp)
  (mew-no-warning-defun mw32script-openp)
  (mew-no-warning-defun mw32script-argument-editing-function)
  (mew-no-warning-defun dos-to-unix-filename))

;;; for NTEmacs User
;;
;; put mw32script.el(in Meadow's archive) into load-path.
;;

;;
;;  ~/.mew settings
;;

;;; for PRINTING
;;    mew-w32-prog-print     ---  print command
;;    mew-w32-prog-print-arg ---  print command argument
;;    mew-w32-cs-print       ---  coding-system for printing
;;    define-process-argument-editing  ---  for argument-editing
;;
;;; example
;;   (setq mew-w32-prog-print     "notepad.exe")
;;   (setq mew-w32-prog-print-arg "/p")
;;   (setq mew-w32-cs-print       'shift_jis-dos)  ;; Meadow
;;   (define-process-argument-editing "/notepad\\.exe$"
;;     (lambda (x)
;;       (general-process-argument-editing-function x nil t)))
;;   (setq mew-print-function 'mew-w32-print-buffer)

;; Win32 programs.
(defvar mew-w32-exec           "fiber.exe")
(defvar mew-w32-prog-print     "notepad.exe")
(defvar mew-w32-prog-print-arg nil)

(defvar mew-default-external-program mew-w32-exec)

(defvar mew-w32-cs-print nil)

(setq mew-which-exec-suffixes (if (and (boundp 'exec-suffixes)
				       exec-suffixes)
				  exec-suffixes
				'(".exe" ".com" ".bat" ".cmd" "")))

(setq mew-delete-temp-file  nil)

;; Emacs version dependent variables.
(cond
 ((featurep 'meadow) ;; Meadow
  (require 'mw32script)
  (mw32script-init))
 ((condition-case nil
      (progn
	(require 'mw32script)
	(load "mw32misc"))
    (file-error nil)) ;; NTEmacs
  (mw32script-make-pathext-regexp)
  (defun mew-w32-argument-editing-function (program args)
    (let ((default-process-argument-editing-function 'identity)
	  (process-argument-editing-alist nil)
	  prog sargs)
      (setq prog (mw32script-openp program))
      (if (and (null prog)
	       (setq prog (mew-which-exec program))
	       (setq sargs (mw32script-argument-editing-function (list prog))))
	  (cons (car sargs) (cons prog args))
	(cons program args))))
  (defadvice call-process
    (before mew-w32-call-process
	    (program &optional infile buffer display &rest args)
	    activate)
    (let ((sargs (mew-w32-argument-editing-function program args)))
      (setq program (car sargs)
	    args (cdr sargs))))
  (defadvice call-process-region
    (before mew-w32-call-process-region
	    (start end program &optional infile buffer display &rest args)
	    activate)
    (let ((sargs (mew-w32-argument-editing-function program args)))
      (setq program (car sargs)
	    args (cdr sargs))))
  (defadvice start-process
    (before mew-w32-start-process
	    (name buffer program &rest program-args)
	    activate)
    (let ((sargs (mew-w32-argument-editing-function program program-args)))
      (setq program (car sargs)
	    program-args (cdr sargs))))))

;; printing
(defun mew-w32-print-buffer ()
  (let ((tempfile (mew-make-temp-name)))
    (mew-frwlet mew-cs-dummy mew-w32-cs-print
      (write-region (point-min) (point-max) tempfile nil nil))
    (setq w32-start-process-show-window t)
    (cond
     ((eq mew-w32-prog-print-arg nil)
      (call-process mew-w32-prog-print nil nil nil tempfile))
     (t
      (call-process mew-w32-prog-print nil nil nil mew-w32-prog-print-arg tempfile)))
    (setq w32-start-process-show-window nil)
    (mew-delete-file tempfile)))


;; MIME setting

(defvar mew-prog-plain    'mew-mime-text/plain)
(defvar mew-prog-html     '(mew-mime-text/html mew-mime-text/html-ext))
(defvar mew-prog-xml      '(mew-mime-text/xml  mew-mime-text/xml-ext))
(defvar mew-prog-patch    '(mew-mime-text/plain mew-mime-text/patch-ext))
(defvar mew-prog-enriched 'mew-mime-text/enriched)
(defvar mew-prog-text     'mew-mime-text/plain)
(defvar mew-prog-audio           mew-w32-exec)
(defvar mew-prog-audio2          mew-w32-exec) ;; dummy
(defvar mew-prog-image           '(mew-mime-image/* mew-mime-image/*-ext))
(defvar mew-prog-iges            mew-w32-exec)
(defvar mew-prog-vrml            mew-w32-exec)
(defvar mew-prog-mesh            mew-w32-exec)
(defvar mew-prog-video           mew-w32-exec)
(defvar mew-prog-rfc822          'mew-mime-message/rfc822)
(defvar mew-prog-rfc822-headers  'mew-mime-text/rfc822-headers)
(defvar mew-prog-external-body   '(mew-mime-external-body mew-mime-external-body-ext))
(defvar mew-prog-delivery-status 'mew-mime-text/plain)
(defvar mew-prog-postscript      mew-w32-exec)
(defvar mew-prog-pgp-keys        '(mew-mime-pgp-keys mew-mime-pgp-keys-ext))
(defvar mew-prog-application/pdf "pdftotext")
(defvar mew-prog-pdf-ext         mew-w32-exec)
(defvar mew-prog-pdf             `(mew-mime-application/pdf ,mew-prog-pdf-ext))
(defvar mew-prog-xml2            '(mew-mime-application/xml
				   mew-mime-application/xml-ext))
(defvar mew-prog-oasys           mew-w32-exec)
(defvar mew-prog-octet-stream    mew-w32-exec)
(defvar mew-prog-msword          mew-w32-exec)
(defvar mew-prog-msexcel         mew-w32-exec)
(defvar mew-prog-mspowerpoint    mew-w32-exec)
(defvar mew-prog-visio           mew-w32-exec)
(defvar mew-prog-ooffice         mew-w32-exec)
(defvar mew-prog-rtf             mew-w32-exec)
(defvar mew-prog-unzip           mew-w32-exec)

;;;
;;; Text/Html, Application/Xml, Image
;;;

(defvar mew-format-html "%s.htm")
(defvar mew-format-xml  "%s.xml")

(defvar mew-prog-text/html           'mew-mime-text/html-w3m) ;; See w3m.el
(defvar mew-prog-text/html-ext       mew-w32-exec)

(defvar mew-prog-text/xml            'mew-mime-text/html-w3m) ;; See w3m.el
(defvar mew-prog-text/xml-ext        mew-w32-exec)

(defvar mew-prog-application/xml     nil)
(defvar mew-prog-application/xml-ext mew-w32-exec)

(defvar mew-prog-image/*         'mew-mime-image/*)
(defvar mew-prog-image/*-ext     mew-w32-exec)

(defvar mew-prog-application/msword nil)
(defvar mew-prog-application/msexcel nil)
(defvar mew-prog-application/mspowerpoint nil)
(defvar mew-prog-application/rtf nil)

(setq mew-cs-database-for-arg
      '((iso-2022-jp . shift_jis-unix)
	(iso-2022-kr . euc-kr-unix)))

(setq mew-prog-grep-max-msgs 2000) ;; for external grep (pick & virtual)

(defvar mew-dir-list-function 'mew-dir-list-without-link-count)

(defun mew-draft-dnd-for-meadow (event)
  (interactive "e")
  (when (eq (posn-window (event-start event)) (selected-window))
    (let ((files (car (cdr (cdr event))))
	  to)
      (if (stringp files)
	  (setq files (list files)))
      (dolist (from files)
	(setq from (dos-to-unix-filename from))
	(when (file-exists-p from)
	  (setq to (file-name-nondirectory from))
	  (unless (mew-attach-p)
	    (mew-draft-prepare-attachments))
	  (goto-char (point-max))
	  (forward-line -2)
	  (mew-attach-next)
	  (cond
	   ((file-directory-p from)
	    (message "Directory cannot be attached"))
	   (t
	    (mew-attach-copy from to))))))))

(provide 'mew-win32)

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

;;; mew-win32.el ends here
