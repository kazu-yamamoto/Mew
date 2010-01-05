;;; mew-lang-latin.el --- Latin specific stuff for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 11, 2002

;;; Code:

(require 'mew)

(defvar mew-latin01-list '(36 38 40 52 56 60 61 62))

(defvar mew-charset-latin-alist
  '(("iso-8859-1"  latin-iso8859-1  latin-iso8859-15)
    ("iso-8859-15" latin-iso8859-15 latin-iso8859-1)))

(defun mew-latin-make-regex (lc lst)
  (mapconcat (lambda (x) (char-to-string (make-char lc x))) lst "\\|"))

(defvar mew-latin0-regex
  (mew-latin-make-regex 'latin-iso8859-15 mew-latin01-list))

(defvar mew-latin1-regex
  (mew-latin-make-regex 'latin-iso8859-1 mew-latin01-list))

(defun mew-latin0-region (beg end)
  (let* (charset ent to from ch lc conflict0 conflict1)
    (save-restriction
      (narrow-to-region beg end)
      (setq conflict0
	    (save-excursion
	      (goto-char (point-min))
	      (re-search-forward mew-latin0-regex nil t)))
      (setq conflict1
	    (save-excursion
	      (goto-char (point-min))
	      (re-search-forward mew-latin1-regex nil t)))
      (unless (and conflict0 conflict1) ;; mew-charset-m17n
	(cond
	 (conflict0
	  (setq charset "iso-8859-15"))
	 (conflict1
	  (setq charset "iso-8859-1"))
	 (t
	  (setq charset mew-charset-latin)))
	(setq ent (assoc charset mew-charset-latin-alist))
	(mew-set '(nil to from) ent)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "\\cl" nil t)
	    (setq ch (split-char (preceding-char)))
	    (setq lc (car ch))
	    (when (eq lc from)
	      (delete-char -1)
	      (insert (make-char to (nth 1 ch))))))))))

(provide 'mew-lang-latin)

;;; Copyright Notice:

;; Copyright (C) 2002-2010 Mew developing team.
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

;;; mew-lang-latin.el ends here
