;;; mew-fib.el --- Filling blanks for Mew

;; Author:  Yoshinari NOMURA <nom@csce.kyushu-u.ac.jp>
;; Created: Nov 29, 1994

;;; Code:

(require 'mew-func)

(defvar mew-fib-item-alist nil)

(defun mew-fib-split (str)
  (let (ret match)
    (while (string-match "[\t \n]*\\([^,]+\\)" str)
      (setq match (substring str (match-beginning 1) (match-end 1)))
      (setq str (substring str (match-end 0)))
      (setq match (if (string-match "[\t \n]+$" match)
		      (substring match 0 (match-beginning 0))
		    match))
      (setq ret (cons match ret)))
    (nreverse ret)))

(defun mew-fib-make-alist ()
  (let ((fib-file (expand-file-name mew-fib-item-file))
	item val ret tmp-val)
    (setq mew-fib-item-alist nil)
    (with-temp-buffer
      (if (file-exists-p fib-file)
	  (mew-insert-file-contents fib-file))
      (goto-char (point-min))
      (delete-matching-lines "^[ \t]*[;#%]")
      (while (re-search-forward "^\\([^:]+\\):[ \t]*\\(.*\\)$" nil t)
	(setq item    (mew-buffer-substring (match-beginning 1) (match-end 1))
	      tmp-val (mew-buffer-substring (match-beginning 2) (match-end 2))
	      val (if (string= tmp-val "") val tmp-val)
	      ret (append ret (mapcar (lambda (arg) (cons (downcase arg) val))
				      (mew-fib-split item))))))
    ret))

(defun mew-fib-fill-default ()
  "Fill |>item<| according to the information from .mew-fib"
  (interactive)
  (save-excursion
    (let (begin end str)
      (setq mew-fib-item-alist (mew-fib-make-alist))
      (goto-char (point-min))
      (while (re-search-forward "|>\\([^<]+\\)<|" nil t)
	(setq begin (match-beginning 1)
	      end (match-end 1)
	      str (mew-buffer-substring begin end))
	(delete-region begin end)
	(backward-char 2)
	(insert (or (cdr (assoc (downcase str) mew-fib-item-alist)) str))))))

(defun mew-fib-delete-frame ()
  "Delete all quotations, '|>' and '<|'. This is the finishing stroke."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "|>\\|<|" nil t)
      (replace-match "" nil t))))

(defun mew-fib-flush-input ()
  "Flush the fib item on the current point."
  (interactive)
  (save-excursion
    (let ((ptr (point)))
      (if (and (search-backward "|>" nil t)
	       (looking-at "|>\\([^<]+\\)<|")
	       (>= ptr (match-beginning 1))
	       (<= ptr (match-end 1)))
	  (delete-region (match-beginning 1)
			 (match-end 1))))))

(defun mew-fib-next-item ()
  "Jump to the next fib item."
  (interactive)
  (if (re-search-forward "|>\\([^<]+\\)<|" nil t)
      (backward-char 2)
    (goto-char (point-min))
    (re-search-forward "|>\\([^<]+\\)<|" nil t)))

(defun mew-fib-previous-item ()
  "Jump to the previous fib item."
  (interactive)
  (if (re-search-backward "|>\\([^<]+\\)<|" nil t)
      (forward-char 2)
    (goto-char (point-max))
    (re-search-backward "|>\\([^<]+\\)<|" nil t)))

(provide 'mew-fib)

;;; Copyright Notice:

;; Copyright (C) 1994-2011 Mew developing team.
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

;;; mew-fib.el ends here
