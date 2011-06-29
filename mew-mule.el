;;; mew-mule.el --- Environment of Mule common for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jul 15, 1998

;;; Code:

;;
;; Charset
;;

(defun mew-charset-m17n ()
  (if (string= mew-charset-m17n mew-utf-8)
      (if mew-internal-utf-8p
	  mew-utf-8
	(if (condition-case nil (require 'un-define) (file-error nil))
	    mew-utf-8
	  mew-iso-2022-jp-2))
    mew-iso-2022-jp-2))

(defun mew-charset-guess-string (str)
  (let ((ecsdb (mew-ecsdb-guess-string str)))
    (if ecsdb
	(mew-cs-to-charset (mew-ecsdb-get-cs ecsdb)) ;; not hcs, take care
      (mew-charset-m17n))))

(defun mew-ecsdb-guess-string (str)
  (with-temp-buffer
    (insert str)
    (mew-ecsdb-guess-region (point-min) (point-max))))

(defun mew-charset-guess-region (beg end)
  "Guess charset for the region."
  (interactive "r")
  (let ((ecsdb (mew-ecsdb-guess-region beg end))
	ret)
    (if (null ecsdb)
	(setq ret (mew-charset-m17n))
      (setq ret (mew-cs-to-charset (mew-ecsdb-get-cs ecsdb)))) ;; not hcs
    (if (interactive-p) (message "%s" ret)) ;; for debug
    ret))

(defun mew-ecsdb-guess-region (beg end)
  (let* ((tcsl (mew-find-cs-region beg end))
	 (N (length tcsl))
	 (alst mew-cs-database-for-encoding)
	 acsl csl ret)
    (dolist (ecsdb alst)
      (setq acsl (mew-ecsdb-get-lcs ecsdb))
      (catch 'loop
	(dotimes (i N)
	  (unless (member (nth i tcsl) acsl)
	    (setq ecsdb nil)
	    (setq acsl nil)
	    (throw 'loop nil))))
      (if (null ret)
	  (setq ret ecsdb)
	(if (and acsl (< (length acsl) (length csl)))
	    (setq ret ecsdb csl acsl))))
    ret))

(autoload 'mew-zenkaku-katakana-region "mew-lang-jp" nil)
(autoload 'mew-latin0-region "mew-lang-latin" nil)

(defun mew-charset-sanity-check (beg end)
  "Eliminate invalid characters"
  (interactive "r")
  (let ((lcs (mew-find-cs-region beg end)))
    (cond
     ((member mew-lc-kana lcs)
      (mew-zenkaku-katakana-region beg end))
     ((and (not (fboundp 'set-charset-priority))
	   (memq 'latin-iso8859-1 lcs) (memq 'latin-iso8859-15 lcs)) ;; xxx
      (mew-latin0-region beg end)))))

;;;
;;;
;;;

(defun mew-cs-to-charset (cs)
  (let ((dcsdb (mew-assoc-equal cs mew-cs-database-for-decoding 1)))
    (if (null dcsdb)
	(mew-charset-m17n)
      (mew-dcsdb-get-charset dcsdb))))

(defun mew-charset-to-cs (charset)
  (when charset
    (let ((dcsdb (assoc (downcase charset) mew-cs-database-for-decoding)))
      (if (null dcsdb)
	  mew-cs-unknown
	(mew-dcsdb-get-cs dcsdb)))))

(defun mew-charset-to-ecsdb (charset)
  (mew-assoc-equal (mew-charset-to-cs charset) mew-cs-database-for-encoding 1))

(defun mew-charset-to-cte (charset)
  (mew-ecsdb-get-cte (mew-charset-to-ecsdb charset)))

(defun mew-charset-to-delsp (charset)
  (mew-ecsdb-get-delsp (mew-charset-to-ecsdb charset)))

;;

(defvar mew-charset-list
  (mapcar 'mew-dcsdb-get-charset mew-cs-database-for-decoding))

;;

(defun mew-cs-encode-arg (arg)
  (let ((cs (mew-ecsdb-cs-for-arg (mew-ecsdb-guess-string arg))))
    (if (mew-coding-system-p cs)
	(mew-cs-encode-string arg cs)
      arg)))

(provide 'mew-mule)

;;; Copyright Notice:

;; Copyright (C) 1998-2011 Mew developing team.
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

;;; mew-mule.el ends here
