;;; mew-vars3.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: May 10, 2006

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro
;;;

(defvar mew-face-spec-alist
  '((:tty   (type tty))
    (:light (background light))
    (:dark  (background dark))))

;; See also mew-header-color-base

(defun mew-face-make-spec (bold &rest spec)
  (let (ret key col)
    (if (and bold (not (member :tty spec)))
	(setq ret (cons (mew-face-spec-func '((class color) (type tty))
					    (mew-face-spec-primitive nil t))
			ret)))
    (while spec
      (setq key  (car spec))
      (setq spec (cdr spec))
      (setq col  (car spec))
      (setq spec (cdr spec))
      (setq ret (cons
		 (mew-face-spec-func
		  (list '(class color)
			(mew-alist-get-value (assoc key mew-face-spec-alist)))
		  (mew-face-spec-primitive col bold))
		 ret)))
    (setq ret (cons (mew-face-spec-func t (mew-face-spec-primitive nil bold))
		    ret))
    (nreverse ret)))

(defmacro mew-setface (sym &rest spec)
  ;; (declare (indent 1))
  `(face-spec-set
    ',(intern (concat "mew-face-" (symbol-name sym)))
    ',(apply 'mew-face-make-spec nil spec)))
(put 'mew-setface 'lisp-indent-function 1)

(defmacro mew-setface-bold (sym &rest spec)
  ;; (declare (indent 1))
  `(face-spec-set
    ',(intern (concat "mew-face-" (symbol-name sym)))
    ',(apply 'mew-face-make-spec 'bold spec)))
(put 'mew-setface-bold 'lisp-indent-function 1)

(defmacro mew-defface (sym doc &rest spec)
  ;; (declare (indent 1))
  `(defface ,(intern (concat "mew-face-" (symbol-name sym)))
     ',(apply 'mew-face-make-spec nil spec)
     ,(concat "*" doc)
     :group 'mew-highlight))
(put 'mew-defface 'lisp-indent-function 1)

(defmacro mew-defface-bold (sym doc &rest spec)
  ;; (declare (indent 1))
  `(defface ,(intern (concat "mew-face-" (symbol-name sym)))
     ',(apply 'mew-face-make-spec 'bold spec)
     ,(concat "*" doc)
     :group 'mew-highlight))
(put 'mew-defface-bold 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight header
;;;

(mew-defface-bold header-subject
  "Face to highlight the value of Subject:"
  :tty "red" :light "Firebrick" :dark "OrangeRed")

(mew-defface-bold header-from
  "Face to highlight the value of From:"
  :tty "yellow" :light "DarkOrange4" :dark "Gold")

(mew-defface-bold header-date
  "Face to highlight the value of Date:"
  :tty "green" :light "ForestGreen" :dark "LimeGreen")

(mew-defface-bold header-to
  "Face to highlight the value of To:"
  :tty "magenta" :light "DarkViolet" :dark "violet")

(mew-defface-bold header-key
  "Face to highlight by default"
  :tty "green" :light "ForestGreen" :dark "LimeGreen")

(mew-defface-bold header-private
  "Face to highlight private field-keys")

(mew-defface-bold header-important
  "Face to highlight important field-keys"
  :tty "cyan" :light "MediumBlue" :dark "SkyBlue")

(mew-defface-bold header-marginal
  "Face to highlight marginal field-values"
  :light "gray50" :dark "gray50")

(mew-defface-bold header-warning
  "Face to highlight non-my-domain addresses on To:/Cc:/Bcc:"
  :tty "red" :light "red" :dark "red")

(mew-defface-bold header-xmew
  "Face to highlight the value of X-Mew:"
  :tty "yellow" :light "chocolate" :dark"chocolate")

(mew-defface-bold header-xmew-bad
  "Face to highlight the value of X-Mew: in bad cases"
  :tty "red" :light "red" :dark "red")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight body
;;;

(mew-defface body-url
  "Face to highlight URL in Message/Draft mode"
  :tty "red" :light "Firebrick" :dark "OrangeRed")

(mew-defface body-comment
  "Face to highlight comments in a body"
  :tty "blue" :light "gray50" :dark "gray50")

(mew-defface body-cite1
  "Face to highlight the first citation"
  :tty "green" :light "ForestGreen" :dark "LimeGreen")

(mew-defface body-cite2
  "Face to highlight the second citation"
  :tty "cyan" :light "MediumBlue" :dark "SkyBlue")

(mew-defface body-cite3
  "Face to highlight the third citation"
  :tty "magenta" :light "DarkViolet" :dark "violet")

(mew-defface body-cite4
  "Face to highlight the forth citation"
  :tty "yellow" :light "DarkOrange4" :dark "Gold")

(mew-defface body-cite5
  "Face to highlight the fifth citation"
  :tty "red" :light "Firebrick" :dark "OrangeRed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight mark
;;;

(mew-defface mark-review
  "Face to highlight the review mark"
  :tty "cyan" :light "MediumBlue" :dark "SkyBlue")

(mew-defface mark-escape
  "Face to highlight the escape mark"
  :tty "magenta" :light "DarkViolet" :dark "violet")

(mew-defface mark-delete
  "Face to highlight the delete mark"
  :tty "red" :light "Firebrick" :dark "OrangeRed")

(mew-defface mark-unlink
  "Face to highlight the unlink mark"
  :tty "yellow" :light "DarkOrange4" :dark "Gold")

(mew-defface mark-refile
  "Face to highlight the refile mark"
  :tty "green" :light "ForestGreen" :dark "LimeGreen")

(mew-defface mark-unread
  "Face to highlight the unread mark")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight eof
;;;

(mew-defface-bold eof-message
  "Face to highlight the 'end of message' string"
  :tty "green" :light "ForestGreen" :dark "LimeGreen")

(mew-defface-bold eof-part
  "Face to highlight the 'end of part' string"
  :tty "yellow" :light "DarkOrange4" :dark "Gold")

(provide 'mew-vars3)

;;; Copyright Notice:

;; Copyright (C) 1996-2011 Mew developing team.
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

;;; mew-vars.el ends here
