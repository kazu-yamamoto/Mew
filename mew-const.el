;;; mew-const.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  1, 1999

;;; Code:

(require 'mew)

;; Constants which are used in two or more files must be here.
;; Constants used only in a file should be defined in the file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Suffix
;;;

(defconst mew-suffix ".mew")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readability
;;;

(defconst mew-sp ?\ )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message fields
;;;

;; Must be capitalized!

(defconst mew-subj:   "Subject:")
(defconst mew-to:     "To:")
(defconst mew-cc:     "Cc:")
(defconst mew-from:   "From:")
(defconst mew-sender: "Sender:")
(defconst mew-date:   "Date:")

(defconst mew-bcc:  "Bcc:")
(defconst mew-dcc:  "Dcc:")
(defconst mew-fcc:  "Fcc:")

(defconst mew-message-id:  "Message-Id:")
(defconst mew-references:  "References:")
(defconst mew-in-reply-to: "In-Reply-To:")

(defconst mew-resent-to:     "Resent-To:")
(defconst mew-resent-cc:     "Resent-Cc:")
(defconst mew-resent-from:   "Resent-From:")
(defconst mew-resent-sender: "Resent-Sender:")
(defconst mew-resent-date:   "Resent-Date:")

(defconst mew-resent-bcc:  "Resent-Bcc:")
(defconst mew-resent-dcc:  "Resent-Dcc:")
(defconst mew-resent-fcc:  "Resent-Fcc:")

(defconst mew-resent-regex "^Resent-")

(defconst mew-resent-message-id: "Resent-Message-Id:")

(defconst mew-reply-to:     "Reply-To:")
(defconst mew-organization: "Organization:")
(defconst mew-newsgroups:   "Newsgroups:")
(defconst mew-distribution: "Distribution:")
(defconst mew-followup-to:  "Followup-To:")

(defconst mew-apparently-to: "Apparently-To:")

;; Must not include Bcc:.
(defconst mew-destination:-list
  `(,mew-to: ,mew-cc: ,mew-dcc: ,mew-bcc: ,mew-apparently-to:))
(defconst mew-resent-dest:-list
  `(,mew-resent-to: ,mew-resent-cc: ,mew-resent-bcc: ,mew-resent-dcc:))

(defconst mew-received: "Received:")

(defconst mew-x-mailer:    "X-Mailer:")
(defconst mew-x-face:      "X-Face:")
(defconst mew-face:        "Face:")
(defconst mew-x-mew:       "X-Mew:")
(defconst mew-x-mew-uidl:  "X-Mew-Uidl:")
(defconst mew-x-mew-ref:   "X-Mew-Ref:")

(defconst mew-keywords: "Keywords:")
(defconst mew-body:	"Body:")

(defconst mew-mv:     "Mime-Version:")
(defconst mew-mv:-num "1.0")

(defconst mew-ct:  "Content-Type:")
(defconst mew-cte: "Content-Transfer-Encoding:")
(defconst mew-cd:  "Content-Description:")
(defconst mew-cid: "Content-Id:")
(defconst mew-cdp: "Content-Disposition:")

(defconst mew-ct-txt "Text/Plain")
(defconst mew-ct-htm "Text/Html")
(defconst mew-ct-xml "Text/Xml")
(defconst mew-ct-trh "Text/Rfc822-Headers")
(defconst mew-ct-msg "Message/Rfc822")
(defconst mew-ct-ext "Message/External-Body")
(defconst mew-ct-sts "Message/Delivery-Status")
(defconst mew-ct-mlm "Multipart/Mixed")
(defconst mew-ct-mla "Multipart/Alternative")
(defconst mew-ct-mls "Multipart/Signed")
(defconst mew-ct-mle "Multipart/Encrypted")
(defconst mew-ct-mld "Multipart/Digest")
(defconst mew-ct-ado "Audio/Basic")
(defconst mew-ct-apo "Application/Octet-Stream")
(defconst mew-ct-aps "Application/Postscript")
(defconst mew-ct-apk "Application/Pgp-Keys")
(defconst mew-ct-pgs "application/pgp-signature") ;; due to the RFC 1847 bug
(defconst mew-ct-pge "application/pgp-encrypted") ;; due to the RFC 1847 bug
(defconst mew-ct-smm "Application/Pkcs7-Mime")
(defconst mew-ct-xsmm "Application/X-Pkcs7-Mime")
(defconst mew-ct-smm-enc "enveloped-data")
(defconst mew-ct-smm-sig "signed-data")
(defconst mew-ct-sms "application/pkcs7-signature")  ;; due to the RFC 1847 bug
(defconst mew-ct-xsms "application/x-pkcs7-signature")  ;; due to the RFC 1847 bug

(defconst mew-us-ascii "us-ascii")
(defconst mew-utf-8 "utf-8")
(defconst mew-iso-2022-jp-2 "iso-2022-jp-2")
(defconst mew-cs-unknown 'unknown)
(defconst mew-cs-m17n-list `(,mew-utf-8 ,mew-iso-2022-jp-2))

(defconst mew-type-txt `(,mew-ct-txt ("charset" ,mew-us-ascii)))
(defconst mew-type-msg `(,mew-ct-msg))
(defconst mew-type-mlm `(,mew-ct-mlm))
(defconst mew-type-ado `(,mew-ct-ado))
(defconst mew-type-apo `(,mew-ct-apo))
(defconst mew-type-apk `(,mew-ct-apk))

(defconst mew-b64  "base64")
(defconst mew-qp   "quoted-printable")
(defconst mew-xg   "x-gzip64")
(defconst mew-7bit "7bit")
(defconst mew-8bit "8bit")
(defconst mew-bin  "binary")
(defconst mew-xuu  "x-uuencode")

(defconst mew-decode-value
  `(,mew-b64 ,mew-qp ,mew-xg ,mew-7bit ,mew-8bit ,mew-bin ,mew-xuu))
(defconst mew-decode-composite-value
  `(,mew-7bit ,mew-8bit ,mew-bin))

(defun mew-cte-p (cte)
  (member (downcase cte) mew-decode-value))

(defun mew-cte-composite-p (cte)
  (member (downcase cte) mew-decode-composite-value))

;; must be list of list.
(defconst mew-mime-fields
  `((,mew-ct:  0 analyze)
    (,mew-cte: 1 extract)
    (,mew-cd:  2 decode)
    (,mew-cid: 3 id)
    (,mew-cdp: 4 analyze)))

(defconst mew-syntax-magic
  (+ (length [key beg end pri]) (length mew-mime-fields)))

(defconst mew-x-mailer
  (format "%s on Emacs %s / Mule %s"
	  mew-version
	  (when (string-match "^\\([.0-9]+\\)\\.[0-9]+$" emacs-version)
	    (match-string 1 emacs-version))
	  mule-version)
  "*A value inserted into X-Mailer: field in Draft mode if *non-nil*.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffers
;;;

(defconst mew-buffer-prefix    " *Mew* ")
(defconst mew-buffer-regex "^ ?\\*Mew\\* ")

(defconst mew-buffer-cache " *mew cache*")
(defconst mew-buffer-cache-prefix (concat "^" (regexp-quote mew-buffer-cache)))

(defconst mew-buffer-message     "*Mew message*")
(defconst mew-buffer-completions "*Mew completions*")
(defconst mew-buffer-addrbook    "*Mew Addrbook*")
(defconst mew-buffer-debug       "*Mew debug*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Suffix
;;;

(defconst mew-ext-suffix        ".ext")
(defconst mew-text-suffix       ".txt")
(defconst mew-audio-suffix      ".au")
(defconst mew-pgp-key-suffix    ".pka")
(defconst mew-pgp-ascii-suffix  ".asc")
(defconst mew-queue-work-suffix ".wrk")
(defconst mew-queue-info-suffix ".mqi")
(defconst mew-draft-info-suffix ".mdi")
(defconst mew-imapq-info-suffix ".iqi")
(defconst mew-backup-suffix     ".old")
(defconst mew-backup-prefix     "#")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Regular expressions
;;;

(defconst mew-regex-files "^[^.]\\|^.[^.]") ;; xxx "[^.]" is enough?

;;;
;;; Sumsyn
;;;

(defconst mew-regex-mark "^\\([^ \n]\\)")
(defun mew-sumsyn-mark () (string-to-char (mew-match-string 1)))

;; fld msg
(defconst mew-regex-sumsyn-short   "\r \\([^ \n]*\\) \\([^ \n]+\\)")

;; fld msg my-id par-id uid siz
(defconst mew-regex-sumsyn-long    "\r \\([^ \n]*\\) \\([^ \n]+\\) \\([^ \n]*\\) \\([^ \n]*\\) \\([^ \n]*\\) \\([0-9]*\\)")

;; fld msg
(defun mew-regex-sumsyn-msg (msg)
                           (format "\r \\([^ \n]*\\) \\(%s\\) " msg))

(defun mew-summary-search-msg (msg)
  (let ((regex (mew-regex-sumsyn-msg msg)))
    (when (or (re-search-forward  regex nil t)
	      (re-search-backward regex nil t))
      (beginning-of-line)
      t)))

(defconst mew-regex-sumsyn-valid   "\r \\([^ \n]*\\) \\([^ \n]+\\)")
(defconst mew-regex-sumsyn-invalid "\r [^ \n]* 0[1-9][0-9]*")

(defun mew-msg-validp (msg)
  (and msg (string-match mew-regex-message-files msg)))

(defun mew-msg-invalidp (msg)
  (and msg (string-match "^0" msg)))

(defun mew-msg-truncatedp (siz)
  (and siz (string-match "^0" siz)))

;; fld msg my-id par-id uid siz
(defun mew-regex-sumsyn-my-id  (my-id)
  (format "\r [^ \n]* [0-9]+ %s [^ \n]*" (regexp-quote my-id)))
(defun mew-regex-sumsyn-par-id (par-id)
  (format "\r [^ \n]* [0-9]+ [^ \n]* %s" (regexp-quote par-id)))

;;;
;;;
;;;

(defconst mew-regex-msg-icon  "^\\([0-9]+\\) ")
(defconst mew-regex-part      "^.... \\([.0-9]+\\) ")
(defconst mew-regex-part-icon "<\\([.0-9]+\\)>")

(defun mew-regex-jmp-part (part) (format "^.... \\(%s\\) " part))

;; [\033\200-\377] does not work due to the multibyte
(defconst mew-regex-esc-or-nonascii "[^\000-\032\034-\177]") ;; \033 = esc
(defconst mew-regex-nonascii "[^\000-\177]")
(defconst mew-regex-singlebyte-nonascii
  "\\(\\`\\|[\000-\177]\\)[^\000-\177]\\([\000-\177]\\|\\'\\)")

;; The followings MUST contain "+" for conversion.
;; \011 = tab, \012 = lf
(defconst mew-regex-ctls "[\000-\037\177]+")
(defconst mew-regex-ctls-wo-tab "[\000-\010\012-\037\177]+")
(defconst mew-regex-ctls-wo-tab-lf "[\000-\010\013-\037\177]+")

;; Can skip \n. Match the first id. For example:
;; In-Reply-To: Your message of "Mon, 5 Oct 1998 13:33:14 +0900"
;;	<19981005133317N.kazu@example.org>
(defconst mew-regex-id "<[^>\n\t]+>")

(defconst mew-regex-case "^\\([^-+$%*,:][^,:]+\\):\\(.*\\)")
(defconst mew-regex-case1 "^\\([^-+$%*,:][^,:]+\\(,[^,:]+\\)*\\):\\(.*\\)")
(defconst mew-regex-case2 "\\([^-+$%*,:][^,:]+\\(,[^,:]+\\)*\\):")

(defconst mew-regex-punycode "xn--\\([-0-9a-zA-z]+\\)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder
;;;

(defconst mew-folder-local   "+")
(defconst mew-folder-pop     "$")
(defconst mew-folder-imap    "%")
(defconst mew-folder-nntp    "-")
(defconst mew-folder-virtual "*")
(defconst mew-folder-prefixes
  `(,mew-folder-local ,mew-folder-pop ,mew-folder-imap
    ,mew-folder-nntp ,mew-folder-virtual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defconst mew-home (file-name-as-directory "~"))

(defconst mew-case-default "default"
  "The default value for case.")

(defconst mew-draft-coverpage "Cover.txt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inherit
;;;

(defconst mew-inherit-case nil)
(defconst mew-inherit-exec-case nil)
(defconst mew-inherit-alt nil)
(defconst mew-inherit-7bit nil)
(defconst mew-inherit-offline nil)
(defconst mew-inherit-decode-signer nil)
(defconst mew-inherit-encode-pgp-signer nil)
(defconst mew-inherit-encode-smime-signer nil)
(defconst mew-inherit-prefetching nil)
(defconst mew-inherit-refile-case nil)
(defconst mew-inherit-refile-proto nil)
(defconst mew-inherit-grep-cmd nil)
(defconst mew-inherit-after-marking nil)
(defconst mew-inherit-pick-tokens nil)
(defconst mew-inherit-pick-ret nil)
(defconst mew-inherit-pick-mewlp nil)
(defconst mew-inherit-pick-omit-and nil)
(defconst mew-inherit-pick-omit-and2 nil)
(defconst mew-inherit-submission nil)
(defconst mew-inherit-complete-folder nil)

(provide 'mew-const)

;;; Copyright Notice:

;; Copyright (C) 2000-2014 Mew developing team.
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

;;; mew-const.el ends here
