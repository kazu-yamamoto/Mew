;;; mew-darwin.el --- Settings for Mew on Darwin

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct 30, 2002

;;; Code:

;;
;; MIME setting
;;

(defvar mew-darwin-exec "open")
(defvar mew-default-external-program mew-darwin-exec)

(defvar mew-prog-plain    'mew-mime-text/plain)
(defvar mew-prog-html     '(mew-mime-text/html mew-mime-text/html-ext))
(defvar mew-prog-xml      '(mew-mime-text/xml  mew-mime-text/xml-ext))
(defvar mew-prog-patch    '(mew-mime-text/plain mew-mime-text/patch-ext))
(defvar mew-prog-enriched 'mew-mime-text/enriched)
(defvar mew-prog-text     'mew-mime-text/plain)
(defvar mew-prog-audio           mew-darwin-exec)
(defvar mew-prog-audio2          mew-darwin-exec) ;; dummy
(defvar mew-prog-image           '(mew-mime-image/* mew-mime-image/*-ext))
(defvar mew-prog-iges            mew-darwin-exec)
(defvar mew-prog-vrml            mew-darwin-exec)
(defvar mew-prog-mesh            mew-darwin-exec)
(defvar mew-prog-video           mew-darwin-exec)
(defvar mew-prog-rfc822          'mew-mime-message/rfc822)
(defvar mew-prog-rfc822-headers  'mew-mime-text/rfc822-headers)
(defvar mew-prog-external-body   '(mew-mime-external-body mew-mime-external-body-ext))
(defvar mew-prog-delivery-status 'mew-mime-text/plain)
(defvar mew-prog-postscript      mew-darwin-exec)
(defvar mew-prog-pgp-keys        '(mew-mime-pgp-keys mew-mime-pgp-keys-ext))

(defvar mew-prog-application/pdf "pdftotext")
(defvar mew-prog-pdf-ext         mew-darwin-exec)
(defvar mew-prog-pdf             `(mew-mime-application/pdf ,mew-prog-pdf-ext))

(defvar mew-prog-xml2            '(mew-mime-application/xml
				   mew-mime-application/xml-ext))
(defvar mew-prog-oasys           mew-darwin-exec)
(defvar mew-prog-octet-stream    mew-darwin-exec)
(defvar mew-prog-msword          mew-darwin-exec)
(defvar mew-prog-msexcel         mew-darwin-exec)
(defvar mew-prog-mspowerpoint    mew-darwin-exec)
(defvar mew-prog-visio           mew-darwin-exec)
(defvar mew-prog-ooffice         mew-darwin-exec)
(defvar mew-prog-rtf             mew-darwin-exec)
(defvar mew-prog-unzip           mew-darwin-exec)

;;;
;;; Text/Html, Application/Xml, Image
;;;

(defvar mew-format-html "%s.html")
(defvar mew-format-xml  "%s.xml")

(defvar mew-prog-text/html           'mew-mime-text/html-w3m) ;; See w3m.el
(defvar mew-prog-text/html-ext       mew-darwin-exec)

(defvar mew-prog-text/xml            'mew-mime-text/html-w3m) ;; See w3m.el
(defvar mew-prog-text/xml-ext        mew-darwin-exec)

(defvar mew-prog-application/xml     nil)
(defvar mew-prog-application/xml-ext mew-darwin-exec)

(defvar mew-prog-image/*         'mew-mime-image/*)
(defvar mew-prog-image/*-ext     mew-darwin-exec)

(defvar mew-prog-application/msword nil)
(defvar mew-prog-application/msexcel nil)
(defvar mew-prog-application/mspowerpoint nil)
(defvar mew-prog-application/rtf nil)

;;

(setq mew-delete-temp-file nil)
(defvar mew-dir-list-function 'mew-dir-list-with-link-count)

(provide 'mew-darwin)

;;; Copyright Notice:

;; Copyright (C) 2002-2014 Mew developing team.
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

;;; mew-darwin.el ends here
