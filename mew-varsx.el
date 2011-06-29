;;; mew-varsx.el --- Variables depends on other variables

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: May 10, 2006

;;; Code:

(defun mew-defvar (sym val)
  (unless (symbol-value sym)
    (set sym val)))

;;
;; Scan
;;

(mew-defvar
 'mew-basic-folders
 (list mew-inbox-folder mew-draft-folder mew-queue-folder mew-postq-folder))

(mew-defvar
 'mew-scan-fields
 (list "Folder:" "Filename:"
       mew-subj: mew-date: mew-from: mew-to: mew-cc:
       mew-ct: mew-cte: mew-x-mew-uidl:
       mew-message-id: mew-in-reply-to: mew-references: mew-x-mew-ref:
       mew-spam: "Body"))

(mew-defvar
 'mew-scan-fields-alias
 '("FLD" "NUM" "SUBJ" "DATE" "FROM" "TO" "CC"
   "CT" "CTE" "UID"
   "ID" "IRT" "REF" "XREF"
   "SPAM" "BODY"))

;;
;; Office
;;

;; strict valid
(mew-defvar
 'mew-regex-message-files
 (format  "^[1-9][0-9]*\\(%s\\)?$" (regexp-quote mew-suffix)))

;; strict invalid
(mew-defvar
 'mew-regex-message-files4
 (format "^0[1-9][0-9]*\\(%s\\)?$" (regexp-quote mew-suffix)))

;; strict all
(mew-defvar
 'mew-regex-message-files3
 (format "^\\([0-9]+\\)\\(%s\\)?$" (regexp-quote mew-suffix)))

;; mewl and grep
(mew-defvar
 'mew-regex-message-files2
 (format "^\\([0-9]+\\)\\(%s\\)?" (regexp-quote mew-suffix)))

;; search
(mew-defvar
 'mew-regex-message-files5
 (format  "\\([0-9]+\\)\\(%s\\)?$" (regexp-quote mew-suffix)))

;;
;; MIME
;;


(mew-defvar
 'mew-mime-content-type
 `(("multipart"    nil         nil     nil            mew-icon-multipart)
   ;;
   ("audio/basic"  "\\.au$"    mew-b64 mew-prog-audio mew-icon-audio)
   ("audio/x-wav"  "\\.wav$"   mew-b64 mew-prog-audio mew-icon-audio)
   ("audio/x-aiff" "\\.aif?f$" mew-b64 mew-prog-audio mew-icon-audio)
   ("audio/x-midi" "\\.midi?$" mew-b64 mew-prog-audio mew-icon-audio)
   ("audio/x-mpeg" "\\.mpga$\\|\\.mp[23]$" mew-b64 mew-prog-audio mew-icon-audio)
   ;;
   ("image/gif"   "\\.gif$"   mew-b64 mew-prog-image mew-icon-image gif)
   ("image/tiff"  "\\.tif?f$" mew-b64 mew-prog-image mew-icon-image tiff)
   ("image/jpeg"  "\\.jpe?g$" mew-b64 mew-prog-image mew-icon-image jpeg)
   ("image/pjpeg" "\\.jfif$"  mew-b64 mew-prog-image mew-icon-image jpeg) ;; MS
   ("image/png"   "\\.png$"   mew-b64 mew-prog-image mew-icon-image png)
   ("image/x-xwd" "\\.xwd$"   mew-b64 mew-prog-image mew-icon-image xwd)
   ("image/x-xbm" "\\.xbm$"   mew-b64 mew-prog-image mew-icon-image xbm)
   ("image/x-xpm" "\\.xpm$"   mew-b64 mew-prog-image mew-icon-image xpm)
   ("image/x-bmp" "\\.bmp$"   mew-b64 mew-prog-image mew-icon-image bmp)
   ("image/x-pcx" "\\.pcx$"   mew-b64 mew-prog-image mew-icon-image PCX)
   ("image/x-tga" "\\.tga$"   mew-b64 mew-prog-image mew-icon-image TGA)
   ("image/vnd.ms-modi" "\\.mdi$" mew-b64 mew-prog-image mew-icon-image)
   ("image"       "^$"        mew-b64 mew-prog-image mew-icon-image)
   ;;
   ("model/iges" "\\.ige?s$" mew-b64 mew-prog-iges  mew-icon-image) ;; xxx
   ("model/vrml" "\\.wrl$"   mew-b64 mew-prog-vrml  mew-icon-image)
   ("model/mesh" "\\.me?sh$" mew-b64 mew-prog-mesh  mew-icon-image)
   ("model"      "^$"        mew-b64 mew-prog-model mew-icon-image)
   ;;
   ("video/mpeg"      "\\.mpe?g$" mew-b64 mew-prog-video mew-icon-video)
   ("video/quicktime" "\\.mov$"   mew-b64 mew-prog-video mew-icon-video)
   ("video/x-msvideo" "\\.avi$"   mew-b64 mew-prog-video mew-icon-video)
   ;;
   ("message/rfc822"          ,(format "^[0-9]+\\(%s\\)?$" mew-suffix) nil
    mew-prog-rfc822           mew-icon-message/rfc822)
   ("message/external-body"   "\\.ext$"  nil
    mew-prog-external-body    mew-icon-message/external-body)
   ("message/delivery-status" "^$"       nil
    mew-prog-delivery-status  mew-icon-text)
   ;;
   ("application/postscript"        "\\.e?ps$" mew-qp
    mew-prog-postscript             mew-icon-application/postscript)
   ("application/pdf"               "\\.pdf$"  mew-b64
    mew-prog-pdf                    mew-icon-application/postscript)
   ;; Thunderbird/3.1.7 only, not registered in IANA
   ("text/pdf"                      "\\.pdf$"  mew-b64
    mew-prog-pdf                    mew-icon-application/postscript)
   ("application/octet-stream"      "\\.pages$"  mew-b64
    mew-default-external-program    mew-icon-application/octet-stream)
   ("application/octet-stream"      "\\.key$"  mew-b64
    mew-default-external-program    mew-icon-application/octet-stream)
   ("application/octet-stream"      "\\.numbers$"  mew-b64
    mew-default-external-program    mew-icon-application/octet-stream)
   ("application/msword"            "\\.doc$"  mew-b64
    mew-prog-msword                 mew-icon-text)
   ("application/vnd.ms-excel"      "\\.xl[st]$" mew-b64
    mew-prog-msexcel                mew-icon-text)
   ("application/vnd.ms-powerpoint" "\\.ppt$"  mew-b64
    mew-prog-mspowerpoint           mew-icon-text)
   ("application/vnd.openxmlformats-officedocument.wordprocessingml.document" "\\.docx$" mew-b64
    mew-prog-msword                 mew-icon-text)
   ("application/vnd.openxmlformats-officedocument.wordprocessingml.template" "\\.dotx$" mew-b64
    mew-prog-msword                 mew-icon-text)
   ("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" "\\.xlsx$" mew-b64
    mew-prog-msexcel                mew-icon-text)
   ("application/vnd.openxmlformats-officedocument.spreadsheetml.template" "\\.xltx$" mew-b64
    mew-prog-msexcel                mew-icon-text)
   ("application/vnd.openxmlformats-officedocument.presentationml.presentation" "\\.pptx$" mew-b64
    mew-prog-mspowerpoint           mew-icon-text)
   ("application/vnd.openxmlformats-officedocument.presentationml.slideshow" "\\.ppsx$" mew-b64
    mew-prog-mspowerpoint           mew-icon-text)
   ("application/vnd.openxmlformats-officedocument.presentationml.template" "\\.potx$" mew-b64
    mew-prog-mspowerpoint           mew-icon-text)
   ("application/vnd.openxmlformats-officedocument.presentationml.slide" "\\.sldx$" mew-b64
    mew-prog-mspowerpoint           mew-icon-text)
   ("application/vnd.visio"         "\\.vsd$"  mew-b64
    mew-prog-visio                  mew-icon-text)
   ("application/ms-tnef"           "\\.dat$" mew-b64
    mew-prog-mstnef                 mew-icon-unknown)
   ("application/rtf"               "\\.rtf$" mew-b64
    mew-prog-rtf                    mew-icon-text)
   ("application/pgp-keys"          "\\.pka$"  nil
    mew-prog-pgp-keys               mew-icon-unknown)
   ("application/x-pkcs7-signature" "\\.p7s$"  mew-b64
    nil			     mew-icon-unknown)
   ("application/vnd.fujitsu.oasys" "\\.oas$"  mew-b64
    mew-prog-oasys                  mew-icon-text)
   ("application/vnd.fujitsu.oasys2" "\\.oa2$" mew-b64
    mew-prog-oasys                  mew-icon-text)
   ("application/vnd.fujitsu.oasys3" "\\.oa3$" mew-b64
    mew-prog-oasys                  mew-icon-text)
   ("application/zip"               "\\.zip$" mew-b64
    mew-prog-unzip                  mew-icon-application/octet-stream)
   ("application/x-zip-compressed"  "\\.zip$" mew-b64
    mew-prog-unzip                  mew-icon-application/octet-stream)
   ("application/octet-stream"
    "\\.tar$\\|\\.tar\\.\\|\\.gz$\\|\\.Z$\\|\\.taz$\\|\\.tgz$\\|\\.tbz$\\|\\.bz2?$\\|\\.lzh$\\|\\.bin$\\|\\.pgp$\\|\\.gpg$\\|\\.exe$\\|\\.dll$"
    mew-b64 mew-prog-octet-stream mew-icon-application/octet-stream)
   ;;
   ("text/html"     "\\.html?$" nil     mew-prog-html      mew-icon-text)
   ("text/enriched" "\\.rtf$"   nil     mew-prog-enriched  mew-icon-text)
   ("text/css"      "\\.css$"   nil     mew-prog-text      mew-icon-text)
   ("text/sgml"     "\\.sgml$"  nil     mew-prog-text      mew-icon-text)
   ("text/xml"      "\\.xml$"   nil     mew-prog-xml       mew-icon-text)
   ("text/calendar" "\\.ics$"   nil     mew-prog-text      mew-icon-text)
   ("text/x-patch"  "\\.diff$\\|\\.patch$"
    nil     mew-prog-patch      mew-icon-text)
   ("text/plain"    "\\.txt$\\|\\.c$\\|\\.h$\\|\\.el$\\|\\.diff$\\|\\.patch$"
    nil     mew-prog-plain     mew-icon-text)
   ("text/rfc822-headers" "\\.hdr$" nil
    mew-prog-rfc822-headers    mew-icon-message/rfc822)
   ("text"         "^$"        nil     mew-prog-text      mew-icon-text)
   ;; Unknown CT: matches here.
   ("application/xml"               "\\.xml$"  mew-b64
    mew-prog-xml2              mew-icon-text)
   (t               "^$"       nil   mew-prog-octet-stream mew-icon-unknown)
   ;; Unknown suffix matches here and return the entry specified
   ;; by mew-content-type.
   (nil             ".*")))

(mew-defvar
 'mew-mime-content-type-for-ooffice
 '(("application/vnd.oasis.opendocument.text"                  "\\.odt")
   ("application/vnd.oasis.opendocument.text-template"         "\\.ott")
   ("application/vnd.oasis.opendocument.text-web"              "\\.oth")
   ("application/vnd.oasis.opendocument.text-master"           "\\.odm")
   ("application/vnd.oasis.opendocument.graphics"              "\\.odg")
   ("application/vnd.oasis.opendocument.graphics-template"     "\\.otg")
   ("application/vnd.oasis.opendocument.presentation"          "\\.odp")
   ("application/vnd.oasis.opendocument.presentation-template" "\\.otp")
   ("application/vnd.oasis.opendocument.spreadsheet"           "\\.ods")
   ("application/vnd.oasis.opendocument.spreadsheet-template"  "\\.ots")
   ("application/vnd.oasis.opendocument.chart"                 "\\.odc")
   ("application/vnd.oasis.opendocument.chart-template"        "\\.otc")
   ("application/vnd.oasis.opendocument.image"                 "\\.odi")
   ("application/vnd.oasis.opendocument.image-template"        "\\.oti")
   ("application/vnd.oasis.opendocument.formula"               "\\.odf")
   ("application/vnd.oasis.opendocument.formula-template"      "\\.otf")
   ("application/vnd.oasis.opendocument.database"              "\\.odb")
   ("application/vnd.sun.xml.writer"           "\\.sxw$")
   ("application/vnd.sun.xml.writer.template"  "\\.stw$")
   ("application/vnd.sun.xml.calc"             "\\.sxc$")
   ("application/vnd.sun.xml.calc.template"    "\\.stc$")
   ("application/vnd.sun.xml.draw"             "\\.sxd$")
   ("application/vnd.sun.xml.draw.template"    "\\.std$")
   ("application/vnd.sun.xml.impress"          "\\.sxi$")
   ("application/vnd.sun.xml.impress.template" "\\.sti$")
   ("application/vnd.sun.xml.writer.global"    "\\.sxg$")
   ("application/vnd.sun.xml.math"             "\\.sxm$")))

(when mew-mime-content-type-for-ooffice
  (setq mew-mime-content-type
	(nconc
	 (mapcar
	  (lambda (x) (list (nth 0 x) (nth 1 x) 'mew-b64 'mew-prog-ooffice 'mew-icon-text))
	  mew-mime-content-type-for-ooffice)
	 mew-mime-content-type)))

(provide 'mew-varsx)

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

;;; mew-varsx.el ends here
