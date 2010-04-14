;;; mew-vars2.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  1, 1999

;;; Code:

(require 'mew-vars)

;;; (1) Variables closer to constant.
;;; (2) Variables for soft-coding.
;;; (3) Difficult options with some macros.
;;; Use mew-add-first, mew-insert-after, mew-replace-with,
;;; and mew-remove-entry to modify the variables.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Marks
;;;

(defvar mew-mark-review ?*)
(defvar mew-mark-escape ?$)
(defvar mew-mark-delete ?D)
(defvar mew-mark-unlink ?X)
(defvar mew-mark-refile ?o)
(defvar mew-mark-read   mew-sp)
(defvar mew-mark-unread ?U)
(defvar mew-mark-tmp    ?\0) ;; temporary use only.
(defvar mew-mark-default-walk mew-mark-review)
(defvar mew-mark-walk         mew-mark-default-walk)
(defvar mew-mark-duplicated   mew-mark-delete)
(defvar mew-mark-show-list (list mew-mark-read mew-mark-unread mew-mark-review))
(defvar mew-mark-show-list2 (list mew-mark-unread mew-mark-review))

(defvar mew-summary-mark-undo-marks
  `(,mew-mark-delete ,mew-mark-unlink ,mew-mark-refile ,mew-mark-read)
  "*A list of marks to be canceled by '\\<mew-summary-mode-map>\\[mew-summary-mark-undo-all]'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Field Magic
;;;

(defvar mew-reply-string "Re: ")
(defvar mew-reply-regex
  "Re\\(\\|[*^]?[0-9]+\\|\\[[0-9]+\\]\\|([0-9]+)\\)[:>] *"
  "Regexp of various Re: expression in Subject:")
(defvar mew-forward-string "Fw: ")
(defvar mew-forward-regex "\\(Fw\\|Fwd\\|Forward\\): *"
  "Regexp of various Fw: expression in Subject:")
(defvar mew-was-regex " *[[(] *\\(was[^a-z]\\|Re:\\).*[])] *$"
  "Regexp of various (was ...) expression in Subject:")

(defvar mew-subject-simplify-replace-alist
  `(;; (REGEXP . NEW) --- NEW may be nil for deletion (same to "")
    ;; replace multiple Re: and Fw: into single Re:
    (,(concat "^" ;; regexp
	      mew-reply-regex
	      "\\("
	      mew-reply-regex
	      "\\|"
	      mew-forward-regex
	      "\\)*")
     mew-reply-string) ;; new text
    ;; replace multiple Fw: and Re: into single Fw:
    (,(concat "^" ;; regexp
	      mew-forward-regex
	      "\\("
	      mew-reply-regex
	      "\\|"
	      mew-forward-regex
	      "\\)*")
     mew-forward-string) ;; new text
    ;; delete extra string (no new text means delete)
    (,mew-was-regex nil)) ;; regexp
  "*Replacement alist to simplify Subject: field body
Each element is cons cell whos car is REGEXP to replace,
cdr is new text.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error
;;;

(defvar mew-error-unknown-charset    "**UNKNOWN CHARSET**")
(defvar mew-error-broken-string      "**BROKEN STRING**")
(defvar mew-error-invalid-b-encoding " **B ENCODING ERROR** ")
(defvar mew-error-invalid-q-encoding " **Q ENCODING ERROR** ")
(defvar mew-error-no-subject         "** no subject **")
(defvar mew-error-broken-address     "**BROKEN ADDRESS**")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Files
;;;

;; In each folder

(defvar mew-summary-cache-file ".mew-summary"
  "Cache file for Summary mode contents.")

(defvar mew-summary-touch-file ".mew-mtime"
  "Time-stamp file for message folders.")

;; Under mew-conf-path

(defcustom mew-addrbook-file "Addrbook"
  "*A file which contains AddrBook information."
  :group 'mew-addrbook
  :type 'string)

(defvar mew-alias-auto-file ".mew-alias")

(defvar mew-refile-msgid-file ".mew-refile-msgid-alist")

(defvar mew-refile-from-file ".mew-refile-from-alist")

(defvar mew-smtp-log-file "Smtplog")
(defvar mew-nntp-log-file "Nntplog")
(defvar mew-refile-log-file "Refilelog")

;; xxx Home

(defvar mew-fib-item-file "~/.mew-fib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modes
;;;

(defvar mew-folder-mode 448 ;; decimal for octal 0700
  "Secure file mode for folders. 448(0700 in octal) is STRONGLY
recommended for privacy reasons.")

(defvar mew-file-mode-mask 432 ;; decimal for octal 0660
  "Secure file mode mask. 432(0660 in octal) is STRONGLY recommended
for privacy reasons.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separators
;;;

(defvar mew-path-separator "/")
(defvar mew-regex-drive-letter (file-name-as-directory "^[a-zA-Z]:"))
(defvar mew-regex-file-absolute (format "^[~%s]" mew-path-separator))
(defvar mew-header-separator "----")
(defvar mew-eoh nil) ;; set by mew-regex-setup
(defvar mew-lwsp "^[ \t]")
(defvar mew-lwsp+ "^[ \t]+")
(defvar mew-address-separator ":, \t\n")
(defvar mew-page-delimiter "^\^L")
(defvar mew-keyval "^\\([^ \t:]+:?\\)[ \t]*")
;; "^\\([^ \t:]+:?\\)[ \t]*\\(.*\\)$" faces a stupid error.
;; "$" does not mean end-of-line if the second match is empty, sigh.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Explanation string in messages
;;;

(defvar mew-bcc-subject "A blind carbon copy")
(defvar mew-bcc-body "This is a blind carbon copy.\n")

(defvar mew-field-comment "(modified by Mew)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addrbook
;;;

(defvar mew-addrbook-switch
  '((shortname mew-addrbook-shortname-get)
    (address   identity)
    (username  mew-addrstr-extract-user)
    (nickname  mew-addrbook-nickname-get)
    (name      mew-addrbook-name-get))
  "Function database to get each field of Addrbook.
'shortname, 'address, 'username, 'nickname, and 'name is defined.")

(defun mew-addrbook-func (key)
  (mew-alist-get-value (assq key mew-addrbook-switch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME types
;;;

(defun mew-ct-textp (ct)
  (string-match "^Text" (capitalize ct)))

(defun mew-ct-imagep (ct)
  (string-match "^Image" (capitalize ct)))

(defun mew-ct-modelp (ct)
  (string-match "^Model" (capitalize ct)))

(defun mew-ct-multipartp (ct)
  (string-match "^Multipart" (capitalize ct)))

(defun mew-ct-alternativep (ct)
  (string-match "^Multipart/Alternative" (capitalize ct)))

(defun mew-ct-partialp (ct)
  (string-match "^Message/Partial" (capitalize ct)))

(defun mew-ct-messagep (ct)
  (string-match "^Message" (capitalize ct)))

(defun mew-ct-linebasep (ct)
  (or (mew-ct-textp ct)
      (mew-case-equal ct mew-ct-aps)
      (mew-case-equal ct mew-ct-msg))) ;; xxx mew-ct-messagep?

(defvar mew-mime-content-type-multipart-list
  `(,mew-ct-mlm ,mew-ct-mla)
  "Candidate of 'Content-Type: Multipart/' when CT: is changed
in draft buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion
;;;

(defconst mew-custom-type-of-field-completion
  '(list (list string function)))

(defcustom mew-field-completion-switch
  '(("To:"          mew-complete-address)
    ("Cc:"          mew-complete-address)
    ("Dcc:"         mew-complete-address)
    ("Bcc:"         mew-complete-address)
    ("Reply-To:"    mew-complete-address)
    ("Fcc:"         mew-complete-fcc-folder)
    ("Resent-To:"   mew-complete-address)
    ("Resent-Cc:"   mew-complete-address)
    ("Resent-Dcc:"  mew-complete-address)
    ("Resent-Bcc:"  mew-complete-address)
    ("Resent-Fcc:"  mew-complete-fcc-folder)
    ("Newsgroups:"  mew-complete-newsgroups)
    ("Followup-To:" mew-complete-newsgroups))
  "*Completion function alist concerned with the key."
  :group 'mew-complete
  :type mew-custom-type-of-field-completion)

(defcustom mew-field-circular-completion-switch
  '(("To:"          mew-circular-complete-domain)
    ("Cc:"          mew-circular-complete-domain)
    ("Dcc:"         mew-circular-complete-domain)
    ("Bcc:"         mew-circular-complete-domain)
    ("Reply-To:"    mew-circular-complete-domain)
    ("Resent-To:"   mew-circular-complete-domain)
    ("Resent-Cc:"   mew-circular-complete-domain)
    ("Resent-Dcc:"  mew-circular-complete-domain)
    ("Resent-Bcc:"  mew-circular-complete-domain)
    ("From:"        mew-circular-complete-from)
    ("Resent-From:" mew-circular-complete-from))
  "*Circular completion function alist concerned with the key."
  :group 'mew-complete
  :type mew-custom-type-of-field-completion)

(defcustom mew-field-expansion-switch
  '(("To:"         mew-expand-address)
    ("Cc:"         mew-expand-address)
    ("Dcc:"        mew-expand-address)
    ("Bcc:"        mew-expand-address)
    ("Reply-To:"   mew-expand-address)
    ("Resent-To:"  mew-expand-address)
    ("Resent-Cc:"  mew-expand-address)
    ("Resent-Dcc:" mew-expand-address)
    ("Resent-Bcc:" mew-expand-address))
  "*Expansion function alist concerned with the key."
  :group 'mew-complete
  :type mew-custom-type-of-field-completion)

(defun mew-field-get-func (key switch)
  (mew-alist-get-value (mew-assoc-match key switch 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME control
;;;

(defvar mew-content-type mew-ct-txt
  "*The default Content-Type: for a file whose suffix is unknown.")

(defvar mew-mime-content-type nil
  "\\<mew-summary-mode-map>Database for MIME content type.
This is a list of entries which consist of <content-type>,
<regular expression of file suffix>, <content-transfer-encoding>,
<symbol for visualizer>, <symbol for icon>, <auxiliary data>.

Here is an example:

	(\"image/png\" \"\\\\.png$\" mew-b64 mew-prog-image mew-icon-image png)

This database is used both when visualizing and composing. For
example, when visualizing, a visualizer is chosen according to
its <content-type>. When composing, content-type is selected
according to the suffix of the file to be attached with <regular
expression of file suffix>.

For <content-transfer-encoding>, 'nil', 'mew-b64', or 'mew-qp'
should be specified. 'mew-b64' and 'mew-qp' means Base64 and
Quoted-Printable, respectively. This is used as an appropriate
content-transfer-encoding when composing.

Each symbol specified at <symbol for visualizer> should have a value
whose format is one of followings:

- \"prog\"
	\"prog\" is an external program.
	(1) The program name is displayed.
	(2) \"prog\" is executed asynchronously.
- func
	<func> is a function.
	(1) <func> is called.
	(2) <func> is called.
- (\"prog\" args async)
	\"prog\" is an external program.
	<args> is a list of arguments for the external program.
	<async>
		If nil, the external program is called synchronously.
		If t, the external program is called asynchronously.
	(1) The program name is displayed.
	(2) \"prog\" is executed asynchronously with <args>.
- (nil \"prog\")
	Equivalent to \"prog\".
- (nil (\"prog\" args async))
	Equivalent to (\"prog\" args async).
- (nil func2)
	(1) Displayed 'type \\[mew-summary-execute-external]'.
	(2) <func2> is called.
- (func1 func2)
	(1) <func1> is called.
	(2) <func2> is called.
	    If the original content-type is Application/Octet-Stream,
	    you are asked to choose <func1> or <func2>.
- (func1 \"prog\")
	(1) <func1> is called.
	(2) \"prog\" is executed asynchronously.
	    If the original content-type is Application/Octet-Stream,
	    you are asked to choose <func1> or \"prog\".
- (func1 (\"prog\" args async))
	(1) <func1> is called.
	(2) \"prog\" is executed asynchronously with <args>.
	    If the original content-type is Application/Octet-Stream,
	    you are asked to choose <func1> or \"prog\".

Note that (1) indicates the action for '\\[mew-summary-display]' and
\(2) indicates the action for '\\[mew-summary-execute-external]'.

If you want to add an entry for new content-type, please let the author
know. Such an entry should be shared by all users and be included in
the next version of Mew.")

(defun mew-ctdb-by-ct (ct)
  (mew-assoc-match2 ct mew-mime-content-type 0))

(defun mew-ctdb-by-file (file)
  (mew-assoc-match2 file mew-mime-content-type 1))

(defun mew-ctdb-ct (attr)
  (and (nth 0 attr) (mew-capitalize (nth 0 attr))))

(defun mew-ctdb-regex (attr)
  (nth 1 attr))

(defun mew-ctdb-cte (attr)
  (symbol-value (nth 2 attr)))

(defun mew-ctdb-prog (attr)
  (let ((val (symbol-value (nth 3 attr))))
    (if (and (listp val) (eq 'if (car val)))
	(eval val)
      val)))

(defun mew-ctdb-icon (attr)
  (symbol-value (nth 4 attr)))

(defun mew-mime-image-format-name (ct)
  (nth 5 (mew-ctdb-by-ct ct)))

(defvar mew-mime-content-type-for-ooffice nil
  "A list of (content-type suffix) for OpenOffice.org to be prepended
'mew-mime-content-type'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME Content-Disposition:
;;;

(defvar mew-mime-content-disposition
  '(("text"      "inline"     t)
    ("image"     "inline"     t)
    ("message"   "inline"     nil)
    ("multipart" nil          nil)
    (t           "attachment" t))
  "(content-type inline/attachment filename)")

(defun mew-cdpdb-by-ct (ct)
  (mew-assoc-match2 ct mew-mime-content-disposition 0))

(defun mew-cdpdb-val (attr)
  (nth 1 attr))

(defun mew-cdpdb-file (attr)
  (nth 2 attr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Privacy
;;;

(defcustom mew-privacy-database
  `((pgp-signature  ((,mew-ct-mls ,mew-ct-pgs)) "PS")
    (pgp-encryption ((,mew-ct-mle ,mew-ct-pge)) "PE")
    (pgp-signature-encryption
     ((,mew-ct-mls ,mew-ct-pgs) (,mew-ct-mle ,mew-ct-pge)) "PSPE")
    (pgp-encryption-signature
     ((,mew-ct-mle ,mew-ct-pge) (,mew-ct-mls ,mew-ct-pgs)) "PEPS")
    (smime-signature  ((,mew-ct-smm ,mew-ct-smm-sig)) "SS")
    (smime-encryption ((,mew-ct-smm ,mew-ct-smm-enc)) "SE")
    (smime-signature-encryption
     ((,mew-ct-smm ,mew-ct-smm-sig) (,mew-ct-smm ,mew-ct-smm-enc)) "SSSE")
    (smime-encryption-signature
     ((,mew-ct-smm ,mew-ct-smm-enc) (,mew-ct-smm ,mew-ct-smm-sig)) "SESS"))
  "*Alist of key, a list of privacy Content-Type, and its mark."
  :group 'mew-privacy
  :type 'sexp)

(defun mew-pcdb-services ()
  (mapcar 'car mew-privacy-database))

(defun mew-pcdb-by-service (service)
  (assq service mew-privacy-database))

(defun mew-pcdb-ct (pcdb)
  (nth 1 pcdb))

(defun mew-pcdb-mark (pcdb)
  (nth 2 pcdb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multipart/Alternative
;;;

(defvar mew-mime-multipart-alternative-list
  `(,mew-ct-txt ".*"))

(defun mew-multipart-alternative-preference (part)
  (let ((ct (mew-syntax-get-value (mew-syntax-get-ct part) 'cap)))
    (mew-member-match ct mew-mime-multipart-alternative-list)))

(defvar mew-mime-external-body-list
  '("anon-ftp" "url" "mail-server"))

(defun mew-mime-external-body-preference (part)
  (let* ((ctl (mew-syntax-get-ct part))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (access-type (mew-syntax-get-param ctl "access-type")))
    (if (and (string= ct mew-ct-ext) (stringp access-type))
	(mew-member-match access-type mew-mime-external-body-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Field database
;;;

;; strictly, content-* are necessary but they are not practical.
(defvar mew-field-database
  `((,mew-to:            mailbox    struct)
    (,mew-cc:            mailbox    struct)
    (,mew-from:          mailbox    struct)
    (,mew-apparently-to: mailbox    struct)
    (,mew-bcc:           mailbox    struct)
    (,mew-dcc:           mailbox    struct)
    (,mew-reply-to:      mailbox    struct)
    (,mew-resent-to:     mailbox    struct)
    (,mew-resent-cc:     mailbox    struct)
    (,mew-resent-from:   mailbox    struct)
    (,mew-mv:            mime       struct)
    (,mew-subj:          text       text)
    (,mew-keywords:      comma-text text)
    (,mew-received:      unstruct   unstruct)
    (,mew-message-id:    unstruct   unstruct)
    (,mew-references:    unstruct   unstruct)
    (,mew-in-reply-to:   unstruct   unstruct)
    (,mew-x-face:	 unstruct   unstruct)
    (,mew-face:          unstruct   unstruct)
    ("Authentication-Results:" unstruct struct2) ;; capitalized
    ("Domainkey-Signature:"    unstruct unstruct) ;; capitalized
    ("Dkim-Signature:"         unstruct unstruct)) ;; capitalized
  "(field enc dec)")

(defun mew-field-type-for-encoding (key)
  (or (nth 1 (assoc (mew-capitalize key) mew-field-database)) 'unstruct))

(defun mew-field-type-for-decoding (key)
  (or (nth 2 (assoc (mew-capitalize key) mew-field-database)) 'text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Theme
;;;

(defvar mew-theme-file nil
  "The file name which contains theme.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight header
;;;

(defcustom mew-field-spec
  '(("^Resent-\\(Sender\\|From\\|To\\|Cc\\|Date\\)" t
     mew-face-header-important
     mew-face-header-important)
    ("^Subject:$" t
     mew-face-header-important
     mew-face-header-subject)
    ("^From:$" t
     mew-face-header-important
     mew-face-header-from)
    ("^\\(To\\|Apparently-To\\):$" t
     mew-face-header-important
     mew-face-header-to)
    ("^\\(Cc\\|Dcc\\|Bcc\\):$" t
     mew-face-header-important
     mew-face-header-to)
    ("^Newsgroups:$" t
     mew-face-header-important
     mew-face-header-to)
    ("^Date:$" t
     mew-face-header-important
     mew-face-header-date)
    ("^Sender:$" t)
    ("^Reply-To:$" t)
    ("^\\(X-Mailer\\|User-Agent\\):$" t)
    ("^X-Mew:$" t
     mew-face-header-important
     mew-face-header-xmew)
    ("^\\(Received\\|Return-Path\\|Errors-To\\):$" nil)
    ("^\\(Path\\|Distribution\\|Xref\\):$" nil)
    ("^NNTP-Posting-" nil)
    ("^\\(Message-Id\\|Posted\\|In-Reply-To\\|References\\|Precedence\\):$" nil)
    ("^DomainKey-Signature:$" nil)
    ("^DKIM-Signature:$" nil)
    ("^Authentication-Results:$"
     mew-authentication-results
     mew-face-header-important
     mew-face-header-xmew
     ("hardfail" mew-face-header-xmew-bad
      "fail" mew-face-header-xmew-bad
      "softfail" mew-face-header-xmew-bad))
    ("^Delivered-" nil)
    ("^List-" nil) ;; RFC 2369
;;    ("^Content-" t)
    ("^\\(Mime-Version\\|Lines\\):$" nil)
    ("^From$" nil)
    ("^Status:$" nil)
    ("^Face:$" nil
     mew-face-header-private
     mew-face-header-marginal)
    ("^\\(X\\|Original\\)-" nil
     mew-face-header-private
     mew-face-header-marginal))
  "*An alist of field spec for Message mode. Each spec
consists of field-regular-expression, visible-p, face-for-key,
and face-for-value. Fields whose visible-p is t are displayed in
Message mode in the defined order. Fields whose visible-p is nil are
hidden in Message mode. Type DEL to see them. Fields not matched
to field-regular-expressions are operated by the value of
'mew-field-other-visible'. If face-for-key is omitted,
'mew-face-header-key' is used. If face-for-value is not
present, mew-face-header-marginal is used."
  :group 'mew-highlight
  :type '(alist :key-type regexp
                :value-type
                  (choice (list boolean)
                          (list boolean face face))))

;; cons the position to the spec.
(defun mew-nspec-by-key (key)
  (mew-assoc-match3 key mew-field-spec 0))

(defun mew-nspec-n (nspec)
  (nth 0 nspec))

(defun mew-nspec-visiblep (nspec)
  (nth 2 nspec))

(defun mew-nspec-keyface (nspec)
  (nth 3 nspec))

(defun mew-nspec-valface (nspec)
  (nth 4 nspec))

(defun mew-nspec-extraface (nspec)
  (nth 5 nspec))

(defun mew-authentication-results (val)
  (save-match-data
    (or (string-match "=hardfail" val)
	(string-match "=fail" val)
	(string-match "=softfail" val))))

(mew-defstruct header-color field key val)
(defvar mew-header-color-alist nil)

;; See also mew-face-make-spec.

(defun mew-header-color-base (field key-color val-color bold)
  (unless val-color (setq val-color key-color))
  (let ((face-key (intern (concat "mew-face-color-" key-color (if bold "-bold"))))
	(face-val (intern (concat "mew-face-color-" val-color (if bold "-bold")))))
    (unless (facep face-key)
      (make-empty-face face-key)
      (face-spec-set face-key (list (mew-face-spec-func t (mew-face-spec-primitive key-color bold)))))
    (unless (facep face-val)
      (make-empty-face face-val)
      (face-spec-set face-val (list (mew-face-spec-func t (mew-face-spec-primitive val-color bold)))))
    (setq mew-header-color-alist
	  (cons (list (capitalize field) face-key face-val) mew-header-color-alist))))

(defun mew-header-color (field key-color &optional val-color)
  "Set the color of FIELD key to KEY-COLOR.
And set the color of FIELD value to VAL-COLOR.
If VAL-COLOR is omitted, set the color of FIELD value to KEY-COLOR."
  (mew-header-color-base field key-color val-color nil))

(defun mew-header-color-bold (field key-color &optional val-color)
  "Set the color of FIELD key to KEY-COLOR.
And set the color of FIELD value to VAL-COLOR.
If VAL-COLOR is omitted, set the color of FIELD value to KEY-COLOR.
The style becomes bold."
  (mew-header-color-base field key-color val-color t))

(defun mew-key-face (key nspec)
  (or (mew-header-color-get-key (assoc key mew-header-color-alist))
      (mew-nspec-keyface nspec)
      'mew-face-header-key))

(defun mew-val-face (key nspec)
  (or (mew-header-color-get-val (assoc key mew-header-color-alist))
      (mew-nspec-valface nspec)
      'mew-face-header-marginal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight body
;;;

(defvar mew-highlight-body-regex-comment "^#+.*")

(defvar mew-highlight-body-regex-cite
  "^\\(\\([ \t]\\{,7\\}\\([>:|]\\|\\w+\\([._-]+\\w+\\)*>+\\)\\)+\\).*")

(defcustom mew-highlight-body-prefix-width 20
  "*Maximum string width assume prefix for fancy highlight a body."
  :group 'mew-highlight
  :type 'integer)

(defvar mew-highlight-body-cite-faces
  '(mew-face-body-cite1
    mew-face-body-cite2
    mew-face-body-cite3
    mew-face-body-cite4
    mew-face-body-cite5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight mark
;;;

(defvar mew-highlight-mark-keywords
  `((,mew-mark-review mew-face-mark-review)
    (,mew-mark-escape mew-face-mark-escape)
    (,mew-mark-delete mew-face-mark-delete)
    (,mew-mark-unlink mew-face-mark-unlink)
    (,mew-mark-refile mew-face-mark-refile)
    (,mew-mark-unread mew-face-mark-unread))
  "A list of mark-face pair to used in Summary/Virtual mode.")

(defun mew-highlight-mark-get-face (mark)
  (mew-alist-get-value (assoc mark mew-highlight-mark-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mark spec
;;;

(defvar mew-mark-afterstep-spec
  `((,mew-mark-review 0 0 0 0 0 0 0)
    (,mew-mark-escape 0 0 0 0 0 0 0)
    (,mew-mark-read   0 0 0 0 0 0 0)
    (,mew-mark-delete 2 0 2 0 0 0 0)
    (,mew-mark-unlink 2 0 2 0 0 0 0)
    (,mew-mark-refile 2 0 2 0 0 0 0))
  "*A list of cursor action spec.
The cursor action spec is a list of a mark and seven cursor
actions after marking.

0th (the first element of a list) is a mark value. A mark value is
ASCII code of the mark. For example, the value of the '*' mark
\(mew-mark-review) is 42.

Seven values following a mark value means as follows:

1st is the case of no mark.
2nd is the case where the new mark is equal to the old one.
3rd is the case where level of the new mark is greater than that of the
    old one.
4th, 5th, and 6th is the case where levels are equal.
    4th and 5th is the case that the old mark has state.
        4th means that the old mark is overrode.
        5th means that the old mark remains.
    6th is the case that the old mark does not have state.
7th is the case where level of the new mark is smaller than that of the
    old one.

The value of cursor actions means as follows:

	0 means staying.
	1 means moving according to the direction,
	2 means moving according to the direction
	  then displaying the next message.

For more detail, see mew-mark-put-mark and mew-mark-afterstep.")

(defun mew-markas-nth (mark case)
  (nth case (assoc mark mew-mark-afterstep-spec)))

(defvar mew-mark-spec
  `((,mew-mark-read   "read"   0 nil nil nil nil nil)
    (,mew-mark-unread "unread" 0 nil nil nil nil nil)
    (,mew-mark-review "review" 1 nil nil nil nil nil)
    (,mew-mark-escape "escape" 1 nil nil nil nil nil)
    (,mew-mark-delete "delete" 2 nil t   nil mew-mark-exec-delete nil)
    (,mew-mark-unlink "unlink" 2 nil t   nil mew-mark-exec-unlink nil)
    (,mew-mark-refile "refile" 2 t   mew-mark-kill-refile mew-mark-unrefile
		                     mew-mark-exec-refile mew-mark-sanity-refile))
  "*A list of lists which consists of
mark, name, level, statefullp, kill-line-p,
undo-func, exec-func, and sanity-fucn.")

;;

(defun mew-mark-get-all-marks ()
  "Collecting all defined marks."
  (mapcar 'car mew-mark-spec))

(defun mew-markdb-by-mark (mark)
  (assoc mark mew-mark-spec))

(defun mew-markdb-name (mark)
  (nth 1 (mew-markdb-by-mark mark)))

(defun mew-markdb-level (mark)
  (nth 2 (mew-markdb-by-mark mark)))

(defun mew-markdb-statefullp (mark)
  (nth 3 (mew-markdb-by-mark mark)))

(defun mew-markdb-killp (mark)
  (nth 4 (mew-markdb-by-mark mark)))

(defun mew-markdb-func-undo (mark)
  (nth 5 (mew-markdb-by-mark mark)))

(defun mew-markdb-func-exec (mark)
  (nth 6 (mew-markdb-by-mark mark)))

(defun mew-markdb-func-sanity (mark)
  (nth 7 (mew-markdb-by-mark mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inbox action
;;;

(defvar mew-inbox-action-alist nil
  "*This variable controls actions when retrieving messages.
Currently this works only when '\\<mew-summary-mode-map>\\[mew-summary-retrieve]' with POP or 'mewl/incm'.
You can put any marks, typically 'o' and 'D' according to message
headers.

This value must be a list of entries. An entry is a list whose first
element is a field defined in 'mew-scan-fields'.

If the second element is a list, the first element of the list must be
a mark (e.g. ?D) or a string which is a comma separated folders (e.g
\"+foo,+bar\" for ?o). Other elements of the list are regular
expressions to be matched to the value of the field.

With the following configuration, Mew marks messages whose Subject:
matches \"adult\" or \"money\" with 'D'. And Mew marks messages whose
Subject is \"daily log\" with 'o' to be refiled \"+log\". Also Mew
marks messages whose Content-Type: contains \"gb2312\" with 'D'.

\(setq mew-inbox-action-alist
      '((\"Subject:\"
	 (?D \"adult\" \"money\")
	 (\"+log\" \"daily log\"))
	(\"Content-Type:\"
	 (?D \"gb2312\"))))

If the second element of the entry is a symbol, it will be called as a
function which returns nil or a mark or a string.

If you are using 'spamassassin' which adds the \"X-Spam-Flag:\" field
to messages and puts them in your mailbox, configure as follows:

\(setq mew-spam: \"X-Spam-Flag:\")

\(defun mew-spam-assassin (val)
  (let ((case-fold-search t))
    (if (string-match \"yes\" val) ?D)))

\(setq mew-inbox-action-alist
      '((\"X-Spam-Flag:\" mew-spam-assassin)))

With this configuration, messages with the \"X-Spam-Flag:\" whose
value is \"Yes\" are automatically marked with 'D' since the
'mew-spam-assassin' function returns 'D' marks when the value is
\"Yes\".

If you are using 'bogofilter' which adds the \"X-Bogosity:\" field
to messages and puts them to your mailbox, configure as follows:

\(setq mew-spam: \"X-Bogosity:\")

\(defun mew-spam-bogofilter (val)
  (let ((case-fold-search t))
    (if (string-match \"yes\" val) ?D)))

\(setq mew-inbox-action-alist
      '((\"X-Bogosity:\" mew-spam-bogofilter)))
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Proto
;;;

(defvar mew-proto mew-folder-local)

(defvar mew-proto-spec
  ;; key go refile flush
  '(("+" "+" "+" "+")
    ("$" "+" nil "+")
    ("%" "%" "%" "+")
    ("*" nil nil nil)
    ("-" "-" nil "-")))

(defun mew-proto-to-body (fld n)
  (if (mew-folder-virtualp fld)
      (setq fld (mew-folder-basename fld)))
  (setq fld (mew-case:folder-folder fld))
  (let ((proto (mew-folder-prefix fld)))
    (unless (member proto mew-folder-prefixes)
      (setq proto (mew-proto))) ;; default proto
    (nth n (assoc proto mew-proto-spec))))

(defun mew-proto-to-go (fld)
  (mew-proto-to-body fld 1))

(defun mew-proto-to-refile (fld)
  (mew-proto-to-body fld 2))

(defun mew-proto-to-flush (fld)
  (mew-proto-to-body fld 3))

(defun mew-proto-inbox-folder (proto &optional case)
  (or proto (setq proto (mew-proto case)))
  (cond
   ((mew-folder-popp  proto) mew-pop-inbox-folder)
   ((mew-folder-imapp proto) (mew-imap-inbox-folder case))
   ((mew-folder-nntpp proto) (mew-nntp-newsgroup case))
   (t ;; local
    (mew-inbox-folder case))))

(defun mew-proto-queue-folder (proto &optional case)
  (or proto (setq proto (mew-proto case)))
  (cond
   ((mew-folder-nntpp proto) (mew-postq-folder case))
   (t (mew-queue-folder case))))

(defun mew-proto-friend-folder (proto &optional case)
  (cond
   ((mew-folder-imapp proto) (mew-imap-friend-folder case))
   (t ;; local
    mew-friend-folder)))

(defun mew-proto-friend-folder-list (proto &optional case)
  (cond
   ((mew-folder-imapp proto) (mew-imap-friend-folder-list case))
   (t ;; local
    (mew-local-friend-folder-list))))

(defun mew-proto-folder-alist (proto &optional case)
  (cond
   ((mew-folder-popp  proto) (mew-pop-folder-alist))
   ((mew-folder-imapp proto) (mew-imap-folder-alist case))
   ((mew-folder-nntpp proto) (mew-nntp-folder-alist case))
   (t ;; local
    (mew-local-folder-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Config
;;;

(defcustom mew-config-alist nil
  "*Alist of config. This is a list of
	(<case> (<key> <value>) (<key> <value>) ...).
  - <case> is a string of case.
  - <key> is a symbol of Mew value with the 'mew-' prefix removed.
  - <value> is a string.

Currently, the following keys are supported:
name, user, mail-domain,
cc, fcc, dcc, reply-to, organization, header-alist, proto,
smtp-server, smtp-port, smtp-ssh-server, smtp-ssl, smtp-ssl-port,
smtp-user, smtp-auth-list,
smtp-msgid-user, smtp-msgid-domain, smtp-helo-domain, smtp-mail-from,
pop-server, pop-port, pop-ssh-server, pop-ssl, pop-ssl-port,
pop-user, pop-auth, pop-auth-list,
pop-size, pop-header-only, pop-delete, pop-body-lines,
pop-proxy-server, pop-proxy-port,
imap-server, imap-port, imap-ssh-server, imap-ssl, imap-ssl-port,
imap-user, imap-auth, imap-auth-list,
imap-size, imap-header-only, imap-delete,
imap-trash-folder, imap-queue-folder, imap-spam-field, imap-spam-word,
imap-proxy-server, imap-proxy-port,
nntp-server, nntp-port, nntp-ssh-server, nntp-ssl, nntp-ssl-port,
nntp-user, nntp-size, nntp-header-only,
nntp-msgid-user, nntp-msgid-domain,
ssl-cert-directory, ssl-verify-level,
inbox-folder, queue-folder, postq-folder,
mailbox-type, mbox-command, mbox-command-arg,
signature-file, content-type, refile-guess-alist,
spam-prog, spam-prog-args, ham-prog, ham-prog-args,
use-old-pgp, pgp-signer, smime-signer, privacy-method,
protect-privacy-always, protect-privacy-always-type,
protect-privacy-encrypted, protect-privacy-encrypted-type,
protect-privacy-with-old-pgp-signature

from = name <user@mail-domain>
message-id = *random*.smtp-msgid-user@smtp-msgid-domain
message-id = *random*.nntp-msgid-user@nntp-msgid-domain

An example is as follows:

\(setq mew-config-alist
      '((mew
	 (mail-domain  \"example.org\")
	 (inbox-folder \"+inbox-mew\"))
	(keio
	 (cc           \"kazu@example.jp\")
	 (user         \"pooh\")
	 (mail-domain  \"example.net\"))
	(default
	 (name         \"Kazu Yamamoto\")
	 (mail-domain  \"example.jp\"))))
"
;; bcc can be used but not recommended
  :group 'mew-env
  :type '(alist :key-type string
                :value-type (repeat (cons string string))))

(defun mew-cfent-by-case (case)
  (if (null case)
      (or (assoc mew-case-default mew-config-alist)
	  (assoc (intern mew-case-default) mew-config-alist))
    (or (assoc case mew-config-alist)
	(assoc (intern case) mew-config-alist))))

(provide 'mew-vars2)

;;; Copyright Notice:

;; Copyright (C) 2000-2010 Mew developing team.
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

;;; mew-vars2.el ends here
