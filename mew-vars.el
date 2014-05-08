;;; mew-vars.el --- Variables and Constants for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 22, 1997

;;; Code:

(require 'mew-env)

;;;
;;; User option variables which are easy to set.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defining groups
;;;

(defgroup mew nil
  "Messaging in the Emacs World."
  :group 'mail)

(defgroup mew-basic nil
  "Basic configuration."
  :group 'mew)

(defgroup mew-env nil
  "Basic environment."
  :group 'mew)

(defgroup mew-folder nil
  "Basic folder."
  :group 'mew)

(defgroup mew-summary nil
  "Summary mode."
  :group 'mew)

(defgroup mew-message nil
  "Message mode."
  :group 'mew)

(defgroup mew-cache nil
  "Message Caching."
  :group 'mew)

(defgroup mew-draft nil
  "Draft mode."
  :group 'mew)

(defgroup mew-cite nil
  "Citation."
  :group 'mew)

(defgroup mew-reply nil
  "Reply to message."
  :group 'mew)

(defgroup mew-refile nil
  "Refiling."
  :group 'mew)

(defgroup mew-complete nil
  "Completion."
  :group 'mew)

(defgroup mew-highlight nil
  "Highlight."
  :group 'faces
  :group 'mew)

(defgroup mew-privacy nil
  "Privacy setting."
  :group 'mew)

(defgroup mew-addrbook nil
  "Address book."
  :group 'mew)

(defgroup mew-net nil
  "Network."
  :group 'mew)

(defgroup mew-pop nil
  "POP."
  :group 'mew)

(defgroup mew-smtp nil
  "SMTP."
  :group 'mew)

(defgroup mew-imap nil
  "IMAP."
  :group 'mew)

(defgroup mew-nntp nil
  "NNTP."
  :group 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment variables
;;;

(defcustom mew-rc-file "~/.mew"
  "*A file to be loaded after Mew's variables are initialized."
  :group 'mew-env
  :type 'file)

(defcustom mew-mail-path "~/Mail"
  "*A directory where folders locate."
  :group 'mew-env
  :type 'directory)

(defcustom mew-conf-path mew-mail-path
  "*A directory where Mew's configuration files locate."
  :group 'mew-env
  :type 'directory)

(defcustom mew-pop-inbox-folder "$inbox"
  "*The default folder for POP."
  :group 'mew-folder
  :type 'string)

(defcustom mew-imap-inbox-folder "%inbox"
  "*The default folder for IMAP."
  :group 'mew-folder
  :type 'string)

(defcustom mew-imap-queue-folder "%queue"
  "*The queue folder for IMAP jobs."
  :group 'mew-folder
  :type 'string)

(defcustom mew-imap-trash-folder "%trash"
  "*The trash folder for IMAP. If non-nil, deleted messages are
moved to this folder. Deleting messages in this folder makes them
really deleted."
  :group 'mew-folder
  :type '(choice string (const nil)))

(defcustom mew-imap-trash-folder-list nil
  "*A list of IMAP folders whose messages marked with 'D' are really deleted."
  :group 'mew-summary
  :type '(choice (const nil) (repeat string)))

(defcustom mew-inbox-folder "+inbox"
  "*The folder where new messages are incorporated."
  :group 'mew-folder
  :type 'string)

(defcustom mew-draft-folder "+draft"
  "*The folder where draft are contained."
  :group 'mew-folder
  :type 'string)

(defcustom mew-trash-folder "+trash"
  "*The trash folder."
  :group 'mew-folder
  :type '(choice string (const nil)))

(defcustom mew-trash-folder-list nil
  "*A list of folders whose messages marked with 'D' are really deleted."
  :group 'mew-summary
  :type '(choice (const nil) (repeat string)))

(defcustom mew-queue-folder "+queue"
  "*The queue folder to send messages."
  :group 'mew-folder
  :type 'string)

(defcustom mew-postq-folder "+postq"
  "*The queue folder to post messages."
  :group 'mew-folder
  :type 'string)

(defcustom mew-attach-folder "+attach"
  "*The folder where attachments are contained."
  :group 'mew-folder
  :type 'string)

(defcustom mew-friend-folder "+from"
  "*The folder where personal messages are refiled."
  :group 'mew-folder
  :type 'string)

(defcustom mew-imap-friend-folder "%from"
  "*The folder where personal messages are refiled for IMAP."
  :group 'mew-folder
  :type 'string)

(defcustom mew-imap-spam-field nil
  "*The field name of anti spam information to be used for IMAP spam filter.
If both 'mew-imap-spam-field' and 'mew-imap-spam-word' are defined,
messages whose the field contains the word are automatically
removed or refiled to 'mew-imap-spam-folder' or 'mew-imap-trash-folder'
\(if defined) while scanning %inbox."
  :group 'mew-imap
  :type '(choice (const :tag "Not use" nil)
		 (string :tag "Spam field")))

(defcustom mew-imap-spam-word nil
  "*The word of anti spam information to be used for IMAP spam filter.
See 'mew-imap-spam-field'."
  :group 'mew-imap
  :type '(choice (const :tag "Not use" nil)
		 (string :tag "Spam word")))

(defcustom mew-imap-spam-pattern nil
  "*IMAP SEARCH pattern to filter spams.
If non-nil, 'mew-imap-spam-field' and 'mew-imap-spam-field'
treated as 'nil'."
  :group 'mew-imap
  :type '(choice (const :tag "Not use" nil)
		 (string :tag "Spam word")))

(defcustom mew-imap-spam-folder nil
  "*A folder to be used for IMAP spam filter.
See 'mew-imap-spam-field'."
  :group 'mew-imap
  :type '(choice (const :tag "Not use" nil)
		 (string :tag "Spam folder")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hooks
;;;

(defcustom mew-env-hook nil
  "*Hook called at initialize time before setting environment."
  :group 'mew-env
  :type 'hook)

(defcustom mew-init-hook nil
  "*Hook called at initialize time."
  :group 'mew-env
  :type 'hook)

(defcustom mew-summary-mode-hook nil
  "*Hook called in Summary mode."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-virtual-mode-hook nil
  "*Hook called in Virtual mode."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-thread-display-hook nil
  "*Hook called after new threads are displayed."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-header-mode-hook nil
  "*Hook called in Header mode."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-draft-mode-hook nil
  "*Hook called in Draft mode."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-draft-mode-newdraft-hook nil
  "*Hook called in Draft mode only when new draft is prepared."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-draft-mode-reedit-hook nil
  "*Hook called in Draft mode when a message not in +draft or +queue
is re-edited."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-draft-mode-reedit-draft-hook nil
  "*Hook called in Draft mode when a message in +draft is re-edited."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-draft-mode-reedit-queue-hook nil
  "*Hook called in Draft mode when a message in +queue is re-edited."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-draft-mode-edit-hook nil
  "*Hook called in Edit mode"
  :group 'mew-draft
  :type 'hook)

(defcustom mew-draft-mode-edit-again-hook nil
  "*Hook called in Draft mode when a message returned with the old style
is edited again."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-message-mode-hook nil
  "*Hook called in Message mode."
  :group 'mew-message
  :type 'hook)

(defcustom mew-message-hook nil
  "*Hook called whenever message displayed."
  :group 'mew-message
  :type 'hook)

(defcustom mew-make-message-hook nil
  "*Hook called before making a message in Draft mode. A good example
is as follows:
  (add-hook 'mew-make-message-hook 'ispell-message)"
  :group 'mew-draft
  :type 'hook)

(defcustom mew-send-hook nil
  "*Hook called before sending/queuing an e-mail message in Draft mode.
Note that this hook is called before composition of the message."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-post-hook nil
  "*Hook called before posting/queuing a NetNews message in Draft mode.
Note that this hook is called before composition of the message."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-real-send-hook nil
  "*Hook called before sending/queuing an e-mail message in Draft mode.
Note that this hook is called after composition of the message."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-real-post-hook nil
  "*Hook called before sending/queuing a NetNews message in Draft mode.
Note that this hook is called after composition of the message."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-smtp-flush-hook nil
  "*Hook called before SMTP runs if messages exist in +queue."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-nntp2-flush-hook nil
  "*Hook called before NNTP runs if messages exist in +postq"
  :group 'mew-draft
  :type 'hook)

(defcustom mew-smtp-sentinel-hook nil
  "*Hook called when a SMTP process finished."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-nntp-sentinel-hook nil
  "*Hook called when a NNTP process to receive messages finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-nntp2-sentinel-hook nil
  "*Hook called when a NNTP process to post messages finished."
  :group 'mew-draft
  :type 'hook)

(defcustom mew-pop-sentinel-hook nil
  "*Hook called when a POP process finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-pop-sentinel-non-biff-hook nil
  "*Hook called when a non-Biff POP process finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-imap-sentinel-hook nil
  "*Hook called when an IMAP process finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-imap-sentinel-non-biff-hook nil
  "*Hook called when a non-Biff IMAP process finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-scan-sentinel-hook nil
  "*Hook called when scan finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-summary-ls-no-scan-hook nil
  "*Hook called when mew-summary-ls does not scan a folder."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-summary-exec-hook nil
  "*Hook called when mew-summary-exec finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-sort-hook nil
  "*Hook called when sort finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-summary-toggle-disp-msg-hook nil
  "*Hook called when mew-summary-toggle-disp-msg finished."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-summary-delete-folder-hook nil
  "*Hook called when a folder deleted.
Eash function is called with a deleted folder as the argument."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-summary-rename-folder-hook nil
  "*Hook called when a folder renamed.
Each function is called with a source folder and a destination folder as the argument."
  :group 'mew-summary
  :type 'hook)

(defcustom mew-syntax-format-hook nil
  "*Hook called when mew-syntax-format is called."
  :group 'mew-message
  :type 'hook)

(defcustom mew-addrbook-mode-hook nil
  "*Hook called in Addrbook mode."
  :group 'mew-addrbook
  :type 'hook)

(defcustom mew-suspend-hook nil
  "*Hook called on suspend."
  :group 'mew-env
  :type 'hook)

(defcustom mew-quit-hook nil
  "*Hook called on quit."
  :group 'mew-env
  :type 'hook)

(defcustom mew-status-update-hook nil
  "*Hook called to update status."
  :group 'mew-env
  :type 'hook)

(defcustom mew-refile-guess-by-from-learn-hook nil
  "*Hook called in mew-refile-guess-by-from-learn."
  :group 'mew-env
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mail Addresses for Draft
;;;

(defcustom mew-mail-address-list nil
  "*The addresses included in this list never appear on the Cc: field
on a draft buffer. If nil, this value is automatically generated from
'mew-config-alist'."
  :group 'mew-draft
  :type '(choice (const nil) (repeat string)))

(defcustom mew-mail-domain-list nil
  "*Your e-mail address domain list like
\(\"example.org\" \"example.jp\").
They are used for mail-domain completion in Draft mode(C-cTAB). If
nil, this value is automatically generated from 'mew-config-alist'."
  :group 'mew-draft
  :type '(choice (const nil) (repeat string)))

(defcustom mew-from-list nil
  "*A list of From: for circular completion in Draft mode. If nil,
this value is automatically generated from 'mew-config-alist'. See
also 'mew-from'."
  :group 'mew-draft
  :type '(choice (const nil) (repeat string)))

(defcustom mew-warn-addresses nil
  "A list of addresses to be warned."
  :group 'mew-draft
  :type '(choice (const nil) (repeat string)))

(defcustom mew-safe-addresses nil
  "A list of addresses not to be warned."
  :group 'mew-draft
  :type '(choice (const nil) (repeat string)))

(defcustom mew-warn-domains nil
  "A list of domains to be warned."
  :group 'mew-draft
  :type '(choice (const nil) (repeat string)))

(defcustom mew-safe-domains nil
  "A list of domains not to be warned."
  :group 'mew-draft
  :type '(choice (const nil) (repeat string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mail Addresses
;;;

(defcustom mew-name (user-full-name)
  "*Friendly name of e-mail address.
i.e. \"Friendly name <user@mail-domain>\"."
  :group 'mew-basic
  :type '(choice string (const nil)))

(defcustom mew-user (user-login-name)
  "*User part of e-mail address."
  :group 'mew-basic
  :type 'string)

(defcustom mew-mail-domain (or (car mew-mail-domain-list)
			       mail-host-address
			       (system-name))
  "*Your e-mail address domain."
  :group 'mew-basic
  :type 'string)


(defvar mew-reply-all-alist
  '((("Followup-To:" "poster")
     ("To:" "From:"))
    ("Followup-To:"
     ("Newsgroups:" "Followup-To:" "Newsgroups:"))
    ("Newsgroups:"
     ("Newsgroups:" "Newsgroups:"))
    ("Reply-To:"
     ("To:" "Reply-To:" "From:")
     ("Cc:" "To:" "Cc:" "Apparently-To:"))
    (t
     ("To:" "From:")
     ("Cc:" "To:" "Cc:" "Apparently-To:")))
  "*Alist to be used to prepare To:/Cc:/Newsgroups: in a reply draft.
For most cases, this alist is used.
For more infomation, see the document of '\\<mew-summary-mode-map>\\[mew-summary-reply]'")

(defvar mew-reply-sender-alist
  '(("Reply-To:"
     ("To:" "Reply-To:" "From:"))
    (t
     ("To:" "From:")))
  "*Alist to be used to prepare To:/Cc:/Newsgroups: in a reply draft.
When '\\[universal-argument]' is specified for '\\<mew-summary-mode-map>\\[mew-summary-reply]' and '\\[mew-summary-reply-with-citation]', this alist is used.
For more infomation, see the document of '\\[mew-summary-reply]'")

(defvar mew-reply-fromme-alist
  '((t
     ("To:" "To:")
     ("Cc:" "Cc:")
     ("Newsgroups:" "Newsgroups:")))
  "*Alist to be used to prepare To:/Cc:/Newsgroups: in a reply draft.
When the message to be replied is sent/posted by ME, this alist is used.
For more infomation, see the document of '\\<mew-summary-mode-map>\\[mew-summary-reply]'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSH/SSL/TLS
;;;

(defcustom mew-ssh-keep-connection t
  "*If non-nil, an SSH process for each service (POP, IMAP, NNTP, SMTP)
is kept for further connections. This must be 't' for IMAP and NNTP."
  :group 'mew-net
  :type 'boolean)

(defcustom mew-ssl-keep-connection t
  "*If non-nil, an SSL/TLS process for each service (POP, IMAP, NNTP, SMTP)
is kept for further connections. This must be 't' for IMAP and NNTP."
  :group 'mew-net
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SMTP
;;;

(defcustom mew-smtp-server "localhost"
  "*The name of your SMTP server. If you want to use a remote SMTP
server, set an appropriate value."
  :group 'mew-smtp
  :type 'string)

(defcustom mew-smtp-port "smtp"
  "*The SMTP port. (e.g. \"smtp\" or 25)"
  :group 'mew-smtp
  :type 'string)

(defcustom mew-smtp-ssh-server nil
  "*The name of SSH server which forwards the SMTP port."
  :group 'mew-smtp
  :type '(choice (const :tag "Not use" nil)
		 (string :tag "Ssh server")))

(defcustom mew-smtp-ssl nil
  "*If non-nil, SMTP connections are made over SSL/TLS."
  :group 'mew-smtp
  :type 'boolean)

(defcustom mew-smtp-ssl-port 465 ;; no universal consensus
  "*The port for SMTP over SSL/TLS. Set this to \"smtp\" if you
want to use TLS."
  :group 'mew-smtp
  :type 'string)

(defcustom mew-smtp-user nil
  "*The user name on your SMTP server. If not configured,
your e-mail address is automatically set"
  :group 'mew-smtp
  :type '(choice string (const nil)))

(defcustom mew-smtp-auth-list '("CRAM-MD5" "PLAIN" "LOGIN")
  "*A list of SMTP AUTH methods in the preferred order.
Currently, \"CRAM-MD5\", \"PLAIN\", and \"LOGIN\" can be used."
  :group 'mew-smtp
  :type '(repeat (choice (const "CRAM-MD5") (const "PLAIN") (const "LOGIN"))))

(defcustom mew-smtp-helo-domain "localhost"
  "*An e-mail domain to tell a SMTP server with HELO/EHLO."
  :group 'mew-smtp
  :type 'string)

(defcustom mew-smtp-mail-from nil
  "*An e-mail address to tell a SMTP server with MAIL FROM:.
If nil, an address specified by the From: field is used."
  :group 'mew-smtp
  :type '(choice string (const nil)))

(defcustom mew-smtp-msgid-user nil
  "*A user part for creation of Message-Id: for SMTP. If nil, the
value of 'mew-user' is used."
  :group 'mew-smtp
  :type '(choice string (const nil)))

(defcustom mew-smtp-msgid-domain nil
  "*A domain name for creation of Message-Id: for SMTP. If nil, the
value of 'mew-mail-domain' is used."
  :group 'mew-smtp
  :type '(choice string (const nil)))

(defvar mew-smtp-timeout-time 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Retrieving messages
;;;

(defcustom mew-mailbox-type 'pop
  "'pop, 'imap or 'mbox"
  :group 'mew-net
  :type '(choice (const pop) (const imap) (const mbox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mbox
;;;

(defcustom mew-mbox-command "incm"
  "*A command to be execute if 'mew-mailbox-type' is 'mbox."
  :group 'mew-pop
  :type '(choice string (const nil)))

(defcustom mew-mbox-command-arg "-u"
  "*Arguments for 'mew-mbox-command'.
For maildir, set this to '-u -d /path/to/mbox'."
  :group 'mew-pop
  :type '(choice string (const nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Biff
;;;

(defcustom mew-biff-interval 5
  "*Minutes of biff interval. This should be smaller than
'mew-passwd-timer-unit' * 'mew-passwd-lifetime'."
  :group 'mew-net
  :type 'integer)

(defcustom mew-biff-function 'mew-biff-bark
  "*A function to be called when messages arrive."
  :group 'mew-net
  :type '(choice function (const nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; POP
;;;

(defcustom mew-pop-server "localhost"
  "*The name of your POP server. If you want to use a remote POP
server, set an appropriate value."
  :group 'mew-pop
  :type 'string)

(defcustom mew-pop-port "pop3"
  "*The POP3 port. (e.g. \"pop3\" or 110)"
  :group 'mew-pop
  :type 'string)

(defcustom mew-pop-ssh-server nil
  "*The name of SSH server which forwards the POP3 port."
  :group 'mew-pop
  :type '(choice string (const nil)))

(defcustom mew-pop-ssl nil
  "*If non-nil, POP connections are made over SSL/TLS."
  :group 'mew-pop
  :type 'boolean)

(defcustom mew-pop-ssl-port "pop3s"
  "*The port for POP over SSL/TLS."
  :group 'mew-pop
  :type 'string)

(defcustom mew-pop-user (user-login-name)
  "*The user name on your POP server."
  :group 'mew-pop
  :type 'string)

(defcustom mew-pop-auth 'apop
  "*The authentication method for POP3.
'pass means the authentication with USER/PASS (i.e. plain password).
'apop means the authentication with APOP.
t means SASL according to 'mew-pop-auth-list'."
  :group 'mew-pop
  :type '(choice (const apop) (const pass) (const t)))

(defcustom mew-pop-auth-list '("CRAM-MD5" "PLAIN")
  "*A list of SASL methods in the preferred order.
Currently, \"CRAM-MD5\" can be used."
  :group 'mew-pop
  :type '(repeat (choice (const "CRAM-MD5") (const "PLAIN"))))

(defcustom mew-pop-delete t
  "*Whether or not delete messages on a POP server after retrieval by
POP. If t, delete the messages. If nil, retain the messages. If
number N, delete the messages N days after the first access.
Otherwise they are not deleted."
  :group 'mew-pop
  :type '(choice
	  (const :tag "Delete immediately" t)
	  (const :tag "Retain the messages" nil)
	  (integer :tag "Delete N days after")))

(defcustom mew-pop-size (* 54 1024) ;; 4K hdr + 50K bdy
  "*The limit size of messages to be retrieved by POP. The default is
55296 byte. 0 means unlimited, so you can get all messages from the
POP server."
  :group 'mew-pop
  :type 'integer)

(defcustom mew-pop-body-lines 40
  "*The limit of body lines to get when the size of message
exceeds 'mew-pop-size'."
  :group 'mew-pop
  :type 'integer)

(defcustom mew-pop-header-only nil
  "*Whether or not the body of a message is retrieved by POP.
If non-nil, only header is retrieved."
  :group 'mew-pop
  :type 'boolean)

(defcustom mew-pop-proxy-server nil
  "*The name of POP proxy server."
  :group 'mew-pop
  :type '(choice (const :tag "Not use" nil)
		 (string :tag "Pop proxy server")))

(defcustom mew-pop-proxy-port nil
  "*The port for POP proxy."
  :group 'mew-pop
  :type '(choice string (const nil)))

(defvar mew-pop-timeout-time 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMAP
;;;

(defcustom mew-imap-server "localhost"
  "*The name of your IMAP server. If you want to use a remote IMAP
server, set an appropriate value."
  :group 'mew-imap
  :type 'string)

(defcustom mew-imap-port "imap"
  "*The IMAP4 port. (e.g. \"imap\" or 143)"
  :group 'mew-imap
  :type 'string)

(defcustom mew-imap-ssh-server nil
  "*The name of SSH server which forwards the IMAP4 port."
  :group 'mew-imap
  :type '(choice string (const nil)))

(defcustom mew-imap-ssl nil
  "*If non-nil, IMAP connections are made over SSL/TLS."
  :group 'mew-imap
  :type 'boolean)

(defcustom mew-imap-ssl-port "imaps"
  "*The port for IMAP over SSL/TLS."
  :group 'mew-imap
  :type 'string)

(defcustom mew-imap-user (user-login-name)
  "*The user name on the IMAP server."
  :group 'mew-imap
  :type 'string)

(defcustom mew-imap-auth t
  "*The authentication method for IMAP4.
nil means the authentication with LOGIN (i.e. plain password).
t means SASL according to 'mew-imap-auth-list'."
  :group 'mew-imap
  :type 'boolean)

(defcustom mew-imap-auth-list '("CRAM-MD5"  "LOGIN")
  "*A list of SASL methods in the preferred order.
Currently, \"CRAM-MD5\" and \"LOGIN\" can be used."
  :group 'mew-imap
  :type '(repeat (choice (const "CRAM-MD5") (const "LOGIN"))))

(defcustom mew-imap-delete t
  "*Whether or not delete messages on an IMAP server after retrieval by
IMAP. If t, delete the messages. If nil, retain the messages. If
number N, delete the messages N days after the first access.
Otherwise they are not deleted."
  :group 'mew-imap
  :type 'boolean)

(defcustom mew-imap-size (* 54 1024) ;; 4K hdr + 50K bdy
  "*The limit size of messages to be retrieved by IMAP. The default is
55296 byte. 0 means unlimited, so you can get all messages from the
IMAP server."
  :group 'mew-imap
  :type 'integer)

(defcustom mew-imap-header-only nil
  "*Whether or not the body of a message is retrieved by IMAP.
If non-nil, only header is retrieved."
  :group 'mew-imap
  :type 'boolean)

(defcustom mew-imap-prefix-list nil
  "A list to define which prefixes of user mailboxes should be used.

For example, the WU IMAP server returns the following value
for the NAMESPACE command.

	((\"\" \"/\")(\"#mhinbox\" NIL)(\"#mh/\" \"/\")) ((\"/\" \"/\")) ((\"#shared/\" \"/\")(\"#ftp/\" \"/\")(\"#news.\" \".\")(\"#public/\" \"/\"))

The first list is prefixes of user mailboxes. Each element consists
of a prefix and a separator.

If this valuable is nil, the prefix in the first element of the
list is used as a prefix of user mailboxes.

If this value is a list of strings, matched elements are used.
If the separator is non-nil, the prefix is used. If the separator
is nil, the prefix is added to mailbox alist as a mailbox.

If you want to used your home directory with WU, configure as
follows:

	(setq mew-imap-prefix-list '(\"\"))

If you want to used the MH directory with WU, configure as
follows:

	(setq mew-imap-prefix-list '(\"#mh/\" \"#mhinbox\"))

Note that %inbox means /var/mail/<user> while %#mhinbox refers to
Mail/inbox.
"
  :group 'mew-imap
  :type '(choice (const nil) (repeat string)))

(defcustom mew-imap-proxy-server nil
  "*The name of IMAP proxy server."
  :group 'mew-imap
  :type '(choice (const :tag "Not use" nil)
		 (string :tag "Imap proxy server")))

(defcustom mew-imap-proxy-port nil
  "*The port for IMAP proxy."
  :group 'mew-imap
  :type '(choice string (const nil)))

(defvar mew-imap-timeout-time 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NNTP
;;;

(defcustom mew-nntp-server "localhost"
  "*The name of your NNTP server. If you want to use a remote NNTP
server, set an appropriate value."
  :group 'mew-nntp
  :type 'string)

(defcustom mew-nntp-port "nntp"
  "*The NNTP port. (e.g. \"nntp\" or 119)"
  :group 'mew-nntp
  :type 'string)

(defcustom mew-nntp-ssh-server nil
  "*The name of SSH server which forwards the NNTP port."
  :group 'mew-nntp
  :type '(choice string (const nil)))

(defcustom mew-nntp-ssl nil
  "*If non-nil, NNTP connections are made over SSL/TLS."
  :group 'mew-nntp
  :type 'boolean)

(defcustom mew-nntp-ssl-port "nntps"
  "*The port for NNTP over SSL/TLS."
  :group 'mew-nntp
  :type 'string)

(defcustom mew-nntp-user nil
  "*The user name on the NNTP server. If non-nil, authentication is
used."
  :group 'mew-nntp
  :type '(choice (const :tag "Not use" nil)
		 (string :tag "Nntp user")))

(defcustom mew-nntp-newsgroup "-fj.mail.reader.mew"
  "*The name of default Newsgroup."
  :group 'mew-nntp
  :type 'string)

(defcustom mew-nntp-size (* 54 1024) ;; 4K hdr + 50K bdy
  "*The limit size of messages to be retrieved by NNTP. The default is
55296 byte. 0 means unlimited, so you can get all messages from the
NNTP server."
  :group 'mew-nntp
  :type 'integer)

(defcustom mew-nntp-header-only nil
  "*Whether or not the body of a message is retrieved by NNTP.
If non-nil, only header is retrieved."
  :group 'mew-nntp
  :type 'boolean)

(defcustom mew-nntp-msgid-user nil
  "*A user part for creation of Message-Id: for NNTP. If nil, the
value of 'mew-user' is used."
  :group 'mew-nntp
  :type '(choice string (const nil)))

(defcustom mew-nntp-msgid-domain nil
  "*A domain name for creation of Message-Id: for NNTP. If nil, the
value of 'mew-mail-domain' is used."
  :group 'mew-nntp
  :type '(choice string (const nil)))

(defvar mew-nntp-timeout-time 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; range
;;;

(defcustom mew-range-list nil
  "*A list of (KEY VALUE) to define RANGE for each folder.

If KEY is t, all folders matches it. The corresponding RANGE is
returned always.

If KEY is a string, match-function is applied according to
'mew-range-list-string-type'. If matched, the corresponding RANGE is
returned.

If KEY is a list of strings, match-function is applied according to
'mew-range-list-list-type'. If a match found out of the strings,
the corresponding RANGE is returned.

Candidate values for 'mew-rage-list-string-type' and
'mew-range-list-list-type' are as follows:

	'regex		Regular expression.
	'recursive	Initial substring match. That is,
			not only a folder specified but all so
			all sub-folders are matched.
	'string		String comparison.

An example is as follows:
	'(((\"+queue\" \"+postq\" \"+draft\") \"all\")
	  ((\"$inbox\") \"sync\")
	  (t \"update\"))

If this value is nil, an appropriate value is set when Mew is booting.
"
  :group 'mew-summary
  :type '(choice
	  (const nil)
	  (repeat
	   (list (choice (string :tag "Folder")
			 (repeat :tag "Folders" (string :tag "Folder"))
			 (const :tag "Other" t))
		 string))))

(defcustom mew-range-list-string-type 'regex
  "*A value to specify an action if the key is a string in
'mew-range-list'. Candidates are 'regex, 'recursive, and 'string."
  :group 'mew-summary
  :type '(choice (const regex) (const recursive) (const string)))

(defcustom mew-range-list-list-type 'recursive
  "*A value to specify an action if the key is a list of strings in
'mew-range-list'. Candidates are 'regex, 'recursive, and 'string."
  :group 'mew-summary
  :type '(choice (const regex) (const recursive) (const string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; unread-mark
;;;

(defcustom mew-unread-mark-list
  '((("+inbox" "$inbox" "%inbox" "-") t)
    (t nil))
  "*A list of (KEY VALUE) to define the read/unread marks for
each folder when scanning. If VALUE is non-nil, messages are
marked as unread. Otherwise, message are not marked, that is,
marked as read.

If KEY is t, all folders matches it. The corresponding VALUE is
returned always.

If KEY is a string, match-function is applied according to
'mew-unread-mark-list-string-type'. If matched, the corresponding
VALUE is returned.

If KEY is a list of strings, match-function is applied according to
'mew-unread-mark-list-list-type'. If a match found out of the strings,
the corresponding VALUE is returned.

Candidate values for 'mew-rage-list-string-type' and
'mew-unread-mark-list-list-type' are as follows:

	'regex		Regular expression.
	'recursive	Initial substring match. That is,
			not only a folder specified but all so
			all sub-folders are matched.
	'string		String comparison.

The default value is:
	'(((\"+inbox\" \"$inbox\" \"%inbox\" \"-\") t)
	  (t nil))

An example for the case where messages are marked as unread in any
folders:
	'((t t))

Note that this variable is effective only when 'mew-use-unread-mark'
is non-nil.
"
  :group 'mew-summary
  :type '(choice
	  (const nil)
	  (repeat
	   (list (choice (string :tag "Folder")
			 (repeat :tag "Folders" (string :tag "Folder"))
			 (const :tag "Other" t))
		 string))))

(defcustom mew-unread-mark-list-string-type 'regex
  "*A value to specify an action if the key is a string in
'mew-unread-mark-list'. Candidates are 'regex, 'recursive, and 'string."
  :group 'mew-summary
  :type '(choice (const regex) (const recursive) (const string)))

(defcustom mew-unread-mark-list-list-type 'recursive
  "*A value to specify an action if the key is a list of strings in
'mew-unread-mark-list'. Candidates are 'regex, 'recursive, and 'string."
  :group 'mew-summary
  :type '(choice (const regex) (const recursive) (const string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan
;;;

(defconst mew-summary-form-header '(mark)
  "*A list to be appended to summary-form if the first element of
summary-form is not an integer. Since Mew assumes that
each line of Summary mode begins with one mark, this variable
MUST be '(mark).")

(defconst mew-custom-type-of-summary-form
  '(choice
    (cons :tag "Setup message number column"
	  (integer :tag "Message number column" )
	  (repeat (choice (string :tag "Insert string")
			  (const :tag "Thread indent" t)
			  (list :tag "Width and symbol"
				:value (0 type)
				(integer :tag "Display width")
				(choice :tag "Symbol"
					:value type
					(const type) (const time)
					(const date) (const year)
					(const size) (const from)
					(const subj) (symbol :tag "Other")))
			  (choice :tag "Symbol"
				  :value type
				  (const type) (const time)
				  (const date) (const year)
				  (const size) (const from)
				  (const subj) (symbol :tag "Other")))))
    (repeat :tag "Use default number column"
	    (choice (integer :tag "Message number column")
		    (string :tag "Insert string")
		    (const :tag "Thread indent" t)
		    (list :tag "Width and symbol"
			  :value (0 type)
			  (integer :tag "Display width")
			  (choice :tag "Symbol"
				  :value type
				  (const type) (const time)
				  (const date) (const year)
				  (const size) (const from)
				  (const subj) (symbol :tag "Other")))
		    (choice :tag "Symbol"
			    :value type
			    (const type) (const time)
			    (const date) (const year)
			    (const size) (const from)
			    (const subj) (symbol :tag "Other"))))))

(defvar mew-summary-form-body-starter "|")

(defcustom mew-summary-form
  `(type (5 date) " " (14 from) " " t (30 subj)
	 ,mew-summary-form-body-starter (0 body))
  "*The format in Summary mode, called summary-form. summary-form is a list
of list, symbol, and string.

\(a) A string is printed as it is.

\(b) A list consists of an integer and a symbol. The symbol specifies a
function to be called. The name of the function is produced by
concatenating 'mew-summary-form-func-prefix' and the symbol name.
\(e.g. 'mew-summary-form-date' for the symbol 'date').

Pre-defined symbols are 'type, 'time, 'date, 'year, 'size, 'from,
'subj and 'body. For more details, see the explanation of the
functions called 'mew-summary-form-<symbol>'.

The integer specifies the width of field which will be filled with a
return string of the function.

A positive value means padding SPCs on the right if necessary.

A negative value means padding SPCs on the left if necessary. If a
negative value is specified, the corresponding function must return an
ASCII string.

0 means the remaining width and is treated as a positive value.

\(c) A symbol except 't' is equivalent to (1 symbol).

\(d) The value of mew-summary-form-header (mark) is appended to
summary-form when used. See also 'mew-summary-form-header'.

\(e) 't' means the position where thread indentation is inserted.

An example is as follows:
	'(type (5 date) \" \" (-4 size) \" \" (14 from) \" \" t (30 subj) \"|\" (0 body))

You can also set this value in 'mew-summary-form-list'."
  :group 'mew-summary
  :type mew-custom-type-of-summary-form)

(defcustom mew-summary-form-list nil
  "*A list of (KEY VALUE) to define 'mew-summary-form' for each
folder. Each component is (key summary-form).

If KEY is t, all folders matches it. The corresponding SUMMARY-FORM is
returned always.

If KEY is a string, match-function is applied according to
'mew-summary-form-list-string-type'. If matched, the corresponding
SUMMARY-FORM is returned.

If KEY is a list of strings, match-function is applied according to
'mew-summary-form-list-list-type'. If a match found out of the strings,
the corresponding SUMMARY-FORM is returned.

Candidate values for 'mew-summary-form-list-string-type' and
'mew-summary-form-list-list-type' are as follows:

	'regex		Regular expression.
	'recursive	Initial substring match. That is,
			not only a folder specified but all so
			all sub-folders are matched.
	'string		String comparison.

SUMMARY-FORM is a list. See 'mew-summary-form' for more information.

An example is as follows:
      '(((\"+inbox\")
	 (type (5 date) \" \" (-4 size) \" \" (14 from) \" \" t (30 subj) \"|\" (0 body)))
	(t
	 (type (5 date) \" \" (14 from) \" \" t (30 subj) \"|\" (0 body))))
"
  :group 'mew-summary
  :type `(choice
	  (const nil)
	  (repeat
	   (list (choice (string :tag "Folder")
			 (repeat :tag "Folders" (string :tag "Folder"))
			 (const :tag "Other" t))
		 ,mew-custom-type-of-summary-form
		 (choice (integer :tag "Thread indent column")
			 (const nil))))))

(defcustom mew-summary-form-list-string-type 'regex
  "*A value to specify an action if the key is a string in
'mew-summary-form-list'. Candidates are 'regex, 'recursive, and 'string."
  :group 'mew-summary
  :type '(choice (const regex) (const recursive) (const string)))

(defcustom mew-summary-form-list-list-type 'recursive
  "*A value to specify an action if the key is a list of strings in
'mew-summary-form-list'. Candidates are 'regex, 'recursive, and 'string."
  :group 'mew-summary
  :type '(choice (const regex) (const recursive) (const string)))

(defcustom mew-summary-form-from-me-prefix "To:"
  "*The prefix to be prepend to an destination address
if the message is originated by me."
  :group 'mew-summary
  :type 'string)

(defcustom mew-summary-form-extract-rule '(nickname)
  "*A list to specify what part to extract from the From: field. Each
element must be 'name, 'comment, 'address, 'nickname, or appropriate
\"regex\".

Consider the following examples:

	A: Kazu Yamamoto <kazu@example.org>
	B: kazu@example.org (Kazu Yamamoto)
	C: Kazuhiko Yamamoto (Kazu) <kazu@example.org>

Each element returns the following value:

	'name		Name part, if any
				A: Kazu Yamamoto
				B: (No match)
				C: Kazuhiko Yamamoto (Kazu)
	'comment	Comment part, if any
				A: (No match)
				B: Kazu Yamamoto
				C: (Kazu)
	'address	Address part
				A: kazu@example.org
				B: kazu@example.org
				C: kazu@example.org

	'nickname       One element of personal information in
                        Addrbook according to 'mew-addrbook-for-summary'.
                        The default value of 'mew-addrbook-for-summary'
                        is 'nickname. So, From: is converted a nickname
                        by default. For more information, see
                        'mew-addrbook-switch'.

	(regex)		The substring first matched

If a element does not match, the next element is applied. If no
element matches to the From: field, or this value is nil, the whole
of the From: field is used."
  :group 'mew-summary
  :type '(repeat (choice regexp (const name) (const comment)
                         (const address) (const nickname))))

(defcustom mew-summary-form-mark-delete nil
  "*If non-nil, the 'D' mark automatically is put onto
duplicated messages. If a character, use it as a mark
instead of the 'D' mark."
  :group 'mew-summary
  :type '(choice (const :tag "Use mew-mark-delete" t)
		 (const :tag "Not use" nil)
		 (character :tag "Use another character")))

(defcustom mew-summary-form-mark-spam nil
  "*If non-nil, the 'D' mark automatically is put onto
SPAM messages. A message is considered a SPAM message
if the MD5 checksum of its body is duplicated in a scan.
If a character, use it as a mark instead of the 'D' mark."
  :group 'mew-summary
  :type '(choice (const :tag "Use mew-mark-delete" t)
		 (const :tag "Not use" nil)
		 (character :tag "Use another character")))

(defcustom mew-summary-form-mark-review nil
  "*If non-nil, the '*' mark automatically is put onto
messages destined to me. If a character, use it as a mark
instead of the '*' mark."
  :group 'mew-summary
  :type '(choice (const :tag "Use mew-mark-review" t)
		 (const :tag "Not use" nil)
		 (character :tag "Use another character")))

(defcustom mew-summary-form-size-0k nil
  "*If non-nil, the size of message is displayed as '0k'
if the size is less than 1k byte."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-summary-form-size-huge t
  "*If non-nil, the size of message is displayed as 'HUGE'
when the size is greater than or equal to 1000 after maximum
quantization by 'mew-summary-form-size-unit'."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-spam: "X-Bogosity:"
  "*The field name which your spam filter inserts.
E.g.
	\"X-Spam-Flag:\" for 'spamassassin'
	\"X-Bogosity:\" for 'bogofilter'"
  :group 'mew-summary
  :type 'string)

(defcustom mew-scan-fields nil
  "*A list which specifies mewl's output.
The first element MUST be \"Folder:\".
The second element MUST be \"Filename:\".
Both \"In-Reply-To:\" and \"References:\" MUST be included for thread.
The last MUST be \"Body\"."
  :group 'mew-summary
  :type '(cons (const "Folder:")
               (cons (const "Filename:")
                     (repeat string))))

(defvar mew-scan-fields-alias nil
  "*A list of aliases for 'mew-scan-fields'.
Functions called MEW-FOO will be defined according to this variable.")

(defvar mew-summary-form-func-prefix "mew-summary-form-"
  "The prefix for symbol used in 'mew-summary-form'.")

(defvar mew-scan-decode-fields (list mew-subj: mew-from: mew-to:))

(defvar mew-scan-decode-bq-body t)

(defvar mew-draft-address-warning-fields
  (list mew-to: mew-cc: mew-dcc: mew-bcc:
	mew-resent-to: mew-resent-cc: mew-resent-dcc: mew-resent-bcc:))

(defcustom mew-thread-column 28
  "A position where thread indentation is inserted
for indentation of thread. This valuable is for backward compatibility
only. Use 't' in 'mew-summary-form instead."
  :group 'mew-summary
  :type 'integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Demo
;;;

(defcustom mew-demo t
  "*Mew demo is displayed at boot time if *non-nil*."
  :group 'mew-env
  :type 'boolean)

(defcustom mew-demo-picture t
  "*A picture of cats is displayed if *non-nil*."
  :group 'mew-env
  :type 'boolean)

(defcustom mew-demo-sit-for 0
  "*Time of demo picture staying."
  :group 'mew-env
  :type '(choice integer (const nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Limits
;;;

(defcustom mew-file-max-size 100000
  "*The max size of messages. If the size of a message is greater
than mew-file-max-size, Mew skips MIME analysis."
  :group 'mew-message
  :type 'integer)

(defcustom mew-header-reasonable-size 10000
  "*The max size of header to be inserted to a temporary buffer to
obtain any fields."
  :group 'mew-message
  :type 'integer)

(defcustom mew-header-max-length 200
  "*If the length of a header exceeds this value,
it is not arranged nor MIME decoded.
See also 'mew-header-max-depth'."
  :group 'mew-message
  :type 'integer)

(defcustom mew-header-max-depth 50
  "*A value to decide loop depth for header field syntax analysis.
It was known as mew-loop-depth.
See also 'mew-header-max-length'."
  :group 'mew-message
  :type 'integer)

(defcustom mew-references-max-count 3
  "*A value for the max number of message IDs in References: when reply.
Non-integer means no limit."
  :group 'mew-reply
  :type '(choice (integer :tag "Max count")
		 (const :tag "No limit" nil)))

(defcustom mew-lisp-max-length 2000
  "*Mew saves an internal lisp structure to a file truncating
to this file."
  :group 'mew-env
  :type 'integer)

(defcustom mew-expand-max-depth 5
  "*A value to limit Addrbook expansion loop."
  :group 'mew-draft
  :type 'integer)

(defcustom mew-log-max-size 512000 ;; 500KB
  "*A value for the max size of log files."
  :group 'mew-draft
  :type 'integer)

(defcustom mew-highlight-body-max-size 10000
  "A limit for highlighting a body. '0' means No-Limitation"
  :group 'mew-highlight
  :type 'integer)

(defcustom mew-scan-max-field-length 5
  "*Max field length which 'mewl' treats."
  :group 'mew-env
  :type 'integer)

(defcustom mew-scan-max-body-length 20
  "*Max body length which 'mewl' treats."
  :group 'mew-env
  :type 'integer)

(defcustom mew-scan-body-length 10
  "*Body length which Mew treats.
Increase this value if 'mew-summary-form-mark-spam' put
the 'D' mark onto different messages."
  :group 'mew-env
  :type 'integer)

(defcustom mew-flowed-fold-threshold 78
  "*A threshold length of lines in a draft body.
if `mew-use-format-flowed' is `t' and any lines are over this
threshold, the format=flowed encoding is used."
  :group 'mew-draft
  :type 'integer)

(defcustom mew-flowed-fold-length 70
  "*A length to which each line is folded when format=flowed is used."
  :group 'mew-draft
  :type 'integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ask?
;;;

(defcustom mew-ask-subject nil
  "*If *non-nil* and Subject: is empty, ask a user its value."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-ask-to nil
  "*If *non-nil*, ask a user to whom he/she sends."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-ask-cc nil
  "*If *non-nil*, ask a user to whom he/she sends Cc."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-ask-fcc nil
  "*If *non-nil* and a folder on Fcc: does not exist, ask a user to
create it. If nil, folders which are not present are created without
any query."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-ask-newsgroups nil
  "*If *non-nil* and Newsgroups: exists, ask a user to send this really."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-ask-range nil
  "*If *non-nil*, Mew asks range if the cache in Summary mode seems
invalid. Otherwise, 'update is used for range in the case."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-ask-cite-prefix nil
  "*If *non-nil*, ask citation prefix when cite a message."
  :group 'mew-cite
  :type 'boolean)

(defcustom mew-ask-pack t
  "*If *non-nil*, ask whether or not you really want to pack."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-ask-send t
  "*If *non-nil*, ask whether or not you really want to send the message."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-ask-post t
  "*If *non-nil*, ask whether or not you really want to post the message."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-ask-flush-queue nil
  "*If *non-nil*, ask whether or not you really want to flush queue."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-ask-flush-case nil
  "*If *non-nil*, ask case when flushing queue."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-ask-charset nil
  "*Mew asks a user whether or not the charset chosen on composing
is appropriate.
	nil:               not ask
	A list of charset: ask if the charset is not a member of the list
	t:                 ask if 'mew-charset-m17n' is used."
  :group 'mew-draft
  :type '(choice (const nil) (const t) (repeat string)))

(defcustom mew-ask-folder-after-join nil
  "*If *non-nil*, Mew asks to move to the folder where the join
message is stored."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-ask-folder-after-burst nil
  "*If *non-nil*, Mew asks to move to the folder where the burst
messages are stored."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-ask-pipe nil
  "*If *non-nil*, Mew confirms when '\\<mew-summary-mode-map>\\[mew-summary-pipe-message]' is used."
  :group 'mew-summary
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Use
;;;

(defcustom mew-use-full-window nil
  "*If non-nil, the entire frame is used for Mew.
Otherwise, windows are configured dynamically."
  :group 'mew-env
  :type 'boolean)

(defcustom mew-use-other-frame-for-draft nil
  "*If non-nil, a new frame is created when a draft is prepared."
  :group 'mew-env
  :type 'boolean)

(defcustom mew-use-other-frame-for-summary nil
  "*If non-nil, a new frame is created when you visit a new folder."
  :group 'mew-env
  :type 'boolean)

(defcustom mew-use-text/enriched t
  "*If non-nil, Mew parses enriched format text messages."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-use-text/html nil
  "*If non-nil, Mew parses text/html messages.
If you want to use 'mew-use-text/html-list', this value should be 'nil'."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-use-text/html-list nil
  "*A list of (KEY VALUE) to define whether or not HTML should be parsed
for each folder.

If KEY is t, all folders matches it. The corresponding boolean is
returned always.

If KEY is a string, match-function is applied according to
'mew-use-text/html-string-type'. If matched, the corresponding
boolean is returned.

If KEY is a list of strings, match-function is applied according
to 'mew-use-text/html-list-type'. If a match found out of the
strings, the corresponding boolean is returned.

Candidate values for 'mew-use-text/html-string-type' and
'mew-use-text/html-list-type' are as follows:

	'regex		Regular expression.
	'recursive	Initial substring match. That is,
			not only a folder specified but all so
			all sub-folders are matched.
	'string		String comparison.
"
  :group 'mew-summary
  :type '(choice
	  (const nil)
	  (repeat
	   (list (choice (string :tag "Folder")
			 (repeat :tag "Folders" (string :tag "Folder"))
			 (const :tag "Other" t))
		 boolean))))

(defcustom mew-use-text/html-string-type 'regex
  "*A value to specify an action if the key is a string in
'mew-use-text/html-list'. Candidates are 'regex, 'recursive, and 'string."
  :group 'mew-summary
  :type '(choice (const regex) (const recursive) (const string)))

(defcustom mew-use-text/html-list-type 'recursive
  "*A value to specify an action if the key is a list of strings in
'mew-use-text/html-list'. Candidates are 'regex, 'recursive, and 'string."
  :group 'mew-summary
  :type '(choice (const regex) (const recursive) (const string)))


(defcustom mew-use-text/xml nil
  "*If non-nil, Mew parses text/xml messages."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-use-symbolic-link-for-forwarding nil
  "*If nil, messages to be forwarded is copied to +draft when
'\\<mew-summary-mode-map>\\[mew-summary-forward]' and '\\[mew-summary-multi-forward]' are used. Otherwise, symbolic links are
created (if the feature is provided)."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-use-cached-passwd nil
  "*if non-nil, passwords are cached."
  :group 'mew-privacy
  :type 'boolean)

(defcustom mew-use-master-passwd nil
  "*if non-nil, a master password is used."
  :group 'mew-privacy
  :type 'boolean)

(defcustom mew-use-8bit nil
  "*If non-nil, the 8bit-clean charset mechanism is used for 8bit
charsets, such as ISO-8859-1, in Draft mode. If nil, an appropriate
MIME encoding such as quoted-printable is applied. This also enables
to attach 8bit messages as it is. If nil, 8bit messages are converted
into the 7bit form."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-use-charset-sanity-check t
  "*If non-nil, the sanity check of character set is used when
composing."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-use-autoconv-when-unknown t
  "*If non-nil, Mew uses 'mew-cs-autoconv' for an unknown coding system."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-use-burst-folder-history nil
  "*If non-nil, the folder specified at the previous burst job
is used as a folder candidate instead of inbox."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-use-biff nil
  "*If non-nil, Mew displays \"Mail(n)\" in the mode line if your
message server receives messages. This is confirmed by POP or IMAP
every 'mew-biff-interval' minutes.

To use this feature, 'mew-use-cached-passwd' should be 't'.

If you want to use the biff functionality against a local mailbox, use
'display-time-mode' instead."
  :group 'mew-env
  :type 'boolean)

(defcustom mew-use-biff-bell nil
  "*If non-nil, Mew beeps when the number of messages in
your POP server becomes non-zero. To use this feature,
'mew-use-biff' should be 't'."
  :group 'mew-env
  :type 'boolean)

(defcustom mew-use-sender nil
  "*If From: contains multiple addresses, Mew adds Sender: anyway.
If From: contains one address and SMTP MAIL FROM
\('mew-smtp-mail-from') is different from it, this variable effects.
If non-nil, Mew adds Sender: and specifies SMTP MAIL FROM to its value.
Otherwise, Mew does not add Sender:."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-use-x-mailer t
  "*If non-nil, X-Mailer: is prepared in Draft mode"
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-use-alternative t
  "*If non-nil, Multipart/Alternative is treated as a singlepart
according to 'mew-mime-multipart-alternative-list'."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-disable-alternative-regex-list nil
  "*A list of regular expression to match the value of X-Mailer:.
If one of them matches, Multiprat/Alternative is treated as
Multipart/Mixed.

An example:
  (setq mew-disable-alternative-regex-list '(\"Apple Mail\"))"
  :group 'mew-summary
  :type '(list regexp))

(defcustom mew-use-text-body t
  "*If non-nil, a single Text/* body or the first Text/* part in a top level
multipart is displayed with its header."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-use-nfs-hack nil
  "*If non-nil, Mew takes some workarounds for NFS."
  :group 'mew-env
  :type 'boolean)

(defcustom mew-use-old-wvhtml nil
  "*If non-nil, Mew calls wvHtml in the old argument style."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-use-net-status t
  "*If non-nil, Mew displays network status to the mode line."
  :group 'mew-net
  :type 'boolean)

(defcustom mew-use-fast-refile nil
  "*If non-nil, 'rassoc' is used to look up the alist of
folders. Since it is a built-in function, it makes marking for refile
much faster especially when the alist is long. But the fast method
is not case-sensitive. So, you should set this variable to 't' only when
you does not use capital letters for your folders. If nil, the old
slow method, which is case-sensitive, is used."
  :group 'mew-refile
  :type 'boolean)

(defcustom mew-use-node-folder t
  "*If non-nil, candidate folders chosen by 'mew-refile-guess-by-folder'
includes node folders as well as leaf folders. Otherwise, candidates
are selected out of leaf folders only."
  :group 'mew-refile
  :type 'boolean)

(defcustom mew-use-case-completion t
  "*If non-nil, 'mew-case' is automatically completed when you
input a folder.  For example, consider that you input a folder in a
local folder after typing '\\<mew-summary-mode-map>\\[mew-summary-goto-folder]'.
Typing '%' automatically inserts 'mew-case' before '%'."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-use-spc-padding t
  "*If non-nil, SPCs are padded from the end of lines of Summary mode
to the right side."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-use-push-mark nil
  "*If non-nil, '\\<mew-summary-mode-map>\\[mew-summary-jump-top]' and '\\<mew-summary-mode-map>\\[mew-summary-jump-bottom]' push the mark."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-use-old-pgp nil
  "*If non-nil, the old fashioned PGP message are used when signing/encrypting.
Otherwise PGP/MIME is used."
  :group 'mew-privacy
  :type 'boolean)

(defcustom mew-use-full-alias nil
  "*If non-nil, the entire addresses of learned address are used
as a key of address completion. Otherwise, the user parts of the
learned address are used."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-use-unread-mark nil
  "*If non-nil, the unread mark is used in Summary mode."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-delete-unread-mark-by-mark t
  "*If nil, the unread mark remains when the cursor is moved
to the current message from the previous message by
putting any mark on the previous one. If non-nil,
the unread mark is deleted."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-use-smtp-auth t
  "*If nil, SMTP AUTH is ignored even an SMTP server
returns the AUTH capability."
  :group 'mew-smtp
  :type 'boolean)

(defcustom mew-smtp-auth-plain-authorize-id nil
  "*If nil, SMTP AUTH PLAIN is created as follow:
	NUL authenticate-id NUL password
If non-nil, it is created as follow:
	authorize-id NUL authenticate-id NUL password
For more information, see RFC 2595."
  :group 'mew-smtp
  :type 'boolean)

(defcustom mew-use-punycode t
  "*If non-nil, punycode strings are decoded into IDN strings"
  :group 'mew-message
  :type 'boolean)

(defcustom mew-use-async-write nil
  "*If non-nil, message retrieval of POP/IMAP/NNTP on Unix uses
asynchronous write for each message and fsync in their sentinel."
  :group 'mew-net
  :type 'boolean)

(defcustom mew-use-format-flowed nil
  "*If non-nil, Text/Plain; format=flowed is used when composing.
If you cite a message, the citation style of format=flowed is used. "
  :group 'mew-draft
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Appearance
;;;

(defcustom mew-window-configuration
  '((summary (1  0))
    (message (8 31))
    (draft   (1  0)))
  "*Ratio of windows"
  :group 'mew-env
  :type '(list
           (list (const summary) (list integer integer))
           (list (const message) (list integer integer))
           (list (const draft) (list integer integer))))

(defcustom mew-mode-line-id "Mew: %12b"
  "*A default value of mode-line-buffer-identification for each Mew mode."
  :group 'mew-summary
  :type 'sexp)

;; Indirect call of propertized-buffer-identification due to XEmacs
(defun mew-mode-line-id ()
  (propertized-buffer-identification mew-mode-line-id))

(defcustom mew-multipart-icon-position 'right
  "*Position where multipart icons are displayed.
If 'left, displayed at the left size of the default toolbar.
If 'right, displayed at the right size of the default toolbar.
Otherwise, not displayed."
  :group 'mew-summary
  :type 'symbol)

(defcustom mew-window-home-buffer "*scratch*"
  "*Buffer name to return if window stack is empty."
  :group 'mew-summary
  :type 'string)

(defcustom mew-window-magic 80
  "*Minimum window width to scan messages in Summary mode."
  :group 'mew-summary
  :type 'integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode
;;;

(defcustom mew-print-function 'lpr-buffer
  "*A function for printing"
  :group 'mew-summary
  :type 'symbol)

(defcustom mew-summary-show-direction 'next
  "*Direction for SPC at end of message.
'up 'down 'next(current direction) 'stop.
Other values are considered as 'stop.
See also 'mew-summary-mark-direction'."
  :group 'mew-summary
  :type '(choice (const next) (const up) (const down) (const stop)))

(defcustom mew-summary-mark-direction 'next
  "*Direction after marking a message
'up 'down 'next(current direction) 'stop.
Other values are considered as 'stop.
See also 'mew-summary-show-direction'."
  :group 'mew-summary
  :type '(choice (const next) (const up) (const down) (const stop)))

(defcustom mew-summary-show-pause nil
  "*Pause on going to next message if *non-nil*.
See also 'mew-summary-show-direction' and 'mew-summary-mark-direction'."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-summary-mark-duplicated-skip 'first
  "*This value decides which duplicated message should not be
marked with 'mew-mark-duplicated' when 'mew-summary-mark-duplicated'
is executed.

Possible value is 'first, 'last, or nil.
nil means to mark all duplicated messages.
Other values are considered as 'first."
  :group 'mew-summary
  :type '(choice (const first) (const last) (const nil)))

(defcustom mew-summary-recenter-p t
  "*If *non-nil*, the current message is recentered in Summary mode when
displayed."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-summary-scan-width nil
  "*If *non-nil*, used as the width of Summary mode."
  :group 'mew-summary
  :type '(choice integer (const nil)))

(defcustom mew-summary-goto-line-then-display t
  "*If non-nil 'mew-summary-goto-line' displays the message
where the cursor jumped."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-summary-jump-top-then-display t
  "*If non-nil 'mew-summary-jump-top' displays the top message
where the cursor jumped to the top."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-summary-jump-bottom-then-display t
  "*If non-nil 'mew-summary-jump-bottom' displays the bottom message
where the cursor jumped to the bottom."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-summary-region-include-cursor-line nil
  "*This value decides whether or not the cursor line is included
to a region."
  :group 'mew-summary
  :type '(choice (const :tag "Include if the cursor is not on the beginning of the line" nil)
		 (const :tag "Include if the cursor is on the end of the line" end)
		 (const :tag "Include always" t)))

(defvar mew-highlight-timer-interval 3)

(defcustom mew-syntax-treat-filename-function
  'mew-syntax-treat-filename-default-function
  "*A function to modify file names specified by Content-Disposition:"
  :group 'mew-summary
  :type 'symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message mode
;;;

(defcustom mew-end-of-message-string "[End of message]"
  "*A value inserted on the end of message buffer if *non-nil*."
  :group 'mew-message
  :type '(choice string (const nil)))

(defcustom mew-end-of-part-string "[Message is continued]"
  "*A value inserted on the end of message buffer if *non-nil*."
  :group 'mew-message
  :type '(choice string (const nil)))

(defcustom mew-break-pages t
  "*If *non-nil*, a message is broken by mew-page-delimiter."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-insert-final-newline t
  "*If *non-nil*, and if text/plain does not have final newline,
insert a newline at the end of buffer. "
  :group 'mew-message
  :type 'boolean)

(defcustom mew-field-other-visible t
  "*If *non-nil*, fields which are not found in 'mew-field-spec'
are displayed after visible fields. Otherwise they are
hidden before visible fields (and after invisible fields)."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-use-header-veil t
  "*If *non-nil*, field lines of To: and Cc:
over 'mew-header-veil-count' are covered with invisible veils."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-header-veil-string "..."
  "*A string to indicate some lines are covered with a veil."
  :group 'mew-message
  :type 'string)

(defcustom mew-header-veil-count 4
  "*The limit number for veil."
  :group 'mew-message
  :type 'integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draft mode
;;;

(defcustom mew-draft-mode-auto-save t
  "*If t, a draft is repeatedly saved to 'buffer-auto-save-file-name'
by 'do-auto-save'. If automatic saving is enabled and Emacs is crashed,
the '.save-' file remains. If this value is nil, automatic saving does
not work resulting that no garbage file remains."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-header-alist nil
  "*List of (key value) for header field to be inserted on draft.
'((\"X-fingerprint:\" \"6B 63 38 88 67 5E 96 8E  CE A4 62 73 3F 11 64 94\")
  (\"X-URL:\" \"http://www.example.org/\"))"
  :group 'mew-draft
  :type '(list (list string string)))

(defcustom mew-require-final-newline t
  "*If non-nil, Mew adds a new line to the draft if ended without a new
line."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-comment-start ">"
  "*The value to be set to comment-start in Draft mode."
  :group 'mew-draft
  :type 'string)

(defcustom mew-comment-start-skip "^> *"
  "*The value to be set to comment-start-skip in Draft mode."
  :group 'mew-draft
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draft mode
;;;

(defcustom mew-reply-to nil
  "*A value inserted into Reply-To: field in Draft mode if *non-nil*.
See also 'mew-config-alist'."
  :group 'mew-draft
  :type '(choice string (const nil)))

(defcustom mew-fcc "+backup"
  "*A value inserted into Fcc: field in Draft mode if *non-nil*.
See also 'mew-config-alist'."
  :group 'mew-draft
  :type '(choice string (const nil)))

(defcustom mew-cc nil
  "*A value inserted into Cc: field in Draft mode if *non-nil*.
See also 'mew-config-alist'."
  :group 'mew-draft
  :type '(choice string (const nil)))

(defcustom mew-bcc nil
  "*A value inserted into Bcc: field in Draft mode if *non-nil*.
See also 'mew-config-alist'."
  :group 'mew-draft
  :type '(choice string (const nil)))

(defcustom mew-dcc nil
  "*A value inserted into Dcc: field in Draft mode if *non-nil*.
If the value is 'me, your address is automatically inserted
according to the case.
See also 'mew-config-alist'."
  :group 'mew-draft
  :type '(choice string (const nil)))

(defcustom mew-from nil
  "*A value inserted into From: field in Draft mode if *non-nil*.
For backward-compatibility. See also 'mew-name', 'mew-user', and
'mew-mail-domain'."
  :group 'mew-draft
  :type '(choice string (const nil)))

(defcustom mew-organization nil
  "*A value inserted into Organization: field in Draft mode if *non-nil*.
See also 'mew-config-alist'."
  :group 'mew-draft
  :type '(choice string (const nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Citation
;;;

(defcustom mew-cite-prefix nil
  "*Prefix of citation."
  :group 'mew-cite
  :type '(choice string (const nil)))

(defcustom mew-cite-hook nil
  "*Hook for an external cite mechanism. If you want to use
super-cite, (setq mew-cite-hook 'sc-cite-original)."
  :group 'mew-cite
  :type 'hook)

(defcustom mew-cite-strings-function 'mew-cite-strings
  "*Function called from mew-cite-original which to create cite labels
according to 'mew-cite-format' and 'mew-cite-fields'."
  :group 'mew-cite
  :type '(choice function (const nil)))

(defcustom mew-before-cite-hook nil
  "Called in mew-summary-reply-with-citation before citation."
  :group 'mew-cite
  :type 'hook)

(defcustom mew-cite-prefix-function nil
  "*Function called on citations. A good candidate is
'mew-cite-prefix-username"
  :group 'mew-cite
  :type '(choice function (const nil)))

(defcustom mew-cite-prefix-confirmp nil
  "*If *non-nil*, citation prefix (such as 'kazu> ') is
confirmed to be used."
  :group 'mew-cite
  :type 'boolean)

(defcustom mew-cite-fields (list mew-from: mew-subj: mew-date:)
  "*The fields that you want to extract as citation label.
If you change this valuable, you must change mew-cite-format.
The value of the first field becomes the first argument for mew-cite-format.
\(e.g. The default first argument is a value of From: field.)
The value of the second field becomes the second argument for mew-cite-format.
....
If this is nil, label is not generated."
  :group 'mew-cite
  :type '(repeat string))

(defcustom mew-cite-format "From: %s\nSubject: %s\nDate: %s\n\n"
  "*Format for the citation label."
  :group 'mew-cite
  :type 'string)

(defcustom mew-draft-cite-fill-mode nil
  "*A method to format a citation header.
If 'wrap, format with fill-region.
If 'truncate, cut over fill-column and insert 'mew-draft-cite-ellipses'."
  :group 'mew-draft
  :type '(choice (symbol :tag "Wrap" wrap)
                 (symbol :tag "Truncate" truncate)
                 (symbol :tag "Asis" nil)))

(defcustom mew-draft-cite-ellipses " .."
  "String that is inserted a truncated line when
'mew-draft-cite-fill-mode' is 'truncate."
  :group 'mew-cite
  :type 'string)

(defcustom mew-draft-cite-label-fill-column nil
  "If *non-nil*, this value is used for 'mew-draft-cite-fill-mode'
instead of 'fill-column'."
  :group 'mew-cite
  :type '(choice integer (const nil)))

(defcustom mew-summary-reply-position 'body
  "If 'body, the cursor locates in the beginning of the body.
Otherwise, the cursor is after To:."
  :group 'mew-cite
  :type 'symbol)

(defcustom mew-summary-reply-with-citation-position 'end
  "If 'body, the cursor locates in the beginning of the body.
If 'end, the cursor locates after the citation.
Otherwise, the cursor is after To:."
  :group 'mew-cite
  :type 'symbol)

(defcustom mew-cite-ignore-region nil
  "*If *non-nil*, the region of a draft is ignored when citing."
  :group 'mew-cite
  :type 'boolean)

(defcustom mew-cite-ignore-mouse-region t
  "*If *non-nil*, a region specified by mouse is ignored when citing."
  :group 'mew-cite
  :type 'boolean)

(defcustom mew-visit-queue-after-sending nil
  "*If *non-nil*, visit to +queue after '\\<mew-draft-mode-map>\\[mew-draft-make-message]' in Draft mode."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-visit-inbox-after-setting-case nil
  "*If *non-nil*, visit to an appropriate inbox after setting a case with '\\<mew-summary-mode-map>\\[mew-summary-set-case]'."
  :group 'mew-summary
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Charset
;;;

(defcustom mew-charset-input-method-alist nil
  "*Alist of charset and input-method. When a message is cited
into Draft mode the charset of the message is found in this
variable, the corresponding input-method is automatically
selected. An example configuration is as follows:

\(setq mew-charset-input-method-alist
      '((\"iso-8859-1\" \"latin-1-postfix\")
	(\"iso-8859-2\" \"latin-2-postfix\")))
"
  :group 'mew-draft
  :type '(list (list string string)))

(defun mew-charset-to-input-method (charset)
  (if (stringp charset)
      (mew-alist-get-value
       (mew-assoc-case-equal
	charset mew-charset-input-method-alist 0))))

(defcustom mew-charset-m17n "utf-8"
  "*A charset to be used if multiple character set are found and
an appropriate charset cannot be chosen. Possible candidates are
\"utf-8\" and \"iso-2022-jp-2\"."
  :group 'mew-draft
  :type '(choice (const "utf-8") (const "iso-2022-jp-2")))

(defcustom mew-charset-latin "iso-8859-15"
  "*This variable is used for selection of charset when composing.
If characters of both Latin 1 and Lain 9 exist in a draft, Mew takes
the following step to decide a charset.

1. If 'unify-8859-on-decoding-mode' is used:
	1.1. Use \"iso-8859-1\" if no loss.
	1.2. Otherwise, use \"utf-8\".
2. If 'unify-8859-on-decoding-mode' is not used:

	2.1. If both ISO-8859-1 and ISO-8859-15 can be used with no
             loss, 'mew-charset-latin' is used.
	2.2. Use \"iso-8859-1\" if no loss.
	2.3. Use \"iso-8859-15\" if no loss.
	2.3. Otherwise, use \"utf-8\".
"
  :group 'mew-draft
  :type '(choice (const "iso-8859-1") (const "iso-8859-15")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Signature
;;;

(defcustom mew-signature-file "~/.signature"
  "*A signature file to be inserted in Draft mode. To support multiple
signature files, use 'c-sig.el'."
  :group 'mew-draft
  :type 'file)

(defcustom mew-signature-insert-last nil
  "*If *non-nil*, the signature file is inserted in the last of body.
Otherwise, it is inserted at the current point. If you created multipart
and mew-signature-as-lastpart is *non-nil*, this variable is ignored."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-signature-as-lastpart nil
  "*If *non-nil*, the signature file is appended as the final part
when you create multipart."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-signature-description "My signature"
  "*This variable is used as a description if the signature is appended
as the final part."
  :group 'mew-draft
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Samba
;;;

(defcustom mew-use-samba-encoding nil
  "*Use SAMBA encoding for non-ASCII file name when saving."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-use-samba-encoding-type 'cap
  "*A type of SAMBA encoding. Either 'cap or 'hex."
  :group 'mew-summary
  :type '(choice (const cap) (const hex)))

(defcustom mew-cs-samba 'shift_jis
  "*A character set before SAMBA encoding."
  :group 'mew-summary
  :type 'coding-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Attachments
;;;

(defcustom mew-attach-move-by-line nil
  "*If non-nil, 'mew-attach-{next,previous}' move the cursor line by line."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-attach-move-next-after-copy nil
  "If non-nil, the cursor moves the next position after copying/linking
in Attachments."
  :group 'mew-draft
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion
;;;

(defcustom mew-fields
  (list mew-from: mew-to: mew-cc: mew-dcc: mew-fcc: mew-bcc: mew-subj:
	mew-reply-to: mew-followup-to: mew-newsgroups: mew-distribution:
	mew-resent-from: mew-resent-to: mew-resent-cc:
	mew-resent-dcc: mew-resent-fcc: mew-resent-bcc:)
  "*Completion field list in Draft mode."
  :group 'mew-complete
  :type '(repeat string))

(defcustom mew-complete-case-ignore-case nil
  "*Non-nil means Mew does not consider case is significant in case
completion."
  :group 'mew-complete
  :type 'boolean)

(defcustom mew-complete-folder-ignore-case nil
  "*Non-nil means Mew does not consider case is significant in folder
completion."
  :group 'mew-complete
  :type 'boolean)

(defcustom mew-complete-address-ignore-case t
  "*Non-nil means Mew does not consider case is significant in address
completion."
  :group 'mew-complete
  :type 'boolean)

(defcustom mew-circular-complete-domain-ignore-case t
  "*Non-nil means Mew does not consider case is significant in
circular domain completion."
  :group 'mew-complete
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Field order
;;;

(defcustom mew-field-order-for-editing
  (list mew-to: mew-newsgroups: mew-cc: mew-subj: mew-from:)
  "*A field order list for editing."
  :group 'mew-draft
  :type '(repeat string))

(defcustom mew-field-order-for-reediting
  (list mew-to: mew-newsgroups: mew-cc: mew-subj: mew-from:)
  "*A field order list for reediting."
  :group 'mew-draft
  :type '(repeat string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fields to be deleted
;;;

(defcustom mew-field-delete-common
  '("From " ">From" "X-Mew")
  "*A field list to be deleted for reediting/resending/forwarding/saving."
  :group 'mew-draft
  :type '(choice (const nil)
		 (repeat :tag "Field list" (string :tag "Field"))))

(defvar mew-field-delete nil) ;; backward compatibility

(defcustom mew-field-delete-for-editing
  (list mew-received: "Return-Path:" "Delivery-Date:" "Delivered-To:"
	mew-message-id: mew-mv: "Content-"
	"Precedence:" mew-organization: "X-"
	"Lines:" "Status:" "Posted:" "Forwarded:" "Replied:" "X-UIDL:"
	"DomainKey-Signature:" "DKIM-Signature:"
	"Authentication-Results:" "List-.*:" "Errors-To:")
  "*A field list to be deleted in Edit mode."
  :group 'mew-draft
  :type '(choice (const nil)
                 (repeat :tag "Field list" (string :tag "Field"))))

(defcustom mew-field-delete-for-reediting
  (or mew-field-delete
      (list mew-received: "Return-Path:" "Delivery-Date:" "Delivered-To:"
	    mew-date: mew-message-id: mew-mv: "Content-"
	    "Precedence:" mew-organization: "X-"
	    "Lines:" "Status:" "Posted:" "Forwarded:" "Replied:" "X-UIDL:"
	    "DomainKey-Signature:" "DKIM-Signature:"
	    "Authentication-Results:" "List-.*:" "Errors-To:" "Sender:"))
  "*A field list to be deleted when edit again."
  :group 'mew-draft
  :type '(choice (const nil)
		 (repeat :tag "Field list" (string :tag "Field"))))

(defcustom mew-field-delete-for-others
  (list mew-received: "Return-Path:" "Delivery-Date:" "Delivered-To:"
	mew-to: mew-cc: mew-from: mew-organization:
	"Lines:" "Status:" "Posted:" "Forwarded:" "Replied:" "X-UIDL:"
	"DomainKey-Signature:" "DKIM-Signature:")
  "*A field list to be deleted when send with different To: and Cc:."
  :group 'mew-draft
  :type '(choice (const nil)
		 (repeat :tag "Field list" (string :tag "Field"))))

(defcustom mew-field-delete-for-resending
  (list mew-received: "Return-Path:" "Delivery-Date:" "Delivered-To:"
	"Resent-" "X-Resent-"
	"Lines:" "Status:" "Posted:" "Forwarded:" "Replied:" "X-UIDL:")
  "*A field list to be deleted when resending."
  :group 'mew-draft
  :type '(choice (const nil)
		 (repeat :tag "Field list" (string :tag "Field"))))

(defcustom mew-field-delete-for-forwarding
  '("Lines:" "Status:" "Posted:" "Forwarded:" "Replied:" "X-UIDL:")
  "*A field list to be deleted when forwarding."
  :group 'mew-draft
  :type '(choice (const nil)
		 (repeat :tag "Field list" (string :tag "Field"))))

(defcustom mew-field-delete-for-saving ()
  "*A field list to be deleted when saving a message to a file."
  :group 'mew-summary
  :type '(choice (const nil)
		 (repeat :tag "Field list" (string :tag "Field"))))

(defcustom mew-field-delete-for-joining
  (list mew-mv: mew-subj: mew-message-id: "Encrypted" "Content-")
  "*A field list to be deleted when joining."
  :group 'mew-summary
  :type '(repeat :tag "Field list" (string :tag "Field")))

(defcustom mew-field-for-printing
  '("Subject:" "From:" "To:" "Cc:" "Date:")
  "*A field list to print"
  :group 'mew-summary
  :type '(repeat :tag "Field list" (string :tag "Field")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Directory stuff
;;;

(defcustom mew-summary-trace-directory t
  "*If non-nil, change to the folder directory when
mew-summary-goto-folder."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-save-dir mew-home
  "*The default directory to save messages or parts in Summary mode.
See also 'mew-copy-dir'."
  :group 'mew-summary
  :type 'directory)

(defcustom mew-summary-preserve-dir nil
  "*If non-nil, the previous directory is used as the default
directory for save, etc. See also 'mew-draft-preserve-dir'."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-copy-dir mew-home
  "*The default directory from which attachments are copied in Draft mode.
See also 'mew-save-dir'."
  :group 'mew-draft
  :type 'directory)

(defcustom mew-draft-preserve-dir nil
  "*If non-nil, the previous directory is used as the default
directory for copy, etc. See also 'mew-summary-preserve-dir'."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-file-append-p nil
  "*If *non-nil*, a message or a part is appended to the existing file
on '\\<mew-summary-mode-map>\\[mew-summary-save]'. Otherwise overwrote."
  :group 'mew-summary
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folders
;;;

(defcustom mew-regex-folder-candidate "^[^.#0-9]\\|^[0-9].*[^0-9]"
  "*Regular expression used in 'mew-dir-list-with-link-count'
and 'mew-dir-list-without-link-count' to list up folder
candidates. The default value \"^[^.#0-9]\\\\|^[0-9].*[^0-9]\" lists up
folders whose name is alphabet only."
  :group 'mew-env
  :type 'regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refile
;;;

(defconst mew-custom-type-of-guess-alist
  '(list (list string (list string string))))

(defcustom mew-refile-guess-alist nil
  "*If non-nil, mew guesses destination folder by using this hint.
The format is like this:

    (setq mew-refile-guess-alist
          '((\"To:\"
              (\"wide@wide\" \"+wide/wide\")
              (\"adam\"      \"+labo/adam\"))
            (\"Newsgroups:\"
              (\"^nifty\\\\.\\\\([^ ]+\\\\)\" \"+Nifty/\\\\1\"))
            (\"From:\"
              (\"uucp\" \"+adm/uucp\")
              (\".*\"   \"+misc\"))
            ))
"
  :group 'mew-refile
  :type mew-custom-type-of-guess-alist)


(defcustom mew-refile-ctrl-multi t
  "*If *non-nil*, guess functions guess multi folders."
  :group 'mew-refile
  :type 'boolean)

(defcustom mew-refile-guess-key-list
  (append mew-resent-dest:-list mew-destination:-list)
  "*A list of field key used by mew-refile-guess-by-folder."
  :group 'mew-refile
  :type '(repeat string))

(defvar mew-refile-guess-control
  '(mew-refile-guess-by-alist
    mew-refile-ctrl-throw
    mew-refile-guess-by-newsgroups
    mew-refile-guess-by-folder
    mew-refile-ctrl-throw
    mew-refile-ctrl-auto-boundary
    mew-refile-guess-by-thread
    mew-refile-ctrl-throw
    mew-refile-guess-by-from-folder
    mew-refile-ctrl-throw
    mew-refile-guess-by-from
    mew-refile-ctrl-throw
    mew-refile-guess-by-default))

(defcustom mew-refile-auto-refile-skip-any-mark nil
  "*If *non-nil*, 'mew-summary-auto-refile' does not touch
any already marked message."
  :group 'mew-refile
  :type 'boolean)

(defcustom mew-refile-auto-refile-confirm nil
  "*If *non-nil*, 'mew-summary-auto-refile' prompts the user for
confirmation before refiling."
  :group 'mew-refile
  :type 'boolean)

(defcustom mew-refile-guess-strip-domainpart t
  "*If *non-nil*, 'mew-refile-guess-by-default' strips domainpart of from"
  :group 'mew-refile
  :type 'boolean)

(defcustom mew-refile-guess-from-me-is-special nil
  "*If *non-nil*, 'mew-refile-guess-by-from-*' think the messages from
yourself as special. They use To: or Cc: instead of From:"
  :group 'mew-refile
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Case guess
;;;

(defcustom mew-case-guess-alist nil
  "*If *non-nil*, this value is used to guess cases when
prepared or composed.
The syntax is exactly the same as 'mew-refile-guess-alist'."
  :group 'mew-draft
  :type mew-custom-type-of-guess-alist)

(defcustom mew-case-guess-when-replied-alist nil
  "*If *non-nil*, this value is used to guess cases when replied.
The syntax is exactly the same as 'mew-refile-guess-alist'."
  :group 'mew-draft
  :type mew-custom-type-of-guess-alist)

(defcustom mew-case-guess-when-prepared t
  "*If *non-nil*, case is guessed when a draft is prepared."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-case-guess-when-composed nil
  "*If *non-nil*, case is guessed when a message is composed."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-case-guess-when-replied t
  "*If *non-nil*, case is guessed when replying to a message."
  :group 'mew-draft
  :type 'boolean)

(defcustom mew-case-guess-addition nil
  "*If *non-nil*, new guessed cases are added to old cases.
Otherwise, the old cases are overridden."
  :group 'mew-draft
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Temporary files
;;;

(defcustom mew-temp-file-initial
  (expand-file-name (user-login-name) (if (fboundp 'temp-directory)
					  (temp-directory)
					(if (boundp 'temporary-file-directory)
					    temporary-file-directory
					  "/tmp")))
  "*Hint to make a secure directory on the local file system. On
setup phase Mew make a secure directory from this variable and set
mew-temp-file a file name prefix contained the directory name. The
directory must be unreadable from others, otherwise it might become a
big security hole. And this directory must not be gained access
through network to prevent tire-tapping. Mew never uses
'call-process-region' rather does use 'call-process' creating a
temporary file with mew-temp-file by itself. If 'call-process-region'
is used, Emacs creates a temporary file (probably in /tmp). So bad
guys can wiretap the temporary file."
  :group 'mew-privacy
  :type 'directory)

(defcustom mew-delete-temp-file t
  "*If *non-nil*, delete temporary files when external commands terminate."
  :group 'mew-summary
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auto
;;;

(defcustom mew-auto-get t
  "If *non-nil*, Mew gets messages from your mailbox automatically.
'\\[universal-argument] \\[mew]' equals to '\\[mew]' with 'mew-auto-get'
reversed."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-auto-flush-queue t
  "If *non-nil* and if there are queued messages in +queue,
they are flushed after getting message (i.e. '\\<mew-summary-mode-map>\\[mew-summary-retrieve]'). This idea saves money in dial up environment."
  :group 'mew-draft
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick and Sort
;;;

(defcustom mew-pick-pattern-list nil
  "*A list of pick pattern which can be circular completed with '\\<mew-input-map>\\[mew-circular-complete-switch]'
The first member is displayed as a default value."
  :group 'mew-summary
  :type '(choice (const nil) (repeat string)))

(defcustom mew-pick-field-list
  '("head=" "to=" "cc=" "subject=" "dcc=" "fcc=" "bcc=" "date="
    "reply-to=" "followup-to=" "from=" "newsgroups=")
  "*A list of key for pick pattern."
  :group 'mew-summary
  :type '(choice (const nil) (repeat string)))

(defcustom mew-sort-default-key "date"
  "*Default sort key when inputting a sort key.
Its format is key:mode where more is found in 'mew-sort-mode'."
  :group 'mew-summary
  :type 'string)

(defcustom mew-sort-default-key-alist nil
  "*List of (folder sort-key) to decide a default sort-key
of a specific folder. An example is follows:
\(setq mew-sort-default-key-alist
      '((\"+tmp/beginners\" \"x-sequence\")
        (\"+tmp/elips\" \"x-mail-count\")))"
  :group 'mew-summary
  :type '(list (list string string)))

(defcustom mew-sort-key-alist
  '(("date" "date") ("subject") ("from") ("to") ("newsgroups")
    ("posted" "date") ("x-sequence" "postnum")
    ("x-mail-count" "num") ("x-ml-count" "num"))
  "*List of fields for 'mew-summary-sort'.
Each element is (FIELD-NAME) or (FIELD-NAME . MODE).
MODE is one of \"date\" (by chronological order) or
               \"num\"  (by numerical order) or
               \"postnum\" (by numerical order of postfix numeric) or
               \"text\" (by alphabetical order) or
               \"ml\"   (by alphabetical order with ml prefix removed) or
               \"mlnum\" (by numerical order of ml number).
\(nil means \"text\")."
  :group 'mew-summary
  :type 'sexp)

(defvar mew-sort-modes '("date" "num" "postnum" "text" "ml" "mlnum"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Range
;;;

(defvar mew-range-all "0")

(defvar mew-range-str-all    "all")
(defvar mew-range-str-update "update")
(defvar mew-range-str-last   "last:")
(defvar mew-range-str-sync   "sync")

(defcustom mew-input-range-list
  (list mew-range-str-update mew-range-str-all mew-range-str-last)
  "*A list used by range completion."
  :group 'mew-complete
  :type '(repeat string))

(defcustom mew-input-range-remote-list
  (list mew-range-str-update mew-range-str-all mew-range-str-last mew-range-str-sync)
  "*A list used by range completion."
  :group 'mew-complete
  :type '(repeat string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight
;;;

(defvar mew-summary-cook-function 'mew-summary-cook-region)

(defcustom mew-use-highlight-mark t
  "*If non-nil, highlight marked lines in Summary/Virtual mode."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-use-highlight-header t
  "*If non-nil, highlight a header in Message/Draft mode."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-use-highlight-body t
  "*If non-nil, highlight a body in Message/Draft mode."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-use-highlight-url t
  "*If non-nil, highlight URLs in Message/Draft mode."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-use-cursor-mark nil
  "*If *non-nil*, show 'mew-cursor-mark' in the beginning of the
cursor line. This is convenient if 'mew-use-highlight-cursor-line' is
not available."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-use-highlight-cursor-line t
  "*Put the 'mew-highlight-cursor-line-face' face on the current line
in Summary/Virtual mode."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-use-highlight-mouse-line nil
  "*Put the 'mew-highlight-mouse-line-face' face on the mouse location
in Summary/Virtual mode."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-use-highlight-x-face mew-icon-p
  "*Iconify X-Face: in Message mode."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-use-highlight-x-face-inversion nil
  "*Invert an X-Face: image in Message mode."
  :group 'mew-highlight
  :type 'boolean)

;;;
;;; Styles and colors
;;;

(defcustom mew-cursor-mark ">"
  "*The mark in the beginning of the cursor line if
'mew-use-cursor-mark' is *non-nil*."
  :group 'mew-highlight
  :type '(choice string (const nil)))

(defcustom mew-highlight-cursor-line-face 'underline
  "*Face to highlight the cursor line in Summary/Virtual mode"
  :group 'mew-highlight
  :type 'face)

(defcustom mew-highlight-mouse-line-face 'highlight
  "*Mouse face to highlight URL in Message/Draft mode"
  :group 'mew-highlight
  :type 'face)

(defcustom mew-regex-url
  (concat
   "\\b\\("
   "\\(\\(file\\|news\\|mailto\\):\\)"
   "\\|"
   "\\(\\(s?https?\\|ftp\\|gopher\\|telnet\\|wais\\)://\\)"
   "\\)"
   "[^ \t\n>)\"]*"
   "[^] \t\n>.,:)\"]")
  "*Regular expression to find URL."
  :group 'mew-highlight
  :type 'regexp)

(defcustom mew-highlight-url-mouse-face 'highlight
  "*Mouse face to highlight URL in Message/Draft mode"
  :group 'mew-highlight
  :type 'face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addrbook
;;;

(defcustom mew-use-auto-alias t
  "*If non-nil, addresses on the To: and Cc: field in Draft mode
will be automatically leaned as alias."
  :group 'mew-addrbook
  :type 'boolean)

(defcustom mew-addrbook-comment-regex "^;.*$\\|#.*$"
  "*Regular expression for Addrbook."
  :group 'mew-addrbook
  :type 'regexp)

(defcustom mew-addrbook-append-domain-p t
  "If non-nil, addresses, which do not have domain part in a header,
will be appended (mew-mail-domain) when composing."
  :group 'mew-addrbook
  :type 'boolean)

(defcustom mew-addrbook-override-by-newone t
  "If non-nil, the 'user' entry in 'mew-alias-auto-alist'
is override by a new entry of (user different-address).
This means that addresses in To: and Cc: in Draft mode are
always learned with an exception 'user' is defined in Addrbook.
If nil,  the old 'user' entry remains."
  :group 'mew-addrbook
  :type 'boolean)

(defcustom mew-addrbook-for-cite-label nil
  "*How to replace the From: value in cite label
with Addrbook. See 'mew-addrbook-switch'."
  :group 'mew-addrbook
  :type 'symbol)

(defcustom mew-addrbook-for-cite-prefix 'username
  "*How to replace the From: value in cite prefix
with Addrbook. See 'mew-addrbook-switch'."
  :group 'mew-addrbook
  :type 'symbol)

(defcustom mew-addrbook-for-address-expansion 'name
  "*How to replace an address in address fields
with Addrbook. See 'mew-addrbook-switch'."
  :group 'mew-addrbook
  :type 'symbol)

(defcustom mew-addrbook-for-summary 'nickname
  "*How to replace a From: address in summary-form with Addrbook.
See 'mew-addrbook-switch' and 'mew-summary-form-extract-rule'."
  :group 'mew-addrbook
  :type 'symbol)

(defcustom mew-addrbook-strip-domainpart t
  "*If *non-nil*, a shortname is created by stripping its domain part
when '\\<mew-summary-mode-map>\\[mew-summary-addrbook-add]' in Summary mode."
  :group 'mew-addrbook
  :type 'boolean)

(defcustom mew-addrbook-alias-not-learn-list nil
  "*A list of a part of address. If an address matches to one out of the
list. the address is not registered to Addrbook."
  :group 'mew-addrbook
  :type '(choice (const nil) (repeat string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cache
;;;

(defcustom mew-cache-size 10
  "*Number of buffer for message cache."
  :group 'mew-cache
  :type 'integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Privacy
;;;

(defcustom mew-draft-privacy-method 'pgp
  "Privacy method to create messages in Draft mode.
You should choose 'pgp or 'smime."
  :group 'mew-privacy
  :type '(choice (const pgp) (const smime)))

(defcustom mew-protect-privacy-always nil
  "*If non-nil, a draft is to be protected according to
'mew-protect-privacy-always-type'."
  :group 'mew-privacy
  :type 'boolean)

(defcustom mew-protect-privacy-always-type 'pgp-signature
  "*A type of privacy protection for all drafts.
Currently, 'pgp-signature, 'pgp-encryption, 'pgp-signature-encryption,
'pgp-encryption-signature, and nil are available. Since signature
does not require receiver's public key, signature service may be
appropriate for this value."
  :group 'mew-privacy
  :type '(choice (const pgp-signature)
                 (const pgp-encryption)
                 (const pgp-signature-encryption)
		 (const pgp-encryption-signature)
		 (const smime-signature)
                 (const smime-encryption)
                 (const smime-signature-encryption)
		 (const smime-encryption-signature)
                 (other :tag "nil" nil)))

(defcustom mew-protect-privacy-encrypted nil
  "*If non-nil, a draft replying a encrypted message is to be protected
according to 'mew-protect-privacy-encrypted-type'."
  :group 'mew-privacy
  :type 'boolean)

(defcustom mew-protect-privacy-encrypted-type 'pgp-encryption
  "*A type of privacy protection for drafts replying encrypted
messages. Currently, 'pgp-signature, 'pgp-encryption,
'pgp-signature-encryption, 'pgp-encryption-signature,
and nil are available."
  :group 'mew-privacy
  :type '(choice (const pgp-signature)
                 (const pgp-encryption)
                 (const pgp-signature-encryption)
		 (const pgp-encryption-signature)
		 (const smime-signature)
		 (const smime-encryption)
		 (const smime-signature-encryption)
		 (const smime-encryption-signature)
                 (other :tag "nil" nil)))

(defcustom mew-protect-privacy-with-old-pgp-signature nil
  "*If non-nil and 'mew-use-old-pgp' is non-nil,
the old fashioned PGP message are used when composing."
  :group 'mew-privacy
  :type 'boolean)

(defcustom mew-passwd-timer-unit 10
  "*Minutes of timer unit to cancel the cached passwords."
  :group 'mew-privacy
  :type 'integer)

(defcustom mew-passwd-lifetime 2
  "*Number of 'mew-passwd-timer-unit' to cancel the cached passwords."
  :group 'mew-privacy
  :type 'integer)

(defcustom mew-passwd-reset-timer t
  "*If non-nil, put the lifetime of a cached password longer when
the password is used."
  :group 'mew-privacy
  :type 'boolean)

(defcustom mew-encrypt-to-myself t
  "*if non-nil, any message/part to be encrypted is encrypted with
your public key as well as receivers' one."
  :group 'mew-privacy
  :type 'boolean)

(defcustom mew-pgp-signer nil
  "*ID of a PGP key to use for sign"
  :group 'mew-privacy
  :type 'string)

(defcustom mew-smime-signer nil
  "*ID of a S/MIME key to use for sign"
  :group 'mew-privacy
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PGP Public key fetch
;;;

(defvar mew-x-pgp-key-list
  '("x-pgp-key:" "x-pgp-key-url:" "x-pgp-public-key:" "x-pgp-public-key-url:"
    "x-pgp5-key:" "x-pgp5-key-url:" "x-pgp5-public-key:"
    "x-pgp5-public-key-url:" "x-public-key:"))

(defvar mew-pgp-pks-port 11371)
(defvar mew-pgp-pks-lang "/pks/lookup?op=get&search=")

;; Tue Dec 16 16:28:11 JST 2003
;; http://pgp.zdv.uni-mainz.de/keyserver/bigbrother/

(defvar mew-pgp-pks-servers
  '(("openpksd.org" t)
    ("blackhole.pca.dfn.de" nil)
    ("pgp.ael.be" nil)
    ("pgp.dtype.org" nil)
    ("pgp.mit.edu" nil)
    ("pgp.nic.ad.jp" nil)
    ("pgp.rediris.es" nil)
    ("pgp.uni-mainz.de" nil)
    ("pgp.uni-paderborn.de" nil)
    ("wwwkeys.1.us.pgp.net" nil)
    ("wwwkeys.2.us.pgp.net" nil)
    ("wwwkeys.3.us.pgp.net" nil)
    ("wwwkeys.ch.pgp.net" nil)
    ("wwwkeys.cz.pgp.net" nil)
    ("wwwkeys.es.pgp.net" nil)
    ("wwwkeys.uk.pgp.net" nil)))

(defun mew-pgp-pks-http-port-p (server alist)
  (nth 1 (assoc server alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSH
;;;

(defvar mew-ssh-number-of-password-prompts 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub-programs
;;;

(defvar mew-prog-mewl        "mewl")
(defvar mew-prog-mime-encode "mewencode")
(defvar mew-prog-mime-decode "mewencode")
(defvar mew-prog-8bit        "mewencode")
(defvar mew-prog-unshar      "unshar")
(defvar mew-prog-tar         "tar")
(defvar mew-prog-compress    "compress")
(defvar mew-prog-gzip        "gzip")
(defvar mew-prog-zip         "zip")
(defcustom mew-prog-pgp      "gpg"
  "*PGP name for version check."
  :group 'mew-privacy
  :type 'string)

(defvar mew-prog-pgpkey      "wget")
(defvar mew-prog-pgpkey-args '("-q" "-O" "-"))

(defvar mew-ssh-prog          "ssh")
(defvar mew-ssh-prog-args     nil)

(defvar mew-prog-uncompface  "uncompface")
(defvar mew-prog-icontopbm   "icontopbm")
(defvar mew-prog-pbmtoxbm    "pbmtoxbm")
(defvar mew-prog-pbminvert   "pnminvert")

(defvar mew-prog-grep "grep")
(defvar mew-prog-grep-opts '("-l" "-e"))

(defvar mew-prog-grep-max-msgs 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X face
;;;

(defcustom mew-x-face-file "~/.xface"
  "*If *non-nil* and the file exists, X-Face: fields is inserted."
  :group 'mew-summary
  :type '(choice file (const nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; spam and ham
;;;

(defcustom mew-spam-prog "bogofilter"
  "*A command which learns spams."
  :group 'mew-summary
  :type 'string)

(defcustom mew-spam-prog-args '("-s" "-N" "-v")
  "*A list of command arguments which learns spams."
  :group 'mew-summary
  :type '(choice (const nil) (repeat string)))

(defcustom mew-ham-prog "bogofilter"
  "*A command which learns hams."
  :group 'mew-summary
  :type 'string)

(defcustom mew-ham-prog-args '("-n" "-S" "-v")
  "*A list of command arguments which learns hams."
  :group 'mew-summary
  :type '(choice (const nil) (repeat string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mode names
;;;

(defvar mew-mode-name-header  "Header")
(defvar mew-mode-name-draft   "Draft")
(defvar mew-mode-name-message "Message")
(defvar mew-mode-name-summary "Summary")
(defvar mew-mode-name-virtual "Virtual")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Types
;;;


(defvar mew-file-type "MewX")
(defvar mew-file-creator "EMAx")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For broken MUAs
;;;

(defcustom mew-decode-broken t
  "If non-nil, Mew decode broken messages.

\(1) Decode quoted encoded-words, which violates RFC 2047.

Good example:
	From: Kazu Yamamoto (=?ISO-2022-JP?B?GyRCOzNLXE9CSScbKEI=?=)
		<kazu@example.org>
Bad example:
	From: Kazu Yamamoto (\"=?ISO-2022-JP?B?GyRCOzNLXE9CSScbKEI=?=\")
		<kazu@example.org>

\(2) Decode raw non-ASCII text, which violates RFC 822.
This is typically found in Subject: field.

Good example:
	Subject: =?iso-2022-jp?B?GyRCJEYkOSRIGyhC?=
Bad example:
	Subject: Raw non-ASCII text.

\(3) Decode invalid MIME parameters, which violates RFC 2231.
\(i)  Raw text.
\(ii) Encorded-word defined in RFC 2047.

Good example:
	Content-Disposition: attachment;
	 filename*=iso-2022-jp''%1B%24B%24F%249%24H%1B%28B

Bad example
	Content-Disposition: attachment;
	 filename=\"=?iso-2022-jp?B?GyRCJEYkOSRIGyhC?=\"

=(4) Decode invalid non-ASCII MIME body, whose charset is US-ASCII or
not present.

Good example:
	Content-Type: text/plain; charset=ISO-2022-jp

	ISO-2022-JP text

Bad example:
	Content-Type: text/plain

	ISO-2022-JP text

\(5) Decode invalid non-ASCII text, whose charset is UTF-8 or not present.

Good example:
	Content-Type: text/plain; charset=ISO-8859-15

	ISO-8859-15 text

Bad example:
	Content-Type: text/plain; charset=UTF-8

	ISO-8859-15 text
"
  :group 'mew-message
  :type 'boolean)

(defcustom mew-use-name-parameter t
  "*If non-nil, the invalid NAME parameter of Content-Type: is checked
as a file name in addition to the legal FILENAME parameter of
Content-Disposition:."
  :group 'mew-message
  :type 'boolean)

(defcustom mew-broken-parameter-list '("name" "filename")
  "*A list of parameter names whose value contains non-ASCII strings
produced by broken MUAs."
  :group 'mew-message
  :type '(repeat string))

(defvar mew-decode-ws-fields '("Subject:"))

(defvar mew-no-warning-fields ()) ;; capitalized

(defvar mew-no-warning-params ()) ;; downcased

(defcustom mew-warning-field-level 1
  "*If warning level of a field is greater than or equal to this value,
the warning is displayed on X-Mew:.
Level 1: syntax is correct, but not recommenced.
Level 2: syntax error."
  :group 'mew-message
  :type 'integer)

(defcustom mew-encode-singlepart-hook nil
  "*Hook called before making a single part."
  :group 'mew-draft
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Preventing warning messages when byte-compiling
;;;

(defvar mew-regex-message-files  nil)
(defvar mew-regex-message-files2 nil)
(defvar mew-regex-message-files3 nil)
(defvar mew-regex-message-files4 nil)
(defvar mew-regex-message-files5 nil)

(defvar mew-basic-folders nil)

(provide 'mew-vars)

;;; Copyright Notice:

;; Copyright (C) 1996-2014 Mew developing team.
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
