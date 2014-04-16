;;; mew-config.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 14, 1999

;;; Code:

(require 'mew)

(defun mew-case-default-p (case)
  (or (null case) (string= case mew-case-default)))

(defvar mew-generate-mail-address-list nil)
(defvar mew-generate-mail-domain-list nil)
(defvar mew-generate-from-list nil)

;;
;;
;;

(defun mew-cfent-value (case key def &optional type)
  (let ((cases (and case (mapcar 'mew-chop (mew-split case ?,))))
	ent ret cfent)
    (or (member mew-case-default cases)
	(setq cases (cons mew-case-default cases)))
    (setq cases (nreverse cases))
    (catch 'loop
      (dolist (cs cases)
	(setq cfent (mew-cfent-by-case cs))
	(setq ent (or (assoc key cfent) ;; old
		      (assoc (intern key) cfent))) ;; new
	(when ent
	  (setq ret (cdr ent)) ;; old
	  (cond
	   ((eq type 'symbol)
	    (if (consp ret) (setq ret (car ret)))
	    (setq ret (symbol-value ret)))
	   ((eq type 'list)
	    (if (and (consp (car ret)) (null (cdr ret))) ;; new
		(setq ret (car ret)))
	    (if (equal ret '(nil))
		(setq ret nil)))
	   (t
	    (if (consp ret) (setq ret (car ret))))) ;; new
	  (if ent (throw 'loop nil))))
      (setq ret def))
    ret))

(defvar mew-config-cases nil)
(defvar mew-config-cases2 nil)
(defvar mew-inbox-folders nil)
(defvar mew-queue-folders nil)
(defvar mew-postq-folders nil)

(defun mew-config-init ()
  (if (null mew-mail-address-list) (setq mew-generate-mail-address-list t))
  (if (null mew-mail-domain-list) (setq mew-generate-mail-domain-list t))
  (if (null mew-from-list) (setq mew-generate-from-list t)))

(defun mew-config-case-name (ent)
  (let ((case (car ent)))
    (if (symbolp case)
	(symbol-name case)
      case)))

(defun mew-config-setup ()
  (if mew-generate-mail-address-list (setq mew-mail-address-list nil))
  (if mew-generate-mail-domain-list (setq mew-mail-domain-list nil))
  (if mew-generate-from-list (setq mew-from-list nil))
  (setq mew-inbox-folders nil)
  (setq mew-queue-folders nil)
  (setq mew-postq-folders nil)
  (setq mew-config-cases (mapcar 'mew-config-case-name mew-config-alist))
  (or (member mew-case-default mew-config-cases)
      (setq mew-config-cases (cons mew-case-default mew-config-cases)))
  (setq mew-config-cases2
	(append mew-folder-prefixes
		(mapcar (lambda (x) (concat x ":")) mew-config-cases)))
  (let ((cases (reverse mew-config-cases)) ;; must copy
	val)
    (dolist (case cases)
      (setq val (mew-inbox-folder case))
      (or (mew-folder-inboxp val)
	  (setq mew-inbox-folders (cons val mew-inbox-folders)))
      (setq val (mew-queue-folder case))
      (or (mew-folder-queuep val)
	  (setq mew-queue-folders (cons val mew-queue-folders)))
      (setq val (mew-postq-folder case))
      (or (mew-folder-postqp val)
	  (setq mew-postq-folders (cons val mew-postq-folders)))
      (when mew-generate-mail-address-list
	(setq val (concat "^" (regexp-quote (mew-mail-address case)) "$"))
	(or (member val mew-mail-address-list)
	    (setq mew-mail-address-list (cons val mew-mail-address-list))))
      (when mew-generate-mail-domain-list
	(setq val (mew-mail-domain case))
	(or (member val mew-mail-domain-list)
	    (setq mew-mail-domain-list (cons val mew-mail-domain-list))))
      (when mew-generate-from-list
	(setq val (mew-from case))
	(or (member val mew-from-list)
	    (setq mew-from-list (cons val mew-from-list)))))))

;;;
;;;
;;;

(defun mew-name (&optional case)
  (mew-cfent-value case "name" mew-name))

(defun mew-user (&optional case)
  (mew-cfent-value case "user" mew-user))

(defun mew-mail-domain (&optional case)
  (mew-cfent-value case "mail-domain" mew-mail-domain))

(defun mew-mail-address (&optional case)
  (concat (mew-user case) "@" (mew-mail-domain case)))

;; mew-from is really strange to maintain backword compatibility.
(defun mew-from (&optional case)
  (or (unless mew-config-alist
	(if mew-generate-from-list
	    mew-from
	  (car mew-from-list)))
      (let ((name (mew-name case))
	    (addr (mew-mail-address case)))
	(if (and name (string-match "[^ \t]" name))
	    (format "%s <%s>" name addr)
	  addr))))

(defun mew-cc (&optional case)
  (let ((cc (mew-cfent-value case "cc" mew-cc)))
    (if (eq cc 'me) (mew-mail-address case) cc)))

(defun mew-fcc (&optional case)
  (mew-cfent-value case "fcc" mew-fcc))

(defun mew-bcc (&optional case)
  (mew-cfent-value case "bcc" mew-bcc))

(defun mew-dcc (&optional case)
  (let ((dcc (mew-cfent-value case "dcc" mew-dcc)))
    (if (eq dcc 'me) (mew-mail-address case) dcc)))

(defun mew-reply-to (&optional case)
  (mew-cfent-value case "reply-to" mew-reply-to))

(defun mew-organization (&optional case)
  (mew-cfent-value case "organization" mew-organization))

;;

(defun mew-ssl-cert-directory (&optional case)
  (mew-cfent-value case "ssl-cert-directory" mew-ssl-cert-directory))

(defun mew-ssl-verify-level (&optional case)
  (mew-cfent-value case "ssl-verify-level" mew-ssl-verify-level))

;;

(defun mew-smtp-server (&optional case)
  (mew-cfent-value case "smtp-server" mew-smtp-server))

(defun mew-smtp-port (&optional case)
  (mew-cfent-value case "smtp-port" mew-smtp-port))

(defun mew-smtp-ssh-server (&optional case)
  (mew-cfent-value case "smtp-ssh-server" mew-smtp-ssh-server))

(defun mew-smtp-ssl (&optional case)
  (mew-cfent-value case "smtp-ssl" mew-smtp-ssl))

(defun mew-smtp-ssl-port (&optional case)
  (mew-cfent-value case "smtp-ssl-port" mew-smtp-ssl-port))

(defun mew-smtp-helo-domain (&optional case)
  (mew-cfent-value case "smtp-helo-domain" mew-smtp-helo-domain))

(defun mew-smtp-user-only (&optional case)
  (let ((user-domain (mew-smtp-user case)))
    (if (string-match "^\\([^@]+\\)@" user-domain)
	(mew-match-string 1 user-domain)
      user-domain)))

(defun mew-smtp-user (&optional case)
  (or (mew-cfent-value case "smtp-user" mew-smtp-user)
      (mew-mail-address case)))

(defun mew-smtp-auth-list (&optional case)
  (mew-cfent-value case "smtp-auth-list" mew-smtp-auth-list 'list))

(defun mew-smtp-mail-from (&optional case)
  (mew-cfent-value case "smtp-mail-from" mew-smtp-mail-from))

(defun mew-smtp-msgid-user (&optional case)
  (or (mew-cfent-value case "smtp-msgid-user" mew-smtp-msgid-user)
      (mew-user case)))

(defun mew-smtp-msgid-domain (&optional case)
  (or (mew-cfent-value case "smtp-msgid-domain" mew-smtp-msgid-domain)
      (mew-mail-domain case)))

(defun mew-smtp-message-id (&optional case)
  (let* ((random (format "%08d" (mew-random)))
	 (domain (mew-smtp-msgid-domain case))
	 (user (mew-smtp-msgid-user case))
	 (time (mew-time-ctz-to-msgid (current-time))))
    (concat "<" time "." random "." user "@" domain ">")))

(defun mew-use-smtp-auth (&optional case)
  (mew-cfent-value case "use-smtp-auth" mew-use-smtp-auth))

(defun mew-smtp-auth-plain-authorize-id (&optional case)
  (mew-cfent-value case "smtp-auth-plain-authorize-id" mew-smtp-auth-plain-authorize-id))

;;

(defun mew-mailbox-type (&optional case)
  (mew-cfent-value case "mailbox-type" mew-mailbox-type))

;;

(defun mew-mbox-command (&optional case)
  (mew-cfent-value case "mbox-command" mew-mbox-command))

(defun mew-mbox-command-arg (&optional case)
  (mew-cfent-value case "mbox-command-arg" mew-mbox-command-arg))

;;

(defun mew-pop-server (&optional case)
  (mew-cfent-value case "pop-server" mew-pop-server))

(defun mew-pop-port (&optional case)
  (mew-cfent-value case "pop-port" mew-pop-port))

(defun mew-pop-auth (&optional case)
  (mew-cfent-value case "pop-auth" mew-pop-auth))

(defun mew-pop-ssh-server (&optional case)
  (mew-cfent-value case "pop-ssh-server" mew-pop-ssh-server))

(defun mew-pop-ssl (&optional case)
  (mew-cfent-value case "pop-ssl" mew-pop-ssl))

(defun mew-pop-ssl-port (&optional case)
  (mew-cfent-value case "pop-ssl-port" mew-pop-ssl-port))

(defun mew-pop-proxy-server (&optional case)
  (mew-cfent-value case "pop-proxy-server" mew-pop-proxy-server))

(defun mew-pop-proxy-port (&optional case)
  (mew-cfent-value case "pop-proxy-port" mew-pop-proxy-port))

(defun mew-pop-user (&optional case)
  (mew-cfent-value case "pop-user" mew-pop-user))

(defun mew-pop-auth-list (&optional case)
  (mew-cfent-value case "pop-auth-list" mew-pop-auth-list 'list))

(defun mew-pop-size (&optional case)
  (mew-cfent-value case "pop-size" mew-pop-size))

(defun mew-pop-body-lines (&optional case)
  (mew-cfent-value case "pop-body-lines" mew-pop-body-lines))

(defun mew-pop-delete (&optional case)
  (mew-cfent-value case "pop-delete" mew-pop-delete))

(defun mew-pop-header-only (&optional case)
  (mew-cfent-value case "pop-header-only" mew-pop-header-only))

;;

(defun mew-imap-server (&optional case)
  (mew-cfent-value case "imap-server" mew-imap-server))

(defun mew-imap-port (&optional case)
  (mew-cfent-value case "imap-port" mew-imap-port))

(defun mew-imap-auth (&optional case)
  (mew-cfent-value case "imap-auth" mew-imap-auth))

(defun mew-imap-ssh-server (&optional case)
  (mew-cfent-value case "imap-ssh-server" mew-imap-ssh-server))

(defun mew-imap-ssl (&optional case)
  (mew-cfent-value case "imap-ssl" mew-imap-ssl))

(defun mew-imap-ssl-port (&optional case)
  (mew-cfent-value case "imap-ssl-port" mew-imap-ssl-port))

(defun mew-imap-proxy-server (&optional case)
  (mew-cfent-value case "imap-proxy-server" mew-imap-proxy-server))

(defun mew-imap-proxy-port (&optional case)
  (mew-cfent-value case "imap-proxy-port" mew-imap-proxy-port))

(defun mew-imap-user (&optional case)
  (mew-cfent-value case "imap-user" mew-imap-user))

(defun mew-imap-auth-list (&optional case)
  (mew-cfent-value case "imap-auth-list" mew-imap-auth-list 'list))

(defun mew-imap-size (&optional case)
  (mew-cfent-value case "imap-size" mew-imap-size))

(defun mew-imap-delete (&optional case)
  (mew-cfent-value case "imap-delete" mew-imap-delete))

(defun mew-imap-header-only (&optional case)
  (mew-cfent-value case "imap-header-only" mew-imap-header-only))

(defun mew-imap-prefix-list (&optional case)
  (mew-cfent-value case "imap-prefix-list" mew-imap-prefix-list 'list))

;;

(defun mew-nntp-server (&optional case)
  (mew-cfent-value case "nntp-server" mew-nntp-server))

(defun mew-nntp-port (&optional case)
  (mew-cfent-value case "nntp-port" mew-nntp-port))

(defun mew-nntp-ssh-server (&optional case)
  (mew-cfent-value case "nntp-ssh-server" mew-nntp-ssh-server))

(defun mew-nntp-ssl (&optional case)
  (mew-cfent-value case "nntp-ssl" mew-nntp-ssl))

(defun mew-nntp-ssl-port (&optional case)
  (mew-cfent-value case "nntp-ssl-port" mew-nntp-ssl-port))

(defun mew-nntp-user (&optional case)
  (mew-cfent-value case "nntp-user" mew-nntp-user))

(defun mew-nntp-size (&optional case)
  (mew-cfent-value case "nntp-size" mew-nntp-size))

(defun mew-nntp-header-only (&optional case)
  (mew-cfent-value case "nntp-header-only" mew-nntp-header-only))

(defun mew-nntp-msgid-user (&optional case)
  (or (mew-cfent-value case "nntp-msgid-user" mew-nntp-msgid-user)
      (mew-user case)))

(defun mew-nntp-msgid-domain (&optional case)
  (or (mew-cfent-value case "nntp-msgid-domain" mew-nntp-msgid-domain)
      (mew-mail-domain case)))

(defun mew-nntp-message-id (&optional case)
  (let* ((random (format "%08d" (mew-random)))
	 (domain (mew-nntp-msgid-domain case))
	 (user (mew-nntp-msgid-user case))
	 (time (mew-time-ctz-to-msgid (current-time))))
    (concat "<" time "." random "." user "@" domain ">")))
;;

(defun mew-inbox-folder (&optional case)
  (mew-cfent-value case "inbox-folder" mew-inbox-folder))

(defun mew-imap-inbox-folder (&optional case)
  (mew-cfent-value case "imap-inbox-folder" mew-imap-inbox-folder))

(defun mew-nntp-newsgroup (&optional case)
  (mew-cfent-value case "nntp-newsgroup" mew-nntp-newsgroup))

(defun mew-imap-friend-folder (&optional case)
  (mew-cfent-value case "imap-friend-folder" mew-imap-friend-folder))

(defun mew-queue-folder (&optional case)
  (mew-cfent-value case "queue-folder" mew-queue-folder))

(defun mew-postq-folder (&optional case)
  (mew-cfent-value case "postq-folder" mew-postq-folder))

(defun mew-imap-queue-folder (&optional case)
  (mew-cfent-value case "imap-queue-folder" mew-imap-queue-folder))

(defun mew-imap-trash-folder (&optional case)
  (mew-cfent-value case "imap-trash-folder" mew-imap-trash-folder))

(defun mew-imap-trash-folder-list (&optional case)
  (mew-cfent-value case "imap-trash-folder-list" mew-imap-trash-folder-list 'list))

(defun mew-header-alist (&optional case)
  (mew-cfent-value case "header-alist" mew-header-alist 'list))

(defun mew-signature-file (&optional case)
  (mew-cfent-value case "signature-file" mew-signature-file))

(defun mew-content-type (&optional case)
  (mew-capitalize (mew-cfent-value case "content-type" mew-content-type)))

(defun mew-imap-spam-field (&optional case)
  (mew-cfent-value case "imap-spam-field" mew-imap-spam-field))

(defun mew-imap-spam-word (&optional case)
  (mew-cfent-value case "imap-spam-word" mew-imap-spam-word))

(defun mew-imap-spam-pattern (&optional case)
  (mew-cfent-value case "imap-spam-pattern" mew-imap-spam-pattern))

(defun mew-imap-spam-folder (&optional case)
  (mew-cfent-value case "imap-spam-folder" mew-imap-spam-folder))

;;

(defun mew-warn-addresses (&optional case)
  (mew-cfent-value case "warn-addresses" mew-warn-addresses))

(defun mew-safe-addresses (&optional case)
  (mew-cfent-value case "safe-addresses" mew-safe-addresses))

(defun mew-warn-domains (&optional case)
  (mew-cfent-value case "warn-domains" mew-warn-domains))

(defun mew-safe-domains (&optional case)
  (mew-cfent-value case "safe-domains" mew-safe-domains))

;;

(defun mew-proto (&optional case)
  (mew-cfent-value case "proto" mew-proto))

;;

(defun mew-ssh-prog (&optional case)
  (mew-cfent-value case "ssh-prog" mew-ssh-prog))

(defun mew-ssh-prog-args (&optional case)
  (mew-cfent-value case "ssh-prog-args" mew-ssh-prog-args))

(defun mew-ssh-prog-ver (&optional case)
  (mew-cfent-value case "ssh-prog-ver" mew-ssh-prog-ver))

(defun mew-spam-prog (&optional case)
  (mew-cfent-value case "spam-prog" mew-spam-prog))

(defun mew-spam-prog-args (&optional case)
  (mew-cfent-value case "spam-prog-args" mew-spam-prog-args))

(defun mew-ham-prog (&optional case)
  (mew-cfent-value case "ham-prog" mew-ham-prog))

(defun mew-ham-prog-args (&optional case)
  (mew-cfent-value case "ham-prog-args" mew-ham-prog-args))

;;

(defun mew-refile-guess-alist (&optional case)
  (mew-cfent-value case "refile-guess-alist" mew-refile-guess-alist 'symbol))

;;

(defun mew-use-old-pgp (&optional case)
  (mew-cfent-value case "use-old-pgp" mew-use-old-pgp))

;;

(defun mew-inbox-action-alist (&optional case)
  (mew-cfent-value case "inbox-action-alist" mew-inbox-action-alist 'list))

;;

(defun mew-draft-privacy-method (&optional case)
  (mew-cfent-value case "draft-privacy-method" mew-draft-privacy-method))

(defun mew-protect-privacy-always (&optional case)
  (mew-cfent-value case "protect-privacy-always" mew-protect-privacy-always))

(defun mew-protect-privacy-always-type (&optional case)
  (mew-cfent-value case "protect-privacy-always-type" mew-protect-privacy-always-type))

(defun mew-protect-privacy-encrypted (&optional case)
  (mew-cfent-value case "protect-privacy-encrypted" mew-protect-privacy-encrypted))

(defun mew-protect-privacy-encrypted-type (&optional case)
  (mew-cfent-value case "protect-privacy-encrypted-type" mew-protect-privacy-encrypted-type))

(defun mew-protect-privacy-with-old-pgp-signature (&optional case)
  (mew-cfent-value case "protect-privacy-with-old-pgp-signature" mew-protect-privacy-with-old-pgp-signature))

;;

(defun mew-pgp-signer (&optional case)
  (mew-cfent-value case "pgp-signer" mew-pgp-signer))

(defun mew-smime-signer (&optional case)
  (mew-cfent-value case "smime-signer" mew-smime-signer))

(defun mew-use-x-mailer (&optional case)
  (mew-cfent-value case "use-x-mailer" mew-use-x-mailer))

(defun mew-use-format-flowed (&optional case)
  (mew-cfent-value case "use-format-flowed" mew-use-format-flowed))

;;;
;;; Setting case
;;;

(defvar mew-case nil)

(defun mew-case-set ()
  "Set the case both for input and output."
  (interactive)
  (setq mew-case (mew-input-case mew-case)))

(defun mew-summary-set-case ()
  "Set the case."
  (interactive)
  (let ((case (mew-case-set))) ;; side effect
    (save-excursion
      (dolist (buf mew-buffers)
	(when (get-buffer buf)
	  (set-buffer buf)
	  (cond
	   ((mew-summary-p)
	    (mew-summary-mode-name mew-mode-name-summary))
	   ((mew-virtual-p)
	    (mew-summary-mode-name mew-mode-name-virtual))))))
    (when mew-visit-inbox-after-setting-case
      (let ((inbox (mew-case-folder
		    case
		    (mew-proto-inbox-folder (mew-proto case) case))))
	(mew-summary-visit-folder inbox)))))

(defun mew-draft-set-case (&optional arg)
  "Guess case and set the case for output to it. The value is
locally stored in Draft mode. If called with '\\[universal-argument]',
you can modify the locally stored value. Then this command replace
fields in the header according to the new value."
  (interactive "P")
  (let ((old-case (mew-tinfo-get-case)) new-case)
    (if arg
	(setq new-case (mew-input-case old-case 'edit))
      (setq new-case (mew-draft-get-case-by-guess))
      (if mew-case-guess-addition
	  (setq new-case (mew-draft-add-case old-case new-case)))
      (setq new-case (mew-input-case (or new-case mew-case))))
    (mew-tinfo-set-case new-case)
    (mew-tinfo-set-use-flowed (mew-use-format-flowed new-case))
    (mew-draft-mode-name (mew-tinfo-get-hdr-file))
    (mew-draft-replace-fields old-case)
    (mew-highlight-header)
    (unless (mew-tinfo-get-hdr-file) (mew-draft-header-keymap))
    (save-buffer)))

(defun mew-draft-replace-fields (old-case)
  (save-excursion
    (goto-char (point-min))
    (let ((new-case (mew-tinfo-get-case))
	  from fcc dcc eoh)
      ;; (mew-header-end) cannot be used here
      (mew-header-goto-end)
      (setq eoh (point))
      (goto-char (point-min))
      (cond
       ((mew-draft-resent-p eoh)
	(setq from mew-resent-from:)
	;; (setq cc mew-resent-cc:)
	(setq fcc mew-resent-fcc:)
	(setq dcc mew-resent-dcc:))
       (t
	(setq from mew-from:)
	;; (setq cc mew-cc:)
	(setq fcc mew-fcc:)
	(setq dcc mew-dcc:)))
      (mew-header-replace-value from (mew-from new-case))
      ;; (mew-header-replace-value cc (mew-cc new-case))
      (mew-header-replace-value dcc (mew-dcc new-case))
      (mew-header-replace-value fcc (mew-fcc new-case))
      (mew-header-replace-value mew-reply-to: (mew-reply-to new-case))
      (mew-header-replace-value mew-organization: (mew-organization new-case))
      (mew-header-delete-lines (mapcar 'mew-alist-get-key (mew-header-alist old-case)))
      (mew-header-delete-lines (mapcar 'mew-alist-get-key (mew-header-alist new-case)))
      (mew-header-delete-lines (list mew-x-face: mew-x-mailer:))
      (mew-header-goto-end)
      (mew-draft-header-insert-xface)
      (mew-draft-header-insert-alist (mew-header-alist new-case))
      ;; X-Mailer: must be the last
      (if (mew-use-x-mailer new-case)
	  (mew-draft-header-insert mew-x-mailer: mew-x-mailer)))))

(defun mew-draft-get-case-by-guess (&optional alist)
  "Guess case according to 'mew-case-guess-alist'."
  (unless alist (setq alist mew-case-guess-alist))
  (let ((cases (mew-refile-guess-by-alist1 alist)))
    (if cases
	(mew-join "," cases)
      nil)))

(defun mew-draft-set-case-by-guess ()
  (let ((case (mew-draft-get-case-by-guess)))
    (when case
      (if mew-case-guess-addition
	  (setq case (mew-draft-add-case (mew-tinfo-get-case) case)))
      (mew-tinfo-set-case case))))

(defun mew-draft-add-case (dst src)
  (if (mew-case-default-p dst)
      src
    (if (> (length src) 0)
	(mew-join
	 "," (nreverse
	      (mew-uniq-list (nreverse (mew-split (concat dst "," src) ?,)))))
      dst)))

(provide 'mew-config)

;;; Copyright Notice:

;; Copyright (C) 1999-2014 Mew developing team.
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

;;; mew-config.el ends here
