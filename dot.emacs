;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A sample file of .emacs or a site configuration file
;;;
;
; This is just a sample. You should customize as you like...
;

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; Optional setup (Read Mail menu):
;(setq read-mail-command 'mew)

;; Optional setup (e.g. C-xm for sending a message):
;(autoload 'mew-user-agent-compose "mew" nil t)
;(if (boundp 'mail-user-agent)
;    (setq mail-user-agent 'mew-user-agent))
;(if (fboundp 'define-mail-user-agent)
;    (define-mail-user-agent
;      'mew-user-agent
;      'mew-user-agent-compose
;      'mew-draft-send-message
;      'mew-draft-kill
;      'mew-send-hook))

;; If you are using Emacs with the --unibyte option or the
;; EMACS_UNIBYTE environment variable for Latin-1, put the following
;; into your "~/.emacs".
;(set-language-environment "Latin-1")
;(set-input-method "latin-1-prefix") ;; or "latin-1-postfix"
;; You MUST remove (standard-display-european 1) from your .emacs.

;;;
;;; End
;;;
