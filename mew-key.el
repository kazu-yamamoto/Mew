;;; mew-key.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  1, 1999

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mode map
;;;

(defvar mew-summary-mode-map  nil)
(defvar mew-message-mode-map  nil)
(defvar mew-draft-mode-map    nil)
(defvar mew-draft-header-map  nil)
(defvar mew-draft-body-map    nil)
(defvar mew-draft-attach-map  nil)
(defvar mew-header-mode-map   nil)
(defvar mew-addrbook-mode-map nil)
(defvar mew-input-map         nil)
(defvar mew-input-folder-map  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode
;;;

(unless mew-summary-mode-map
  (setq mew-summary-mode-map (make-sparse-keymap))
  (define-key mew-summary-mode-map "0"    'digit-argument)
  (define-key mew-summary-mode-map "1"    'digit-argument)
  (define-key mew-summary-mode-map "2"    'digit-argument)
  (define-key mew-summary-mode-map "3"    'digit-argument)
  (define-key mew-summary-mode-map "4"    'digit-argument)
  (define-key mew-summary-mode-map "5"    'digit-argument)
  (define-key mew-summary-mode-map "6"    'digit-argument)
  (define-key mew-summary-mode-map "7"    'digit-argument)
  (define-key mew-summary-mode-map "8"    'digit-argument)
  (define-key mew-summary-mode-map "9"    'digit-argument)
  (define-key mew-summary-mode-map " "    'mew-summary-display)
  (define-key mew-summary-mode-map "."    'mew-summary-analyze-again)
  (define-key mew-summary-mode-map ":"
    'mew-summary-analyze-again-alternative)
  (define-key mew-summary-mode-map ","    'mew-summary-display-asis)
  (define-key mew-summary-mode-map ";"    'mew-summary-trace-path)
;;  (define-key mew-summary-mode-map "\e<"  'mew-summary-jump-top)
;;  (define-key mew-summary-mode-map "\e>"  'mew-summary-jump-bottom)
  (define-key mew-summary-mode-map "<"    'mew-summary-scroll-right)
  (define-key mew-summary-mode-map ">"    'mew-summary-scroll-left)
  (define-key mew-summary-mode-map "\d"   'mew-summary-prev-page)
  (define-key mew-summary-mode-map "\r"   'mew-summary-scroll-up)
  (define-key mew-summary-mode-map "-"    'mew-summary-scroll-down)
  (define-key mew-summary-mode-map "\e\r" 'mew-summary-scroll-down)
  (define-key mew-summary-mode-map "g"    'mew-summary-goto-folder)
  (define-key mew-summary-mode-map "j"    'mew-summary-goto-line)
  (define-key mew-summary-mode-map "i"    'mew-summary-retrieve)
  (define-key mew-summary-mode-map "I"    'mew-summary-retrieve-message)
  (define-key mew-summary-mode-map "a"    'mew-summary-reply)
  (define-key mew-summary-mode-map "A"    'mew-summary-reply-with-citation)
  (define-key mew-summary-mode-map "D"    'mew-summary-clean-trash)
  (define-key mew-summary-mode-map "E"    'mew-summary-reedit)
  (define-key mew-summary-mode-map "\ei"  'mew-summary-edit-again)
  (define-key mew-summary-mode-map "\ee"  'mew-summary-edit)
  (define-key mew-summary-mode-map "f"    'mew-summary-forward)
  (define-key mew-summary-mode-map "F"    'mew-summary-multi-forward)
  (define-key mew-summary-mode-map "r"    'mew-summary-resend)
  (define-key mew-summary-mode-map "$"    'mew-summary-escape)
  (define-key mew-summary-mode-map "*"    'mew-summary-review)
  (define-key mew-summary-mode-map "\eu"  'mew-summary-unread)
  (define-key mew-summary-mode-map "y"    'mew-summary-save)
  (define-key mew-summary-mode-map "b"    'mew-summary-store)
  (define-key mew-summary-mode-map "Y"    'mew-summary-cite)
  (define-key mew-summary-mode-map "u"    'mew-summary-undo)
  (define-key mew-summary-mode-map "U"    'mew-summary-undo-all)
  (define-key mew-summary-mode-map "n"    'mew-summary-display-down)
  (define-key mew-summary-mode-map "p"    'mew-summary-display-up)
  (define-key mew-summary-mode-map "\C-n" 'mew-summary-next-line)
  (define-key mew-summary-mode-map "\C-p" 'mew-summary-previous-line)
  (define-key mew-summary-mode-map "e"    'mew-summary-exchange-marks)
  (define-key mew-summary-mode-map "M"	  (make-sparse-keymap))
  (define-key mew-summary-mode-map "Md"   'mew-summary-mark-duplicated)
  (define-key mew-summary-mode-map "M*"   'mew-summary-redo)
  (define-key mew-summary-mode-map "N"    'mew-summary-display-review-down)
  (define-key mew-summary-mode-map "P"    'mew-summary-display-review-up)
  (define-key mew-summary-mode-map "\en"  'mew-summary-find-keyword-down)
  (define-key mew-summary-mode-map "\ep"  'mew-summary-find-keyword-up)
  (define-key mew-summary-mode-map "w"    'mew-summary-write)
  (define-key mew-summary-mode-map "W"    'mew-summary-send-to-others)
  (define-key mew-summary-mode-map "B"    'mew-summary-burst)
  (define-key mew-summary-mode-map "K"    'mew-summary-old-burst)
  (define-key mew-summary-mode-map "J"    'mew-summary-join)
  (define-key mew-summary-mode-map "Z"    'mew-status-update)
  (define-key mew-summary-mode-map "#"    'mew-summary-print)
  (define-key mew-summary-mode-map "|"    'mew-summary-pipe-message)
  (define-key mew-summary-mode-map "\\"   'mew-summary-cmd-msg)
  (define-key mew-summary-mode-map "q"    'mew-summary-suspend)
  (define-key mew-summary-mode-map "Q"    'mew-summary-quit)
  (define-key mew-summary-mode-map "C"    'mew-summary-set-case)
  (define-key mew-summary-mode-map "h"    'mew-summary-goto-msg-mode)
  (define-key mew-summary-mode-map "\C-c\C-a" 'mew-summary-addrbook-add)
  (define-key mew-summary-mode-map "\C-c\C-c" 'mew-summary-send-message)
  (define-key mew-summary-mode-map "\C-c\C-e" 'mew-summary-execute-external)
  (define-key mew-summary-mode-map "\C-c\t"   'mew-summary-find-file)
  (define-key mew-summary-mode-map "\C-c\C-f" 'mew-pgp-fetch-key)
  (define-key mew-summary-mode-map "\C-c\C-v" 'mew-pgp-select)
  (define-key mew-summary-mode-map "\C-c\C-m" 'mew-passwd-change)
  (define-key mew-summary-mode-map "\C-c\C-s" 'mew-summary-isearch-forward)
  (define-key mew-summary-mode-map "\C-c\C-r" 'mew-summary-isearch-backward)
  (define-key mew-summary-mode-map "\C-c\C-o"
    'mew-summary-jump-to-draft-buffer)
  (define-key mew-summary-mode-map "\ea"  'mew-summary-alias-edit)
  (define-key mew-summary-mode-map "\el"  'mew-summary-recenter)
  (define-key mew-summary-mode-map "\et"  'mew-summary-uudecode)
  (define-key mew-summary-mode-map "\es"  'mew-summary-sort)
  (define-key mew-summary-mode-map "\eb"  'mew-summary-burst-multi)
  (define-key mew-summary-mode-map "\e\\" 'mew-summary-cmd-msgs)
  (define-key mew-summary-mode-map "v"    'mew-summary-toggle-disp-msg)
  (define-key mew-summary-mode-map "V"    'mew-summary-selection-by-msgid)
  (define-key mew-summary-mode-map "\C-c\C-l" 'mew-summary-convert-local-cs)
  (define-key mew-summary-mode-map "\C-c\C-z" 'mew-summary-decode-old-pgp)
  (define-key mew-summary-mode-map "\C-c\C-q" 'mew-summary-kill)
  (define-key mew-summary-mode-map "\C-c\C-x" 'mew-summary-x-face)
  (define-key mew-summary-mode-map "\C-c\C-k" 'mew-summary-kill-subprocess)
  (define-key mew-summary-mode-map "^"    'mew-summary-parent)
  (define-key mew-summary-mode-map "&"    'mew-summary-child)
  (define-key mew-summary-mode-map "("    'mew-summary-thread-sibling-up)
  (define-key mew-summary-mode-map ")"    'mew-summary-thread-sibling-down)
  (define-key mew-summary-mode-map "m"	  (make-sparse-keymap))
  (define-key mew-summary-mode-map "md"   'mew-summary-mark-delete)
  (define-key mew-summary-mode-map "m\ed" 'mew-summary-mark-unlink)
  (define-key mew-summary-mode-map "m$"   'mew-summary-mark-escape)
  (define-key mew-summary-mode-map "m*"   'mew-summary-mark-review)
  (define-key mew-summary-mode-map "m\eu" 'mew-summary-mark-unread)
  (define-key mew-summary-mode-map "ms"   'mew-summary-mark-swap)
  (define-key mew-summary-mode-map "mo"   'mew-summary-mark-refile)
  (define-key mew-summary-mode-map "mc"   'mew-summary-mark-copy)
  (define-key mew-summary-mode-map "mr"   'mew-summary-mark-regexp)
  (define-key mew-summary-mode-map "ma"   'mew-summary-mark-all)
  (define-key mew-summary-mode-map "mu"   'mew-summary-mark-undo-all)
  (define-key mew-summary-mode-map "mI"   'mew-summary-mark-retrieve-message)
  (define-key mew-summary-mode-map "ml"	  (make-sparse-keymap))
  (define-key mew-summary-mode-map "mlc"  'mew-summary-mark-local-copy)
  (define-key mew-summary-mode-map "mli"  'mew-summary-mark-imap-copy)
  (define-key mew-summary-mode-map "/"    'mew-summary-selection-by-pick)
  (define-key mew-summary-mode-map "?"    'mew-summary-pick)
  (define-key mew-summary-mode-map "'"    'mew-summary-grep-old)
  (define-key mew-summary-mode-map "m/"   'mew-summary-selection-by-mark)
  (define-key mew-summary-mode-map "mt"   'mew-summary-mark-thread)
  (define-key mew-summary-mode-map "t"    (make-sparse-keymap))
  (define-key mew-summary-mode-map "tt"   'mew-summary-make-thread)
  (define-key mew-summary-mode-map "tn"   'mew-thread-down)
  (define-key mew-summary-mode-map "tp"   'mew-thread-up)
  (define-key mew-summary-mode-map "t "   'mew-thread-toggle)
  (define-key mew-summary-mode-map "ta"   'mew-thread-toggle-all)
  (define-key mew-summary-mode-map "t("   'mew-thread-all-graft)
  (define-key mew-summary-mode-map "t)"   'mew-thread-all-prune)
  (define-key mew-summary-mode-map "t*"   'mew-thread-mark-review)
  (define-key mew-summary-mode-map "t$"   'mew-thread-mark-escape)
  (define-key mew-summary-mode-map "td"   'mew-thread-mark-delete)
  (define-key mew-summary-mode-map "to"   'mew-thread-mark-refile)
  (define-key mew-summary-mode-map "tc"   'mew-thread-mark-copy)
  (define-key mew-summary-mode-map "t\ed" 'mew-thread-mark-unlink)
  (define-key mew-summary-mode-map "tu"   'mew-thread-unmark)
  (define-key mew-summary-mode-map "tg"   'mew-thread-glue)
  (define-key mew-summary-mode-map "tr"   'mew-summary-regexp-make-thread)
  (define-key mew-summary-mode-map "z"    (make-sparse-keymap))
  (define-key mew-summary-mode-map "z8"   'mew-summary-toggle-8bit)
  (define-key mew-summary-mode-map "zc"   'mew-summary-cache-clean-up)
  (define-key mew-summary-mode-map "zd"   'mew-summary-toggle-debug)
  (define-key mew-summary-mode-map "zp"   'mew-summary-toggle-policy)
  (define-key mew-summary-mode-map "zo"   'mew-summary-toggle-pgp)
  (define-key mew-summary-mode-map "zv"   'mew-summary-toggle-header-veil)
  (define-key mew-summary-mode-map "zw"   'mew-summary-toggle-warning)
  (define-key mew-summary-mode-map "zi"   'mew-summary-toggle-invisible)
  (define-key mew-summary-mode-map "z "   'mew-summary-toggle-mark-regex)
  (define-key mew-summary-mode-map "zz"   'mew-summary-unzip)
  (define-key mew-summary-mode-map "l"	  (make-sparse-keymap))
  (define-key mew-summary-mode-map "lc"   'mew-summary-local-copy)
  (define-key mew-summary-mode-map "li"   'mew-summary-imap-copy)
  (define-key mew-summary-mode-map "lh"   'mew-summary-learn-ham)
  (define-key mew-summary-mode-map "ls"   'mew-summary-learn-spam)
  (define-key mew-summary-mode-map "="    'mew-summary-info)
  (define-key mew-summary-mode-map "_"    'mew-summary-line)
  (define-key mew-summary-mode-map "k"	  (make-sparse-keymap))
  (define-key mew-summary-mode-map "kc"   'mew-summary-search-change-method)
  (define-key mew-summary-mode-map "k?"   'mew-summary-search)
  (define-key mew-summary-mode-map "k/"   'mew-summary-selection-by-search)
  (define-key mew-summary-mode-map "km"   'mew-summary-make-index-folder)
  (define-key mew-summary-mode-map "kM"   'mew-summary-make-index-all)
  (define-key mew-summary-mode-map "ki"   'mew-summary-make-id-index-folder)
  (define-key mew-summary-mode-map "kI"   'mew-summary-make-id-index-all)
  (define-key mew-summary-mode-map "kj"   'mew-summary-goto-original-message)
  ;;
  ;; not provided in Virtual mode
  ;;
  (define-key mew-summary-mode-map "!"    'mew-summary-refile-again)
  (define-key mew-summary-mode-map "o"    'mew-summary-refile)
  (define-key mew-summary-mode-map "c"    'mew-summary-copy)
  (define-key mew-summary-mode-map "d"    'mew-summary-delete)
  (define-key mew-summary-mode-map "\ed"  'mew-summary-unlink)
  (define-key mew-summary-mode-map "x"    'mew-summary-exec)
  (define-key mew-summary-mode-map "X"    'mew-summary-exec-one)
  (define-key mew-summary-mode-map "lx"   'mew-summary-exec-offline)
  (define-key mew-summary-mode-map "lX"   'mew-summary-exec-offline-one)
  (define-key mew-summary-mode-map "mxd"   'mew-summary-exec-delete)
  (define-key mew-summary-mode-map "mx\ed" 'mew-summary-exec-unlink)
  (define-key mew-summary-mode-map "mxo"   'mew-summary-exec-refile)
  (define-key mew-summary-mode-map "s"    'mew-summary-ls)
  (define-key mew-summary-mode-map "O"    'mew-summary-pack)
  (define-key mew-summary-mode-map "S"    'mew-summary-selection-by-sort)
  (define-key mew-summary-mode-map "\C-c\C-b" 'mew-summary-exchange-point)
  (define-key mew-summary-mode-map "\eo"  'mew-summary-auto-refile)
  (define-key mew-summary-mode-map "R"	  (make-sparse-keymap))
  (define-key mew-summary-mode-map "Rd"   'mew-summary-delete-folder)
  (define-key mew-summary-mode-map "Rr"   'mew-summary-rename-folder)
  ;;
  (define-key mew-summary-mode-map [mouse-2] 'mew-summary-mouse-show))

(defvar mew-summary-mode-menu-spec
  '("Mew"
    ["Show"     mew-summary-display      t]
    ["Next"     mew-summary-display-down t]
    ["Previous" mew-summary-display-up   t]
    ["Top"      mew-summary-jump-top     t]
    ["Bottom"   mew-summary-jump-bottom  t]
    "----"
    ["Delete"       mew-summary-delete       (mew-summary-p)]
    ["Refile"       mew-summary-refile       (mew-summary-case1)]
    ["Refile again" mew-summary-refile-again (mew-summary-case1)]
    ["Mark review"  mew-summary-mark-review  t]
    ["Mark escape"  mew-summary-mark-escape  t]
    ["Undo"         mew-summary-undo         t]
    ["Undo all"     mew-summary-undo-all     t]
    ["Execute"      mew-summary-exec         (mew-summary-p)]
    ["Suspend"      mew-summary-suspend      t]
    ["Quit"         mew-summary-quit         t]
    "----"
    ("Manipulate folder"
     ["Retrieve"     mew-summary-retrieve    t]
     ["List"         mew-summary-ls          (mew-summary-p)]
     ["Sort"         mew-summary-sort        (mew-summary-case2)]
     ["Burst"        mew-summary-burst       t]
     ["Go to folder" mew-summary-goto-folder t]
     )
    ("Manipulate file"
     ["Save"                           mew-summary-save              t]
     ["Convert body's character set"   mew-summary-convert-local-cs  t]
     ["Display X-Face"                 mew-summary-x-face            t]
     )
    ("Write/Reply/Forward"
     ["Write"               mew-summary-send   t]
     ["Reedit"              mew-summary-reedit t]
     ["Reply"               mew-summary-reply  (mew-summary-case1)]
     ["Reply with citation" mew-summary-reply-with-citation (mew-summary-case1)]
     ["Forward"       mew-summary-forward       (mew-summary-case1)]
     ["Multi forward" mew-summary-multi-forward (mew-summary-case1)]
     )
    ("Select"
     ["Pick then mark"     mew-summary-pick        (mew-pickable-p)]
     ["Selection"          mew-summary-selection-by-pick     (mew-pickable-p)]
     )
    ("Misc"
     ["Clean +trash"         mew-summary-clean-trash      t]
     ["Recenter"             mew-summary-recenter         t]
     ["Uudecode"             mew-summary-uudecode         t]
     ["Unshar"               mew-summary-unshar           t]
     ["Multi burst"          mew-summary-burst-multi      t]
     ["Print"                mew-summary-print            t]
     ["Pipe message"         mew-summary-pipe-message     t]
     ["Isearch forward"      mew-summary-isearch-forward  t]
     ["Isearch backward"     mew-summary-isearch-backward t]
     ["Toggle disp msg"      mew-summary-toggle-disp-msg  t]
     ["Set cases"            mew-summary-set-case         t]
     ["PGP public key fetch" mew-pgp-fetch-key            t]
     ["Kill Sub-Process"     mew-summary-kill-subprocess (mew-summary-case3)]
     )))

(defvar mew-summary-mode-toolbar-menu
  '("Mew Part Commands"
    ["Save"  mew-summary-save  t]
    ["Reply" mew-summary-reply t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message mode
;;;

(unless mew-message-mode-map
  (setq mew-message-mode-map (make-sparse-keymap))
  (define-key mew-message-mode-map " "    'mew-message-next-page)
  (define-key mew-message-mode-map "\d"   'mew-message-prev-page)
  (define-key mew-message-mode-map "n"    'mew-message-next-msg)
  (define-key mew-message-mode-map "p"    'mew-message-prev-msg)
  (define-key mew-message-mode-map "h"    'mew-message-goto-summary)
  (define-key mew-message-mode-map "a"    'mew-message-reply)
  (define-key mew-message-mode-map "A"    'mew-message-reply-with-citation)
  (define-key mew-message-mode-map "f"    'mew-message-forward)
  (define-key mew-message-mode-map "r"    'mew-message-resend)
  (define-key mew-message-mode-map "<"    'mew-message-scroll-right)
  (define-key mew-message-mode-map ">"    'mew-message-scroll-left)
  (define-key mew-message-mode-map "_"    'mew-message-line)
  (define-key mew-message-mode-map "g"    'mew-browse-url-at-point)
  (define-key mew-message-mode-map "\C-i" 'mew-message-goto-next-url))

(defvar mew-message-mode-menu-spec
  '("Mew"
    ["Next part"    mew-message-next-msg     t]
    ["Prev part"    mew-message-prev-msg     t]
    ["Next page"    mew-message-next-page    t]
    ["Prev page"    mew-message-prev-page    t]
    ["Goto summary" mew-message-goto-summary t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draft mode
;;;

(defun mew-draft-share-keymap (symmap)
  (if (featurep 'meadow)
      (define-key (symbol-value symmap) [drag-n-drop] 'mew-draft-dnd-for-meadow))
  (define-key (symbol-value symmap) "\C-x\C-s" 'mew-draft-save-buffer)
  (define-key (symbol-value symmap) "\C-c\C-m" 'mew-draft-make-message)
  (define-key (symbol-value symmap) "\C-c\C-c" 'mew-draft-send-message)
  (define-key (symbol-value symmap) "\C-c\C-o" 'mew-draft-set-case)
  (define-key (symbol-value symmap) "\C-c\C-a" 'mew-draft-prepare-attachments)
  (define-key (symbol-value symmap) "\C-c\C-l" 'mew-draft-rehighlight)
  (define-key (symbol-value symmap) "\C-c\C-q" 'mew-draft-kill)
  (define-key (symbol-value symmap) "\C-c\C-s" 'mew-draft-sign-message)
  (define-key (symbol-value symmap) "\C-c\C-e" 'mew-draft-encrypt-message)
  (define-key (symbol-value symmap) "\C-c\C-b" 'mew-draft-sign-encrypt-message)
  (define-key (symbol-value symmap) "\C-c\C-r" 'mew-draft-encrypt-sign-message)
  (define-key (symbol-value symmap) "\C-c\C-p\C-m"
    'mew-draft-set-privacy-method)
  (define-key (symbol-value symmap) "\C-c\C-p\C-a"
    'mew-draft-toggle-privacy-always)
  (define-key (symbol-value symmap) "\C-c\C-p\C-e"
    'mew-draft-toggle-privacy-encrypted)
  (define-key (symbol-value symmap) "\C-c\C-p\C-d"
    'mew-draft-set-privacy-type)
  (define-key (symbol-value symmap) "\C-c\C-p\C-f"
    'mew-draft-use-format-flowed))

(unless mew-draft-header-map
  (setq mew-draft-header-map (make-sparse-keymap))
  (define-key mew-draft-header-map "\t"     'mew-draft-header-comp)
  (define-key mew-draft-header-map "\C-c\t" 'mew-draft-circular-comp)
  (define-key mew-draft-header-map "\e\t"   'mew-draft-expand)
  (define-key mew-draft-header-map "\e\C-e" 'mew-draft-addrbook-expand)
  (mew-draft-share-keymap 'mew-draft-header-map))

(unless mew-draft-body-map
  (setq mew-draft-body-map (make-sparse-keymap))
  (set-keymap-parent mew-draft-body-map text-mode-map)
  (define-key mew-draft-body-map "\C-c\t"       'mew-draft-insert-signature)
  (define-key mew-draft-body-map "\C-c\C-y"     'mew-draft-cite)
  (define-key mew-draft-body-map "\C-c\C-t"     'mew-draft-yank)
  (define-key mew-draft-body-map "\C-c\C-f"     'mew-draft-encode-flowed)
  (define-key mew-draft-body-map "\C-c\C-n\C-f" 'mew-fib-fill-default)
  (define-key mew-draft-body-map "\C-c\C-n\C-k" 'mew-fib-delete-frame)
  (define-key mew-draft-body-map "\C-c\C-n\C-n" 'mew-fib-next-item)
  (define-key mew-draft-body-map "\C-c\C-n\C-p" 'mew-fib-previous-item)
  (define-key mew-draft-body-map "\C-c\C-n\C-z" 'mew-fib-flush-input)
  (mew-draft-share-keymap 'mew-draft-body-map))

(unless mew-draft-mode-map
  (setq mew-draft-mode-map (make-sparse-keymap))
  (set-keymap-parent mew-draft-mode-map mew-draft-body-map))

(defvar mew-draft-mode-toolbar-menu
  '("Attachment Commands"
    ["Insert a File by Linking"
     mew-attach-link
     (mew-attach-not-line012-1)]
    ["Insert a File by Copying"
     mew-attach-copy
     (mew-attach-not-line012-1)]
    ["Insert Audio"
     mew-attach-audio
     (mew-attach-not-line012-1)]
    ["Insert an External Reference"
     mew-attach-external-body
     (mew-attach-not-line012-1)]
    ["Insert a Sub-Multipart"
     mew-attach-multipart
     (mew-attach-not-line012-1)]
    ["Read a New File into a Buffer"
     mew-attach-find-new-file
     (mew-attach-not-line012-1)]
    ["Insert PGP public keys"
     mew-attach-pgp-public-key
     (mew-attach-not-line012-1)]
    "----"
    ["Delete This Part"
     mew-attach-delete
     (mew-attach-not-line012-1-dot)]
    "----"
    ["Describe This Part"
     mew-attach-description
     (mew-attach-not-line0-1-dot)]
    ["Specify A File Name"
     mew-attach-disposition
     (mew-attach-not-line012-1-dot)]
    ["Change the Type"
     mew-attach-type
     (mew-attach-not-line0-1-dot)]
    ["Encode with Gzip64"
     mew-attach-gzip64
     (mew-attach-not-line0-1-dot)]
    ["Encode with Base64"
     mew-attach-base64
     (mew-attach-not-line0-1-dot)]	
    ["Encode with Quoted-Printable"
     mew-attach-quoted-printable
     (mew-attach-not-line0-1-dot)]
    ["Sign with PGP"
     mew-attach-pgp-sign
     (mew-attach-not-line0-1-dot)]
    ["Encrypt with PGP"
     mew-attach-pgp-enc
     (mew-attach-not-line0-1-dot)]
    "----"
    ["Read This File into a Buffer"
     mew-attach-find-file
     (mew-attach-not-line012-1-dot)]))

(defvar mew-draft-mode-menu-spec
  (list  ;; need to eval mew-draft-mode-toolbar-menu
   "Mew"
   ["Cite"                mew-draft-cite t]
   ["Cite without Label"  mew-draft-yank t]
   ["Flowed"              mew-draft-use-format-flowed :style toggle
    :selected mew-use-format-flowed]
   mew-draft-mode-toolbar-menu
   ["Queue Message"       mew-draft-make-message        t]
   ["Send Message"        mew-draft-send-message        t]
   ["Prepare Attachments" mew-draft-prepare-attachments (not (mew-attach-p))]
   ["Set cases"	          mew-draft-set-case		t]
   ["Insert Signature"    mew-draft-insert-signature    t]
   ["Kill Draft"          mew-draft-kill                t]
   "----"
   '("PGP"
     ["PGP Sign"              mew-pgp-sign-message         t]
     ["PGP Encrypt"           mew-pgp-encrypt-message      t]
     ["PGP Sign then Encrypt" mew-pgp-sign-encrypt-message t]
     ["PGP Encrypt then Sign" mew-pgp-encrypt-sign-message t])
   '("Privacy"
     ["All messages"               mew-draft-toggle-privacy-always    t]
     ["Msgs replying to encrypted" mew-draft-toggle-privacy-encrypted t]
     ["This message"		   mew-draft-set-privacy-type         t])
   '("FIB"
     ["FIB next item"     mew-fib-next-item     (not (mew-attach-p))]
     ["FIB previous item" mew-fib-previous-item (not (mew-attach-p))]
     ["FIB flush input"   mew-fib-flush-input   (not (mew-attach-p))]
     ["FIB fill default"  mew-fib-fill-default  (not (mew-attach-p))]
     ["FIB delete frame"  mew-fib-delete-frame  (not (mew-attach-p))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header mode
;;;

(unless mew-header-mode-map
  (setq mew-header-mode-map (make-sparse-keymap))
  (define-key mew-header-mode-map "\C-x\C-s" 'mew-draft-save-buffer)
  (define-key mew-header-mode-map "\t"       'mew-draft-header-comp)
  (define-key mew-header-mode-map "\C-c\t"   'mew-draft-circular-comp)
  (define-key mew-header-mode-map "\e\t"     'mew-draft-expand)
  (define-key mew-header-mode-map "\C-c\C-m" 'mew-header-make-message)
  (define-key mew-header-mode-map "\C-c\C-c" 'mew-header-send-message)
  (define-key mew-header-mode-map "\C-c\C-q" 'mew-draft-kill)
  (define-key mew-header-mode-map "\C-c\C-o" 'mew-draft-set-case))

(defvar mew-header-mode-menu-spec
  '("Mew"
    ["Queue Message"   mew-header-make-message t]
    ["Send Message"    mew-header-send-message t]
    ["Set cases"       mew-draft-set-case      t]
    ["Kill Draft"      mew-draft-kill          t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Attach mode
;;;

(unless mew-draft-attach-map
  (setq mew-draft-attach-map (make-keymap))
  (define-key mew-draft-attach-map "\C-m"   'mew-attach-newline)
  (define-key mew-draft-attach-map "\C-f"   'mew-attach-forward)
  (define-key mew-draft-attach-map "\C-b"   'mew-attach-backforward)
  (define-key mew-draft-attach-map "\C-n"   'mew-attach-next)
  (define-key mew-draft-attach-map "\C-p"   'mew-attach-previous)
  (define-key mew-draft-attach-map "a"      'mew-attach-audio)
  (define-key mew-draft-attach-map "c"      'mew-attach-copy)
  (define-key mew-draft-attach-map "C"      'mew-attach-charset)
  (define-key mew-draft-attach-map "I"      'mew-attach-icharset)
  (define-key mew-draft-attach-map "d"      'mew-attach-delete)
  (define-key mew-draft-attach-map "y"      'mew-attach-link-message)
  (define-key mew-draft-attach-map "D"      'mew-attach-description)
  (define-key mew-draft-attach-map "P"      'mew-attach-disposition)
  (define-key mew-draft-attach-map "e"      'mew-attach-external-body)
  (define-key mew-draft-attach-map "f"      'mew-attach-find-file)
  (define-key mew-draft-attach-map "F"      'mew-attach-find-new-file)
  (define-key mew-draft-attach-map "l"      'mew-attach-link)
  (define-key mew-draft-attach-map "m"      'mew-attach-multipart)
  (define-key mew-draft-attach-map "T"      'mew-attach-type)
  (define-key mew-draft-attach-map "t"      'mew-attach-toggle)
  (define-key mew-draft-attach-map "G"      'mew-attach-gzip64)
  (define-key mew-draft-attach-map "B"      'mew-attach-base64)
  (define-key mew-draft-attach-map "Q"      'mew-attach-quoted-printable)
  (define-key mew-draft-attach-map "Z"      'mew-attach-zip)
  (define-key mew-draft-attach-map "S"      'mew-attach-pgp-sign)
  (define-key mew-draft-attach-map "E"      'mew-attach-pgp-enc)
  (define-key mew-draft-attach-map "\es"    'mew-attach-smime-sign)
  (define-key mew-draft-attach-map "\ee"    'mew-attach-smime-enc)
  (define-key mew-draft-attach-map "p"      'mew-attach-pgp-public-key)
  (define-key mew-draft-attach-map "U"      'mew-attach-undo)
  (define-key mew-draft-attach-map "\C-c\t" 'mew-draft-insert-signature)
  (mew-draft-share-keymap 'mew-draft-attach-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addrbook mode
;;;

(unless mew-addrbook-mode-map
  (setq mew-addrbook-mode-map (make-sparse-keymap))
  (set-keymap-parent mew-addrbook-mode-map text-mode-map)
  (define-key mew-addrbook-mode-map "\C-c\C-c" 'mew-addrbook-register)
  (define-key mew-addrbook-mode-map "\C-c\C-q" 'mew-addrbook-kill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Minibuffer
;;;

(unless mew-input-map
  (setq mew-input-map
	(if (boundp 'minibuffer-local-map)
	    (copy-keymap minibuffer-local-map)
	  (make-sparse-keymap)))
  (define-key mew-input-map "\r"     'mew-input-exit-minibuffer)
  (define-key mew-input-map "\n"     'mew-input-exit-minibuffer)
  (define-key mew-input-map ","      'mew-input-comma)
  (define-key mew-input-map " "      'mew-input-complete)
  (define-key mew-input-map "\t"     'mew-input-complete)
  (define-key mew-input-map "?"      'mew-input-complete)
  (define-key mew-input-map "\C-c\t" 'mew-circular-complete-switch)
  (define-key mew-input-map [menu-bar minibuf]
    (cons "Mew" (make-sparse-keymap "Mew")))
  (define-key mew-input-map [menu-bar minibuf quit]
    (list 'menu-item "Quit" 'keyboard-escape-quit
          :help "Abort input and exit minibuffer"))
  (define-key mew-input-map [menu-bar minibuf return]
    (list 'menu-item "Enter" 'exit-minibuffer
          :help "Terminate input and exit minibuffer")))

(unless mew-input-folder-map
  (setq mew-input-folder-map (copy-keymap mew-input-map))
  (let ((c 33)) ;; excluding SPC
    (while (<= c 128) ;; including delete
      (define-key mew-input-folder-map (char-to-string c)
	'mew-input-folder-self-insert)
      (setq c (1+ c))))
  (define-key mew-input-folder-map "\d"     'mew-input-folder-self-insert)
  (define-key mew-input-folder-map "\C-g"   'mew-input-folder-abort-minibuffer)
  (define-key mew-input-folder-map "\r"     'mew-input-folder-exit-minibuffer)
  (define-key mew-input-folder-map "\n"     'mew-input-folder-exit-minibuffer)
  (define-key mew-input-folder-map "+"      'mew-input-folder-prefix)
  (define-key mew-input-folder-map "%"      'mew-input-folder-prefix)
  (define-key mew-input-folder-map "$"      'mew-input-folder-prefix)
  (define-key mew-input-folder-map "-"      'mew-input-folder-prefix)
  (define-key mew-input-folder-map "*"      'mew-input-folder-prefix)
  (define-key mew-input-folder-map ","      'mew-input-folder-comma)
  (define-key mew-input-folder-map "\C-s"   'mew-input-folder-search-forward)
  (define-key mew-input-folder-map "\C-r"   'mew-input-folder-search-backward)
  (define-key mew-input-folder-map "\C-n"   'mew-input-folder-search-forward)
  (define-key mew-input-folder-map "\C-p"   'mew-input-folder-search-backward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toolbar
;;;

(defvar mew-summary-toolbar-spec
  '((mew-summary-display      . "mew-show")
    (mew-summary-display-down . "mew-next")
    (mew-summary-display-up   . "mew-prev")
    (mew-summary-retrieve     . "mew-inc")
    (mew-summary-send         . "mew-write")
    (mew-summary-reply        . "mew-reply")
    (mew-summary-forward      . "mew-forward")
    (mew-summary-refile       . "mew-refile")))

(defvar mew-message-toolbar-spec
  '((mew-message-next-msg . "mew-next")
    (mew-message-prev-msg . "mew-prev")
    (mew-massage-reply    . "mew-reply")
    (mew-message-forward  . "mew-forward")))

(defvar mew-draft-toolbar-spec
  '((mew-draft-make-message        . "mew-queue")
    (mew-draft-send-message        . "mew-send")
    (mew-draft-cite                . "mew-cite")
    (mew-draft-yank                . "mew-yank")
    (mew-draft-prepare-attachments . "mew-attach")
    (mew-pgp-sign-message          . "mew-pgp-sign")
    (mew-pgp-encrypt-message       . "mew-pgp-enc")
    (mew-pgp-sign-encrypt-message  . "mew-pgp-sigenc")))

(defvar mew-header-toolbar-spec
  '((mew-header-make-message . "mew-queue")
    (mew-header-send-message . "mew-send")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Icons
;;;

(defun mew-which-mew-etc ()
  (let ((file "mew.el"))
    (catch 'loop
      (dolist (path load-path)
	(if (file-exists-p (expand-file-name file path))
	    (throw 'loop (expand-file-name "etc" path)))))))

(defvar mew-icon-directory (mew-which-mew-etc))

(defvar mew-icon-separate nil)
(defvar mew-icon-blank nil)
(defvar mew-icon-audio nil)
(defvar mew-icon-image nil)
(defvar mew-icon-video nil)
(defvar mew-icon-application/postscript nil)
(defvar mew-icon-application/octet-stream nil)
(defvar mew-icon-message/rfc822 nil)
(defvar mew-icon-message/external-body nil)
(defvar mew-icon-text nil)
(defvar mew-icon-multipart nil)
(defvar mew-icon-unknown nil)

(defvar mew-icon-spec
  '((separate                 . "mew-sep")
    (text                     . "mew-Text")
    (multipart                . "mew-Folder")
    (blank                    . "mew-Blank")
    (audio                    . "mew-Audio")
    (image                    . "mew-Image")
    (video                    . "mew-Video")
    (application/postscript   . "mew-Postscript")
    (application/octet-stream . "mew-Octet-Stream")
    (message/rfc822           . "mew-Rfc822")
    (message/external-body    . "mew-External")
    (unknown                  . "mew-Unknown")))

(provide 'mew-key)

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

;;; mew-key.el ends here
