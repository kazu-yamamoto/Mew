;;; mew-thread.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  1, 1999

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;

(defvar mew-use-sorted-thread t)

(defcustom mew-use-complete-thread t
  "If non-nil, threads are made using two passes.

First pass -  Repeat the following procedure in numerical order:
	(1.0) Pick one message from the message list.
	(1.1) Register the current message-id: to DB.
	(1.2) Find its parent message-id: in DB.
	(1.3) If found, register the current message as a child of
	      the parent.
	(1.4) Otherwise, register the current message to the top
	      node list.

Here we have pretty good threads.  However, if the messages are not
sorted by Date:, it is possible that some top nodes can be
connected to other threads.  If 'mew-use-complete-thread' is non-nil,
the second pass is carried out.

Second pass - Repeat the following procedure for top nodes linearly:
	(2.0) Pick one message from the top node list.
	(2.1) Find its parent message-id: in DB.
	(2.2) If found, register the current message as a child of
	      the parent.
	(2.3) Otherwise, register the current message to the new top
	      node list.

If you have bogus messages and the second pass is carried out, thread
structure MAY loop. This results in an infinite loop of visualizing
threads (not making threads).

Mew does not provide any loop detection/avoidance mechanism. So, you
should understand this risk."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-thread-indent-strings [" +" " +" " |" "  "]
  "*Vector of strings to be used for indentation of thread.

This consists of four members; 1st member for prefixing to a child
message that is not the last one, 2nd member is for prefixing to the
last child, 3rd and 4th members are for prefixing to grand-child thread trees,
4th member is for the child tree of the last child message.

Example1: [\" +\" \" +\" \" |\" \"  \"] makes thread view below.

    Message 1
     +Message 2
     | +Message 3
     +Message 4
       +Message 5

Example2: [\"  \" \"  \" \"  \" \"  \"] makes thread view below.

    Message 1
      Message 2
        Message 3
      Message 4
        Message 5

All members must have the same length."
  :group 'mew-summary
  :type 'sexp)

(defcustom mew-use-thread-cursor nil
  "*If non-nil, move cursor after the indentation of thread."
  :group 'mew-summary
  :type 'boolean)

(defvar mew-use-thread-separator nil
  "*If non-nil, the specified string is inserted between threads.")
(defvar mew-thread-separator "--")

(defun mew-thread-insert-separator ()
  (if (and mew-use-thread-separator
	   (/= (save-excursion (beginning-of-line) (point)) 1))
      (insert mew-thread-separator "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread info macro
;;;

(defun mew-thread-make-entry ()
  (make-vector 5 nil))

(defun mew-thread-get-myid (entry)
  (aref entry 0))

(defun mew-thread-get-prntid (entry)
  (aref entry 1))

(defun mew-thread-get-child (entry)
  (aref entry 2))

(defun mew-thread-get-msg (entry)
  (aref entry 3))

(defun mew-thread-get-line (entry)
  (aref entry 4))

(defun mew-thread-set-myid (entry myid)
  (aset entry 0 myid))

(defun mew-thread-set-prntid (entry prntid)
  (aset entry 1 prntid))

(defun mew-thread-set-child (entry child)
  (aset entry 2 child))

(defun mew-thread-set-msg (entry msg)
  (aset entry 3 msg))

(defun mew-thread-set-line (entry line)
  (aset entry 4 line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread setup
;;;

(defvar mew-thread-indent-length nil)
(defvar mew-thread-indent-width nil)

(defun mew-thread-setup ()
  (let ((idt1 (aref mew-thread-indent-strings 0))
	(idt2 (aref mew-thread-indent-strings 1))
	(idt3 (aref mew-thread-indent-strings 2))
	(idt4 (aref mew-thread-indent-strings 3)))
    (unless (and (= (string-width idt1) (string-width idt2))
		 (= (string-width idt2) (string-width idt3))
		 (= (string-width idt3) (string-width idt4)))
      (error
       "All members of mew-thread-indent-strings must have the same length"))
    (setq mew-thread-indent-width  (string-width idt1))
    (setq mew-thread-indent-length (length idt1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands
;;;

(defun mew-summary-mark-thread ()
  "Make threads for messages marked with '*'."
  (interactive)
  (mew-summary-thread-region (point-min) (point-max) 'mark))

(defun mew-thread-cache-valid-p (vfolder)
  (let ((cfolder (mew-summary-folder-name 'ext))
	ofld)
    (when (get-buffer vfolder)
      (with-current-buffer vfolder
	(setq ofld (mew-vinfo-get-original-folder))
	(and (equal ofld cfolder)
	     (get-buffer ofld)
	     (equal (mew-sinfo-get-cache-time)
		    (progn (set-buffer ofld) (mew-sinfo-get-cache-time))))))))

(defun mew-summary-make-thread (&optional arg)
  "If called in Summary mode or Selection, make threads for
all messages.

If called with '\\[universal-argument]', make threads for
messages in the region.

If called in Thread, switch back to the corresponding Summary
mode or Selection."
  (interactive "P")
  (if (mew-mark-active-p) (setq arg t))
  (if arg
      (let ((begend (mew-summary-get-region)))
	(mew-summary-thread-region (car begend) (cdr begend)))
    (mew-summary-goto-message)
    (mew-decode-syntax-delete)
    (let* ((msg (mew-summary-message-number))
	   (disp (mew-sinfo-get-disp-msg))
	   (folder (mew-summary-folder-name 'ext)) ;; xxx
	   fld vfolder)
      (cond
       ((mew-thread-p)
	(setq fld (mew-vinfo-get-original-folder))
	(if (not (and fld (get-buffer fld)))
	    (message "No original folder")
	  (mew-summary-visit-folder fld nil 'no-ls)
	  (mew-summary-toggle-disp-msg (if disp 'on 'off))
	  (if (not msg)
	      (goto-char (point-max))
	    (mew-summary-move-and-display msg))))
       ((and (setq vfolder (mew-folder-to-thread folder))
	     (mew-thread-cache-valid-p vfolder))
	(mew-summary-visit-folder vfolder)
	(mew-summary-toggle-disp-msg (if disp 'on 'off))
	(when msg
	  (mew-summary-move-and-display msg)
	  (mew-thread-move-cursor)))
       ((mew-selection-p)
	(mew-summary-thread-region (point-min) (point-max) nil msg))
       (t
	(mew-summary-thread-region (point-min) (point-max) nil msg))))))

(defun mew-summary-regexp-make-thread (&optional args)
  "Make threads for messages matched to a specified regular expression."
  (interactive "P")
  (mew-decode-syntax-delete)
  (let ((regex "") iter)
    (while (string= regex "")
      (setq regex (read-string "Regexp: ")))
    (if args
	(setq iter (lambda () (re-search-forward regex nil t)))
      (setq iter (lambda () (mew-summary-search-regexp-visible regex))))
    (mew-summary-thread-region (point-min) (point-max) nil nil iter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making thread
;;;

(defun mew-thread-get-iter (mark iter)
  (cond
   (iter iter)
   (mark (lambda () (re-search-forward mew-regex-msg-review nil t)))
   (t    (lambda () (not (eobp))))))

(defun mew-thread-create-db (size)
  (let ((dbsize
	 (cond
	  ((<= size 211) 211)
	  ((<= size 1511) 1511)
	  ((<= size 7211) 7211)
	  (t 18211))))
    (make-vector dbsize 0))) ;; hash

(defun mew-thread-pass-1 (db func)
  (let (start msg my-id prnt-id prnt-cld me top line prnt)
    (save-excursion
      (goto-char (point-min))
      (while (funcall func)
	(beginning-of-line)
	(setq start (point))
	(if (not (mew-sumsyn-match mew-regex-sumsyn-long))
	    (forward-line)
	  (setq msg     (mew-sumsyn-message-number))
	  (setq my-id   (mew-sumsyn-my-id))
	  (setq prnt-id (mew-sumsyn-parent-id))
	  (forward-line)
	  ;; Throw away properties here and give properties later.
	  ;; This is faster than inheriting properties.
	  (setq line (mew-buffer-substring start (point)))
	  (setq me (mew-thread-make-entry))
	  (mew-thread-set-msg me msg)
	  (mew-thread-set-line me line)
	  (if (string= my-id "")
	      (setq top (cons me top))
	    ;; some broken messages refer themselves
	    ;; don't register me here so that his parent
	    ;; will not be found.
	    (if (or (string= prnt-id "") (string= my-id prnt-id))
		(setq top (cons me top))
	      (mew-thread-set-prntid me prnt-id)
	      (setq prnt (symbol-value (intern-soft prnt-id db)))
	      (if (null prnt)
		  (setq top (cons me top))
		(setq prnt-cld (mew-thread-get-child prnt))
		(if prnt-cld
		    (nconc prnt-cld (list me))
		  (mew-thread-set-child prnt (list me)))))
	    (mew-thread-set-myid me my-id)
	    (set (intern my-id db) me)))))
    top))

(defun mew-summary-setup-vfolder (db top column)
  (let* ((ofolder (mew-summary-folder-name 'ext))
	 (vfolder (mew-folder-to-thread ofolder))
	 (pfolder (mew-summary-physical-folder))
	 (disp (mew-sinfo-get-disp-msg))
	 (ctime (mew-sinfo-get-cache-time))
	 (case (mew-sinfo-get-case)))
    (mew-summary-switch-to-folder vfolder)
    (mew-vinfo-set-mode 'thread)
    (mew-vinfo-set-physical-folder pfolder)
    (mew-vinfo-set-original-folder ofolder)
    (mew-erase-buffer)
    (mew-hscroll)
    (mew-summary-toggle-disp-msg (if disp 'on 'off))
    (mew-sinfo-set-cache-time ctime)
    (mew-sinfo-set-case case)
    (setq mew-summary-buffer-raw t)
    (mew-vinfo-set-db db)
    (mew-vinfo-set-top top)
    (mew-vinfo-set-column column)))

(defun mew-thread-pass-2 (db top)
  (if (null mew-use-complete-thread)
      (nreverse top)
    ;; This may create looped thread.
    ;; See mew-use-complete-thread for more information.
    (let (prnt prnt-id prnt-cld ret)
      (dolist (me top)
	(if (not (and (mew-thread-get-myid me)
		      (setq prnt-id (mew-thread-get-prntid me))))
	    (setq ret (cons me ret))
	  (setq prnt (symbol-value (intern-soft prnt-id db)))
	  (if (null prnt)
	      (setq ret (cons me ret))
	    (setq prnt-cld (mew-thread-get-child prnt))
	    (if prnt-cld
		(setq prnt-cld (nconc prnt-cld (list me)))
	      (mew-thread-set-child prnt (list me))))))
      ret)))

(defun mew-thread-postscript (mark disp-msg)
  (when mark (mew-mark-undo-mark mew-mark-review))
  (jit-lock-register 'mew-summary-cook-region)
  (mew-summary-set-count-line)
  (set-buffer-modified-p nil)
  (if disp-msg
      (mew-summary-move-and-display disp-msg)
    (goto-char (point-max)))
  (mew-thread-move-cursor))

(defun mew-thread-debug-info (tm1 tm2 tm3 tm4 tm5 tm6)
  (when (mew-debug 'thread)
    (let* ((t1 (mew-time-calc tm2 tm1))
	   (t2 (mew-time-calc tm4 tm3))
	   (t3 (mew-time-calc tm6 tm5)))
      (message "pass1 %f, pass2 %f, visual %f" t1 t2 t3))))

(defun mew-summary-thread-region (beg end &optional mark disp-msg iter)
  "Make threads for messages in a region.  If you want to know how
threads are created, see 'mew-use-complete-thread'."
  (interactive "r")
  (when (mew-summary-exclusive-p)
    (let* ((column (or (mew-sinfo-get-summary-column) ;; scanned
		       ;; Summary only
		       (mew-get-summary-column (mew-summary-folder-name 'ext))))
	   db top tm1 tm2 tm3 tm4 tm5 tm6)
      (save-restriction
	(narrow-to-region beg end)
	(setq db (mew-thread-create-db (count-lines beg end)))
	;;
	(message "Making thread (first pass)...")
	(setq tm1 (current-time))
	(setq top (mew-thread-pass-1 db (mew-thread-get-iter mark iter)))
	(setq tm2 (current-time)))
      ;;
      (if (null top)
	  (message "No target messages")
	(message "Making thread (second pass)...")
	(setq tm3 (current-time))
	(setq top (mew-thread-pass-2 db top))
	(setq tm4 (current-time))
	;;
	(mew-summary-setup-vfolder db top column)
	;;
	(message "Displaying thread...")
	(setq tm5 (current-time))
	(mew-summary-thread-print-top (mew-vinfo-get-top) column)
	(setq tm6 (current-time))
	;;
	(mew-thread-postscript mark disp-msg)	
	;;
	(message "Displaying thread...done")
	(run-hooks 'mew-thread-display-hook)
	(mew-thread-debug-info tm1 tm2 tm3 tm4 tm5 tm6)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subfunctions
;;;

(defun mew-thread-put-property (beg end level)
  (put-text-property beg end 'mew-thread-indent level))

(defun mew-thread-get-property (beg)
  (get-text-property beg 'mew-thread-indent))

(defun mew-thread-previous-property (beg)
  (previous-single-property-change beg 'mew-thread-indent))

(defun mew-thread-next-property (beg)
  (next-single-property-change beg 'mew-thread-indent))

(defun mew-thread-next-property2 (beg end level)
  (text-property-any beg end 'mew-thread-indent level))

(defun mew-thread-adjust-body (level)
  (when (mew-summary-goto-body)
    (mew-elet
     (let ((end (point))
	   (width (* level mew-thread-indent-width))
	   (sum 0))
       (while (< sum width)
	 (setq sum (+ (char-width (char-before)) sum))
	 (forward-char -1))
       (delete-region (point) end)
       (when (/= sum width)
	 (insert (make-string (- sum width) mew-sp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Visualizing thread
;;;

(defun mew-summary-thread-print-top (top column)
  (let (cld)
    (dolist (me top)
      (setq cld (mew-thread-get-child me))
      (mew-elet
       (mew-thread-insert-separator)
       (insert (mew-thread-get-line me))
       (forward-line -1)
       (move-to-column column)
       (mew-thread-put-property (point) (1+ (point)) 0)
       (forward-line))
      (if cld (mew-summary-thread-print-tree cld column)))))

(defun mew-summary-thread-print-tree (tree column)
  (let ((tree-stack nil)
	(prefix "")
	(level 1) pos)
    (while tree
      (let* ((me (car tree))
	     (next (cdr tree))
	     (cld (mew-thread-get-child me)))
	(mew-elet
	 (insert (mew-thread-get-line me))
	 (forward-line -1)
	 (move-to-column column)
	 (setq pos (point))
	 (if next
	     (insert prefix (aref mew-thread-indent-strings 0))
	   (insert prefix (aref mew-thread-indent-strings 1)))
	 (mew-thread-put-property pos (point) level)
	 (mew-thread-adjust-body level)
	 (forward-line))
	;;
	(setq tree next)
	(cond
	 (cld
	  (if next
	      (setq prefix
		    (concat prefix (aref mew-thread-indent-strings 2)))
	    (setq prefix
		  (concat prefix (aref mew-thread-indent-strings 3))))
	  (setq tree-stack (cons tree tree-stack))
	  (setq tree cld)
	  (setq level (1+ level)))
	 (t
	  (while (and (null tree) tree-stack)
	    (setq prefix (substring prefix 0 (- mew-thread-indent-length)))
	    (setq tree (car tree-stack))
	    (setq tree-stack (cdr tree-stack))
	    (setq level (1- level)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Marking thread
;;;

(defun mew-thread-mark-review ()
  "Put the '*' mark on all messages of the current sub-thread."
  (interactive)
  (mew-thread-mark mew-mark-review))

(defun mew-thread-mark-delete ()
  "Put the 'D' mark on all messages of the current sub-thread."
  (interactive)
  (mew-summary-not-in-nntp
   (mew-thread-mark mew-mark-delete 'valid-only)))

(defun mew-thread-mark-unlink ()
  "Put the 'X' mark on all messages of the current sub-thread."
  (interactive)
  (mew-thread-mark mew-mark-unlink 'valid-only))

(defun mew-thread-mark-escape ()
  "Put the '$' mark on all messages of the current sub-thread."
  (interactive)
  (mew-thread-mark mew-mark-escape))

(defun mew-thread-mark-refile ()
  "Put the 'o' mark on all messages of the current sub-thread."
  (interactive)
  (mew-thread-only
   (let* ((fld (mew-folder-basename (mew-summary-folder-name 'ext)))
	  (folders (mew-summary-refile-body nil nil nil 'no-mark))
	  (folders-str (mew-join "," folders))
	  (func (lambda ()
		  (mew-summary-refile-override-body folders-str)
		  (unless (mew-virtual-p)
		    (mew-summary-refile-log fld folders-str))))
	  alist)
     (when folders
       (setq alist (mew-thread-mark mew-mark-refile 'valid-only func))
       (mew-refile-set-from-alist alist folders)))))

(defun mew-thread-mark-copy ()
  "Put the 'o' mark on all messages of the current sub-thread
with the current folder as a candidate in addition to guessed folders."
  (interactive)
  (mew-thread-only
   (let* ((folders (mew-summary-refile-body
		    nil nil nil 'no-mark (mew-summary-folder-name)))
	  (folders-str (mew-join "," folders))
	  (func (lambda () (mew-summary-refile-override-body folders-str)))
	  alist)
     (when folders
       (setq alist (mew-thread-mark mew-mark-refile 'valid-only func))
       (mew-refile-set-from-alist alist folders)))))

(defun mew-refile-set-from-alist (alist folders)
  (let (fld)
    (dolist (ent alist)
      (setq fld (car ent))
      (dolist (msg (sort (copy-sequence (cdr ent)) '<)) ;; sort has side effect
	(setq msg (number-to-string msg))
	(when (get-buffer fld)
	  (with-current-buffer fld
	    (mew-refile-reset msg)
	    (mew-refile-set msg folders)))))))

(defun mew-thread-mark (mark &optional valid-only func)
  (mew-thread-only
   (mew-summary-msg-or-part
    (let ((regex (if valid-only mew-regex-sumsyn-valid mew-regex-sumsyn-short))
	  (column (mew-vinfo-get-column))
	  indent cindent fld msg alist bottom pruned)
      (mew-summary-goto-message)
      (mew-decode-syntax-delete)
      (save-excursion
	(beginning-of-line)
	(when (looking-at
	       (concat "^." (regexp-quote (char-to-string mew-mark-thread-root))))
	  (setq pruned (point))
	  (mew-thread-graft 'nomsg))
	(move-to-column column)
	(setq indent (mew-thread-get-property (point)))
	(when (mew-sumsyn-match regex)
	  (setq fld (mew-sumsyn-folder-name))
	  (setq msg (mew-sumsyn-message-number))
	  (if func (funcall func))
	  (mew-mark-put mark)
	  (mew-mark-alist-set alist fld msg))
	(forward-line)
	(catch 'loop
	  (while (not (eobp))
	    (move-to-column column)
	    (when (setq cindent	(mew-thread-get-property (point)))
	      (if (>= indent cindent)
		  (throw 'loop nil)
		(when (mew-sumsyn-match regex)
		  (setq fld (mew-sumsyn-folder-name))
		  (setq msg (mew-sumsyn-message-number))
		  (if func (funcall func))
		  (mew-mark-put mark)
		  (mew-mark-alist-set alist fld msg))))
	    (forward-line)))
	(beginning-of-line)
	(setq bottom (point))
	(mew-summary-mark-in-physical-alist alist mark func)
	(when pruned
	  (goto-char pruned)
	  (mew-thread-prune 'nomsg)))
      (mew-push-mark)
      (let ((mew-summary-down-function (lambda () (goto-char bottom))))
	(mew-summary-display-after mew-summary-mark-direction))
      alist))))

(defun mew-thread-undo (fld msg)
  (let* ((mark (mew-summary-get-mark))
	 (func (mew-markdb-func-undo mark)))
    (and func (fboundp func) (funcall func fld msg))))

(defun mew-thread-unmark ()
  "Unmark messages under this sub-thread."
  (interactive)
  (mew-thread-only
   (mew-summary-msg-or-part
    (let ((column (mew-vinfo-get-column))
	  fld msg alist indent cindent pruned)
      (mew-summary-goto-message)
      (mew-thread-move-cursor)
      (mew-decode-syntax-delete)
      (save-excursion
	(beginning-of-line)
	(when (looking-at
	       (concat "^." (regexp-quote (char-to-string mew-mark-thread-root))))
	  (setq pruned (point))
	  (mew-thread-graft 'nomsg))
	(move-to-column column)
	(setq indent (mew-thread-get-property (point)))
	(setq fld (mew-summary-folder-name))
	(setq msg (mew-summary-message-number))
	(mew-mark-alist-set alist fld msg)
	(mew-thread-undo fld msg)
	(mew-mark-unmark)
	(forward-line)
	(catch 'loop
	  (while (not (eobp))
	    (move-to-column column)
	    (when (setq cindent (mew-thread-get-property (point)))
	      (if (>= indent cindent)
		  (throw 'loop nil)
		(setq fld (mew-summary-folder-name))
		(setq msg (mew-summary-message-number))
		(mew-mark-alist-set alist fld msg)
		(mew-thread-undo fld msg)
		(mew-mark-unmark)))
	    (forward-line)))
	(when pruned
	  (goto-char pruned)
	  (mew-thread-prune 'nomsg))
	(mew-thread-unmark-physical-from-alist alist))))))

(defun mew-thread-unmark-physical-from-alist (alist)
  (let (fld msgs)
    (dolist (ent alist)
      (setq fld (car ent))
      (setq msgs (sort (copy-sequence (cdr ent)) '<)) ;; sort has side effect
      (when (get-buffer fld)
	(set-buffer fld)
	(save-excursion
	  (goto-char (point-min))
	  (dolist (msg msgs)
	    (setq msg (number-to-string msg))
	    (when (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
	      (mew-thread-undo fld msg)
	      (mew-mark-unmark))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread utilities
;;;

(defun mew-thread-up ()
  "Move onto the top of the current thread. If the current message is
a top node, move onto the top of the previous thread."
  (interactive)
  (mew-thread-only
   (let (here pos)
     (mew-summary-goto-message)
     (save-excursion
       (mew-decode-syntax-delete)
       (beginning-of-line)
       (setq pos (point))
       (catch 'loop
	 (while (and (not (bobp))
		     (setq pos (mew-thread-previous-property pos)))
	   (when (and pos (eq (mew-thread-get-property pos) 0))
	     (throw 'loop (setq here pos))))))
     (if (not here)
	 (message "No more threads")
       (goto-char here)
       (mew-thread-move-cursor)
       (mew-summary-display)))))

(defun mew-thread-down ()
  "Move onto the top of the next thread."
  (interactive)
  (mew-thread-only
   (let (here)
     (mew-summary-goto-message)
     (save-excursion
       (mew-decode-syntax-delete)
       (forward-line)
       (setq here (mew-thread-next-property2 (point) (point-max) 0)))
     (if (not here)
	 (message "No more threads")
       (goto-char here)
       (unless (mew-summary-message-number) (forward-line))
       (mew-thread-move-cursor)
       (mew-summary-display)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Diag
;;;

(defun mew-summary-parent-global (par-id)
  (mew-summary-diag-global par-id "-p" "Parent"))

(defun mew-summary-child-global (my-id)
  (mew-summary-diag-global my-id "-c" "Child"))

(defun mew-summary-diag-global (id opt who)
  (mew-msgid-check
   (let ((db (mew-expand-file "+" mew-id-db-file))
	 (regex (format "\\(.*\\)/\\([0-9]+\\)\\(%s\\)?$" (regexp-quote mew-suffix)))
	 path msg folder)
     (with-temp-buffer
       (mew-piolet mew-cs-text-for-read mew-cs-text-for-write
	 (call-process mew-prog-smew nil t nil opt id db "")
	 (goto-char (point-min))
	 (when (looking-at regex)
	   (setq path (mew-match-string 1))
	   (setq msg (mew-match-string 2)))))
     (if (not msg)
	 nil
       (setq folder (mew-folder-path-to-folder path))
       (when folder
	 (mew-summary-visit-folder folder nil 'no-ls)
	 (if (mew-summary-search-msg msg)
	     (progn
	       (mew-summary-display)
	       t)
	   (concat who " not found. Scan 'update would be necessary")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parent
;;;

(defun mew-summary-parent ()
  "Move onto the parent message of the current message."
  (interactive)
  (mew-summary-goto-message)
  (mew-decode-syntax-delete)
  (let ((par-id (mew-summary-parent-id)) result)
    (cond
     ((or (null par-id) (string= par-id ""))
      (message "No parent"))
     ((mew-summary-parent-local par-id)
      (message "Parent found"))
     ((and (y-or-n-p "No parent in this folder. Find in others? ")
	   (setq result (mew-summary-parent-global par-id)))
      (if (eq result t)
	  (message "Parent found")
	(message "%s" result)))
     (t
      (message "Parent not found")))))

(defun mew-summary-parent-local (par-id)
  (let ((pos (point))
	(key (mew-regex-sumsyn-my-id par-id)))
    (if (or (re-search-backward key nil t)
	    (re-search-forward  key nil t))
	(progn
	  (mew-thread-move-cursor)
	  (mew-summary-display)
	  t)
      (goto-char pos)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Child
;;;

(defun mew-summary-child ()
  "Move onto the first child message of the current message."
  (interactive)
  (mew-summary-goto-message)
  (mew-decode-syntax-delete)
  (let ((my-id (mew-summary-my-id)) result)
    (cond
     ((or (null my-id) (string= my-id ""))
      (message "No child"))
     ((mew-summary-child-local my-id)
      (message "Child found"))
     ((and (y-or-n-p "No child in this folder. Find in others? ")
           (setq result (mew-summary-child-global my-id)))
      (if (eq result t)
	  (message "Child found")
	(message "%s" result)))
     (t
      (message "Child not found")))))

(defun mew-summary-child-local (my-id)
  (let ((pos (point))
        (key (mew-regex-sumsyn-par-id my-id)))
  (if (or (re-search-forward  key nil t)
          (re-search-backward key nil t))
      (progn
        (mew-thread-move-cursor)
        (mew-summary-display)
        t)
    (goto-char pos)
    nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sibling
;;;

(defun mew-summary-thread-sibling-up ()
  "Search backward by one sibling message of the current message."
  (interactive)
  (let ((pos (point))
	(par-id (mew-summary-parent-id))
	key)
    (if (or (null par-id) (string= par-id ""))
	(message "No sibling")
      (setq key (mew-regex-sumsyn-par-id par-id))
      (if (re-search-backward key nil t)
	  (progn
	    (mew-thread-move-cursor)
	    (mew-summary-display)
	    (message "Sibling found"))
	(goto-char pos)
	(message "Sibling not found")))))

(defun mew-summary-thread-sibling-down ()
  "Search forward by one sibling message of the current message."
  (interactive)
  (let ((pos (point))
	(par-id (mew-summary-parent-id))
	key)
    (if (or (null par-id) (string= par-id ""))
	(message "No sibling")
      (setq key (mew-regex-sumsyn-par-id par-id))
      (forward-line)
      (if (re-search-forward key nil t)
	  (progn
	    (mew-thread-move-cursor)
	    (mew-summary-display)
	    (message "Sibling found"))
	(goto-char pos)
	(message "Sibling not found")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread sub-functions
;;;

(defun mew-thread-move-cursor ()
  "Move cursor after indentation of thread."
  (if (and mew-use-thread-cursor
	   (mew-thread-p)
	   (mew-summary-message-number))
      (let (indent)
	(move-to-column (mew-vinfo-get-column))
	(if (setq indent (mew-thread-get-property (point)))
	    (unless (= indent 0)
	      (goto-char (mew-thread-next-property (point))))
	  (beginning-of-line)))
    (beginning-of-line)))

(defun mew-summary-thread-get-msglst (tree &optional add-separator)
  "Get a list of message in the thread order specified by TREE."
  (let ((tree-stack nil) (level 0) msgs me cld)
    (while tree
      (setq me (car tree))
      (setq cld (mew-thread-get-child me))
      (if (and mew-use-thread-separator add-separator (= level 0))
	  (setq msgs (cons "s" msgs))) ;; "s" thread-separator line
      (setq msgs (cons (mew-thread-get-msg me) msgs))
      (setq tree (cdr tree))
      (if (null cld)
	  (while (and (null tree) tree-stack)
	    (setq tree (car tree-stack))
	    (setq tree-stack (cdr tree-stack))
	    (setq level (1- level)))
	(setq tree-stack (cons tree tree-stack))
	(setq tree cld)
	(setq level (1+ level))))
    (if (and mew-use-thread-separator add-separator)
	;; discard first "s"
	(cdr (nreverse msgs))
      (nreverse msgs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hide/disclose children
;;;

(defvar mew-mark-thread-root ?+)

(defun mew-thread-toggle ()
  "If children of a message are displayed, they will hide and
\"+\" is displayed on the parent.
If the children are hidden, they will appear."
  (interactive)
  (mew-thread-only
   (mew-summary-goto-message)
   (mew-decode-syntax-delete)
   (if (looking-at (concat "^." (regexp-quote (char-to-string mew-mark-thread-root))))
       (mew-thread-graft)
     (mew-thread-prune))
   (mew-thread-move-cursor)
   (set-buffer-modified-p nil)))

(defun mew-thread-toggle-all ()
  "Toggle appearance of children for all threads."
  (interactive)
  (mew-thread-only
   (let (here)
     (save-excursion
       (goto-char (point-min))
       (mew-decode-syntax-delete)
       (while (setq here (mew-thread-next-property2 (point) (point-max) 0))
	 (goto-char here)
       (beginning-of-line)
       (if (looking-at (concat "^." (regexp-quote (char-to-string mew-mark-thread-root))))
	   (mew-thread-graft)
	 (mew-thread-prune))
       (forward-line))
       (mew-thread-move-cursor)
       (set-buffer-modified-p nil)))))

(defun mew-thread-all-prune ()
  "Hide all children."
  (interactive)
  (mew-thread-only
   (mew-summary-goto-message)
   (mew-decode-syntax-delete)
   (save-excursion
     (goto-char (point-min))
     (let (pos)
       (while (setq pos (mew-thread-next-property2 (point) (point-max) 0))
	 (goto-char pos)
	 (mew-thread-prune 'nomsg)
	 (forward-line))))
   (when (eq (get-text-property (point) 'invisible) t)
     (mew-re-search-backward-visible mew-regex-msg-or-part))
   (mew-thread-move-cursor)
   (set-buffer-modified-p nil)))

(defun mew-thread-all-graft ()
  "Display all children."
  (interactive)
  (mew-thread-only
   (mew-summary-goto-message)
   (mew-decode-syntax-delete)
   (save-excursion
     (goto-char (point-min))
     (let ((regex (concat "^." (regexp-quote (char-to-string mew-mark-thread-root)))))
       (while (re-search-forward regex nil t)
	 (mew-thread-graft 'nomsg)
	 (forward-line))))
   (mew-thread-move-cursor)
   (set-buffer-modified-p nil)))

(defun mew-thread-prune (&optional nomsg)
  (beginning-of-line)
  (let ((pos (mew-thread-next-property (point))))
    (unless (and pos (eq (mew-thread-get-property pos) 0)) ;; root
      (catch 'loop
	(while (setq pos (mew-thread-previous-property pos))
	  (when (and pos (eq (mew-thread-get-property pos) 0))
	    (throw 'loop (goto-char pos)))))))
  (beginning-of-line)
  (save-excursion
    (forward-line)
    (let ((beg (point))
	  (next (mew-thread-next-property2 (point) (point-max) 0)))
      (goto-char (or next (point-max)))
      (forward-line -1)
      (if (mew-summary-message-number) (forward-line))
      (if (= beg (point))
	  (or nomsg (message "No children to be pruned"))
	(mew-elet
	 (put-text-property beg (point) 'invisible t)
	 (goto-char beg)
	 (forward-line -1)
	 (forward-char)
	 (put-text-property (point) (1+ (point)) 'invisible t)
	 (insert mew-mark-thread-root))))))

(defun mew-thread-graft (&optional nomsg)
  (save-excursion
    (forward-line)
    (let ((start (point))
	  (next (mew-thread-next-property2 (point) (point-max) 0))
	  beg end)
      (goto-char (or next (point-max)))
      (forward-line -1)
      (if (mew-summary-message-number) (forward-line))
      (setq end (point))
      (if (= start end)
	  (or nomsg (message "No children to be leaned"))
	(mew-elet
	 (goto-char start)
	 (setq beg start)
	 (while (search-forward "\r" end t)
	   (put-text-property beg (1- (point)) 'invisible nil)
	   (forward-line)
	   (put-text-property (1- (point)) (point) 'invisible nil)
	   (setq beg (point)))
	 (goto-char start)
	 (forward-line -1)
	 (forward-char)
	 (delete-char 1)
	 (put-text-property (point) (1+ (point)) 'invisible nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread editing
;;;

(defun mew-thread-glue ()
  "Glue a thread/message to the current message as its child(ren).
The thread/message is specified with the mark(\\[set-mark-command])."
  (interactive)
  (mew-summary-msg
   (let* ((zmacs-regions nil)
	  (pos (marker-position (mark-marker))))
     (cond
      ((null pos)
       (message "No marker"))
      ((mew-thread-p)
       (mew-thread-glue-it))
      (t ;; summary or virtual
       (mew-summary-glue-it))))))

(defun mew-summary-glue-it ()
  (save-excursion
    (mew-summary-goto-message)
    (beginning-of-line)
    ;; parent
    (let ((id (mew-summary-my-id))
	  fld msg)
      (goto-char (mark-marker));; user's mark
      ;; children
      (when (mew-sumsyn-match mew-regex-sumsyn-long)
	(setq fld (mew-sumsyn-folder-name))
	(setq msg (mew-sumsyn-message-number)))
      (when (and fld msg id)
	(mew-thread-change-parent-id id)
	(mew-thread-save-xref fld msg id)
	(if (mew-virtual-p)
	    (mew-summary-change-parent-id fld msg id))
	(message "Glued")))))

(defun mew-thread-glue-it ()
  (let ((column (mew-vinfo-get-column))
	(width 0) (wd 0) (adjust 0)
	(prefix "")
	fld msg id beg end tree indent idt pbeg pindent has-child m)
    (save-excursion
      ;; parent
      (mew-summary-goto-message)
      (beginning-of-line)
      (setq id (mew-summary-my-id))
      (move-to-column column)
      (setq beg (point))
      (setq pindent (mew-thread-get-property (point)))
      (goto-char (mew-thread-next-property (point)))
      (setq end (point))
      (unless (= pindent 0)
	(while (< width mew-thread-indent-width)
	  (forward-char -1)
	  (setq width (+ width (char-width (char-after)))))
	(setq prefix (mew-buffer-substring beg (point)))
	(if (string= (mew-buffer-substring (point) end)
		     (aref mew-thread-indent-strings 0))
	    (setq prefix (concat prefix (aref mew-thread-indent-strings 2)))
	  (setq prefix (concat prefix (aref mew-thread-indent-strings 3)))))
      (when (mew-summary-goto-body)
	(while (> (point) beg)
	  (setq wd (+ wd (char-width (char-before))))
	  (forward-char -1)))
      (setq wd (/ wd mew-thread-indent-width))
      (if (> pindent wd) (setq adjust (- pindent wd)))
      (setq pindent (1+ pindent))
      (forward-line)
      ;; the next line of parent
      (setq m (point-marker))
      (unless (looking-at mew-regex-thread-separator)
	(move-to-column column)
	(if (and (mew-thread-get-property (point))
		 (= (mew-thread-get-property (point)) pindent))
	    (setq has-child t)))
      (move-to-column column)
      ;; children
      (goto-char (mark-marker));; user's mark
      (when (mew-sumsyn-match mew-regex-sumsyn-long)
	(setq fld (mew-sumsyn-folder-name))
	(setq msg (mew-sumsyn-message-number)))
      (when (and fld msg id)
	(mew-elet
	 (mew-syntax-change-parent-id id)
	 (beginning-of-line)
	 (setq beg (point))
	 (move-to-column column)
	 (setq pbeg (point))
	 (setq indent (mew-thread-get-property (point)))
	 (insert prefix)
	 (if has-child
	     (insert (aref mew-thread-indent-strings 0))
	   (insert (aref mew-thread-indent-strings 1)))
	 (goto-char (mew-thread-next-property (point)))
	 (mew-thread-put-property pbeg (point) (+ indent pindent))
	 (mew-thread-adjust-body (- pindent adjust))
	 (catch 'loop
	   (while t
	     (forward-line)
	     (move-to-column column)
	     (setq pbeg (point))
	     (setq idt (mew-thread-get-property (point)))
	     (if (or (null idt) (<= idt indent))
		 (throw 'loop nil))
	     (insert prefix)
	     (if has-child
		 (insert (aref mew-thread-indent-strings 2))
	       (insert (aref mew-thread-indent-strings 3)))
	     (goto-char (mew-thread-next-property (point)))
	     (mew-thread-put-property pbeg (point) (+ idt pindent))
	     (mew-thread-adjust-body (- pindent adjust))))
	 (beginning-of-line)
	 (setq end (point))
	 (when (looking-at mew-regex-thread-separator)
	   (forward-line)
	   (delete-region end (point)))
	 ;; This must be "buffer-substring".
	 (setq tree (buffer-substring beg end))
	 (delete-region beg end)
	 ;; the next line of parent
	 (goto-char m)
	 (insert tree)
	 (set-buffer-modified-p nil))
	(mew-summary-change-parent-id fld msg id)
	(mew-thread-save-xref fld msg id)))))

(defun mew-summary-change-parent-id (fld msg id)
  (set-buffer fld)
  (save-excursion
    (when (mew-summary-search-msg msg)
      (mew-thread-change-parent-id id))))

(defun mew-thread-change-parent-id (id)
  (mew-elet
   (mew-syntax-change-parent-id id))
  (unless (mew-virtual-p)
    (mew-summary-folder-cache-save))
  (set-buffer-modified-p nil))

(defun mew-thread-save-xref (fld msg id)
  (with-temp-buffer
    (let ((file (mew-expand-msg fld msg)))
      (mew-insert-file-contents2 file)
      (mew-header-delete-lines (list mew-x-mew-ref:))
      (goto-char (point-min))
      (mew-header-insert mew-x-mew-ref: id)
      (write-region (point-min) (point-max) file nil 'no-msg))))

(provide 'mew-thread)

;;; Copyright Notice:

;; Copyright (C) 2000-2011 Mew developing team.
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

;;; mew-thread.el ends here
