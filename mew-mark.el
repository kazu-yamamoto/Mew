;;; mew-mark.el --- Marking for Mew Summary and Virtual mode

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  2, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; undo-func
;;;

(defun mew-mark-unrefile (fld msg)
  "Delete refile state and delete the mark."
  (mew-summary-refile-remove-body)
  (unless (mew-virtual-p)
    (mew-refile-reset msg)
    (mew-summary-refile-unlog fld)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undo
;;;

(defun mew-mark-unmark ()
  (save-excursion
    (mew-summary-goto-mark)
    (when (looking-at mew-regex-mark)
      (mew-mark-remove))))

(defun mew-summary-undo (&optional count)
  "Cancel the mark in COUNT times."
  (interactive "P")
  (mew-mark-put-mark-loop 'mew-summary-undo-one count 'stayp))

(defun mew-summary-undo-one (&optional no-msg)
  "Cancel the mark on this message."
  (if (eobp)
      (or no-msg (message "No message"))
    (let (mark func fld msg)
      (save-excursion
	(mew-summary-goto-message)
	(setq mark (mew-summary-get-mark))
	(if (null mark)
	    (or no-msg (message "No mark"))
	  (setq func (mew-markdb-func-undo mark))
	  (or (fboundp func) (setq func nil))
	  (setq fld (mew-summary-folder-name))
	  (setq msg (mew-summary-message-number))
	  (mew-mark-unmark)
	  (if func (funcall func fld msg))
	  (if (mew-virtual-for-one-summary)
	      (mew-summary-unmark-in-physical fld msg func)))))))

;;

(defun mew-summary-undo-all ()
  "Cancel all marks according to what you input."
  (interactive)
  (let ((char (mew-input-mark)))
    (if char (mew-mark-undo-mark char))))

(defun mew-mark-undo-mark (mark &optional no-msg virtual-only)
  "Undo MARK on the entire buffer.
If optional argument NO-MSG is non-nil, no message is displayed."
  (or no-msg (message "Unmarking..."))
  (mew-decode-syntax-delete)
  (let* ((regex (mew-mark-regex mark))
	 (func (mew-markdb-func-undo mark))
	 (case-fold-search nil)
	 (reviewp (char-equal mark mew-mark-review))
	 (one-summary (and (not virtual-only) (mew-virtual-for-one-summary)))
	 alist fld msg reviews)
    (or (fboundp func) (setq func nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(setq fld (mew-summary-folder-name))
	(setq msg (mew-summary-message-number))
	(mew-mark-remove)
	(if one-summary (mew-mark-alist-set alist fld msg))
	(when reviewp (setq reviews (cons msg reviews)))
	(if func (funcall func fld msg))
	(forward-line))
      (set-buffer-modified-p nil)
      (mew-sinfo-set-mark-review (nreverse reviews))
      (if one-summary (mew-summary-unmark-in-physical-alist alist func))))
  (or no-msg (message "Unmarking...done")))

;;

(defun mew-summary-mark-undo-all ()
  "Unmark all message marked with 'o' or 'D' or 'X'."
  (interactive)
  (message "Unmarking...")
  (dolist (mark mew-summary-mark-undo-marks)
    (mew-mark-undo-mark mark 'no-msg))
  (message "Unmarking...done"))

;;

(defun mew-summary-redo ()
  "\\<mew-summary-mode-map>Recover the `*' marks which are canceled by the last `\\[mew-summary-undo-all]*'."
  (interactive)
  (let ((mark mew-mark-review)
	(reviews (mew-sinfo-get-mark-review)))
    (save-excursion
      (mew-mark-undo-mark mark 'no-msg)
      (goto-char (point-min))
      (dolist (msg reviews)
	(when (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
	  (mew-mark-put mark)
	  (forward-line))))
    (set-buffer-modified-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic functions and macros for mark
;;;

(defun mew-summary-get-mark ()
  "Get a mark on the current message."
  (save-excursion
    (mew-summary-goto-mark)
    (when (looking-at mew-regex-mark)
      (mew-sumsyn-mark)))) ;; return char

(defun mew-summary-markable ()
  (let ((mark (mew-summary-get-mark))
	(case-fold-search nil))
    (or (null mark) (char-equal mark mew-mark-unread))))

(defun mew-summary-mark-as (mark)
  "Mark this message with MARK if possible."
  (when (mew-summary-markable)
    (mew-mark-put mark)))

(defun mew-mark-put (mark)
  (save-excursion
    (mew-summary-goto-mark)
    (let* ((beg (point)) (end (1+ beg)))
      (mew-elet
       ;; This code is awkward but for invisible.
       (forward-char 1)
       (insert-and-inherit (char-to-string mark)) ;; inherit highlight
       (forward-char -2)
       (delete-char 1)
       (mew-front-nonsticky beg end)
       (mew-highlight-mark-line mark))
      (mew-mark-hist-set (mew-summary-message-number) mark))))

(defun mew-mark-remove ()
  (save-excursion
    (mew-summary-goto-mark)
    (let* ((mark mew-mark-read)
	   (beg (point)) (end (1+ beg)))
      (mew-elet
       ;; This code is awkward but for invisible.
       (forward-char 1)
       (insert-and-inherit (char-to-string mark)) ;; inherit highlight
       (forward-char -2)
       (delete-char 1)
       (mew-front-nonsticky beg end)
       (mew-highlight-unmark-line))
      (mew-mark-hist-set (mew-summary-message-number) mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Entire buffer
;;;

(defun mew-summary-mark-exist-p (mark-list)
  "See if this Summary mode has one or more marked messages."
  (let ((regex (mew-mark-list-regex mark-list)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regex nil t))))

(defun mew-summary-mark-collect (mark &optional begin end)
  "This function returns a list of message number."
  (save-excursion
    (let ((regex (mew-mark-regex mark))
	  (msglist nil)
	  (case-fold-search nil))
      (goto-char (if begin begin (point-min)))
      (while (re-search-forward regex end t)
	(setq msglist (cons (mew-summary-message-number) msglist)))
      (nreverse msglist))))

(defun mew-summary-mark-collect2 (mark)
  "For Virtual mode, this function returns a list of
cons pairs of folder name and message number."
  (save-excursion
    (let ((regex (mew-mark-regex mark))
          (msglist nil)
	  (case-fold-search nil))
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (setq msglist (cons
                       (cons
                        (mew-summary-folder-name)
                        (mew-summary-message-number))
                       msglist)))
      (nreverse msglist))))

(defun mew-summary-mark-collect4 ()
  (save-excursion
    (let (ret mrk msg)
      (goto-char (point-min))
      (while (re-search-forward mew-regex-mark nil t)
	(setq mrk (mew-sumsyn-mark))
	(setq msg (mew-summary-message-number))
	(setq ret (cons (list msg mrk) ret))
	(forward-line))
      (nreverse ret))))

(defun mew-summary-mark-collect5 ()
  (save-excursion
    (let ((beg (mew-net-invalid-cache-start))
	  ret mrk msg)
      (when beg
	(goto-char beg)
	(while (re-search-forward mew-regex-mark nil t)
	  (setq mrk (mew-sumsyn-mark))
	  (setq msg (mew-summary-message-number))
	  (setq msg (number-to-string (string-to-number msg)))
	  (setq ret (cons (list msg mrk) ret))
	  (forward-line))
	(nreverse ret)))))

(defun mew-summary-mark-recover (mdb &optional refdb refs)
  (let ((opos (point))
	(case-fold-search nil)
	msg mrk new-ref ref-ent)
    (goto-char (point-min))
    (dolist (ent mdb)
      (mew-set '(msg mrk) ent)
      (when (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
	(if (or (not (char-equal mrk mew-mark-refile)) (null refdb))
	    (mew-mark-put mrk)
	  (when (setq ref-ent (assoc msg refdb))
	    (mew-mark-put mrk)
	    (setq new-ref (cons ref-ent new-ref))))
	(forward-line)))
    (dolist (ref refs)
      (when (setq ref-ent (assoc ref refdb))
	(setq new-ref (cons ref-ent new-ref))))
    (goto-char opos)
    (nreverse new-ref)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base function
;;;

(defun mew-mark-afterstep (mark case)
  "Move the cursor after marking according to MARK's CASE.
See also mew-mark-afterstep-spec."
  (let ((action (mew-markas-nth mark case)))
    (cond
     ((eq action 0)
      ()) ;; stay
     ((eq action 1)
      (mew-summary-goto-message)
      (mew-decode-syntax-delete)
      (cond
       ((eq mew-summary-mark-direction 'up)
	(forward-line -1))
       ((eq mew-summary-mark-direction 'down)
	(forward-line))
       ((eq mew-summary-mark-direction 'next)
	(if (eq (mew-sinfo-get-direction) 'up)
	    (forward-line -1)
	  (forward-line)))))
     ((eq action 2)
      (mew-summary-goto-message)
      (mew-decode-syntax-delete)
      (beginning-of-line)
      (mew-push-mark)
      (let ((mew-inherit-after-marking t))
	(mew-summary-display-after mew-summary-mark-direction))))))

(mew-defstruct mark-hist msg mark)

(defun mew-mark-hist-set (msg mark)
  (let* ((hist (mew-sinfo-get-mark-hist))
	 (ent (assoc msg hist))
	 (num (string-to-number msg))
	 curr prev)
    (cond
     (ent
      (if mark (setcar (nthcdr 1 ent) mark)))
     (t
      (setq ent (mew-make-mark-hist :msg msg :mark mark))
      (setq curr hist)
      (catch 'loop
	(while curr
	  (if (> (string-to-number (nth 0 (car curr))) num)
	      (progn
		(if (null prev)
		    (setq hist (cons ent hist))
		  (setcdr prev (cons ent (cdr prev))))
		(throw 'loop nil)))
	  (setq prev curr)
	  (setq curr (cdr curr)))
	(setq hist (nconc hist (list ent))))
      (mew-sinfo-set-mark-hist hist)))))

(defun mew-mark-put-mark (newmark &optional no-msg valid-only)
  "Put the NEWMARK on the current line if possible.
If NO-MSG is non-nil, no message is displayed.
NO-MSG also means that this function is being called in loop."
  (mew-summary-msg-or-part
   (let (oldmark oldlevel oldname newlevel newname case msg fld marked validp)
     (save-excursion
       (mew-summary-goto-message)
       (when (mew-sumsyn-match mew-regex-sumsyn-short)
	 (setq msg (mew-sumsyn-message-number))
	 (setq fld (mew-sumsyn-folder-name))
	 (setq validp (or (not valid-only) (mew-msg-validp msg))))
       (if (not validp)
	   (unless no-msg (message "Cannot mark this invalid message with '%c'" newmark))
	 (setq oldmark (mew-summary-get-mark))
	 (setq oldlevel (mew-markdb-level oldmark))
	 (setq oldname (mew-markdb-name oldmark))
	 (setq newlevel (mew-markdb-level newmark))
	 (setq newname (mew-markdb-name newmark))
	 (cond
	  ((null oldmark);; no mark
	   (setq case 1)
	   (mew-mark-put newmark)
	   (setq marked t))
	  ((eq oldmark newmark)
	   (setq case 2)
	   (or no-msg
	       (mew-markdb-statefullp oldmark)
	       (message "Already marked as '%s'" oldname)))
	  ((< oldlevel newlevel)
	   (setq case 3)
	   (mew-summary-undo-one no-msg)
	   (mew-mark-put newmark)
	   (setq marked t))
	  ((= oldlevel newlevel)
	   (cond
	    ((mew-markdb-statefullp oldmark)
	     (if (or no-msg
		     (y-or-n-p (format "Already marked as '%s'. %s it? "
				       oldname (mew-capitalize newname))))
		 (progn
		   (setq case 4)
		   (mew-summary-undo-one no-msg)
		   (mew-mark-put newmark)
		   (setq marked t))
	       (setq case 5)))
	    (t
	     (setq case 6)
	     (mew-summary-undo-one no-msg)
	     (mew-mark-put newmark)
	     (setq marked t))))
	  (t ;; > oldlevel newlevel
	   (setq case 7)
	   (message "Cannot mark here because '%s' is stronger than '%s'"
		    oldname newname)))))
     (when validp
       (if (and marked (mew-virtual-for-one-summary))
	   (mew-summary-mark-in-physical fld msg newmark))
       (or no-msg (mew-mark-afterstep newmark case))
       (set-buffer-modified-p nil)))))

(defun mew-mark-put-mark-loop (func count stayp)
  "Unless COUNT is numeric, just call FUNC once.
The cursor moves forward. STAYP has no effect.
If COUNT is positive, call FUNC in COUNT times moving the cursor forward.
If COUNT is negative, call FUNC in COUNT times moving the cursor backward.
If COUNT is numeric and STAYP is non-nil, the cursor stays in the
original position."
  (if (and (not (integerp count)) (mew-mark-active-p))
      (let ((begend (mew-summary-get-region)))
	(goto-char (car begend))
	(setq stayp nil)
	(setq count (count-lines (point) (cdr begend)))
	(mew-mark-put-mark-loop1 func count stayp))
    (mew-mark-put-mark-loop1 func count stayp)))

(defun mew-mark-put-mark-loop1 (func count stayp)
  (when (and func (fboundp func))
    (mew-summary-msg-or-part
     (if (integerp count)
	 (let ((start (point)))
	   (mew-decode-syntax-delete)
	   ;; positive loop
	   (while (and (> count 0) (not (eobp)))
	     (setq count (1- count))
	     (funcall func 'no-msg)
	     (forward-line))
	   ;; negative loop
	   (while (< count 0)
	     (if (bobp)
		 ;; need to call the func
		 (setq count 0)
	       (setq count (1+ count)))
	     (funcall func 'no-msg)
	     (forward-line -1))
	   (and stayp (goto-char start)))
       ;; just one
       (funcall func))
     (set-buffer-modified-p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Physical
;;;

(defun mew-summary-mark-in-physical (fld msg new-mark)
  (save-excursion
    (when (get-buffer fld)
      (set-buffer fld)
      (save-excursion
	(when (mew-summary-search-msg msg)
	  (mew-mark-put new-mark)
	  (set-buffer-modified-p nil))))))

(defun mew-summary-unmark-in-physical (fld msg &optional func)
  (save-excursion
    (when (get-buffer fld)
      (set-buffer fld)
      (save-excursion
	(when (mew-summary-search-msg msg)
	  (mew-mark-remove)
	  (if func (funcall func fld msg))
	  (set-buffer-modified-p nil))))))

(defmacro mew-mark-alist-set (alist fld msg)
  `(let ((imsg (string-to-number ,msg))
	 (fld-msgs (assoc ,fld ,alist)))
     (if fld-msgs
	 (nconc fld-msgs (list imsg))
       (setq ,alist (cons (list ,fld imsg) ,alist)))))

(defun mew-summary-mark-in-physical-alist (alist mark &optional func)
  (save-excursion
    (let (fld msg msgs)
      (dolist (ent alist)
	(setq fld (car ent))
	(setq msgs (sort (copy-sequence (cdr ent)) '<)) ;; sort has side effect
	(when (get-buffer fld)
	  (set-buffer fld)
	  (save-excursion
	    (goto-char (point-min))
	    (dolist (m msgs)
	      (setq msg (number-to-string m))
	      (when (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
		(if func (funcall func))
		(mew-mark-put mark)
		(forward-line)))
	    (set-buffer-modified-p nil)))))))

(defun mew-summary-unmark-in-physical-alist (alist func)
  (save-excursion
    (let (fld msg msgs)
      (dolist (ent alist)
	(setq fld (car ent))
	(setq msgs (sort (copy-sequence (cdr ent)) '<)) ;; sort has side effect
	(when (get-buffer fld)
	  (set-buffer fld)
	  (save-excursion
	    (goto-char (point-min))
	    (dolist (m msgs)
	      (setq msg (number-to-string m))
	      (when (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
		(mew-mark-remove)
		(if func (funcall func fld msg))
		(forward-line)))
	    (set-buffer-modified-p nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Review: "*" in Summary mode
;;;

(defun mew-summary-review (&optional count)
  "\\<mew-summary-mode-map>
Put the review mark (default is '*') in COUNT times.
Use '\\[mew-summary-display-review-down]' or '\\[mew-summary-display-review-up]' to jump to a message marked with '*'.
See also '\\[mew-summary-mark-refile]', '\\[mew-summary-mark-delete]', '\\[mew-summary-mark-regexp]', and '\\[mew-summary-mark-all]'."
  (interactive "P")
  (mew-mark-put-mark-loop 'mew-summary-review-one count nil))

(defun mew-summary-review-one (&optional no-msg)
  "Put the review mark (default is '*') on this message."
  (mew-mark-put-mark mew-mark-review no-msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Escape: "$" in Summary mode
;;;

(defun mew-summary-escape (&optional count)
  "\\<mew-summary-mode-map>
Put the escape mark (default is '$') in COUNT times."
  (interactive "P")
  (mew-mark-put-mark-loop 'mew-summary-escape-one count nil))

(defun mew-summary-escape-one (&optional no-msg)
  "Put the escape mark (default is '$') on this message."
  (mew-mark-put-mark mew-mark-escape no-msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete: "D" in Summary mode
;;;

(defun mew-summary-delete (&optional count)
  "Put the delete mark (default is 'D') in COUNT times."
  (interactive "P")
  (mew-summary-not-in-nntp
   (mew-mark-put-mark-loop 'mew-summary-delete-one count nil)))

(defun mew-summary-delete-one (&optional no-msg)
  "Put the delete mark (default is 'D') on this message."
  (mew-mark-put-mark mew-mark-delete no-msg 'valid-only))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unlink: "X" in Summary mode
;;;

(defun mew-summary-unlink (&optional count)
  "Put the unlink mark (default is 'X') in COUNT times."
  (interactive "P")
  (mew-summary-not-in-nntp
   (mew-mark-put-mark-loop 'mew-summary-unlink-one count nil)))

(defun mew-summary-unlink-one (&optional no-msg)
  "Put the unlink mark (default is 'X') on this message."
  (mew-mark-put-mark mew-mark-unlink no-msg 'valid-only))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unread: "U" in Summary mode
;;;

(defun mew-summary-unread (&optional count)
  "\\<mew-summary-mode-map>
Put the unread mark (default is 'U') in COUNT times."
  (interactive "P")
  (mew-mark-put-mark-loop 'mew-summary-unread-one count nil))

(defun mew-summary-unread-one (&optional no-msg)
  "Put the unread mark (default is 'U') on this message."
  (mew-mark-put-mark mew-mark-unread no-msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; All messages
;;;

(defun mew-summary-mark-all (&optional arg)
  "Put the '*' mark onto all messages which are not marked."
  (interactive "P")
  (mew-decode-syntax-delete)
  (if (mew-mark-active-p) (setq arg t))
  (if arg
      (let ((begend (mew-summary-get-region)))
	(mew-summary-mark-all-region (car begend) (cdr begend)))
    (mew-summary-mark-all-region (point-min) (point-max))))

(defun mew-summary-mark-all-region (beg end)
  "Put the '*' mark onto all messages which are not marked between
BEG and END."
  (interactive "r")
  (let ((regex (mew-mark-regex mew-sp)) ;; not marked
	(mark mew-mark-review) ;; someday ...
	(one-summary (mew-virtual-for-one-summary))
	fld msg alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regex end t)
	(mew-summary-mark-as mark)
	(when one-summary
	  (setq fld (mew-summary-folder-name))
	  (setq msg (mew-summary-message-number))
	  (mew-mark-alist-set alist fld msg))
	(forward-line))
      (set-buffer-modified-p nil))
    (if one-summary (mew-summary-mark-in-physical-alist alist mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Regex
;;;

(defun mew-summary-search-regexp-visible (regex)
  (catch 'loop
    (while (re-search-forward regex nil t)
      (beginning-of-line)
      (unless (search-forward "\r" (match-end 0) t)
	(throw 'loop t))
      (forward-line))))

(defun mew-summary-mark-regexp (&optional args)
  "Put the '*' mark onto all messages matched to a regular expression."
  (interactive "P")
  (mew-decode-syntax-delete)
  (let ((regex (read-string "Regexp: "))
	(mark mew-mark-review) ;; someday ...
	(one-summary (mew-virtual-for-one-summary))
        (n 0)
	fld msg alist)
    (while (string= regex "")
      (setq regex (read-string "Regexp: ")))
    (save-excursion
      (goto-char (point-min))
      (while (if args
		 (re-search-forward regex nil t)
	       (mew-summary-search-regexp-visible regex))
	(when (and (mew-summary-markable)
		   (mew-sumsyn-match mew-regex-sumsyn-short))
	  (when one-summary
	    (setq fld (mew-sumsyn-folder-name))
	    (setq msg (mew-sumsyn-message-number))
	    (mew-mark-alist-set alist fld msg))
	  (mew-mark-put mark)
	  (setq n (1+ n)))
	(forward-line))
      (set-buffer-modified-p nil))
    (if one-summary (mew-summary-mark-in-physical-alist alist mark))
    (cond
     ((= n 1)
      (message "1 message marked"))
     ((> n 1)
      (message "%d messages marked" n))
     (t
      (message "No message to be marked")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exchange
;;;

(defun mew-summary-exchange-mark (oldmark newmark &optional valid-only)
  (let ((regex (mew-mark-regex oldmark))
	(case-fold-search nil)
	(one-summary (mew-virtual-for-one-summary))
	(i 0)
	fld msg alist)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(when (mew-sumsyn-match mew-regex-sumsyn-short)
	  (setq fld (mew-sumsyn-folder-name))
	  (setq msg (mew-sumsyn-message-number))
	  (when (or (not valid-only) (mew-msg-validp msg))
	    (mew-mark-put newmark)
	    (if one-summary (mew-mark-alist-set alist fld msg))))
	(forward-line)
	(setq i (1+ i))))
    (if (= i 0)
	(message "No marked messages")
      (set-buffer-modified-p nil)
      (if one-summary (mew-summary-mark-in-physical-alist alist newmark)))))

(defun mew-summary-mark-delete ()	;; * -> D
  "Put the delete mark onto all messages marked with '*'."
  (interactive)
  (mew-summary-not-in-nntp
   (mew-summary-exchange-mark mew-mark-review mew-mark-delete 'valid-only)))

(defun mew-summary-mark-unlink ()	;; * -> X
  "Put the delete mark onto all messages marked with '*'."
  (interactive)
  (mew-summary-exchange-mark mew-mark-review mew-mark-unlink 'valid-only))

(defun mew-summary-mark-escape ()	;; * -> $
  "Change the '*' mark into the '$' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-review mew-mark-escape))

(defun mew-summary-mark-review ()	;; $ -> *
  "Change the '$' mark into the '*' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-escape mew-mark-review))

(defun mew-summary-mark-unread ()	;; * -> U
  "Change the '*' mark into the 'U' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-review mew-mark-unread))

(defun mew-summary-mark-swap ()		;; $ <-> *
  "Swap the '$' mark and the '*' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-escape mew-mark-tmp)
  (mew-summary-exchange-mark mew-mark-review mew-mark-escape)
  (mew-summary-exchange-mark mew-mark-tmp mew-mark-review))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Marking duplicated messages
;;;

(defun mew-summary-mark-duplicated (&optional arg)
  "Put the mark specified by 'mew-mark-duplicated' on duplicated
messages. If called with '\\[universal-argument]', process in the
region."
  (interactive "P")
  (mew-summary-not-in-queue
   (mew-summary-not-in-draft
    (if (and (or (eq mew-mark-duplicated mew-mark-delete)
		 (eq mew-mark-duplicated mew-mark-unlink))
	     (eq mew-summary-mark-duplicated-skip nil))
	(message "Cannot mark because messages may lost by this setting")
      (let ((reversep (eq mew-summary-mark-duplicated-skip 'last))
	    (count 0) my-id dup-id size ids beg end region)
	(if (mew-mark-active-p) (setq arg t))
	(cond
	 (arg
	  (setq region (mew-summary-get-region))
	  (setq beg (car region))
	  (setq end (cdr region)))
	 (t
	  (setq beg (point-min))
	  (setq end (point-max))))
	(message "Marking duplications...")
	(save-excursion
	  ;; from mew-summary-thread-region in mew-threaed.el
	  (setq size (count-lines beg end))
	  (cond
	   ((<= size 211)
	    (setq size 211))
	   ((<= size 1511)
	    (setq size 1511))
	   ((<= size 7211)
	    (setq size 7211))
	   (t
	    (setq size 18211)))
	  (setq ids (make-vector size 0)) ;; hash
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char (if reversep (point-max) (point-min)))
	    (catch 'loop
	      (while t
		(when (and (mew-summary-message-number)
			   (mew-summary-markable))
		  (setq my-id (mew-summary-my-id))
		  (when (> (length my-id) 0)
		    (setq dup-id (intern-soft my-id ids))
		    (if (null dup-id)
			;; first time (no duplication)
			(set (intern my-id ids) t)
		      (when (symbol-value dup-id)
			;; second time (first duplication)
			(unless mew-summary-mark-duplicated-skip
			  (save-excursion
			    (when (re-search-backward
				   (mew-regex-sumsyn-my-id my-id) nil t)
			      (mew-mark-put-mark mew-mark-duplicated 'no-msg)
			      (setq count (1+ count))))
			  (set (intern my-id ids) nil)))
		      (mew-mark-put-mark mew-mark-duplicated 'no-msg)
		      (setq count (1+ count)))))
		(cond
		 (reversep
		  (beginning-of-line)
		  (if (bobp)
		      (throw 'loop t)
		    (forward-line -1)))
		 (t
		  (forward-line 1)
		  (if (eobp)
		      (throw 'loop t))))))))
	(cond
	 ((= count 0)
	  (message "Marking duplications...done  (no duplication)"))
	 ((= count 1)
	  (message "Marking duplications...done  (1 msg is marked)"))
	 (t
	  (message "Marking duplications...done  (%d msgs are marked)" count))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reviewing
;;;

(defun mew-summary-set-walk-mark ()
  (let* ((msg (format "Input mark(%c): " mew-mark-default-walk))
	 (mew-mark-spec (cons (list 13) mew-mark-spec)) ;; adding \r
	 (char (mew-input-mark msg)))
    (cond
     ((null char)
      ;; mew-input-mark displays a message.
      )
     ((char-equal char ?\r)
      (setq mew-mark-walk mew-mark-default-walk)
      (message "Target mark was set to '%c'" mew-mark-walk))
     (char
      (setq mew-mark-walk char)
      (message "Target mark was set to '%c'" mew-mark-walk)))))

(defun mew-summary-down-mark (mark)
  (let ((case-fold-search nil))
    (forward-line)
    (cond
     ((re-search-forward (mew-mark-regex mark) nil t)
      (beginning-of-line)
      t)
     (t
      (forward-line -1)
      (message "No more marked messages")
      nil))))

(defun mew-summary-display-review-down (&optional arg)
  "Jump to the message marked with '*' below.
If called with '\\[universal-argument]', you can change the target mark.
After that, this command jumps to the message marked with
the specified mark."
  (interactive "P")
  (if arg
      (mew-summary-set-walk-mark)
    (if (mew-summary-down-mark mew-mark-walk)
	(mew-summary-display))))

(defun mew-summary-up-mark (mark)
  (let ((case-fold-search nil))
    (cond
     ((re-search-backward (mew-mark-regex mark) nil t)
      t)
     (t
      (message "No more marked messages")
      nil))))

(defun mew-summary-display-review-up (&optional arg)
  "Jump to the message marked with '*' above.
If called with '\\[universal-argument]', you can change the target mark.
After that, this command jumps to the message marked with
the specified mark."
  (interactive "P")
  (if arg
      (mew-summary-set-walk-mark)
    (if (mew-summary-up-mark mew-mark-walk)
	(mew-summary-display))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mark exchange
;;;

(defun mew-summary-input-exchangeable-mark (msg)
  (let (mark)
    (catch 'loop
      (while t
	(setq mark (mew-input-mark msg))
	(cond
	 ((null mark)
	  (message "That is not a mark")
	  (mew-let-user-read))
	 ((mew-markdb-statefullp mark)
	  (message "'%c' cannot be exchanged" mark)
	  (mew-let-user-read))
	 (t
	  (throw 'loop mark)))))))

(defun mew-summary-exchange-marks ()
  "Exchange the first input mark to the second one.
The 'o' mark is not exchangeable."
  (interactive)
  (let* ((from (mew-summary-input-exchangeable-mark "Input mark from: "))
	 (to  (mew-summary-input-exchangeable-mark "Input mark to: "))
	 (regex (mew-mark-regex from))
	 (case-fold-search nil))
    (unless (char-equal from to)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (mew-summary-undo-one 'nomsg)
	  (mew-mark-put-mark to 'nomsg))
	(forward-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clean up marks!
;;;

(defun mew-mark-init ()
  (add-hook 'kill-emacs-hook 'mew-sinfo-clean-up))

(defun mew-sinfo-clean-up ()
  "Saving marks. Typically called by kill-emacs-hook."
  (remove-hook 'kill-emacs-hook 'mew-sinfo-clean-up)
  (mew-decode-syntax-delete)
  (save-excursion
    (dolist (buf mew-buffers)
      (when (bufferp (get-buffer buf))
	(set-buffer buf)
	(when (mew-summary-p)
	  (mew-sinfo-save))))))

(provide 'mew-mark)

;;; Copyright Notice:

;; Copyright (C) 1997-2014 Mew developing team.
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

;;; mew-mark.el ends here
