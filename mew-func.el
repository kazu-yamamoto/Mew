;;; mew-func.el --- Basic functions for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mode
;;;

(defun mew-summary-p ()
  (eq major-mode 'mew-summary-mode))

(defun mew-virtual-p ()
  (eq major-mode 'mew-virtual-mode))

(defun mew-message-p ()
  (eq major-mode 'mew-message-mode))

(defun mew-draft-p ()
  (eq major-mode 'mew-draft-mode))

(defun mew-header-p ()
  (eq major-mode 'mew-header-mode))

(defun mew-draft-or-header-p ()
  (memq major-mode '(mew-draft-mode mew-header-mode)))

(defun mew-selection-p ()
  (eq (mew-vinfo-get-mode) 'selection))

(defun mew-thread-p ()
  (eq (mew-vinfo-get-mode) 'thread))

(defun mew-summary-or-virtual-p ()
  (or (mew-summary-p) (mew-virtual-p)))

(defun mew-virtual-for-one-summary ()
  (mew-vinfo-get-physical-folder))

(defun mew-pickable-p ()
  (or (mew-summary-p) (mew-virtual-for-one-summary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List functions
;;;

(defun mew-case-equal (str1 str2)
  (string= (downcase str1) (downcase str2)))

(defun mew-folder-recursive-match (key folder)
  "Initial substring match for folders.
If FOLDER is a sub-folder of KEY or KEY itself, t is returned."
  ;; file-name-as-directory should be first
  ;; because the path separator may be regex-non-safe.
  (let ((regex (if (string-match "^[-+%$]$" key)
		   (mew-folder-regex key)
		 (mew-folder-regex (file-name-as-directory key)))))
    (string-match regex (file-name-as-directory folder))))

(defun mew-member-case-equal (str list)
  "Return the position equal to STR in LIST. Case is ignored."
  (let ((n 0) (dstr (downcase str)))
    (catch 'member
      (dolist (x list)
	(if (string= (downcase x) dstr) (throw 'member n))
	(setq n (1+ n))))))

(defun mew-member* (x list)
  "Member in a nested list."
  (catch 'found
    (dolist (ent list)
      (if (consp ent)
	  (if (mew-member* x ent) (throw 'found t))
	(if (equal x ent) (throw 'found t))))))

(defun mew-member-match (str list &optional ignore-case)
  "Return the position matched to STR in LIST. If
IGNORE-CASE is t, matching is performed by ignoring case."
  (let ((n 0) (case-fold-search ignore-case))
    (catch 'member
      (dolist (x list)
	(if (string-match x str) (throw 'member n))
	(setq n (1+ n))))))

(defun mew-member-match2 (regex list &optional ignore-case)
  (let ((case-fold-search ignore-case))
    (catch 'member
      (dolist (x list)
	(if (string-match regex x) (throw 'member x))))))

(defun mew-uniq-list (lst)
  "Destructively uniqfy elements of LST.
This is O(N^2). So, do not use this function with a large LST."
  (let ((tmp lst))
    (while tmp (setq tmp (setcdr tmp (delete (car tmp) (cdr tmp))))))
  lst)

(defun mew-uniq-alist (alist)
  "Uniqfy elements of ALIST."
  (let ((vec (make-vector 511 0)) ;; hash
	str ret)
    (dolist (ent alist)
      (setq str (car ent))
      (cond
       ((not (stringp str))
	(setq ret (cons ent ret)))
       ((intern-soft str vec)
	())
       (t
	(setq ret (cons ent ret))
	(intern str vec))))
    (nreverse ret)))

(defun mew-delete (key alist)
  "Destructively delete elements whose first member is equal to key"
  (if (null key)
      alist
    (let (ret)
      (while (and (listp (nth 0 alist)) (equal (car (nth 0 alist)) key))
	(setq alist (cdr alist)))
      (setq ret alist)
      (while alist ;; cannot use dolist
	(if (and (listp (nth 1 alist)) (equal (car (nth 1 alist)) key))
	    (setcdr alist (cdr (cdr alist)))
	  (setq alist (cdr alist))))
      ret)))

(defmacro mew-ntake (n lst)
  `(if (> (length ,lst) ,n) (setcdr (nthcdr (1- ,n) ,lst) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Associative list functions
;;;

(defun mew-assoc-equal (key alist nth)
  (let (n)
    (catch 'loop
      (dolist (a alist)
	(setq n (nth nth a))
	(if (or (equal n key) (eq n t)) (throw 'loop a))))))

(defun mew-assoc-case-equal (key alist nth)
  (let ((skey (downcase key)) n)
    (catch 'loop
      (dolist (a alist)
	(setq n (nth nth a))
	(if (or (and (stringp n) (string= (downcase n) skey))
		(eq n t))
	    (throw 'loop a))))))

(defun mew-assoc-match (key alist nth)
  "Return list in ALIST that KEY regex is matched to its NTH element.
Case is ignored. Note that the NTH element is 't',
the list is always selected."
  (let ((case-fold-search t) n)
    (catch 'loop
      (dolist (a alist)
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match key n))
		(equal n key) (eq n t))
	    (throw 'loop a))))))

(defun mew-assoc-match2 (key alist nth)
  "Return list in ALIST whose NTH regex is matched to KEY.
Case is ignored. Note that the NTH element is 't',
the list is always selected."
  (let ((case-fold-search t) n)
    (catch 'loop
      (dolist (a alist)
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match n key))
		(equal n key) (eq n t))
	    (throw 'loop a))))))

(defun mew-assoc-match3 (key alist nth)
  "Return list in ALIST whose NTH regex is matched to KEY.
Case is ignored. Note that the NTH element is 't',
the list is always selected. The deference from mew-assoc-match2
is that this returns the position of a selected list in addition
to the list itself."
  (let ((case-fold-search t) (i 0) n)
    (catch 'loop
      (dolist (a alist)
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match n key))
		(equal n key) (eq n t))
	    (throw 'loop (cons i a)))
	(setq i (1+ i))))))

(defun mew-assoc-member (key lol nth)
  "Return a list member of LoL whose NTH list contains
a member equal to KEY."
  (mew-assoc-member-base key lol nth 'member))

(defun mew-assoc-member-case-equal (key lol nth)
  "Return a list member of LoL whose NTH list contains
a member equal to KEY ignoring case."
  (mew-assoc-member-base key lol nth 'mew-member-case-equal))

(defun mew-assoc-member-base (key lol nth func)
  "Return a list member of LoL whose NTH list contains KEY
in the context of FUNC."
  (catch 'loop
    (dolist (l lol)
      (if (and (listp (nth nth l)) (funcall func key (nth nth l)))
	  (throw 'loop l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; backward compatibility of alist
;;;

(defalias 'mew-alist-get-key 'car)
(defun mew-alist-get-value (ent)
  (let ((value (cdr ent)))
    (if (consp value)
	(car value) ;; new
      value))) ;; old

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; getting next
;;;

(defun mew-get-next (LIST MEM)
  "Return of a member in LIST which is the next member of MEM."
  (let (frst next crnt)
    (setq frst (car LIST))
    (setq LIST (cdr LIST))
    (setq next (car LIST))
    (if (equal frst MEM)
	(if next next frst)
    (catch 'loop
      (while LIST ;; cannot use dolist
	(setq crnt next)
	(setq LIST (cdr LIST))
	(setq next (car LIST))
	(if (equal crnt MEM)
	    (throw 'loop (if next next frst))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifying list
;;;

(defmacro mew-add-first (variable value)
  `(setq ,variable (cons ,value ,variable)))

(defmacro mew-addq (variable value)
  `(if (and ,value (not (member ,value ,variable)))
       (setq ,variable (cons ,value ,variable))))

(defmacro mew-insert-after (variable value key)
  `(let ((var ,variable))
     (catch 'loop
       (while var ;; cannot use dolist
	 (if (equal (nth 0 (car var)) ,key)
	     (throw 'loop (setcdr var (cons ,value (cdr var)))))
	 (setq var (cdr var))))))

(defmacro mew-replace-with (variable value key)
  `(let ((var ,variable))
     (catch 'loop
       (while var ;; cannot use dolist
	 (if (equal (nth 0 (car var)) ,key)
	     (throw 'loop (setcar var ,value)))
	 (setq var (cdr var))))))

(defmacro mew-remove-entry (variable key)
  `(let ((crn ,variable) prv)
     (if (equal (nth 0 (car crn)) ,key)
	 (setq ,variable (cdr crn))
       (setq prv crn)
       (setq crn (cdr crn))
       (catch 'loop
	 (while crn ;; cannot use dolist
	   (if (equal (nth 0 (car crn)) ,key)
	       (throw 'loop (setcdr prv (cdr crn))))
	   (setq prv crn)
	   (setq crn (cdr crn)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; String
;;;

(defun mew-make-string (len)
  (make-string len ?a))

(defun mew-replace-character (string from to)
  "Replace characters equal to FROM to TO in STRING."
  (dotimes (cnt (length string) string)
    (if (char-equal (aref string cnt) from)
	(aset string cnt to))))

(defun mew-replace-white-space (string)
  "Replace white characters to a space."
  (while (string-match "[\n\t]+" string)
    (setq string (replace-match " " nil t string)))
  (while (string-match "  +" string)
    (setq string (replace-match " " nil t string)))
  string)

(defun mew-replace-white-space2 (string)
  "Replace white characters to under score."
  (while (string-match "[\n\t\r ]+" string)
    (setq string (replace-match "_" nil t string)))
  string)

(defun mew-capitalize (ostr)
  "Syntax table independent version of capitalize.
Words are separated by '/' and '-'."
  (let* ((len (length ostr))
	 (nstr (mew-make-string len))
	 (topp t) c)
    (dotimes (i len nstr)
      (setq c (aref ostr i))
      (cond
       (topp
	(aset nstr i (upcase c))
	(setq topp nil))
       ((or (char-equal c ?/) (char-equal c ?-))
	(aset nstr i c)
	(setq topp t))
       (t
	(aset nstr i (downcase c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insertion
;;;

(defun mew-insert (form str)
  (when str
    (if form
	(insert (format form str))
      (insert str))))

(defun mew-insert-message (fld msg rcs size)
  (let ((file (mew-expand-msg fld msg)))
    (cond
     ((not (file-readable-p file))
      (error "%s does not exist" (mew-concat-folder fld msg)))
     ((= (mew-file-get-size file) 0)
      (error "The size of %s is 0" (mew-concat-folder fld msg)))
     ((file-readable-p file)
      (let ((old-cs (and (boundp 'buffer-file-coding-system)
			 buffer-file-coding-system)))
	(mew-frwlet rcs mew-cs-dummy
	  (mew-insert-file-contents file nil 0 size))
	(if (boundp 'buffer-file-coding-system)
	    (setq buffer-file-coding-system old-cs)))
      ;; return physical size
      (cons (mew-file-get-time file) (mew-file-get-size file))))))

(defun mew-insert-manual (&rest args)
  (insert (substitute-command-keys (apply 'concat args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stolen form Perl
;;;

(defun mew-join (separator string-list)
  (mapconcat 'identity string-list separator))

(defun mew-split (str sepchar)
  "Return a list of strings splitting STR with SEPCHAR."
  (let ((len (length str)) (start 0) ret)
    (dotimes (i len)
      (when (char-equal (aref str i) sepchar)
	(setq ret (cons (substring str start i) ret))
	(setq start (1+ i))))
    (if (/= start len)
	(setq ret (cons (substring str start) ret)))
    (nreverse ret)))

(defun mew-remove-single-quote (str)
  (let* ((len (length str))
	 (ret (mew-make-string len))
	 (j 0))
    (dotimes (i len)
      (unless (char-equal (aref str i) ?')
	(aset ret j (aref str i))
	(setq j (1+ j))))
    (substring ret 0 j)))

(defun mew-split-quoted (str sepchar &optional qopen qclose no-single)
  "Return a list of strings splitting STR with SEPCHAR.
SEPCHARs in double-quoted strings are ignored.
If QUOTEDCHAR is provided, SEPCHARs between QOPEN and QCLOSE are
also ignored."
  (let ((qlevel 0) (len (length str)) (start 0) dblq sub ret c)
    (if (and qopen (not qclose)) (setq qclose qopen))
    (dotimes (i len)
      (setq c (aref str i))
      (cond
       ((char-equal ?\\ c)
	(setq i (1+ i)))
       ((or (char-equal ?\" c) (and (not no-single) (char-equal ?' c)))
	(setq dblq (not dblq)))
       ((and qopen (char-equal c qopen))
	(setq qlevel (1+ qlevel)))
       ((and qclose (char-equal c qclose))
	(setq qlevel (1- qlevel)))
       ((char-equal c sepchar)
	(unless (or dblq (>= qlevel 1))
	  (setq sub (substring str start i))
	  (unless no-single
	    (setq sub (mew-remove-single-quote sub)))
	  (setq ret (cons sub ret))
	  (setq start (1+ i))))))
    (when (/= start len)
      (setq sub (substring str start))
      (unless no-single
	(setq sub (mew-remove-single-quote sub)))
      (setq ret (cons sub ret)))
    (nreverse ret)))

(defun mew-chop (str)
  "Split off preceding and following white spaces."
  (let ((i 0) (j (length str)) c)
    (catch 'loop
      (while (< i j)
	(setq c (aref str i))
	(if (or (char-equal c mew-sp) (char-equal c ?\t))
	    (setq i (1+ i))
	  (throw 'loop nil))))
    (setq j (1- j))
    (catch 'loop
      (while (< i j)
	(setq c (aref str j))
	(if (or (char-equal c mew-sp) (char-equal c ?\t))
	    (setq j (1- j))
	  (throw 'loop nil))))
    (substring str i (1+ j))))

(defun mew-quote-string (str qchar targets)
  "If characters in STR is a member of TARGETS, QCHAR is prepended to them."
  (let* ((len (length str))
	(ret (mew-make-string (* len 2)))
	(j 0) c)
    (dotimes (i len)
      (setq c (aref str i))
      (when (member c targets)
	(aset ret j qchar)
	(setq j (1+ j)))
      (aset ret j c)
      (setq j (1+ j)))
    (substring ret 0 j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder
;;;

(defun mew-folder-regex (folder)
  (concat "^" (regexp-quote folder)))

;;

(defun mew-case:folder-case (case:folder)
  "Extract case from case:folder. If case does not exist, nil is returned."
  ;; "case" must be distinguished from drive
  (if (string-match mew-regex-case1 case:folder)
      (mew-match-string 1 case:folder)))

(defun mew-case:folder-folder (case:folder)
  "Extract folder from case:folder."
  ;; "case" must be distinguished from drive
  (if (string-match mew-regex-case1 case:folder)
      (mew-match-string 3 case:folder)
    case:folder))

(defun mew-case-folder (case folder)
  "Concat case and folder to produce case:folder.
If case is \"default\", it is not prepended."
  (if (and case
	   (not (string= case mew-case-default))
	   (mew-folder-remotep folder))
      (concat case ":" folder)
    folder))

;;

(defun mew-folder-absolutep (folder)
  (or (string-match mew-regex-file-absolute folder)
      (string-match mew-regex-drive-letter folder)))

(defun mew-folder-localp (folder)
  (string-match "^\\+" folder))

(defun mew-folder-remotep (folder)
  (string-match "^[-%$]" folder))

(defun mew-folder-popp (folder)
  (string-match "^\\$" folder))

(defun mew-folder-nntpp (folder)
  (string-match "^-" folder))

(defun mew-folder-imapp (folder)
  (string-match "^%" folder))

;; (concat "^" (regexp-quote mew-folder-virtual))
(defun mew-folder-virtualp (folder)
  (string-match "^\\*" folder))

(defun mew-virtual-thread-p (vfolder)
  (with-current-buffer vfolder
    (mew-thread-p)))

;;

(defun mew-folder-prefix (folder)
  (substring folder 0 1))

(defun mew-folder-string (folder)
  (substring folder 1))

(defun mew-string-to-local (folder)
  (concat mew-folder-local folder))

(defun mew-folder-path-to-folder (path &optional has-proto)
  (let (case proto)
    ;; depends on folder
    (mew-set '(case proto) (mew-summary-case-proto))
    (if (mew-folder-localp proto)
	(if has-proto
	    path
	  (mew-string-to-local path))
      (let* ((base-path (mew-folder-string
			 (mew-path-to-folder
			  (mew-expand-folder (mew-case-folder case proto)))))
	     (path-regex (concat "^" (regexp-quote base-path)))
	     (no-proto-path path))
	(when has-proto
	  (setq no-proto-path (mew-case:folder-folder no-proto-path))
	  (setq no-proto-path (mew-folder-string no-proto-path)))
	(if (string-match path-regex no-proto-path)
	    (mew-case-folder
	     case
	     (concat proto (substring no-proto-path (match-end 0))))
	  path)))))

;;

(defun mew-folder-to-selection (folder)
  (concat mew-folder-virtual (mew-folder-basename folder)))

(defun mew-folder-to-thread (folder)
  (concat mew-folder-virtual (mew-folder-basename folder) mew-folder-virtual))

(defun mew-folder-basename (folder)
  ;; Upper functions may use string-match.
  ;; So, string-match must not be used here.
  (if (eq (aref folder 0) ?*)
      (if (eq (aref folder (1- (length folder))) ?*)
	  (substring folder 1 -1)
	(substring folder 1))
    folder))

;;

(defun mew-summary-physical-folder ()
  (cond
   ((mew-summary-p)
    (mew-summary-folder-name 'ext))
   ((mew-virtual-p)
    (mew-vinfo-get-physical-folder))))

;;

(defun mew-local-to-dir (folder)
  (if (string-match "^[+]" folder)
      (substring folder 1)
    folder))

;;

(defun mew-folder-inboxp (folder)
  (member folder mew-inbox-folders))

(defun mew-folder-queuep (folder)
  (member folder mew-queue-folders))

(defun mew-folder-postqp (folder)
  (member folder mew-postq-folders))

(defun mew-folder-draftp (folder)
  (equal folder mew-draft-folder))

;;

(defun mew-folder-imap-queuep ()
  (string= (mew-sinfo-get-folder)
	   (mew-imap-queue-folder (mew-sinfo-get-case))))

;;

(defun mew-canonicalize-folder (folder)
  (cond
   ((mew-folder-localp folder)
    folder)
   ((mew-folder-imapp folder)
    folder)
   ((file-name-absolute-p folder)
    folder)
   (t
    (mew-string-to-local folder))))

(defun mew-path-to-folder (path)
  (let ((regex (concat "^" (regexp-quote (file-name-as-directory (expand-file-name mew-mail-path))))))
    (if (string-match regex path)
	(mew-string-to-local (substring path (match-end 0)))
      path)))

(defun mew-canonicalize-case-folder (case:folder)
  (let ((case (mew-case:folder-case case:folder))
	(folder (mew-case:folder-folder case:folder))
	len)
    (cond
     ((mew-folder-localp folder)   (directory-file-name folder))
     ((mew-folder-absolutep folder)(directory-file-name folder))
     ((mew-folder-virtualp folder) folder)
     (t
      (if (null case)
	  (setq folder case:folder)
	(if (string= case mew-case-default)
	    (progn
	      (setq case nil)
	      (setq case:folder folder))))
      (cond
       ((mew-folder-popp folder) case:folder)
       ((mew-folder-imapp folder)
	(mew-imap-directory-file-name
	 case:folder (or case ;; visit
			 mew-inherit-case))) ;; refile
       ((mew-folder-nntpp folder)
	(setq len (1- (length case:folder)))
	(if (char-equal (aref case:folder len) ?.)
	    (substring case:folder 0 len)
	  case:folder)))))))

;;

(defun mew-expand-folder2 (case:folder)
  "Expanding FOLDER to a relative path to '+'"
  (let ((case (mew-case:folder-case case:folder))
	(folder (mew-case:folder-folder case:folder))
	subdir)
    ;; The length of "case" must be longer than or equal to 2.
    (setq subdir (mew-folder-string folder))
    (cond
     ((mew-folder-popp folder)
      (mew-concat-folder (mew-pop-folder case) subdir))
     ((mew-folder-nntpp folder)
      (mew-concat-folder (mew-nntp-folder case) subdir))
     ((mew-folder-imapp folder)
      (mew-concat-folder (mew-imap-folder case)
			 (mew-imap-expand-folder
			  case
			  (mew-imap-utf-7-encode-string subdir))))
     (t folder))))

(defun mew-expand-folder (folder)
  "Expanding FOLDER to its absolute path"
  (when (stringp folder)
    (let (dir)
      (setq folder (mew-expand-folder2 folder))
      (if (mew-folder-localp folder)
	  (setq dir (expand-file-name (mew-folder-string folder) mew-mail-path))
	;; absolute path
	;; "C:/" -> t, "C:" -> nil , "CC:/" -> nil
	(setq dir (expand-file-name folder)))
      dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Suffix support
;;;

(defvar mew-use-suffix nil)

(defun mew-msg-new-filename (file)
  (if mew-use-suffix
      (format "%s%s" file mew-suffix)
    file))

(defun mew-msg-get-filename (file)
  (let ((sfile (format "%s%s" file mew-suffix)))
    (if (file-exists-p sfile)
	sfile
      file)))

(defun mew-expand-msg (folder message)
  (let ((file (expand-file-name message (mew-expand-folder folder))))
    (mew-msg-get-filename file)))

(defun mew-expand-new-msg (folder message)
  (mew-msg-new-filename (expand-file-name message (mew-expand-folder folder))))

(defun mew-expand-file (folder file)
  (expand-file-name file (mew-expand-folder folder)))

(defun mew-concat-folder (folder subfolder)
  (concat (file-name-as-directory folder) subfolder))

(defun mew-concat-folder2 (folder subfolder case)
  (if (mew-folder-imapp folder)
      (concat (mew-imap-file-name-as-directory folder case) subfolder)
    (concat (file-name-as-directory folder) subfolder)))

(defun mew-dir-messages (dir &optional regex full)
  ;; directory_files uses ENCODE_FILE() for DIR and
  ;;                      DECODE_FILE() for results(FILES).
  ;; Both ENCODE_FILE() and DECODE_FILE() are quite slow.
  ;; Since DECODE_FILE() is used many times, directory_files is
  ;; too slow when {default-,}file-name-coding-system are non-nil.
  ;; If {default-,}file-name-coding-system are bound to nil,
  ;; ENCODE_FILE() and DECODE_FILE() are skipped, resulting speed up.
  ;; So, we need to encode DIR by ourselves.
  (let ((edir (expand-file-name dir))
	(cs default-file-name-coding-system))
    (setq edir (mew-cs-encode-string edir cs))
    (or regex (setq regex mew-regex-message-files))
    (mew-alet
     (directory-files edir full regex 'no-sort))))

(defun mew-folder-messages (folder)
  (let* ((dir (mew-expand-folder folder))
	 msgs nums)
    (when (file-directory-p dir)
      (setq msgs (mew-dir-messages dir))
      (setq nums (sort (mapcar 'string-to-number msgs) '<))
      (mapcar 'number-to-string nums))))

(defun mew-folder-new-message (folder &optional num-only cache)
  (let* ((dir (mew-expand-folder folder))
	 (regex (if cache mew-regex-message-files3 mew-regex-message-files))
	 (max 0)
	 cur maxfile maxpath)
    ;; xxx create if there is no directory?
    (when (file-directory-p dir)
      (dolist (msg (mew-dir-messages dir regex))
	(setq cur (string-to-number (file-name-sans-extension msg)))
	(if (> cur max) (setq max cur)))
      (setq max (1+ max))
      (setq maxfile (number-to-string max))
      (setq maxpath (mew-expand-new-msg folder maxfile))
      (while (get-file-buffer maxpath) ;; xxx
	;; file not exist but there is a buffer.
	(setq max (1+ max))
	(setq maxfile (number-to-string max))
	(setq maxpath (mew-expand-new-msg folder maxfile)))
      (while (file-exists-p maxpath)
	(setq maxfile (read-string (format "%s/%s exists. Input a message number: " max folder)))
	(while (not (string-match mew-regex-message-files maxfile))
	  (setq maxfile (read-string "Input NUMBER: ")))
	(setq maxpath (mew-expand-new-msg folder maxfile)))
      (if num-only
	  (mew-msg-new-filename maxfile)
	maxpath))))

(defun mew-touch-folder (fld)
  (when (stringp mew-summary-touch-file)
    (let ((file (mew-expand-file fld mew-summary-touch-file)))
      (when (file-writable-p file)
	(write-region "touched by Mew." nil file nil 'no-msg)
	(mew-set-file-modes file)))))

;; this kind of defmacro can't recurse
;; if +foo exists and we insert +foo/bar,
;; +foo/ should be inserted, too. But +foo/ does not match to +foo...
(defmacro mew-folder-insert (folder lst subdir)
  `(unless (mew-assoc-equal ,folder ,lst 0)
     (let ((case-fold-search nil)
	   (max (1- (length ,lst)))
	   (pair (mew-folder-func ,folder ,subdir))
	   (min 0)
	   mid crr prv)
       (while (> (- max min) 20) ;; 20 is enough?
	 (setq mid (/ (+ min max) 2))
	 (if (string< (car (nth mid ,lst)) ,folder)
	     (setq min mid)
	   (setq max mid)))
       (setq crr (nthcdr min ,lst))
       (while (and crr (string< (car (car crr)) ,folder))
	 (setq prv crr)
	 (setq crr (cdr crr)))
       (if prv
	   (setcdr prv (cons pair crr))
	 (setq ,lst (cons pair crr))))))

(defmacro mew-folder-delete (folder lst)
  `(setq ,lst (delete (assoc ,folder ,lst) ,lst)))

(defun mew-folder-node-p (folder &optional case)
  (cond
   ((mew-folder-localp folder)
    (let* ((dir (file-name-as-directory folder))
	   (regex (concat "^" (regexp-quote dir))))
      (mew-assoc-match regex (mew-local-folder-alist) 0)))
   ((mew-folder-imapp folder)
    (let* ((dir (mew-imap-file-name-as-directory folder case))
	   (regex (concat "^" (regexp-quote dir))))
      (mew-assoc-match regex (mew-imap-folder-alist case) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder spec
;;;

(defconst mew-folder-spec-type
  '((regex     . string-match)
    (recursive . mew-folder-recursive-match)
    (string    . string=)))

(defun mew-folder-spec-func (type)
  (or (cdr (assq type mew-folder-spec-type)) 'string=))

(defun mew-folder-spec (folder lst str-type lst-type)
  (let ((str-func (mew-folder-spec-func str-type))
	(lst-func (mew-folder-spec-func lst-type))
	keys values ret)
    (setq folder (mew-case:folder-folder folder))
    (catch 'loop
      (dolist (ent lst)
	(setq keys (car ent))
	(setq values (cdr ent))
	(cond
	 ((eq keys t)
	  (throw 'loop (setq ret values)))
	 ((stringp keys)
	  (if (funcall str-func keys folder)
	      (throw 'loop (setq ret values))))
	 ((listp keys)
	  (dolist (key keys)
	    (if (funcall lst-func key folder)
		(throw 'loop (setq ret values))))))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Directory
;;;

(defun mew-parent-directory (path)
  (directory-file-name (file-name-directory path)))

(defun mew-rename-directory (src dst)
  (let ((parent (mew-parent-directory (directory-file-name dst))))
    (mew-make-directory parent)
    (rename-file src dst)))

(defun mew-make-directory (path)
  (let ((parent (mew-parent-directory path)))
    (unless (file-directory-p parent)
      (mew-make-directory parent))
    (cond
     ((file-directory-p path)
      ()) ;; do nothing
     ((file-exists-p path)
      (delete-file path)
      (make-directory path))
     (t
      (make-directory path)))
    (if (/= mew-folder-mode (file-modes path))
	(set-file-modes path mew-folder-mode))))

(defun mew-delete-directory-recursively (dir)
  (when (file-directory-p dir)
    (dolist (file (directory-files dir 'full mew-regex-files 'no-sort))
      (cond
       ((file-symlink-p file)
	;; never chase symlink which points a directory
	(delete-file file))
       ((file-directory-p file)
	(mew-delete-directory-recursively file))
       (t
	(delete-file file))))
    (unless (directory-files dir 'full mew-regex-files 'no-sort)
      (delete-directory dir))))

(defun mew-check-directory (dir)
  (unless (file-directory-p dir)
    (unless (file-exists-p dir)
      (mew-make-directory dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File existence
;;;

(defun mew-which (file path)
  (catch 'loop
    (dolist (dir path)
      (if (file-exists-p (expand-file-name file dir))
	  (throw 'loop (expand-file-name file dir))))))

(defun mew-which-el (elfile)
  (or (mew-which (concat elfile ".el") load-path)
      (mew-which (concat elfile ".elc") load-path)))

(defvar mew-which-exec-suffixes '(""))

(defun mew-which-exec (execfile)
  (let (file)
    (catch 'detect
      (dolist (suffix mew-which-exec-suffixes)
	(when (setq file (mew-which (concat execfile suffix) exec-path))
	  (throw 'detect file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File attribute
;;;

;; Functions to get other attributes are implemented in C level.

(defun mew-file-get-links (file)
  (let ((w32-get-true-file-link-count t)) ;; for Meadow
    (nth 1 (file-attributes file))))

(defun mew-file-get-time (file)
  (nth 5 (file-attributes file)))

(defun mew-file-get-size (file)
  (nth 7 (file-attributes file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File
;;;

(defun mew-file-chase-links (file)
  "Chase links in FILENAME until a name that is not a link.
Does not examine containing directories for links."
  (let ((ret file) exp)
    (while (setq exp (file-symlink-p ret))
      (setq ret (expand-file-name exp (file-name-directory ret))))
    ret))

(defun mew-file-from-home (str)
  (if (string-match (expand-file-name mew-home) str)
      (concat mew-home (substring str (match-end 0)))
    str))

(defun mew-prepend-prefix (file prefix)
  (if (file-name-absolute-p file)
      (concat (file-name-directory file) prefix (file-name-nondirectory file))
    (concat prefix file)))

(defun mew-rotate-log-files (file-name)
  (let ((i 8) (file (expand-file-name file-name mew-conf-path)))
    (when (and (file-exists-p file)
	       (>= (mew-file-get-size file) mew-log-max-size))
      (while (>= i 0)
	(if (file-exists-p (format "%s.%d" file i))
	    (rename-file (format "%s.%d" file i)
			 (format "%s.%d" file (1+ i)) t))
	(setq i (1- i)))
      (rename-file file (format "%s.0" file)))))

(defun mew-remove-drive-letter (file)
  (if (string-match mew-regex-drive-letter file)
      (substring file 2)
    file))

(defun mew-get-file-modes (path)
  (let* ((dir (file-name-directory path))
	 (dirmode (file-modes dir)))
    (logand dirmode mew-file-mode-mask)))

(defun mew-set-file-modes (path)
  (set-file-modes path (mew-get-file-modes path)))

(defun mew-delete-file (file)
  (if (and (stringp file) (file-exists-p file)) (delete-file file)))

(defun mew-insert-file-contents (&rest args)
  "A safe version of insert-file-contents.
This checks -*-coding:ctext;-*- internally when including a file."
  (let ((after-insert-file-functions nil))
    ;; preventing after-insert-file-set-buffer-file-coding-system
    (apply 'mew-insert-file-contents2 args)))

(defun mew-insert-file-contents2 (&rest args)
  "Mew version of insert-file-contents.
This sets  buffer-file-coding-system."
  (let ((auto-image-file-mode nil))
    (apply 'insert-file-contents args)))

(defun mew-find-file-noselect (&rest args)
  "A safe version of find-file-noselect.
This checks -*-coding:ctext;-*- internally when including a file.
But this does not set buffer-file-coding-system."
  (let ((after-insert-file-functions nil))
    ;; preventing after-insert-file-set-buffer-file-coding-system
    (apply 'mew-find-file-noselect2 args)))

(defun mew-find-file-noselect2 (&rest args)
  "A safe version of find-file-noselect.
This checks -*-coding:ctext;-*- internally when including a file
and sets buffer-file-coding-system."
  (let ((auto-image-file-mode nil)
	(format-alist nil)
	(auto-mode-alist nil)
        (enable-local-variables nil)
	(find-file-hook nil)
	(large-file-warning-threshold nil))
    (apply 'find-file-noselect args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; temp name
;;;

(defun mew-make-temp-name (&optional fname)
  (unless (file-exists-p mew-temp-dir)
    (mew-make-directory mew-temp-dir)) ;; just in case
  (if fname
      ;; File name of a temporary file should be ASCII only.
      (if (and (string-match "^[ -~]+$" fname)
	       (not (file-exists-p (expand-file-name fname mew-temp-dir))))
	  (expand-file-name fname mew-temp-dir)	
	(if (string-match "\\.[ -~]+$" fname)
	    (concat (make-temp-name mew-temp-file) (mew-match-string 0 fname))
	  (make-temp-name mew-temp-file)))
    (make-temp-name mew-temp-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Random
;;;

(defvar mew-random-base-09 "0123456789")
(defvar mew-random-base-az "abcdefghijklmnopqrstuvwxwz")

(defun mew-random ()
  (let* ((vec (recent-keys))
	 (ran (+ (random) (emacs-pid)))
	 c)
    (dotimes (i (length vec))
      (setq c (aref vec i))
      (if (integerp c) (setq ran (+ ran c))))
    (abs ran)))

(defun mew-random-string (len nump)
  (let* ((base (if nump mew-random-base-09 mew-random-base-az))
	 (baselen (length base))
	 (ret (mew-make-string len)))
    (dotimes (i len ret)
      (aset ret i (aref base (% (mew-random) baselen))))))

(defun mew-random-filename (dir len nump &optional suffix)
  (let ((cnt 0) (max 20) ;; ad hoc
	file filepath)
    (setq file (concat (mew-random-string len nump) suffix))
    (setq filepath (expand-file-name file dir))
    (while (and (file-exists-p filepath) (< cnt max))
      (setq file (concat (mew-random-string len nump) suffix))
      (setq filepath (expand-file-name file dir))
      (setq cnt (1+ cnt)))
    (if (file-exists-p filepath)
	nil
      filepath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer
;;;

(defun mew-erase-buffer ()
  (mew-elet
   (widen)
   (erase-buffer)
   (buffer-disable-undo)))

(defun mew-remove-buffer (buf)
  (if (and buf (get-buffer buf)) (kill-buffer buf)))

(defmacro mew-elet (&rest body)
  `(let ((buffer-read-only nil)
	 (inhibit-read-only t)
	 (after-change-functions nil)
	 (mark-active nil))
     ,@body))

(defun mew-push-mark ()
  "Set the mark for \\[exchange-point-and-mark]"
  (let ((mark-active nil))
    (push-mark (point) t t)))

(defun mew-region-bytes (beg end buf)
  ;; string-bytes() acts differently on each Emacs.
  ;; set-buffer-multibyte is also buggy.
  ;; So, use this way.
  (with-current-buffer buf
    (if (fboundp 'string-as-unibyte)
	(length (string-as-unibyte (mew-buffer-substring beg end)))
      (- end beg))))

(defun mew-count-lines (beg end)
  "Return number of lines between BEG and END."
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (setq beg (point))
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (if (or (not (mew-decode-syntax-p))
	    (not (equal (mew-decode-syntax-buffer) (current-buffer))))
	(mew-count-lines1 beg end)
      (let ((mbeg (mew-decode-syntax-begin))
	    (mend (mew-decode-syntax-end)))
	(if (or (<= end mbeg) (>= beg mend))
	    (mew-count-lines1 beg end)
	  (+ (mew-count-lines1 beg mbeg) (mew-count-lines1 mend end)))))))

(defun mew-count-lines1 (beg end)
  (if (>= beg end)
      0
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (not (and (mew-thread-p) mew-use-thread-separator))
	  (- (buffer-size) (forward-line (buffer-size)))
	(let ((regex mew-regex-thread-separator)
	      (lines 0))
	  (while (not (eobp))
	    (unless (looking-at regex)
	      (setq lines (1+ lines)))
	    (forward-line))
	  lines)))))

(defun mew-buffer-list (regex &optional listp mode)
  (let (ret)
    (dolist (buf (mapcar 'buffer-name (buffer-list)))
      (when (and (string-match regex buf)
		 (or (not mode)
		     (and mode (get-buffer buf)
			  (with-current-buffer buf
			    (eq major-mode mode)))))
	(if listp
	    (setq ret (cons (list buf) ret))
	  (setq ret (cons buf ret)))))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; process
;;;

(defmacro mew-filter (&rest body)
  `(let ((pbuf (process-buffer process)) ;; MUST use 'process'
	 (obuf (buffer-name))
	 (inhibit-eol-conversion nil)) ;; \r\n as is
     (if (and (bufferp pbuf)
	      (buffer-name pbuf)) ;; check a killed buffer
	 ;; must use buffer-name instead of current-buffer
	 ;; so that get-buffer can detect killed buffer.
	 (unwind-protect
	     (progn
	       ;; buffer surely exists.
	       (set-buffer (process-buffer process)) ;; necessary
	       ,@body)
	   (if (get-buffer obuf)
	       ;; the body sometimes kills obuf.
	       (set-buffer obuf))))))

(defun mew-start-process-disp (name buffer program &rest program-args)
  (let ((disp (cdr (assq 'display (frame-parameters))))
	(process-environment (copy-sequence process-environment)))
    (if disp (setenv "DISPLAY" disp))
    (apply 'start-process name buffer program program-args)))

(defun mew-start-process-lang (name buffer program &rest program-args)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LANGUAGE" "C")
    (setenv "LC_ALL" "C")
    (setenv "LANG" "C")
    (apply 'start-process name buffer program program-args)))

(defun mew-call-process-lang (prog &optional infile buffer display &rest args)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LANGUAGE" "C")
    (setenv "LC_ALL" "C")
    (setenv "LANG" "C")
    (apply 'call-process prog infile buffer display args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global info
;;;

(defun mew-info (name)
  (cond
   ((vectorp name) name) ;; just for .mqi
   ((stringp name)
    (if (or (not (intern-soft name))
	    (not (boundp (intern name))))
	(if (string-match "^mew-[^-]+-info-" name)
	    (let* ((sym (intern (concat (mew-match-string 0 name) "list")))
		   (lst (symbol-value sym))
		   (len (length lst)))
	      (set (intern name) (make-vector len nil)))))
    (symbol-value (intern-soft name)))))

(defun mew-info-defun (prefix lst)
  (let ((i 0))
    (dolist (ent lst)
      (fset (intern (concat prefix "get-" ent))
	    `(lambda (arg)
	       (cond
		((stringp arg) (aref (mew-info arg) ,i))
		((vectorp arg) (aref arg ,i)))))
      (fset (intern (concat prefix "set-" ent))
	    `(lambda (arg value)
	       (cond
		((stringp arg) (aset (mew-info arg) ,i value))
		((vectorp arg) (aset arg ,i value)))))
      (setq i (1+ i)))))

(defun mew-info-clean-up (arg &optional start)
  (let ((i (or start 0)) vec len)
    (cond
     ((stringp arg) (setq vec (mew-info arg)))
     ((vectorp arg) (setq vec arg)))
    (setq len (length vec))
    (while (< i len)
      (aset vec i nil)
      (setq i (1+ i)))))

(defun mew-net-get-ssh-process (pnm)
  (aref (mew-info pnm) 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer local info
;;;

(defun mew-blinfo (sym)
  (let* ((name (symbol-name sym))
	 (lname (concat name "-list"))
	 (lsym (intern lname))
	 (lst (symbol-value lsym))
	 (len (length lst)))
    (set sym (make-vector len nil))
    (symbol-value sym)))

(defun mew-blinfo-defun (blv-sym lst)
  (let ((i 0))
    (dolist (ent lst)
      (fset (intern (concat (symbol-name blv-sym) "-get-" ent))
	    `(lambda ()
	       (cond
		((null ,blv-sym) (aref (mew-blinfo (quote ,blv-sym)) ,i))
		((vectorp ,blv-sym) (aref ,blv-sym ,i)))))
      (fset (intern (concat (symbol-name blv-sym) "-set-" ent))
	    `(lambda (value)
	       (cond
		((null ,blv-sym) (aset (mew-blinfo (quote ,blv-sym)) ,i value))
		((vectorp ,blv-sym) (aset ,blv-sym ,i value)))))
      (setq i (1+ i)))))

(defvar mew-ainfo-list '("icon" "win-cfg"))
(mew-blinfo-defun 'mew-ainfo mew-ainfo-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Address
;;;

(defun mew-get-my-address ()
  (or (mew-header-parse-address mew-from:) (mew-mail-address)))

;;

(defun mew-get-my-address-regex-list ()
  "This creates a list of regular expression used to tell
whether or not a given address is mine. The list is created
from (mew-user), (mew-mail-address), and 'mew-mail-address-list'."
  (cons (concat "^" (regexp-quote (mew-user)) "$")
	(cons (concat "^" (regexp-quote (mew-mail-address)) "$")
	      mew-mail-address-list)))

(defun mew-is-my-address (addrs from)
  (and from
       (let ((case-fold-search t))
	 (catch (quote match)
	   (car (mapcar (lambda (arg) (and (string-match arg from)
					   (throw (quote match) t)))
			addrs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp load/save
;;;

(defun mew-lisp-load (filename)
  "Load lisp from FILENAME"
  (let ((fullname (if (file-name-absolute-p filename)
		      filename
		    (expand-file-name filename mew-conf-path))))
    (if (file-readable-p fullname)
	(with-temp-buffer
	  (mew-frwlet mew-cs-m17n mew-cs-dummy
	    (mew-insert-file-contents fullname))
	  (goto-char (point-min))
	  (condition-case nil
	      (read (current-buffer))
	    (error ()))))))

(defun mew-lisp-save (filename lisp &optional nobackup unlimit)
  "Save LISP to FILENAME. LISP is truncated to mew-lisp-max-length
by side-effect."
  (let* ((fullname (if (file-name-absolute-p filename)
		       filename
		     (expand-file-name filename mew-conf-path)))
	 (backname (concat fullname mew-backup-suffix))
	 print-length print-level) ;; for Emacs 21
    (when (file-writable-p fullname)
      (if nobackup
	  (mew-delete-file fullname)
	(if (file-exists-p fullname)
	    (rename-file fullname backname 'override)))
      (when (and (not unlimit) (> (length lisp) mew-lisp-max-length))
	(setq lisp (copy-sequence lisp)) ;; no side effect
	(mew-ntake mew-lisp-max-length lisp))
      (with-temp-buffer
	(if (> (length lisp) mew-lisp-max-length)
	    (print lisp (current-buffer))
	  (pp lisp (current-buffer)))
	(mew-frwlet mew-cs-dummy mew-cs-m17n
	  (write-region (point-min) (point-max) fullname nil 'no-msg)))
      (mew-set-file-modes fullname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Month
;;;

(defvar mew-time-mon-alist
  '(("Jan" .  1) ("Feb" .  2) ("Mar" .  3) ("Apr" .  4)
    ("May" .  5) ("Jun" .  6) ("Jul" .  7) ("Aug" .  8)
    ("Sep" .  9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defun mew-time-mon-str-to-int (str)
  (or (cdr (assoc (capitalize str) mew-time-mon-alist)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Time Zone
;;;

;; Define symbol time zone defined in RFC822 only.
(defvar mew-time-tmzn-alist
  '(("UT"  .  0) ("GMT" .  0)
    ("EST" . -5) ("EDT" . -4)
    ("CST" . -6) ("CDT" . -5)
    ("MST" . -7) ("MDT" . -6)
    ("PST" . -8) ("PDT" . -7)))

(defun mew-time-tmzn-str-to-int (str)
  (cdr (assoc (upcase str) mew-time-tmzn-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RFC 822
;;; Date: Wed, 26 Jul 2000 21:18:35 +0900 (JST)
;;;

(defvar mew-time-rfc-regex
  "\\([0-9]+\\)[ \t]+\\([a-zA-Z]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\([ \t]+\\([-+0-9a-zA-Z]+\\)\\)?")

(defmacro mew-time-rfc-day  ()
  '(string-to-number (substring s (match-beginning 1) (match-end 1))))

(defmacro mew-time-rfc-mon  ()
  '(substring s (match-beginning 2) (match-end 2)))

(defmacro mew-time-rfc-year ()
  '(string-to-number (substring s (match-beginning 3) (match-end 3))))

(defmacro mew-time-rfc-hour ()
  '(string-to-number (substring s (match-beginning 4) (match-end 4))))

(defmacro mew-time-rfc-min  ()
  '(string-to-number (substring s (match-beginning 5) (match-end 5))))

(defmacro mew-time-rfc-sec  ()
  '(if (match-beginning 7)
       (string-to-number (substring s (match-beginning 7) (match-end 7)))
     0))

;; returns seconds
(defmacro mew-time-rfc-tmzn ()
  '(if (match-beginning 9)
       (let ((tmzn (substring s (match-beginning 9) (match-end 9)))
	     int)
	 (cond
	  ((string-match "^[-+][0-9]+$" tmzn)
	   (setq int (string-to-number tmzn))
	   (+ (* (/ int 100) 3600) (* (% int 100) 60)))
	  ((setq int (mew-time-tmzn-str-to-int tmzn))
	   (* int 3600))
	  (t 0)))
     0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort key for Date:
;;;

;; "20000726121835"
(defun mew-time-ctz-to-sortkey (time)
  (let ((system-time-locale "C"))
    (format-time-string "%Y%m%d%H%M%S" time)))

(defun mew-time-ctz-to-sortkey-invalid (sec min hour day mon year)
  (format "%04d%02d%02d%02d%02d%02d" year mon day hour min sec))

;; "20000726121835"
(defun mew-time-rfc-to-sortkey (s &optional tzadj)
  (if (string-match mew-time-rfc-regex s)
      (let ((year (mew-time-rfc-year))
	    (mon  (mew-time-mon-str-to-int (mew-time-rfc-mon)))
	    (day  (mew-time-rfc-day))
	    (hour (mew-time-rfc-hour))
	    (min  (mew-time-rfc-min))
	    (sec  (mew-time-rfc-sec))
	    (tmzn (mew-time-rfc-tmzn)))
	(cond
	 ((< year 50)
	  (setq year (+ year 2000)))
	 ((< year 150)
	  (setq year (+ year 1900))))
	(if (or (< year 1970) (>= year 2038))
	    ;; invalid data
	    (mew-time-ctz-to-sortkey-invalid sec min hour day mon year)
	  (setq sec (- sec tmzn))
	  (if tzadj (setq sec (+ sec (car (current-time-zone)))))
	  (mew-time-ctz-to-sortkey (encode-time sec min hour day mon year))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User friendly time
;;;

(defun mew-time-tmzn-int ()
  (let ((tz (car (current-time-zone))))
    (if (< tz 0)
	(format "-%02d%02d" (/ (- tz) 3600) (/ (% (- tz) 3600) 60))
      (format "+%02d%02d" (/ tz 3600) (/ (% tz 3600) 60)))))

;; Wed, 26 Jul 2000 21:18:35 +0900 (JST)
(defun mew-time-ctz-to-rfc (time)
  (let* ((system-time-locale "C")
	 ;; A bug of Emacs 22.3 on Windows
	 (time-zone-name (format-time-string "%Z" time))
	 (date (format-time-string "%a, %d %b %Y %T %z" time)))
    (if (string= time-zone-name "")
	date
      (concat date (format " (%s)" time-zone-name)))))

;; 2000/07/12 16:22:30
(defun mew-time-ctz-to-logtime (time)
  (let ((system-time-locale "C"))
    (format-time-string "%Y/%m/%d %H:%M:%S" time)))

;; 20000712.155559
(defun mew-time-ctz-to-msgid (time)
  (let ((system-time-locale "C"))
    (format-time-string "%Y%m%d.%H%M%S" time)))

;;

(defun mew-time-calc (new old)
  (let ((million 1000000)
	(sec (+ (* 65536 (- (nth 0 new) (nth 0 old)))
		(- (nth 1 new) (nth 1 old))))
	(usec (- (nth 2 new) (nth 2 old))))
    (if (< usec 0)
        (setq sec (1- sec)
              usec (+ usec million))
      (if (>= usec million)
          (setq sec (1+ sec)
                usec (- usec million))))
    (+ sec (/ usec (float million)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multibyte
;;;

(defun mew-set-buffer-multibyte (arg)
  (if (fboundp 'set-buffer-multibyte)
      (set-buffer-multibyte arg)))

(defun mew-set-string-multibyte (str)
  (if (fboundp 'string-as-multibyte)
      (string-as-multibyte str)
    str))

(defun mew-multibyte-string-p (str)
  (if (fboundp 'multibyte-string-p)
      (multibyte-string-p str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Network
;;;

(defun mew-dot-insert ()
  (goto-char (point-min))
  (while (re-search-forward "^\\." nil t)
    (insert ".")
    (forward-line)))

(defun mew-dot-delete ()
  (goto-char (point-max))
  (forward-line -1)
  (delete-region (point) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "^\\." nil t)
    (delete-char -1)
    (forward-line)))

(defun mew-crlf-to-lf ()
  (while (search-forward "\r\n" nil t) (replace-match "\n" nil t)))

(defun mew-lf-to-crlf ()
  (while (search-forward "\n" nil t) (replace-match "\r\n" nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sit-for
;;;

(defun mew-redraw (&optional time)
  (sit-for (or time 0)))

(defmacro mew-rendezvous (who)
  ;; Wait for the termination of WHO.
  ;; Emacs does not provide synchronize mechanism with
  ;; an asynchronous process. So, take this way.
  `(while ,who
     (sit-for 0.1)
     ;; accept-process-output or sleep-for is not enough
     (discard-input)))

(defun mew-let-user-read ()
  (sit-for 1.5))

(defun mew-warn (&rest msg)
  (apply 'message msg)
  (ding)
  (mew-let-user-read))

(defun mew-timing ()
  (sit-for 0.01))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiple set
;;;

(defun mew-set (vars vals)
  (dolist (var vars)
    (if var (set var (car vals))) ;; var can be nil to skip
    (setq vals (cdr vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structure
;;;

;;;
;;; Some old code uses assoc. So, you cannot put the structure name
;;; into the car.
;;;

(defun mew-keyword-number-pair (spec)
  (let ((len (length spec)) key ret)
    (dotimes (i len (nreverse ret))
      (setq key (intern (concat ":" (symbol-name (car spec)))))
      (setq ret (cons (cons key i) ret))
      (setq spec (cdr spec)))))

(defmacro mew-defstruct (type &rest spec)
  `(progn
     (mew-defstruct-constructor ,type ,@spec)
     (mew-defstruct-s/getter ,type ,@spec)))

(defmacro mew-defstruct-constructor (type &rest spec)
  `(defun ,(intern (concat "mew-make-" (symbol-name type))) (&rest args)
     (let* ((alist (quote ,(mew-keyword-number-pair spec)))
	    (struct (make-list (length alist) nil))
	    key val key-num)
       (while args ;; cannot use dolist
	 (setq key  (car args))
	 (setq args (cdr args))
	 (setq val  (car args))
	 (setq args (cdr args))
	 (unless (keywordp key)
	   (error "'%s' is not a keyword" key))
	 (setq key-num (assoc key alist))
	 (if key-num
	     (setcar (nthcdr (cdr key-num) struct) val)
	   (error "'%s' is unknown" key)))
       struct)))

(defmacro mew-defstruct-s/getter (type &rest spec)
  `(let* ((type-name (symbol-name ',type))
	  (keys ',spec)
	  (len (length keys))
	  member-name setter getter)
     (dotimes (i len)
       (setq member-name (symbol-name (car keys)))
       (setq setter (intern (format "mew-%s-set-%s" type-name member-name)))
       (fset setter (list 'lambda '(struct value) (list 'setcar (list 'nthcdr i 'struct) 'value) 'struct))
       (setq getter (intern (format "mew-%s-get-%s" type-name member-name)))
       (fset getter (list 'lambda '(struct) (list 'nth i 'struct)))
       (setq keys (cdr keys)))))

(provide 'mew-func)

;;; Copyright Notice:

;; Copyright (C) 1997-2010 Mew developing team.
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

;;; mew-func.el ends here
