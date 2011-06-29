;;; mew-mime.el --- MIME launcher for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997

;;; Code:

(require 'mew)
(eval-when-compile
  (cond
   ((memq system-type '(windows-nt cygwin))
    (require 'mew-win32))
   ((eq system-type 'darwin)
    (require 'mew-darwin))
   (t
    (require 'mew-unix)))
  (mew-no-warning-defun w3m-region)
  (mew-no-warning-defun w3m-expand-file-name-as-url))

(defvar mew-process-file-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Configuration subroutines
;;;

(defun mew-progspec-get-prog (def)
  (cond
   ((stringp def) def)
   ((and (listp def) (stringp (car def))) (car def))))

(defun mew-progspec-get-args (def)
  (if (listp def) (nth 1 def)))

(defun mew-progspec-get-async (def)
  (cond
   ((stringp def) t)
   ((listp def) (nth 2 def))))

(defun mew-progsec-args-convert (args arg)
  (let (converted ret)
    (setq ret (mapcar (lambda (x)
			(if (not (string-match "%s" x))
			    x
			  (setq converted t)
			  (format x arg)))
		      args))
    (unless converted (setq ret (append ret (list arg))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Start and call process
;;;

(defun mew-mime-start-process (program options file)
  (let ((process-connection-type mew-connection-type1) pro)
    (message "Starting %s..." program)
    (setq pro (apply 'mew-start-process-disp
		     (format "*mew %s*" program)
		     nil
		     program
		     (append options (list file))))
    (set-process-sentinel pro 'mew-mime-start-process-sentinel)
    (message "Starting %s...done" program)
    (setq mew-process-file-alist (cons (cons pro file) mew-process-file-alist))
    t)) ;; to next part

(defun mew-mime-start-process-sentinel (process event)
  (let* ((al (assoc process mew-process-file-alist))
	 (file (cdr al)))
    (save-excursion
      ;; A launcher program may be executed.
      ;; The launcher program executes an application according to
      ;; file's suffix or something.
      ;; The time when the launcher is finished is not the time when
      ;; the application is finished. So, we can't delete the temporary
      ;; file here. Hoping that the file will be deleted when Mew is
      ;; finished.
      (if mew-delete-temp-file (mew-delete-file file))
      (setq mew-process-file-alist (delq al mew-process-file-alist)))))

(defun mew-mime-call-process (program options file)
  (message "Calling %s..." program)
  (apply 'call-process program file nil nil options)
  (mew-delete-file file)
  (message "Calling %s...done" program)
  t) ;; to next part

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part handler
;;;

;; see also mew-summary-execute-external().

(defun mew-mime-part (cache fullpart nums)
  ;; called in Message buffer
  ;; if nums is nil, it means singlepart.
  (let* ((syntax  (mew-syntax-get-entry fullpart nums))
	 (begin   (mew-syntax-get-begin syntax))
	 (end     (mew-syntax-get-end   syntax))
	 (ctl     (mew-syntax-get-ct    syntax))
	 (cte     (mew-syntax-get-cte   syntax))
	 (ct      (mew-syntax-get-value ctl 'cap))
	 (cdpl    (mew-syntax-get-cdp syntax))
	 (fname   (mew-syntax-get-filename cdpl ctl))
	 (cd      (mew-syntax-get-cd syntax))
	 (params  (mew-syntax-get-params ctl))
	 (program (mew-ctdb-prog (mew-ctdb-by-ct ct)))
	 func2)
    (when (listp program)
      (setq func2 (nth 1 program))
      (setq program (nth 0 program)))
    (cond
     ((null program)
      (mew-mime-function func2 cache begin end ct cte cd fname))
     ((symbolp program)
      (when (fboundp program)
	(cond
	 ((eq program 'mew-mime-message/rfc822)
	  (funcall program cache syntax)) ;; for recursive MIME
	 ((eq program 'mew-mime-application/octet-stream)
	  (funcall program cache begin end params ct cte cd fname))
	 ((mew-ct-imagep ct)
	  (funcall program cache begin end params fname ct cte))
	 (t
	  (funcall program cache begin end params)))))
     (t ;; string
      (insert " ######  ######  #######  #####  ######     #    #     #\n"
	      " #     # #     # #     # #     # #     #   # #   ##   ##\n"
	      " #     # #     # #     # #       #     #  #   #  # # # #\n"
	      " ######  ######  #     # #  #### ######  #     # #  #  #\n"
	      " #       #   #   #     # #     # #   #   ####### #     #\n"
	      " #       #    #  #     # #     # #    #  #     # #     #\n"
	      " #       #     # #######  #####  #     # #     # #     #\n"
	      "\n\n")
      (mew-insert "Content-Type:\t%s\n" ct)
      (mew-insert "Encoding:\t%s\n" cte)
      (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
      (mew-insert "Filename:\t%s\n" fname)
      (mew-insert "Description:\t%s\n" cd)
      (mew-insert "Program:\t%s\n" program)
      (insert "\n")
      (mew-mime-part-messages t)))))

(defun mew-mime-function (func cache begin end &optional ct cte cd fname)
  (insert " ####### #     # #     #  #####  #######   ###   ####### #     #\n"
	  " #       #     # ##    # #     #    #       #    #     # ##    #\n"
	  " #       #     # # #   # #          #       #    #     # # #   #\n"
	  " #####   #     # #  #  # #          #       #    #     # #  #  #\n"
	  " #       #     # #   # # #          #       #    #     # #   # #\n"
	  " #       #     # #    ## #     #    #       #    #     # #    ##\n"
	  " #        #####  #     #  #####     #      ###   ####### #     #\n"
	  "\n\n")
  (mew-insert "Content-Type:\t%s\n" ct)
  (mew-insert "Encoding:\t%s\n" cte)
  (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
  (mew-insert "Filename:\t%s\n" fname)
  (mew-insert "Description:\t%s\n" cd)
  (mew-insert "Function:\t%s\n" func)
  (insert "\n")
  (mew-mime-part-messages func))

(defun mew-mime-part-messages (prog)
  (cond
   ((and prog (fboundp prog))
    (mew-insert-manual
     "To execute an external function, type "
     "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"
     "\n"))
   (prog
    (mew-insert-manual
     "To execute an external command, type "
     "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"
     "\n")))
  (mew-insert-manual
   "To save this part, type "
   "'\\<mew-summary-mode-map>\\[mew-summary-save]'.\n"
   "To insert this part in Message mode, type "
   "'\\<mew-summary-mode-map>\\[mew-summary-display-asis]'.\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/Plain
;;;

(defvar mew-flowed-auto-wrap t)

(defun mew-mime-text/plain (cache begin end &optional params)
  ;; called in Message buffer
  (when (> end begin)
    (save-excursion
      ;; We need to keep properties (e.g. citation color)
      ;; in a message cache.
      ;; This must be "insert-buffer-substring".
      (insert-buffer-substring cache begin end)
      (when mew-insert-final-newline
	(save-excursion
	  (goto-char (point-max))
	  (if (not (bolp)) (insert "\n"))))
      (if (and mew-flowed-auto-wrap
	       (mew-syntax-get-param params "format")
	       (string= (downcase (mew-syntax-get-param params "format"))
			"flowed"))
	  (mew-message-line))
      ;; Page breaks
      (when mew-break-pages
	(goto-char (point-min))
	(mew-message-narrow-to-page)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/Enriched
;;;

(defun mew-mime-text/enriched (cache begin end &optional params)
  ;; called in Message buffer
  (when (> end begin)
    (save-excursion
      (let ((start (point)))
	;; We need to keep composite properties of charset.
	;; This must be "insert-buffer-substring".
	(insert-buffer-substring cache begin end)
	;; Highlight
	(when mew-use-text/enriched
	  (condition-case nil
	      ;; format.el is buggy.
	      (format-decode-region start (point-max) 'text/enriched)
	    (args-out-of-range (message nil))) ;; just clear
	  (enriched-mode 0)))
      ;; Page breaks
      (when mew-break-pages
	(goto-char (point-min))
	(mew-message-narrow-to-page)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/Html
;;;

(defun mew-mime-text/html (cache begin end &optional params)
  ;; called in Message buffer
  (if (and (symbolp mew-prog-text/html) (fboundp mew-prog-text/html))
      (save-excursion
	(let ((start (point))
	      folder)
	  ;; We need to keep composite properties of charset.
	  ;; This must be "insert-buffer-substring".
	  (insert-buffer-substring cache begin end)
	  (with-current-buffer cache
	    (setq folder (mew-cinfo-get-fld)))
	  (if (or mew-use-text/html
		  (and mew-use-text/html-list
		       (mew-folder-spec folder
					mew-use-text/html-list
					mew-use-text/html-string-type
					mew-use-text/html-list-type)))
	      (funcall mew-prog-text/html start (point-max))
	    (mew-message-for-summary "To parse HTML, type '\\[mew-summary-analyze-again]'"))))
    (insert " #     # ####### #     # #\n"
	    " #     #    #    ##   ## #\n"
	    " #     #    #    # # # # #\n"
	    " #######    #    #  #  # #\n"
	    " #     #    #    #     # #\n"
	    " #     #    #    #     # #\n"
	    " #     #    #    #     # #######\n"
	    "\n\n")
    (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
    (mew-insert "Browser:\t%s\n" (or (mew-progspec-get-prog mew-prog-text/html-ext) "none"))
    (insert "\n")
    (mew-mime-part-messages t)))

(defun mew-text/html-detect-cs (begin end)
  (let ((case-fold-search t))
    (save-excursion
      (goto-char begin)
      (when (or (re-search-forward
		 (concat "<meta[ \t]+http-equiv=\"?content-type\"?[ \t]"
			 "+content=\"[^;]+"
			 ";[ \t]*charset=\"?\\([^\"]+\\)\"?[ \t]*/?>")
		 (min end (+ begin 1024)) t)
		(re-search-forward
		 (concat "<meta[ \t]+content=\"[^;]+"
			 ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
			 "[ \t]+http-equiv=\"?content-type\"?[ \t]*/?>")
		 (min end (+ begin 1024)) t)
		(re-search-forward
		 "<\\?xml.*encoding=['\"]\\([^'\"]+\\)['\"] *\\?>"
		 (min end (+ begin 1024)) t))
	(mew-charset-to-cs (mew-match-string 1))))))

(defun mew-mime-text/html-ext (cache begin end &optional params)
  (mew-mime-markup-language-ext
   mew-prog-text/html-ext cache begin end params "HTML" mew-format-html))

(defun mew-mime-markup-language-ext (program cache begin end params tag form)
  ;; called in Message buffer
  (when (> end begin)
    (let* ((file (format form (mew-make-temp-name)))
	   (prog (mew-progspec-get-prog program))
	   (args (mew-progsec-args-convert (mew-progspec-get-args program) file))
	   wcs)
      (with-current-buffer cache
	(message "Displaying %s..." tag)
	;; charset check
	(setq wcs (mew-text/html-detect-cs begin end))
	;; note that application/xml may have the charset parameter
	(unless (mew-coding-system-p wcs)
	  (setq wcs (mew-charset-to-cs
		     (mew-syntax-get-param params "charset"))))
	(unless (mew-coding-system-p wcs)
	  (setq wcs mew-cs-text-for-write))
	(mew-frwlet mew-cs-dummy wcs
	  (write-region begin end file nil 'no-msg)
	  (apply 'mew-start-process-disp prog nil prog args))
	(message "Displaying %s...done" tag)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/XML
;;;

(defun mew-mime-text/xml (cache begin end &optional params)
  ;; called in Message buffer
  (if (and (symbolp mew-prog-text/xml) (fboundp mew-prog-text/xml))
      (save-excursion
	(let ((start (point)))
	  ;; We need to keep composite properties of charset.
	  ;; This must be "insert-buffer-substring".
	  (insert-buffer-substring cache begin end)
	  (if mew-use-text/xml
	      (funcall mew-prog-text/xml start (point-max))
	    (mew-message-for-summary "To parse XML, type '\\[mew-summary-analyze-again]'"))))
    (insert " #     # #     # #\n"
	    "  #   #  ##   ## #\n"
	    "   # #   # # # # #\n"
	    "    #    #  #  # #\n"
	    "   # #   #     # #\n"
	    "  #   #  #     # #\n"
	    " #     # #     # #######\n"
	    "\n\n")
    (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
    (mew-insert "Browser:\t%s\n" (or (mew-progspec-get-prog mew-prog-text/xml-ext) "none"))
    (insert "\n")
    (mew-mime-part-messages t)))

(defun mew-mime-text/xml-ext (cache begin end &optional params)
  (mew-mime-markup-language-ext
   mew-prog-text/xml-ext cache begin end params "XML" mew-format-xml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application/Xml
;;;

(defun mew-mime-application/xml (cache begin end &optional params)
  ;; called in Message buffer
  (if (and (symbolp mew-prog-application/xml)
	   (fboundp mew-prog-application/xml))
      (save-excursion
	(let ((start (point)))
	  ;; We need to keep composite properties of charset.
	  ;; This must be "insert-buffer-substring".
	  (insert-buffer-substring cache begin end)
	  (funcall mew-prog-application/xml start (point-max))))
    (insert " #     # #     # #\n"
	    "  #   #  ##   ## #\n"
	    "   # #   # # # # #\n"
	    "    #    #  #  # #\n"
	    "   # #   #     # #\n"
	    "  #   #  #     # #\n"
	    " #     # #     # #######\n"
	    "\n\n")
    (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
    (mew-insert "Browser:\t%s\n" (or (mew-progspec-get-prog mew-prog-application/xml-ext) "none"))
    (insert "\n")
    (mew-mime-part-messages t)))

(defun mew-mime-application/xml-ext (cache begin end &optional params)
  (mew-mime-markup-language-ext
   mew-prog-application/xml-ext cache begin end params "XML" mew-format-xml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/X-Patch
;;;

(defun mew-mime-text/patch-ext (cache begin end &optional params)
  (when (> end begin)
    (save-excursion
      (switch-to-buffer-other-frame "*Mew Patch*")
      (delete-other-windows)
      (erase-buffer)
      (insert-buffer-substring cache begin end)
      (diff-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image/*
;;;

(defun mew-mime-image/* (cache begin end &optional params fname ct cte)
  (let* ((format (mew-mime-image-format-name ct)))
    (if (mew-image-inline-p format)
	(mew-mime-image cache begin end format)
      (insert "  #####  #     #    #     #####  #######\n"
	      "    #    ##   ##   # #   #     # #      \n"
	      "    #    # # # #  #   #  #       #      \n"
	      "    #    #  #  # #     # #  #### #######\n"
	      "    #    #     # ####### #     # #      \n"
	      "    #    #     # #     # #     # #      \n"
	      "  #####  #     # #     #  #####  #######\n"
	      "\n\n")
      (mew-insert "Content-Type:\t%s\n" ct)
      (mew-insert "Encoding:\t%s\n" cte)
      (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
      (mew-insert "Filename:\t%s\n" fname)
      (mew-insert "Program:\t%s\n" mew-prog-image/*-ext)
      (insert "\n")
      (mew-mime-part-messages t))))

(defun mew-mime-image/*-ext (cache begin end &optional params fname ct cte)
  (let ((file (mew-make-temp-name fname)))
    (with-current-buffer cache
      (mew-flet
       (write-region begin end file nil 'no-msg)))
    (mew-mime-image-ext file)))

(defun mew-mime-image-ext (file)
  (let* ((spec mew-prog-image/*-ext)
	 (prog (mew-progspec-get-prog spec))
	 (args (mew-progspec-get-args spec)))
    (mew-mime-start-process prog args file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message/Rfc822
;;;

(defun mew-mime-text/rfc822-headers (cache begin end &optional params)
  ;; called in Message buffer
  (when (> end begin)
    (save-excursion
      ;; We need to keep properties of a header.
      ;; This must be "insert-buffer-substring".
      (insert-buffer-substring cache begin end)
      (mew-header-arrange (point-min) (point-max)))))

(defun mew-mime-message/rfc822 (cache part)
  ;; called in Message buffer
  (let* ((hbeg (mew-syntax-get-begin part))
	 (hend (mew-syntax-get-end   part))
	 (body (mew-syntax-get-part part)))
    ;; We need to keep properties of a header.
    ;; This must be "insert-buffer-substring".
    (insert-buffer-substring cache hbeg hend)
    (mew-header-arrange (point-min) (point-max))
    (cond
     ;; Displaying the text/plain body or the first part of
     ;; top level multipart if it is text/plain.
     ;; see also mew-syntax-singlepart
     ((mew-syntax-singlepart-p body)
      (mew-mime-part cache body nil)) ;; nil is single
     ((mew-syntax-multipart-p body)
      (let* ((first (mew-syntax-get-part body))
	     (ct (mew-syntax-get-value (mew-syntax-get-ct first) 'cap)))
	(when (and (mew-xinfo-get-text-body) (mew-ct-textp ct))
	  (mew-mime-part cache first nil))))))) ;; nil is single

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application/Octet-Stream
;;;

(defun mew-mime-application/octet-stream (cache begin end &optional params ct cte cd fl)
  (insert " ######    ###   #     #    #    ######  #     #\n"
	  " #     #    #    ##    #   # #   #     #  #   #\n"
	  " #     #    #    # #   #  #   #  #     #   # #\n"
	  " ######     #    #  #  # #     # ######     #\n"
	  " #     #    #    #   # # ####### #   #      #\n"
	  " #     #    #    #    ## #     # #    #     #\n"
	  " ######    ###   #     # #     # #     #    #\n"
	  "\n\n")
  (mew-insert "Content-Type:\t%s\n" ct)
  (mew-insert "Encoding:\t%s\n" cte)
  (when params
    (mew-insert
     "Parameters:\t%s\n"
     (mapconcat (lambda (x) (concat (nth 0 x) "=" (nth 1 x))) params ", ")))
  (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
  (mew-insert "Filename:\t%s\n" fl)
  (mew-insert "Description:\t%s\n" cd)
  (mew-insert-manual
   "\nTo specify appropriate Content-Type: \n"
   "and execute an internal/external function/command, "
   "type \\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n")
  (insert "\n")
  (mew-mime-part-messages nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application/Msoffice
;;;

(defun mew-mime-application/msword (cache begin end &optional parameter)
  (mew-mime-application/msoffice
   mew-prog-application/msword cache begin end parameter))

(defun mew-mime-application/msexcel (cache begin end &optional parameter)
  (mew-mime-application/msoffice
   mew-prog-application/msexcel cache begin end parameter))

(defun mew-mime-application/mspowerpoint (cache begin end &optional parameter)
  (mew-mime-application/msoffice
   mew-prog-application/mspowerpoint cache begin end parameter))

(defun mew-mime-application/rtf (cache begin end &optional parameter)
  (mew-mime-application/msoffice
   mew-prog-application/rtf cache begin end parameter))

(defun mew-mime-application/rtf-ext (cache begin end &optional parameter)
  (save-excursion
    (mew-mime-application/rtf cache begin end parameter)))

(defun mew-mime-application/msoffice (prog cache begin end &optional parameter)
  (let ((doit t) file1 file2)
    (unless mew-internal-utf-8p
      (condition-case nil
	  (require 'un-define)
	(file-error
	 (setq doit nil)
	 (insert "To display this, install Mule-UCS for UTF-8.\n"))))
    (unless (mew-which-exec prog)
      (setq doit nil)
      (insert "To display this, install \"" prog "\".\n"))
    (condition-case nil
	(require 'mew-w3m)
      (file-error
       (setq doit nil)
       (insert "To display this, install \"w3m.el\".\n")))
    (if (not doit)
	(progn
	  (insert "\n")
	  (mew-mime-part-messages nil))
      (message "Displaying an MS document...")
      (mew-erase-buffer)
      (setq file1 (mew-make-temp-name))
      (with-current-buffer cache
	(mew-flet
	 (write-region begin end file1 nil 'no-msg)))
      (setq file2 (mew-make-temp-name))
      (if (eq prog mew-prog-application/msword)
	  (mew-frwlet 'utf-8 mew-cs-dummy
	    (if mew-use-old-wvhtml
		(call-process prog nil nil nil file1 file2)
	      (call-process prog nil nil nil
			    "--charset=utf-8"
			    (concat "--targetdir=" (file-name-directory file2))
			    file1
			    (file-name-nondirectory file2)))
	    (let ((buffer-file-coding-system)) ;; to prevent the side effect
	      (mew-insert-file-contents file2)))
	(if (eq prog mew-prog-application/rtf)
	    (mew-frwlet 'shift_jis mew-cs-dummy
	      (call-process prog nil (current-buffer) nil file1))
	  (mew-frwlet 'utf-8 mew-cs-dummy
	    (call-process prog nil (current-buffer) nil file1))))
      (mew-delete-file file1)
      (mew-delete-file file2)
      (save-excursion
	(w3m-region (point-min) (point-max)
		    (w3m-expand-file-name-as-url (file-name-directory file2))))
      (message "Displaying an MS document...done"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PDF
;;;

(defun mew-mime-application/pdf (cache begin end &optional parameter)
  (message "Displaying a PDF document...")
  (mew-erase-buffer)
  (let ((doit t) (prog mew-prog-application/pdf)
	file1 file2)
    (unless (mew-which-exec prog)
      (setq doit nil)
      (mew-elet (insert "To display this, install \"" prog "\".\n")))
    (if (not doit)
	(progn
	  (mew-elet (insert "\n"))
	  (mew-mime-part-messages t))
      (setq file1 (mew-make-temp-name))
      (with-current-buffer cache
	(mew-flet
	 (write-region begin end file1 nil 'no-msg)))
      (setq file2 (mew-make-temp-name))
      (call-process prog nil nil nil "-layout" "-enc" "UTF-8" file1 file2)
      (when (file-exists-p file2)
	(mew-frwlet 'utf-8 mew-cs-dummy
	  (insert-file-contents file2)))
      (if (file-exists-p file1) (delete-file file1))
      (if (file-exists-p file2) (delete-file file2))
  (message "Displaying a PDF document...done"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application/Ms-Tnef
;;;

(defun mew-mime-application-ms-tnef (cache begin end &optional parameter)
  (insert " ####### #     # ####### ########\n"
	  "    #    ##    # #       #\n"
	  "    #    # #   # #       #\n"
	  "    #    #  #  # ####### ######\n"
	  "    #    #   # # #       #\n"
	  "    #    #    ## #       #\n"
	  "    #    #     # ####### #\n"
	  "\n\n")
  (mew-insert-manual
   "To extract files, type "
   "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"))

(defvar mew-prog-tnef "tnef")

(defun mew-mime-application-ms-tnef-ext (cache begin end &optional parameter)
  ;; called in Message buffer
  (let ((file (mew-make-temp-name))
	(dir (mew-input-directory-name mew-home)))
    (if (not (mew-which-exec mew-prog-tnef))
	(message "'%s' not found" mew-prog-tnef)
      (with-current-buffer cache
	(mew-plet
	 (write-region begin end file nil 'no-msg)))
      (mew-erase-buffer)
      (insert "Extracted: \n\n")
      (call-process mew-prog-tnef file t nil "--verbose" "-C" dir)
      (mew-delete-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing external commands
;;;

(defun mew-summary-ask-ct (ct fname)
  (let (pt fl)
    (cond
     (fname
      (setq ct (or (mew-ctdb-ct (mew-ctdb-by-file fname))
		   (mew-content-type (mew-sinfo-get-case))))
      (setq pt "Type for %s (%s): ")
      (setq fl fname))
     (t
      (setq pt "Type %s(%s): ")
      (setq fl "")))
    (mew-input-type pt fl ct mew-mime-content-type-list)))

(defun mew-summary-execute-external (&optional ask-command)
  "Execute an external command according to Content-Type:.
If this command is executed on the entire message, the first part
is chosen as a target.

If Content-Type of the target part is Application/Octet-Stream,
Content-Type is automatically asked. And if it has both
internal and external visualization mechanisms, you are asked
which you want to use.

If executed with '\\[universal-argument]', you can specify a
command to be executed.

See 'mew-mime-content-type' to know how actions can be defined."
  (interactive "P")
  (mew-summary-msg-or-part
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number2))
	  (nums (mew-syntax-nums))
	  (cache (mew-cache-hit fld msg 'must-hit))
	  (syntax (mew-cache-decode-syntax cache))
	  (stx (mew-syntax-get-entry syntax nums))
	  (ctl (mew-syntax-get-ct stx))
	  (ct (mew-syntax-get-value ctl 'cap))
	  (win (selected-window))
	  begin end params cdpl fname program options async was-apo erase-p
	  pro-opt ent1 ent2)
     (when (and (string= ct mew-ct-msg)
		(or (not ask-command)
		    (not (y-or-n-p "Save the entire message (y) or the first part (n)? "))))
       (setq stx (mew-syntax-get-part stx))
       (if (mew-syntax-multipart-p stx)
	   (setq stx (mew-syntax-get-part stx)))
       (setq ctl (mew-syntax-get-ct stx))
       (setq ct (mew-syntax-get-value ctl 'cap)))
     (setq begin (mew-syntax-get-begin stx))
     (setq end (mew-syntax-get-end stx))
     (setq params (mew-syntax-get-params ctl))
     (setq cdpl (mew-syntax-get-cdp stx))
     (setq fname (mew-syntax-get-filename cdpl ctl))
     (when (or (string= ct mew-ct-apo)
	       (eq (mew-ctdb-prog (mew-ctdb-by-ct ct))
		   'mew-mime-application/octet-stream))
       (setq ct (mew-summary-ask-ct ct fname))
       (setq was-apo t))
     (if (not ask-command)
	 (setq program (mew-ctdb-prog (mew-ctdb-by-ct ct)))
       (setq pro-opt (mew-input-command mew-default-external-program))
       (setq program (car pro-opt))
       (setq options (cdr pro-opt)))
     ;; prog
     ;; func => called for both
     ;; (prog ...)
     ;; (nil prog) == prog
     ;; (nil (prog...)) == (prog...)
     ;; (nil func) != func
     ;; (func func) => need to select
     ;; (func prog) => need to select
     ;; (func (prog ...)) => need to select
     (cond
      ((stringp program)
       (setq async t))
      ((symbolp program)
       (setq erase-p t))
      ((listp program)
       (setq ent1 (nth 0 program))
       (setq ent2 (nth 1 program))
       (cond
	((stringp ent1)
	 (mew-set '(program options async) program))
	((null ent1)
	 (cond
	  ((stringp ent2)
	   (setq program ent2)
	   (setq async t))
	  ((listp ent2)
	   (mew-set '(program options async) ent2))
	  (t ;; symbol
	   (setq program ent2))))
	(t ;; symbol
	 (if (and was-apo
		  (y-or-n-p "Internal (y) or External (n)? "))
	     (progn
	       (setq erase-p t)
	       (setq program ent1))
	   (setq program ent2))
	 (cond
	  ((stringp program)
	   (setq async t))
	  ((listp program)
	   (mew-set '(program options async) program)))))))
     (mew-summary-toggle-disp-msg 'on)
     (mew-window-configure 'message)
     (if erase-p (mew-erase-buffer))
     ;; message buffer
     (unwind-protect
	 (mew-elet
	  (cond
	   ((stringp program)
	    (mew-summary-execute-program
	     program ct ctl cache begin end params fname options async))
	   ((symbolp program)
	    (mew-summary-execute-symbol
	     program ct ctl cache begin end params fname was-apo)))
	  (mew-summary-display-postscript 'no-hook))
       (select-window win)))))

(defun mew-summary-execute-symbol (program ct ctl cache begin end params fname was-apo)
  (cond
   ((not (fboundp program))
    (message "%s is not implemented" (symbol-name program)))
   ((eq program mew-prog-rfc822)
    (message "%s cannot be executed" mew-prog-rfc822))
   ((or (mew-ct-imagep ct) (mew-ct-modelp ct))
    (funcall program cache begin end params fname ct))
   (t
    (let ((mew-use-text/html t)
	  (mbuf (current-buffer))
	  tbuf)
      (if (and was-apo (mew-ct-textp ct)) ;; decode-broken?
	  (with-temp-buffer
	    (insert-buffer-substring cache begin end)
	    (mew-cs-decode-region (point-min) (point-max) mew-cs-autoconv)
	    (setq tbuf (current-buffer))
	    (setq begin (point-min))
	    (setq end (point-max))
	    (set-buffer mbuf)
	    (funcall program tbuf begin end params))
	(funcall program cache begin end params))))))

;;; external
(defun mew-summary-execute-program (program ct ctl cache begin end params fname options async)
  (if (not (mew-which-exec program))
      (message "%s does not exist" program)
    (let ((file (mew-make-temp-name fname))
	  wcs)
      (with-current-buffer cache
	;; NEVER use call-process-region for privacy reasons
	(cond
	 ((not (mew-ct-linebasep ct))
	  (setq wcs mew-cs-binary))
	 ((not (mew-ct-textp ct))
	  (setq wcs mew-cs-text-for-write))
	 (t
	  (cond
	   ((or (string= mew-ct-htm ct) (string= mew-ct-xml ct))
	    (setq wcs (mew-text/html-detect-cs begin end))
	    (unless (mew-coding-system-p wcs)
	      (setq wcs (mew-charset-to-cs
			 (mew-syntax-get-param ctl "charset")))))
	   (t
	    (setq wcs (mew-charset-to-cs
		       (mew-syntax-get-param ctl "charset")))))
	  (unless (mew-coding-system-p wcs)
	    (setq wcs (if mew-decode-broken
			  (mew-charset-to-cs
			   (mew-charset-guess-region
			    begin end))
			mew-cs-text-for-write)))))
	(mew-frwlet mew-cs-dummy wcs
	  (write-region begin end file nil 'no-msg))
	(if async
	    (mew-mime-start-process program options file)
	  (mew-mime-call-process program options file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; unzip
;;;

(defvar mew-ct-zip-list '("Application/Zip"
			  "Application/X-Zip-Compressed"
			  "Application/Octet-Stream"))

(defun mew-ct-zip-p (ct)
  (member ct mew-ct-zip-list))

(defun mew-summary-unzip ()
  (interactive)
  (let* ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number2))
	 (nums (mew-syntax-nums))
	 (cache (mew-cache-hit fld msg 'must-hit))
	 (syntax (mew-cache-decode-syntax cache))
	 (stx (mew-syntax-get-entry syntax nums))
	 (ct (mew-syntax-get-value (mew-syntax-get-ct stx) 'cap))
	 (file0 (mew-syntax-get-param (mew-syntax-get-cdp stx) "filename"))
	 (beg (mew-syntax-get-begin stx))
	 (end (mew-syntax-get-end stx))
	 (size (- end beg))
	 (dir mew-temp-dir)
	 file size1 end1)
    (if (not (mew-ct-zip-p ct))
	(message "Cannot unzip"))
    (setq file (mew-unzip-file cache beg end dir file0))
    (if (not file)
	(message "unzip failed")
      (let ((ct (mew-ctdb-ct (mew-ctdb-by-file file)))
	    cs)
	(unless ct
	  (let ((txtp (mew-ct-textp (mew-content-type "default")))
		(file- (file-name-nondirectory file)))
	    (setq ct (if txtp
			 (if (y-or-n-p (format "\"%s\" as text? " file-))
			     mew-ct-txt mew-ct-apo)
		       (if (y-or-n-p (format "\"%s\" as binary? " file-))
			   mew-ct-apo mew-ct-txt)))))
	(setq cs (if (mew-ct-textp ct) mew-cs-autoconv mew-cs-binary))
	(with-current-buffer cache
	  (delete-region beg end)
	  (goto-char beg)
	  (save-restriction
	    (narrow-to-region beg beg)
	    (mew-frwlet cs mew-cs-dummy
	      (insert-file-contents (expand-file-name file dir)))
	    (mew-syntax-set-ct stx (list ct))
	    (setq end1 (point-max))
	    (mew-syntax-set-end stx end1)
	    (setq size1 (- end1 beg))
	    (mew-syntax-set-cdp stx (mew-syntax-cdp-format ct file)))
	  (mew-decode-syntax-adjust-message mew-decode-syntax beg (- size1 size))
	  (mew-xinfo-set-multi-form nil)
	  (mew-decode-syntax-set))
	(let ((current (point))
	      (start (mew-decode-syntax-begin)))
	  (mew-decode-syntax-delete)
	  (mew-decode-syntax-copy cache)
	  (goto-char start)
	  (forward-line -1)
	  (mew-decode-syntax-print (current-buffer)
				   mew-decode-syntax
				   (mew-xinfo-get-multi-form)
				 (mew-xinfo-get-icon-spec))
	  (goto-char current)
	  (mew-summary-display 'redisplay))))))

(defun mew-decode-syntax-adjust (val threshold inc)
  (if (> val threshold) (+ val inc) val))

(defun mew-decode-syntax-adjust-single (syntax threshold inc)
  (unless (= (mew-syntax-get-begin syntax) threshold)
    (mew-syntax-set-begin
     syntax (mew-decode-syntax-adjust
	     (mew-syntax-get-begin syntax) threshold inc))
    (mew-syntax-set-end
     syntax (mew-decode-syntax-adjust
	     (mew-syntax-get-end syntax) threshold inc))))

(defun mew-decode-syntax-adjust-message (syntax threshold inc)
  (mew-decode-syntax-adjust-single syntax threshold inc)
  (let ((body (mew-syntax-get-part syntax)))
    (if (mew-syntax-multipart-p body)
	(mew-decode-syntax-adjust-multi body threshold inc)
      (mew-decode-syntax-adjust-single body threshold inc))))

(defun mew-decode-syntax-adjust-multi (syntax threshold inc)
  (mew-decode-syntax-adjust-single syntax threshold inc)
    (let ((i mew-syntax-magic)
	  (len (length syntax))
	  part)
      (while (< i len)
	(setq part (aref syntax i))
	(cond
	 ((mew-syntax-singlepart-p part)
	  (mew-decode-syntax-adjust-single part threshold inc))
	 ((mew-syntax-multipart-p part)
	  (mew-decode-syntax-adjust-multi part threshold inc))
	 ((mew-syntax-message-p part)
	  (mew-decode-syntax-adjust-message part threshold inc)))
	(setq i (1+ i)))))

(defun mew-unzip-file (buf beg end dir file)
  (let* ((zipfile (expand-file-name file dir))
	 (encrypted (mew-zip-encrypted-p buf beg))
	 (password (if encrypted (read-passwd "Zip password: ")))
	 (args0 (list "-o" "-d" dir zipfile))
	 (args (if password (cons "-P" (cons password args0)) args0)))
    (with-current-buffer buf
      (mew-frwlet mew-cs-dummy mew-cs-binary
	(write-region beg end zipfile nil 'no-msg)))
    (with-temp-buffer
      (apply 'call-process "unzip" nil t nil args)
      (goto-char (point-max))
      (forward-line -1)
      (beginning-of-line)
      (when (looking-at "^ *[a-z]+: \\([^ ]+\\)")
	(mew-match-string 1)))))

(defun mew-zip-encrypted-p (buf beg)
  (with-current-buffer buf
    (goto-char beg)
    (forward-char 6)
    (= (% (char-after) 2) 1)))

(provide 'mew-mime)

;;; Copyright Notice:

;; Copyright (C) 1997-2011 Mew developing team.
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

;;; mew-mime.el ends here
