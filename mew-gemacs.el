;;; mew-gemacs.el --- Environment of Graphical Emacs for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jun 22, 2000

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menu setting
;;;

(easy-menu-define
 mew-summary-mode-menu
 mew-summary-mode-map
 "Menu used in Summary mode."
 mew-summary-mode-menu-spec)

(easy-menu-define
 mew-message-mode-menu
 mew-message-mode-map
 "Menu used in Message mode."
 mew-message-mode-menu-spec)

(easy-menu-define
 mew-draft-mode-menu
 mew-draft-mode-map
 "Menu used in Draft mode."
 mew-draft-mode-menu-spec)

(easy-menu-define
 mew-header-mode-menu
 mew-header-mode-map
 "Menu used in Header mode."
 mew-header-mode-menu-spec)

(easy-menu-define
 mew-draft-header-menu
 mew-draft-header-map
 "Menu used in Draft mode."
 mew-draft-mode-menu-spec)

(easy-menu-define
 mew-draft-attach-menu
 mew-draft-attach-map
 "Menu used in Draft mode."
 mew-draft-mode-menu-spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toolbar functions
;;;

(defvar mew-summary-toolbar nil)
(defvar mew-draft-toolbar nil)

(defun mew-toolbar-make (map alist)
  (let ((tool-bar-map (make-sparse-keymap)) ;; for tool-bar-add-item-from-menu
	(data-directory mew-icon-directory))
    (dolist (a alist)
      (if (fboundp 'tool-bar-local-item-from-menu)
	  ;; Emacs 21.3.50 or later
	  (tool-bar-local-item-from-menu (car a) (cdr a) tool-bar-map map)
	;; Emacs 21.3 or earlier
	;; The target map is tool-bar-map
	(tool-bar-add-item-from-menu (car a) (cdr a) map)))
    tool-bar-map))

(defun mew-summary-setup-decoration ()
  (if mew-icon-p
      (set (make-local-variable 'tool-bar-map)
	   (mew-toolbar-make mew-summary-mode-map mew-summary-toolbar-spec))))

(defun mew-message-setup-decoration ()
  (if mew-icon-p
      (set (make-local-variable 'tool-bar-map)
	   (mew-toolbar-make mew-message-mode-map mew-message-toolbar-spec))))

(defun mew-draft-setup-decoration ()
  (if mew-icon-p
      (set (make-local-variable 'tool-bar-map)
	   (mew-toolbar-make mew-draft-mode-map mew-draft-toolbar-spec))))

(defun mew-header-setup-decoration ()
  (if mew-icon-p
      (set (make-local-variable 'tool-bar-map)
	   (mew-toolbar-make mew-header-mode-map mew-header-toolbar-spec))))

(defun mew-summary-toolbar-update ())
(defun mew-message-toolbar-update ())
(defun mew-draft-toolbar-update ())
(defun mew-header-toolbar-update ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of messages
;;;

(defun mew-message-set-end-of-message ()
  (overlay-put (mew-minfo-get-eom) 'before-string mew-end-of-message-string))

(defun mew-message-set-end-of-part ()
  (overlay-put (mew-minfo-get-eom) 'before-string mew-end-of-part-string))

(defun mew-message-set-end-of-nil ()
  (overlay-put (mew-minfo-get-eom) 'before-string nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Veil
;;;

(defun mew-header-veil-make ()
  (let ((ov (mew-overlay-make 1 1)))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'before-string mew-header-veil-string)
    (delete-overlay ov) ;; detach from the buffer
    ov))

(defun mew-toggle-header-veil (ov)
  (cond
   ((overlay-get ov 'invisible)
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'before-string nil))
   (t
    (overlay-put ov 'invisible t)
    (overlay-put ov 'before-string mew-header-veil-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image
;;;

(defcustom mew-image-display-resize t
  "*If non-nil, image will be displayed with fitting to frame size"
  :group 'mew-message
  :type 'boolean)

(defcustom mew-image-display-resize-care-height t
  "*If nil, image will be displayed with fitting to only frame width"
  :group 'mew-message
  :type 'boolean)

(defcustom mew-image-width-margin 45
  "*A value for width margin to display image when resizing"
  :group 'mew-message
  :type 'integer)

(defcustom mew-image-height-margin 200
  "A value for height margin to display image when resizing"
  :group 'mew-message
  :type 'integer)

(defvar mew-image-l-endian ?\x49)
(defvar mew-image-b-endian ?\x4d)

(defun mew-image-inline-p (format)
  ;; display-graphic-p
  (and window-system (image-type-available-p format)))

(defun mew-img-get-n (op len)
  (let* ((size 0))
    (if (eq op ?\x49)   ; I(?\x49) or M(?\x4d)
	(dotimes (n len)
	  (setq size (+ size (* (char-after) (expt ?\x100 n))))
	  (forward-char))
      (while (< 0 len)
	  (setq len (- len 1))
	  (setq size (+ size (* (char-after) (expt ?\x100 len))))
	  (forward-char)))
    size))

(defun mew-jpeg-size ()
  (let (c size width height)
    (save-excursion
      (catch 'loop
	(while t
	  (setq c (char-after))
	  (forward-char)
	  (unless (eq c ?\xff)
	    (throw 'loop nil))
	  (setq c (char-after))
	  (forward-char)
	  (cond
	   ((or (eq c ?\xd8)                      ;; SOI
		(eq c ?\xd9)                      ;; EOI
		(and (>= c ?\xd0) (<= c ?\xd7)))) ;; RSTm
	   ((eq c ?\xc0)
	    (forward-char 3)
	    (setq height (mew-img-get-n mew-image-b-endian 2))
	    (setq width (mew-img-get-n mew-image-b-endian 2))
	    (backward-char)
	    (throw 'loop nil))
	   ((and (>= c ?\xc1) (<= c ?\xfe))
	    (setq size (mew-img-get-n mew-image-b-endian 2))
	    (forward-char (- size 2)))
	   (t
	    (throw 'loop nil))))))
    (cons width height)))

(defun mew-png-size ()
  (let (width height)
    (save-excursion
      (forward-char 18)
      ;; length is four bytes
      ;; but we takes lower two bytes
      (setq width (mew-img-get-n mew-image-b-endian 2))
      (forward-char 2)
      (setq height (mew-img-get-n mew-image-b-endian 2))
      (cons width height))))

(defun mew-gif-size ()
  (let (width height)
    (save-excursion
      (forward-char 6)
      (setq width (mew-img-get-n mew-image-l-endian 2))
      (setq height (mew-img-get-n mew-image-l-endian 2))
      (cons width height))))

(defun mew-tiff-size ()
  (let (endian size entry tag width height)
    (save-excursion
      (setq endian (char-after))
      (forward-char 4)
      (setq size (mew-img-get-n endian 4))
      (forward-char (- size 8)) ;; jump to IFD
      ;; IFD
      (setq entry (mew-img-get-n endian 2))
      (catch 'loop
	(while t
	  (setq entry (- entry 1))
	  (setq tag (mew-img-get-n endian 2))
	  (cond
	   ((eq tag 256)
	    (forward-char 6)
	    (setq width (mew-img-get-n endian 2))
	    (forward-char 2))
	   ((eq tag 257)
	    (forward-char 6)
	    (setq height (mew-img-get-n endian 2))
	    (forward-char 2))
	   (t
	    (forward-char 10)))
	  (unless (< 1 entry)
	    (throw 'loop nil)))))
    (cons width height)))

(defvar mew-image-alist
  '((jpeg mew-jpeg-size "jpegtopnm" "pnmtojpeg")
    (png  mew-png-size  "pngtopnm"  "pnmtopng")
    (gif  mew-gif-size  "giftopnm"  "pnmtogif")
    (tiff mew-tiff-size "tifftopnm" "pnmtotiff")))

(defun mew-image-format-ent (format)
  (assoc format mew-image-alist))

(defun mew-image-get-func (ent)
  (nth 1 ent))

(defun mew-image-get-prog (ent)
  (nth 2 ent))

(defun mew-image-get-prog2 (ent)
  (nth 3 ent))

(defun mew-mime-image (cache begin end format)
  (message "Loading image...")
  (set-buffer (mew-buffer-message))
  (let* ((width (- (frame-pixel-width (selected-frame)) mew-image-width-margin))
	 (height (- (frame-pixel-height (selected-frame)) mew-image-height-margin))
	 (ent (mew-image-format-ent format))
	 (prog (mew-image-get-prog ent))
	 (prog2 (mew-image-get-prog2 ent))
	 (func-size (mew-image-get-func ent))
	 image-size image-width image-height image)
    (with-temp-buffer
      (mew-plet
       (insert-buffer-substring cache begin end)
       (mew-set-buffer-multibyte nil)
       (when (and prog prog2 func-size)
	 (goto-char (point-min))
	 (setq image-size (funcall func-size))
	 (setq image-width (car image-size))
	 (setq image-height (cdr image-size))
	 (when (and image-width image-height
		    (or (< width image-width)
			(and mew-image-display-resize-care-height (< height image-height)))
		    (mew-which-exec prog))
	   (message "Resizing image...")
	   (call-process-region (point-min) (point-max) prog
				t '(t nil) nil)
	   (if mew-image-display-resize-care-height
	       (call-process-region (point-min) (point-max) "pnmscale"
				    t '(t nil) nil
				    "-xysize"
				    (format "%d" width)
				    (format "%d" height))
	     (call-process-region (point-min) (point-max) "pnmscale"
				  t '(t nil) nil
				  "-xsize" (format "%d" width)))
	   (if (and (string< emacs-version "22") ;; xxx
		    (mew-which-exec prog2))
	       (call-process-region (point-min) (point-max) prog2
				    t '(t nil) nil)
	     (setq format 'pbm))
	   (message "Resizing image...done")))
       (setq image (mew-buffer-substring (point-min) (point-max)))))
    (mew-elet
     (condition-case nil
	 (insert-image (mew-create-image image format t))
       (error ()))))
  (goto-char (point-min))
  (message "Loading image...done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transient
;;;

(defun mew-mark-active-p ()
  (and transient-mark-mode mark-active))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X Face
;;;

(defun mew-x-face-create ()
  (mew-create-image
   (string-as-unibyte (mew-buffer-substring (point-min) (point-max)))
   nil t))

(defun mew-x-face-display (xface)
  (save-excursion
    (goto-char (point-min))
    (let ((regex2 (concat "^\\(" mew-from: "\\).*")))
      (when (re-search-forward regex2 nil t)
	(goto-char (match-end 1))
	(insert-image xface)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSL/SSH/TLS notification
;;;

(defvar mew-secure-format nil)

;; Emacs 21.x has a bug that an image cannot be displayed
;; if it is specified in mode-line-process.

(defvar mew-secure-format2
  (if (display-graphic-p)
      (let ((data-directory mew-icon-directory))
	(concat " " (propertize "Sec" 'display
				(find-image '((:type xpm :file "mew-lock.xpm" :ascent center))))))
    " [Sec]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Click hack
;;;

(define-key mew-message-mode-map [mouse-2] 'mew-browse-url-at-mouse)
(if (and (featurep 'emacs) ;; preventing XEmacs's byte compiler from hanging
	 (eq system-type 'darwin))
    ;; for Mac
    (define-key mew-message-mode-map [M-down-mouse-1] 'mew-browse-url-at-mouse))

(provide 'mew-gemacs)

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

;;; mew-gemacs.el ends here
