;;; mew-blvs.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  1, 1999

;;; Code:

(require 'mew)

;;;
;;; Buffer local variables
;;;

;; All buffer local variables which Mew defines should be here.
;; They are made buffer local by make-variable-buffer-local.
;; They are used internal purpose only (i.e. not user options).
;; When Mew makes a buffer local variable which another package
;; defines, make-local-variable should be used.

;; Inevitable due to mode-line.
(defvar mew-summary-buffer-process nil)
(defvar mew-summary-buffer-process-status nil)
(defvar mew-summary-buffer-process-error nil)
(defvar mew-summary-buffer-secure-process nil)
(defvar mew-summary-buffer-left-msgs nil)
(defvar mew-summary-buffer-raw nil)

(defvar mew-decode-syntax nil) ;; just for convenience
(defvar mew-encode-syntax nil) ;; just for convenience

(defvar mew-ainfo nil) ;; Any buffers
(defvar mew-cinfo nil) ;; Cache
(defvar mew-dinfo nil) ;; Decode
(defvar mew-minfo nil) ;; Message mode
(defvar mew-sinfo nil) ;; Summary mode
(defvar mew-tinfo nil) ;; Draft mode
(defvar mew-vinfo nil) ;; Virtual mode
(defvar mew-xinfo nil) ;; Analyzed

(mapc 'make-variable-buffer-local
      (list 'mew-summary-buffer-process
	    'mew-summary-buffer-process-status
	    'mew-summary-buffer-process-error
	    'mew-summary-buffer-secure-process
	    'mew-summary-buffer-left-msgs
	    'mew-summary-buffer-raw
	    'mew-decode-syntax
	    'mew-encode-syntax
	    'mew-ainfo
	    'mew-cinfo
	    'mew-dinfo
	    'mew-minfo
	    'mew-sinfo
	    'mew-tinfo
	    'mew-vinfo
	    'mew-xinfo))

(provide 'mew-blvs)

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

;;; mew-blvs.el ends here
