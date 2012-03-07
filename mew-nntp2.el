;;; mew-nntp2.el for posting

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 19, 1999

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NNTP2 info
;;;

(defvar mew-nntp2-info-list
  '(;; parameters to be saved
    "raw-header" "newsgroups" "fcc" "msgid" "logtime"
    "case" ;; save for re-edit, not for sending
    ;; parameters used internally
    "server" "port" "ssh-server" "user" "account"
    "status" "process" "ssh-process" "ssl-process"
    "qfld" "messages"
    ;; parameters used internally and should be initialized
    "string" "error" "done" "imapp"))

(mew-info-defun "mew-nntp2-" mew-nntp2-info-list)

(defvar mew-nntp2-info-list-save-length 6)
(defvar mew-nntp2-info-list-clean-length 16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FSM
;;;

(defvar mew-nntp2-fsm
  '(("greeting"    ("20[01]" . "mode-reader")) ;; a broken server returns 200 even if post is allowed
    ("mode-reader" (t        . "authinfo"))
    ("authinfo"    ("381"    . "authpass"))
    ("authpass"    ("281"    . "next") (t . "wpwd"))
    ("post"        ("340"    . "post-post"))
    ("post-post"   ("240"    . "done"))
    ("quit"        (t        . "noop"))))

(defun mew-nntp2-fsm-by-status (status)
  (assoc status mew-nntp2-fsm))

(defun mew-nntp2-fsm-next (status code)
  (cdr (mew-assoc-match2 code (nthcdr 1 (mew-nntp2-fsm-by-status status)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filters 2
;;;

(defun mew-nntp2-command-mode-reader (pro pnm)
  (mew-nntp2-process-send-string pro "MODE READER"))

(defun mew-nntp2-command-authinfo (pro pnm)
  (let ((user (mew-nntp2-get-user pnm)))
    (if user
	(mew-nntp2-process-send-string pro "AUTHINFO USER %s" user)
      (mew-nntp2-set-status pnm "next")
      (mew-nntp2-command-next pro pnm))))

(defun mew-nntp2-command-authpass (pro pnm)
  (let* ((prompt (format "NNTP password (%s): " (mew-nntp2-get-account pnm)))
         (pass (mew-nntp2-input-passwd prompt pnm)))
    (mew-nntp2-process-send-string pro "AUTHINFO PASS %s" pass)))

(defun mew-nntp2-command-wpwd (pro pnm)
  (mew-passwd-set-passwd (mew-nntp2-passtag pnm) nil)
  (mew-nntp2-set-error pnm "NNTP password is wrong!")
  (mew-nntp2-command-quit2 pro pnm))

(defun mew-nntp2-command-next (pro pnm)
  (let ((msgs (mew-nntp2-get-messages pnm))
	(qfld (mew-nntp2-get-qfld pnm))
	(case (mew-nntp2-get-case pnm))
	msg)
    (if msgs
	(progn
	  (setq msg (car msgs))
	  (setq msgs (cdr msgs))
	  (mew-queue-insert-file pnm mew-nntp2-info-list-save-length qfld msg)
	  (mew-set-buffer-multibyte nil)
	  (mew-info-clean-up pnm mew-nntp2-info-list-clean-length)
	  (mew-nntp2-set-case pnm case) ;; override
	  (mew-nntp2-set-messages pnm msgs)
	  (set-process-buffer pro (current-buffer))
	  (mew-nntp2-set-status pnm "post")
	  (mew-nntp2-command-post pro pnm))
      (mew-nntp2-set-status pnm "quit")
      (mew-nntp2-command-quit pro pnm))))

(defun mew-nntp2-command-post (pro pnm)
  (mew-nntp2-process-send-string pro "POST"))

(defun mew-nntp2-command-post-post (pro pnm)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (mew-dot-insert)
  (mew-eol-fix-for-write)
  (set-buffer-modified-p nil)
  (process-send-region pro (point-min) (point-max))
  (mew-nntp2-process-send-string pro "."))

(defun mew-nntp2-command-done (pro pnm)
  (let ((fcc (mew-nntp2-get-fcc pnm))
	(case (mew-nntp2-get-case pnm))
	(back (mew-queue-backup (buffer-file-name) mew-queue-info-suffix))
	imapp)
    ;; mew-folder-new-message may be slow if the folder contains
    ;; a lot of messages. So, let's Fcc in background.
    (setq imapp (mew-net-fcc-message case fcc back))
    (mew-nntp2-set-imapp pnm imapp)
    (mew-nntp2-log pnm)
    (mew-nntp2-set-status pro "next")
    (mew-nntp2-command-next pro pnm)))

(defun mew-nntp2-command-quit (pro pnm)
  (mew-nntp2-set-done pnm t)
  (mew-nntp2-process-send-string pro "QUIT"))

(defun mew-nntp2-command-quit2 (pro pnm)
  ;; error is set
  (mew-nntp2-set-done pnm t)
  (when (and (processp pro) (eq (process-status pro) 'open))
    (mew-nntp2-set-status pnm "quit")
    (mew-nntp2-process-send-string pro "QUIT")))

(defun mew-nntp2-command-noop (pro pnm)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defconst mew-nntp2-info-prefix "mew-nntp2-info-")

(defun mew-nntp2-info-name (case)
  (let ((server (mew-nntp-server case))
	(port (mew-*-to-string (mew-nntp-port case)))
	(sshsrv (mew-nntp-ssh-server case))
	(name mew-nntp2-info-prefix))
    (setq name (concat name server))
    (unless (mew-port-equal port mew-nntp-port)
      (setq name (concat name ":" port)))
    (if sshsrv
	(concat name "%" sshsrv)
      name)))

(defun mew-nntp2-process-send-string (pro &rest args)
  (let ((str (apply 'format args)))
    (mew-nntp2-debug "=SEND=" str)
    (if (and (processp pro) (eq (process-status pro) 'open))
	(process-send-string pro (concat str mew-cs-eol))
      (message "NNTP time out"))))

(defun mew-nntp2-passtag (pnm)
  (let ((server (mew-nntp2-get-server pnm))
	(port (mew-nntp2-get-port pnm))
	(user (mew-nntp2-get-user pnm)))
    (concat user "@" server ":" port)))

(defun mew-nntp2-input-passwd (prompt pnm)
  (let* ((tag (mew-nntp2-passtag pnm))
         (pro (mew-nntp2-get-process pnm))
         (pass (mew-input-passwd prompt tag)))
    (unless (and (processp pro) (eq (process-status pro) 'open))
      (mew-passwd-set-passwd tag nil))
    pass))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening NNTP
;;;

(defun mew-nntp2-open (pnm server port)
  (let ((sprt (mew-*-to-port port))
	pro tm)
    (condition-case emsg
	(progn
	  (setq tm (run-at-time mew-nntp-timeout-time nil 'mew-nntp2-timeout))
	  (message "Connecting to the NNTP server...")
	  (setq pro (open-network-stream pnm nil server sprt))
	  (mew-process-silent-exit pro)
	  (mew-set-process-cs pro mew-cs-text-for-net mew-cs-text-for-net)
	  (message "Connecting to the NNTP server...done"))
      (quit
       (setq pro nil)
       (message "Cannot connect to the NNTP server"))
      (error
       (setq pro nil)
       (message "%s, %s" (nth 1 emsg) (nth 2 emsg))))
    (if tm (cancel-timer tm))
    pro))

(defun mew-nntp2-timeout ()
  (signal 'quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-nntp2-send-message (case qfld msgs)
  (let ((server (mew-nntp-server case))
        (user (mew-nntp-user case))
	(port (mew-*-to-string (mew-nntp-port case)))
	(pnm (mew-nntp2-info-name case))
	(sshsrv (mew-nntp-ssh-server case))
	(sslp (mew-nntp-ssl case))
	(sslport (mew-nntp-ssl-port case))
	process sshname sshpro sslname sslpro lport tls)
    (cond
     (sshsrv
      (setq sshpro (mew-open-ssh-stream case server port sshsrv))
      (when sshpro
	(setq sshname (process-name sshpro))
	(setq lport (mew-ssh-pnm-to-lport sshname))
	(when lport
	  (setq process (mew-nntp2-open pnm "localhost" lport)))))
     (sslp
      (if (mew-port-equal port sslport) (setq tls mew-tls-nntp))
      (setq sslpro (mew-open-ssl-stream case server sslport tls))
      (when sslpro
	(setq sslname (process-name sslpro))
	(setq lport (mew-ssl-pnm-to-lport sslname))
	(when lport
	  (setq process (mew-nntp2-open pnm mew-ssl-localhost lport)))))
     (t
      (setq process (mew-nntp2-open pnm server port))))
    (if (null process)
	(cond
	 ((and sshsrv (null sshpro))
	  (message "Cannot create to the SSH connection"))
	 ((and sslp (null sslpro))
	  (message "Cannot create to the SSL/TLS connection"))
	 (t
	  (message "Cannot connect to the NNTP server")))
      (mew-info-clean-up pnm mew-nntp2-info-list-clean-length)
      (mew-nntp2-set-case pnm case)
      (mew-nntp2-set-qfld pnm qfld)
      (mew-nntp2-set-messages pnm msgs)
      (mew-nntp2-set-server pnm server)
      (mew-nntp2-set-port pnm port)
      (mew-nntp2-set-ssh-server pnm sshsrv)
      (mew-nntp2-set-ssh-process pnm sshpro)
      (mew-nntp2-set-ssl-process pnm sslpro)
      (mew-nntp2-set-user pnm user)
      (mew-nntp2-set-account pnm (format "%s@%s" user server))
      (mew-nntp2-set-status pnm "greeting")
      ;;
      (set-process-buffer process nil)
      (set-process-sentinel process 'mew-nntp2-sentinel)
      (set-process-filter process 'mew-nntp2-filter)
      (message "Posting in background..."))))

(defun mew-nntp2-flush-queue (case &optional qfld)
  (let (msgs)
    (unless qfld (setq qfld (mew-postq-folder case)))
    (if (mew-nntp2-get-server (mew-nntp2-info-name case)) ;; lock
	(message "%s is locked" qfld)
      (setq msgs (mew-folder-messages qfld))
      (when msgs
	(mew-summary-folder-cache-clean qfld)
	(run-hooks 'mew-nntp2-flush-hook)
	(message "Flushing %s..." qfld)
	(mew-nntp2-send-message case qfld msgs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel 2
;;;

(defun mew-nntp2-debug (label string)
  (when (mew-debug 'net)
    (with-current-buffer (get-buffer-create mew-buffer-debug)
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-nntp2-filter (process string)
  (let* ((pnm (process-name process))
	 (status (mew-nntp2-get-status pnm))
	 (str (concat (mew-nntp2-get-string pnm) string))
	 (buf (process-buffer process))
	 next func code)
    (save-excursion
      (mew-nntp2-debug (upcase status) string)
      (if (and buf (get-buffer buf)) (set-buffer buf))
      ;; NNTP server's strings should be short enough.
      (mew-nntp2-set-string pnm str)
      (cond
       ((and (string-match "\n$" str)
	     (string-match "^\\([0-9][0-9][0-9]\\) " str))
	(setq code (match-string 1 str))
	(setq next (mew-nntp2-fsm-next status code))
	(cond
	 (next
	  (mew-nntp2-set-status pnm next)
	  (setq func (intern-soft (concat "mew-nntp2-command-" next)))
	  (and func (funcall func process pnm))
	  (mew-nntp2-set-string pnm nil))
	 (t
	  (if (string-match "^pwd-" status)
	      (mew-nntp2-set-error pnm "NNTP password is wrong!")
	    (if (string-match "\n$" str)
		(setq str (substring str 0 -1)))
	    (unless (string-match "[.!]$" str)
	      (setq str (concat str "."))))
	  (mew-nntp2-set-error pnm str)
	  (mew-nntp2-command-quit2 process pnm))))
       (t ()))))) ;; stay

(defun mew-nntp2-sentinel (process event)
  (let* ((pnm (process-name process))
	 (buf (process-buffer process))
	 (qfld (mew-nntp2-get-qfld pnm))
	 (case (mew-nntp2-get-case pnm))
	 (done (mew-nntp2-get-done pnm))
	 (imapp (mew-nntp2-get-imapp pnm))
	 (error (mew-nntp2-get-error pnm))
	 (sshpro (mew-nntp2-get-ssh-process pnm))
	 (sslpro (mew-nntp2-get-ssl-process pnm)))
    (save-excursion
      (mew-nntp2-debug "NNTP SENTINEL" event)
      (cond
       (error
	(when buf
	  ;; A message file is not inserted at the beginning of the NNTP
	  ;; session.
	  (set-buffer buf)
	  (mew-nntp2-queue case error))
	(mew-nntp2-log pnm error)
	(message-box (format "%s  This mail has been queued to %s" error qfld)))
       (done
	(message "Posting in background...done"))
       (t
	(if (null buf)
	    (message "NNTP connection is lost")
	  (set-buffer buf)
	  (mew-nntp2-queue case "NNTP connection is lost"))))
      (mew-info-clean-up pnm)
      (if (and (processp sshpro) (not mew-ssh-keep-connection))
	  (process-send-string sshpro "exit\n"))
      (if (and (processp sslpro) (not mew-ssl-keep-connection))
	  (delete-process sslpro))
      (run-hooks 'mew-nntp2-sentinel-hook)
      (if imapp (mew-imap2-fcc case))
      (when buf (mew-remove-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Queuing
;;;

(defun mew-nntp2-queue (case err)
  ;; Must be in a buffer where a message is contained.
  (let* ((pnm (mew-nntp2-info-name case))
	 (qfld (mew-postq-folder case))
	 (oname (buffer-name))
	 (work (buffer-file-name))
	 file-info file info nname)
    (mew-local-folder-check qfld)
    (setq file-info (mew-queue-enqueue work qfld))
    (mew-set '(file info) file-info)
    (setq file (file-name-nondirectory file))
    (setq nname (mew-concat-folder qfld file))
    (if (mew-draft-p)
	(mew-nntp2-set-case pnm (mew-tinfo-get-case)))
    ;;
    (let* ((n mew-nntp2-info-list-save-length)
	   (data (make-vector n nil)))
      (dotimes (i n)
	(aset data i (aref (mew-info pnm) i)))
      (mew-lisp-save info data))
    ;;
    (mew-remove-buffer (current-buffer))
    (message "%s has been queued to %s (%s)" oname nname err)
    (mew-touch-folder qfld)
    (file-name-sans-extension file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Logging
;;;

(defun mew-nntp2-log (pnm &optional err)
  (let ((logtime (mew-nntp2-get-logtime pnm))
	(msgid (mew-nntp2-get-msgid pnm))
	(newsgroups (mew-nntp2-get-newsgroups pnm))
	(server (mew-nntp2-get-server pnm))
	(sshsrv (mew-nntp2-get-ssh-server pnm))
	(sslp (mew-nntp2-get-ssl-process pnm)))
    (with-temp-buffer
      (and logtime (insert logtime))
      (and msgid (insert " id=" msgid))
      (and server (insert " server=" server))
      (and sshsrv (insert " sshsrv=" sshsrv))
      (and sslp (insert " SSL/TLS"))
      (and newsgroups (insert " newsgroups=" newsgroups))
      (if err
	  (insert " status=" "("
                  (substring err 0 (string-match "\n+$" err))
                  ")")
	(insert " status=sent"))
      (insert "\n")
      (write-region (point-min) (point-max)
		    (expand-file-name mew-nntp-log-file mew-conf-path)
		    'append 'no-msg))))

(provide 'mew-nntp2)

;;; Copyright Notice:

;; Copyright (C) 1999-2012 Mew developing team.
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

;;; mew-nntp2.el ends here
