;;; -*- lexical-binding: t; -*-
;;; mew-test.el --- ERT tests for Mew

;; Author:  Mew developing team
;; Created: Sep 22, 2026

;;; Commentary:

;; Tests for functions which do not depend on the user's environment,
;; that is, no folders, no network and no external programs.
;;
;; Run them with:
;;
;;	make test
;;
;; or directly:
;;
;;	emacs -Q -batch -L . -l mew-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-func.el: strings
;;;

(ert-deftest mew-test-split ()
  (should (equal (mew-split "a:b::c" ?:) '("a" "b" "" "c")))
  (should (equal (mew-split "abc" ?:) '("abc")))
  (should (equal (mew-split "" ?:) nil)))

(ert-deftest mew-test-split-quoted ()
  (should (equal (mew-split-quoted "a,b,c" ?,) '("a" "b" "c")))
  ;; A separator in a quoted string is not a separator.
  (should (equal (mew-split-quoted "\"a,b\",c" ?,) '("\"a,b\"" "c")))
  ;; Single quotes are removed unless NO-SINGLE.
  (should (equal (mew-split-quoted "'x','y'" ?,) '("x" "y")))
  (should (equal (mew-split-quoted "'x','y'" ?, nil nil 'no-single)
                 '("'x'" "'y'")))
  ;; QOPEN/QCLOSE, as mew-encode-canonicalize-address uses it.
  (should (equal (mew-split-quoted "to:a,b;,c" ?, ?: ?\; 'no-single)
                 '("to:a,b;" "c"))))

(ert-deftest mew-test-split-quoted-backslash ()
  "A backslash escapes the next character.
`dotimes' rebinds its variable for each iteration since Emacs 28, so
the loop must not rely on `setq' of the loop variable to skip a
character."
  ;; An escaped double quote must not toggle the quoted state.
  (should (equal (mew-split-quoted "\"a\\\"b\",c" ?,) '("\"a\\\"b\"" "c")))
  ;; An escaped separator is not a separator.
  (should (equal (mew-split-quoted "a\\,b,c" ?,) '("a\\,b" "c")))
  ;; A trailing backslash must not run off the end of the string.
  (should (equal (mew-split-quoted "a,b\\" ?,) '("a" "b\\"))))

(ert-deftest mew-test-chop ()
  (should (equal (mew-chop "  a b \t") "a b"))
  (should (equal (mew-chop "abc") "abc")))

(ert-deftest mew-test-capitalize ()
  (should (equal (mew-capitalize "content-type") "Content-Type"))
  (should (equal (mew-capitalize "TEXT/PLAIN") "Text/Plain")))

(ert-deftest mew-test-quote-string ()
  (should (equal (mew-quote-string "a\"b" ?\\ '(?\")) "a\\\"b"))
  (should (equal (mew-quote-string "ab" ?\\ '(?\")) "ab")))

(ert-deftest mew-test-remove-single-quote ()
  "Both branches are covered: `string-replace' from Emacs 28.1, and
the loop before it, which the oldest Emacs of the CI matrix runs."
  (should (equal (mew-remove-single-quote "'a'b'") "ab"))
  (should (equal (mew-remove-single-quote "ab") "ab"))
  (should (equal (mew-remove-single-quote "") ""))
  (should (equal (mew-remove-single-quote "'''") ""))
  (should (equal (mew-remove-single-quote "'日'本'") "日本")))

(ert-deftest mew-test-set-string-multibyte ()
  "The bytes of a unibyte string are read as Emacs's own encoding.
This is what the obsolete `string-as-multibyte' did."
  ;; Already multibyte: unchanged.
  (should (equal (mew-set-string-multibyte "日本語") "日本語"))
  ;; The utf-8 bytes of a string are read back into it.
  (should (equal (mew-set-string-multibyte
                  (encode-coding-string "日本語" 'utf-8))
                 "日本語"))
  (should (equal (mew-set-string-multibyte (unibyte-string 226 128 148)) "—"))
  ;; A byte which is not valid utf-8 stays one byte instead of signalling.
  (let ((raw (mew-set-string-multibyte (unibyte-string 233 233))))
    (should (multibyte-string-p raw))
    (should (= (length raw) 2)))
  (should (equal (mew-set-string-multibyte "") "")))

(ert-deftest mew-test-replace-white-space ()
  (should (equal (mew-replace-white-space "a \t\n  b") "a b"))
  (should (equal (mew-replace-white-space2 "a \t\r\n b") "a_b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-func.el: files
;;;

(ert-deftest mew-test-file-get-links ()
  "A file which has not been linked to has one link."
  (let ((file (make-temp-file "mew-test")))
    (unwind-protect
        (should (equal (mew-file-get-links file) 1))
      (delete-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-func.el: lists
;;;

(ert-deftest mew-test-uniq-list ()
  (should (equal (mew-uniq-list (list 1 2 1 3 2)) '(1 2 3))))

(ert-deftest mew-test-delete ()
  (should (equal (mew-delete "k" (list (list "k" 1) (list "j" 2) (list "k" 3)))
                 '(("j" 2))))
  (should (equal (mew-delete nil (list (list "k" 1))) '(("k" 1)))))

(ert-deftest mew-test-get-next ()
  (should (eq (mew-get-next '(a b c) 'b) 'c))
  ;; The next member of the last one is the first one.
  (should (eq (mew-get-next '(a b c) 'c) 'a)))

(ert-deftest mew-test-member-case-equal ()
  (should (equal (mew-member-case-equal "CC" '("to" "cc" "bcc")) 1))
  (should-not (mew-member-case-equal "x" '("to" "cc"))))

(ert-deftest mew-test-assoc-case-equal ()
  (should (equal (mew-assoc-case-equal "B" '(("a" 1) ("b" 2)) 0) '("b" 2)))
  (should-not (mew-assoc-case-equal "c" '(("a" 1) ("b" 2)) 0)))

(ert-deftest mew-test-set ()
  "The variables have to be the caller's own.
This file is compiled with lexical binding, so a `mew-set' which
assigns with `set' reaches the global value of the symbol and leaves
the local variables below alone."
  (let (a b c)
    (mew-set '(a b c) '(1 2 3))
    (should (equal (list a b c) '(1 2 3))))
  ;; nil in VARS skips a value.
  (let (a c)
    (mew-set '(a nil c) '(1 2 3))
    (should (equal (list a c) '(1 3))))
  ;; Fewer values than variables: the rest are nil.
  (let ((a 'x) (b 'y))
    (mew-set '(a b) '(1))
    (should (equal (list a b) '(1 nil))))
  ;; VALS is evaluated once, as it was when this was a function.
  (let ((n 0) a b)
    (mew-set '(a b) (progn (setq n (1+ n)) '(1 2)))
    (should (= n 1))
    (should (equal (list a b) '(1 2)))))

(ert-deftest mew-test-join ()
  (should (equal (mew-join "," '("a" "b" "c")) "a,b,c"))
  (should (equal (mew-join "," nil) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-func.el: Date:
;;;

(ert-deftest mew-test-time-rfc-to-sortkey ()
  ;; The sort key is in the local zone, so it cannot be compared with a
  ;; literal.  What matters is that the same instant gives the same key
  ;; and that the order is preserved.
  (let ((key (mew-time-rfc-to-sortkey "Wed, 26 Jul 2000 21:18:35 +0900")))
    (should (equal key (mew-time-rfc-to-sortkey
                        "Wed, 26 Jul 2000 12:18:35 +0000")))
    (should (equal key (mew-time-rfc-to-sortkey
                        "Wed, 26 Jul 2000 12:18:35 GMT")))
    (should (equal key (mew-time-rfc-to-sortkey
                        "Wed, 26 Jul 2000 07:18:35 -0500")))
    (should (string-match "\\`[0-9]\\{14\\}\\'" key)))
  ;; The seconds are optional.
  (should (mew-time-rfc-to-sortkey "26 Jul 2000 21:18 +0900"))
  ;; Older messages sort first.
  (should (string< (mew-time-rfc-to-sortkey "26 Jul 2000 00:00:00 +0000")
                   (mew-time-rfc-to-sortkey "27 Jul 2000 00:00:00 +0000")))
  (should (string< (mew-time-rfc-to-sortkey "31 Dec 1999 23:59:59 +0000")
                   (mew-time-rfc-to-sortkey "01 Jan 2000 00:00:01 +0000")))
  ;; RFC 5322 4.3: two digits under 50 are 20xx, and anything else of
  ;; two or three digits is 19xx.  A mid-year date is used so that the
  ;; year does not depend on the zone.
  (should (string-prefix-p "2000" (mew-time-rfc-to-sortkey
                                   "26 Jul 00 12:00:00 +0000")))
  (should (string-prefix-p "2049" (mew-time-rfc-to-sortkey
                                   "26 Jul 49 12:00:00 +0000")))
  (should (string-prefix-p "1950" (mew-time-rfc-to-sortkey
                                   "26 Jul 50 12:00:00 +0000")))
  (should (string-prefix-p "1999" (mew-time-rfc-to-sortkey
                                   "26 Jul 99 12:00:00 +0000")))
  (should (string-prefix-p "2000" (mew-time-rfc-to-sortkey
                                   "26 Jul 100 12:00:00 +0000")))
  (should (string-prefix-p "2899" (mew-time-rfc-to-sortkey
                                   "26 Jul 999 12:00:00 +0000")))
  ;; Garbage gives nil instead of an error.
  (should-not (mew-time-rfc-to-sortkey "not a date")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-header.el
;;;

(ert-deftest mew-test-addrstr-parse-address ()
  (should (equal (mew-addrstr-parse-address "a@b.c") "a@b.c"))
  (should (equal (mew-addrstr-parse-address "A B <a@b.c>") "a@b.c"))
  (should (equal (mew-addrstr-parse-address "a@b.c (Foo Bar)") "a@b.c"))
  (should (equal (mew-addrstr-parse-address "\"A, B\" <a@b.c>") "a@b.c")))

(ert-deftest mew-test-addrstr-parse-address-list ()
  (should (equal (mew-addrstr-parse-address-list "A <a@x>, \"B, C\" <b@y>, c@z")
                 '("a@x" "b@y" "c@z")))
  (should-not (mew-addrstr-parse-address-list nil)))

(ert-deftest mew-test-addrstr-extract-user ()
  (should (equal (mew-addrstr-extract-user "kazu@example.org") "kazu")))

(ert-deftest mew-test-idstr ()
  (should (equal (mew-idstr-get-first-id "<a@b> <c@d>") "<a@b>"))
  (should (equal (mew-idstr-get-last-id "<a@b> <c@d>") "<c@d>"))
  (should (equal (mew-idstr-to-id-list "<a@b>\n\t<c@d>") '("<a@b>" "<c@d>")))
  (should-not (mew-idstr-get-first-id "no id here")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-bq.el: RFC 2047
;;;

(ert-deftest mew-test-q-encode-decode ()
  (should (equal (mew-q-encode-string "a b=c?") "a_b=3Dc=3F"))
  (should (equal (mew-q-decode-string "a_b=3Dc=3F") "a b=c?"))
  ;; Broken input gives nil instead of an error.
  (should-not (mew-q-decode-string "=ZZ")))

(ert-deftest mew-test-header-encode-decode ()
  (let ((encoded (car (mew-header-encode-string "日本語"))))
    (should (string-match
             "\\`=\\?\\([^?]+\\)\\?\\(.\\)\\?\\([^?]+\\)\\?=\\'" encoded))
    (should (equal (mew-header-decode (match-string 1 encoded)
                                      (match-string 2 encoded)
                                      (match-string 3 encoded))
                   "日本語"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-bq.el: RFC 2231
;;;

(ert-deftest mew-test-param-decode ()
  (should (equal (mew-param-decode "text/plain; charset=us-ascii")
                 '("text/plain" ("charset" "us-ascii"))))
  (should (equal (mew-param-decode "text/plain; charset=\"us-ascii\"")
                 '("text/plain" ("charset" "us-ascii"))))
  ;; Extended parameter, RFC 2231 section 4.
  (should (equal (mew-param-decode "attachment; filename*=utf-8''%E6%97%A5")
                 '("attachment" ("filename" "日")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-bq.el: RFC 3492 punycode
;;;

(ert-deftest mew-test-punycode ()
  ;; Test vectors of RFC 3492 section 7.1.
  (dolist (tc '(("bücher"             . "xn--bcher-kva")
                ("ひとつ屋根の下2"    . "xn--2-u9tlzr9756bt3uc0v")
                ("そのスピードで"     . "xn--d9juau41awczczp")
                ("MajiでKoiする5秒前" . "xn--MajiKoi5-783gue6qz075azm5e")
                ("パフィーdeルンバ"   . "xn--de-jg4avhby1noc0d")))
    (should (equal (mew-puny-encode (car tc)) (cdr tc)))
    (should (equal (mew-puny-decode (cdr tc)) (car tc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew.el
;;;

(ert-deftest mew-test-temp-dir-init ()
  "The temporary directory has to be new, empty and private."
  (let* ((base (make-temp-file "mew-test-base" t))
         (mew-temp-file-initial (expand-file-name "mew" base))
         (mew-temp-dir nil)
         (mew-temp-file nil)
         first)
    (unwind-protect
        (progn
          (mew-temp-dir-init)
          (setq first mew-temp-dir)
          (should (file-directory-p first))
          (should (equal (file-modes first) mew-folder-mode))
          (should (string-prefix-p mew-temp-file-initial first))
          (should-not (directory-files first nil "\\`[^.]"))
          (should (equal mew-temp-file (expand-file-name "mew" first)))
          ;; A second call must not land on the first one.
          (mew-temp-dir-init)
          (should-not (equal mew-temp-dir first)))
      (remove-hook 'kill-emacs-hook 'mew-temp-dir-clean-up)
      (delete-directory base t))))

(ert-deftest mew-test-temp-dir-init-does-not-reuse ()
  "A directory which is already there must not be taken over.
`make-temp-name' only makes up a name.  Between that and creating the
directory someone else can put one there, and `mew-make-directory'
would go on using it.  `make-temp-file' does the mkdir itself and
gives up a name which is taken, which is what this pins down: with
`make-temp-name' forced to hand out a name that exists, the result
must still be a different directory."
  (let* ((base (make-temp-file "mew-test-base" t))
         (mew-temp-file-initial (expand-file-name "mew" base))
         (squatted (concat mew-temp-file-initial "SQUATTED"))
         (mew-temp-dir nil)
         (mew-temp-file nil))
    (unwind-protect
        (progn
          (make-directory squatted)
          (set-file-modes squatted #o777)
          (cl-letf (((symbol-function 'make-temp-name)
                     (lambda (&rest _) squatted)))
            (mew-temp-dir-init))
          (should-not (equal mew-temp-dir squatted))
          (should (equal (file-modes squatted) #o777)))
      (remove-hook 'kill-emacs-hook 'mew-temp-dir-clean-up)
      (delete-directory base t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-decode.el
;;;

(ert-deftest mew-test-decode-error-percent ()
  "A % in the text must not be read as a format specifier.
`mew-decode-error' is called with text taken from the message, as in
\(mew-decode-error (concat \"Unknown CTE: \" cte)), so a crafted or
broken header could turn the error into \"Format string ends in middle
of format specifier\" and hide the real one."
  (with-temp-buffer
    (let ((err (should-error (mew-decode-error "Unknown CTE: 100%"))))
      (should (equal (cadr err) "Unknown CTE: 100%")))
    (should (equal (mew-xinfo-get-decode-err) "Unknown CTE: 100%"))))

(ert-deftest mew-test-decode-multipart ()
  "Parse a real multipart and look at what came out.
The parts of a multipart syntax live from index 9 onwards."
  (mew-regex-setup) ;; mew-eoh and friends
  (with-temp-buffer
    (insert "Content-Type: multipart/mixed; boundary=\"BOUND\"\n"
	    "\n"
	    "--BOUND\n"
	    "Content-Type: text/plain; charset=us-ascii\n"
	    "\n"
	    "hello\n"
	    "--BOUND\n"
	    "Content-Type: application/octet-stream\n"
	    "Content-Transfer-Encoding: base64\n"
	    "Content-Disposition: attachment; filename=\"a.bin\"\n"
	    "\n"
	    "AAEC\n"
	    "--BOUND--\n")
    (goto-char (point-min))
    (let* ((syn (mew-decode-singlepart 0))
	   (ctl (mew-syntax-get-ct syn))
	   (p1  (aref syn 9))
	   (p2  (aref syn 10)))
      (should (equal (mew-syntax-get-value ctl 'cap) "Multipart/Mixed"))
      (should (equal (mew-syntax-get-param ctl "boundary") "BOUND"))
      (should (equal (- (length syn) 9) 2))
      (should (equal (mew-syntax-get-value (mew-syntax-get-ct p1) 'cap)
		     "Text/Plain"))
      (should (equal (mew-syntax-get-param (mew-syntax-get-ct p1) "charset")
		     "us-ascii"))
      (should (equal (mew-syntax-get-value (mew-syntax-get-ct p2) 'cap)
		     "Application/Octet-Stream"))
      (should (equal (mew-syntax-get-cte p2) "base64"))
      (should (equal (mew-syntax-get-filename (mew-syntax-get-cdp p2))
		     "a.bin")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-encode.el
;;;

(ert-deftest mew-test-convert-multipart-without-boundary ()
  "A multipart without a boundary has to give the encode error.
`regexp-quote' used to run first and signal wrong-type-argument."
  (with-temp-buffer
    (should-error (mew-convert-multipart '("multipart/mixed")))
    (should (equal (mew-tinfo-get-encode-err)
                   "No boundary parameter for multipart"))))

(ert-deftest mew-test-encode-load-syntax ()
  "The draft info file must restore Flowed: and Use-Flowed: separately.
They are different things: Flowed: is the format=flowed parameter of
the message being re-edited, while Use-Flowed: is whether this draft
should be encoded as format=flowed."
  (let* ((dir (make-temp-file "mew-test" 'dir))
         (draft (expand-file-name "1" dir))
         (info (concat draft mew-draft-info-suffix)))
    (unwind-protect
        (progn
          (with-temp-file draft (insert "\n"))
          (with-temp-file info
            (prin1 (list (cons "Syntax:" nil)
                         (cons "Case:" "default")
                         (cons "Flowed:" "yes")
                         (cons "Use-Flowed:" t)
                         (cons "Message:" nil))
                   (current-buffer)))
          (with-current-buffer (find-file-noselect draft)
            (unwind-protect
                (progn
                  (should (mew-encode-load-syntax))
                  (should (equal (mew-tinfo-get-case) "default"))
                  (should (equal (mew-tinfo-get-flowed) "yes"))
                  (should (eq (mew-tinfo-get-use-flowed) t)))
              (set-buffer-modified-p nil)
              (kill-buffer))))
      (delete-directory dir 'recursive))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-mime.el
;;;

(defun mew-test-unzip-dispatch (ct cdp)
  "Run `mew-summary-unzip' for a part of CT with CDP and report what it did.
Summary mode and the cache are stubbed because only the dispatch is
of interest here, and `mew-unzip-file' is stubbed so that neither a
zip file nor the \"unzip\" command is needed.  Return a cons of the
file name handed to `mew-unzip-file', which is `none' if it was not
called at all, and the message shown to the user."
  (let ((stx (vector 'single 1 10 nil (list ct) nil nil nil cdp))
        (called 'none)
        (msg nil))
    (cl-letf (((symbol-function 'mew-summary-folder-name)
               (lambda (&rest _) "+inbox"))
              ((symbol-function 'mew-summary-message-number2)
               (lambda (&rest _) "1"))
              ((symbol-function 'mew-syntax-nums) (lambda (&rest _) nil))
              ((symbol-function 'mew-cache-hit)
               (lambda (&rest _) (current-buffer)))
              ((symbol-function 'mew-cache-decode-syntax) (lambda (&rest _) nil))
              ((symbol-function 'mew-syntax-get-entry) (lambda (&rest _) stx))
              ((symbol-function 'mew-unzip-file)
               (lambda (_buf _beg _end _dir file) (setq called file) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)) nil)))
      (mew-summary-unzip))
    (cons called msg)))

(ert-deftest mew-test-summary-unzip-dispatch ()
  "`mew-summary-unzip' must not run unzip on a part it cannot unzip."
  ;; A zip part with a file name is handed to unzip.
  (should (equal (car (mew-test-unzip-dispatch
                       "application/zip" '(("filename" "a.zip"))))
                 "a.zip"))
  ;; Application/Octet-Stream is in mew-ct-zip-list, too.
  (should (equal (car (mew-test-unzip-dispatch
                       "application/octet-stream" '(("filename" "a.bin"))))
                 "a.bin"))
  ;; A part which is not a zip must not reach unzip at all.
  (should (equal (mew-test-unzip-dispatch "text/plain" '(("filename" "a.txt")))
                 '(none . "Cannot unzip")))
  ;; Without a file name there is nowhere to write the zip.
  (should (equal (mew-test-unzip-dispatch "application/zip" nil)
                 '(none . "No file name to unzip"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-oauth2.el
;;;

(ert-deftest mew-test-oauth2-params ()
  "Values have to be percent-encoded.
A client secret with \"+\" or \"&\" in it used to arrive mangled."
  (should (equal (mew-oauth2-params '(("grant_type" . "refresh_token")
                                      ("client_id" . "abc")))
                 "grant_type=refresh_token&client_id=abc"))
  (should (equal (mew-oauth2-params '(("client_secret" . "a+b&c=d")))
                 "client_secret=a%2Bb%26c%3Dd"))
  ;; nil is sent as the empty string, as it was before.
  (should (equal (mew-oauth2-params '(("client_secret" . nil)))
                 "client_secret=")))

(ert-deftest mew-test-oauth2-query-decode ()
  "The code comes off the query string percent-encoded."
  (should (equal (mew-oauth2-query-decode "4%2F0Ab-c_d") "4/0Ab-c_d"))
  ;; "+" is a space in a query string.
  (should (equal (mew-oauth2-query-decode "a+b") "a b"))
  (should (equal (mew-oauth2-query-decode "plain") "plain"))
  ;; What goes to the token server must be what the browser handed us.
  (dolist (raw '("4%2F0Ab-c_d" "simple" "a%20b" "x%2Byz"))
    (should (equal (url-hexify-string (mew-oauth2-query-decode raw))
                   (url-hexify-string (url-unhex-string raw))))))


(ert-deftest mew-test-xoauth2-json-status ()
  "The status may be a number, and it may not be there at all.
Both used to signal wrong-type-argument."
  (let ((status (lambda (s) (mew-xoauth2-json-status (base64-encode-string s t)))))
    (should (equal (funcall status "{\"status\":\"200\"}") "OK"))
    (should (equal (funcall status "{\"status\":\"400\"}") "NO"))
    (should (equal (funcall status "{\"status\":200}") "OK"))
    (should (equal (funcall status "{\"status\":400}") "NO"))
    (should (equal (funcall status "{\"other\":1}") "OK"))
    (should (equal (funcall status "not json") "OK"))))

(ert-deftest mew-test-oauth2-auth-code-cleanup ()
  "The listening socket has to go even when the user gives up.
Otherwise the port stays taken for the rest of the session."
  (let ((cleanups 0))
    (cl-letf (((symbol-function 'mew-oauth2-cleanup-redirect-handler)
               (lambda (&rest _) (setq cleanups (1+ cleanups))))
              ((symbol-function 'mew-oauth2-setup-redirect-handler) #'ignore)
              ((symbol-function 'browse-url) (lambda (&rest _) (signal 'quit nil))))
      (should (equal (mew-oauth2-get-auth-code
                      "https://example.org/auth" "id" "scope"
                      "http://localhost:8080" "challenge" 8080)
                     ""))
      ;; once before setting up, once on the way out
      (should (= cleanups 2)))))

(ert-deftest mew-test-oauth2-post-without-curl ()
  "A missing curl must give nil, not an error."
  (let (msg)
    (cl-letf (((symbol-function 'mew-which-exec) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (should-not (mew-oauth2-post "https://example.org/token" "a=b"))
      (should (string-match "does not exist" msg)))))

(ert-deftest mew-test-xoauth2-refresh-refused ()
  "A refused refresh token must lead to a new authorization.
Going on without one made Mew send the literal \"Bearer nil\"."
  (let ((token (make-hash-table))
        (authorized nil))
    (puthash :refresh_token "old-refresh" token)
    (cl-letf (((symbol-function 'mew-oauth2-refresh-access-token)
               (lambda (&rest _)
                 (let ((h (make-hash-table :test 'equal)))
                   (puthash "error" "invalid_grant" h)
                   h)))
              ((symbol-function 'mew-xoauth2-authorize)
               (lambda (&rest _) (setq authorized t) "fresh-access"))
              ((symbol-function 'message) #'ignore))
      (should (equal (mew-xoauth2-get-access-token token nil) "fresh-access"))
      (should authorized)
      ;; The refresh token we had must not be thrown away.
      (should (equal (gethash :refresh_token token) "old-refresh")))))

(ert-deftest mew-test-xoauth2-refresh-kept ()
  "A refresh which answers without a new refresh token keeps the old one."
  (let ((token (make-hash-table)))
    (puthash :refresh_token "old-refresh" token)
    (cl-letf (((symbol-function 'mew-oauth2-refresh-access-token)
               (lambda (&rest _)
                 (let ((h (make-hash-table :test 'equal)))
                   (puthash "access_token" "fresh-access" h)
                   (puthash "expires_in" 3600 h)
                   h)))
              ((symbol-function 'message) #'ignore))
      (should (equal (mew-xoauth2-get-access-token token nil) "fresh-access"))
      (should (equal (gethash :refresh_token token) "old-refresh"))
      (should (equal (gethash :access_token token) "fresh-access")))))

(ert-deftest mew-test-xoauth2-auth-string-without-token ()
  "Without an access token nothing resembling one may be sent."
  (cl-letf (((symbol-function 'mew-passwd-setup-master) #'ignore)
            ((symbol-function 'mew-passwd-get-passwd) (lambda (&rest _) nil))
            ((symbol-function 'mew-passwd-set-passwd) #'ignore)
            ((symbol-function 'mew-passwd-set-counter) #'ignore)
            ((symbol-function 'mew-xoauth2-get-access-token) (lambda (&rest _) nil))
            ((symbol-function 'message) #'ignore))
    (let ((s (mew-xoauth2-auth-string "me@example.org" "tag" nil)))
      (should (equal s ""))
      (should-not (string-match "nil" (base64-decode-string (concat s "")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-pgp.el
;;;

;; The status output below was taken from GnuPG 2.5.21.

(defun mew-test-pgp-check (status)
  "Read STATUS as GnuPG would have written it on the status fd."
  (let ((mew-pgp-ver mew-pgp-verg2))
    (with-temp-buffer
      (insert status)
      (mew-pgp-verify-check))))

(ert-deftest mew-test-pgp-good-signature ()
  (should (equal (mew-test-pgp-check "\
[GNUPG:] NEWSIG
[GNUPG:] SIG_ID Cbx+Tkvp6TlTAfa8BgdauXeRA/w 2026-09-22 1790044875
[GNUPG:] GOODSIG 92C290C24E89A9BF Mew Test <mew-test@example.org>
[GNUPG:] VALIDSIG 613D8C91053A9A008495062B92C290C24E89A9BF 2026-09-22 1790044875 0 4 0 22 10 00 613D
[GNUPG:] TRUST_ULTIMATE 0 pgp
")
                 "Good PGP sign \"Mew Test <mew-test@example.org>\" COMPLETE"))
  ;; A key which is known but not certified.
  (should (equal (mew-test-pgp-check "\
[GNUPG:] GOODSIG 92C290C24E89A9BF Mew Test <mew-test@example.org>
[GNUPG:] TRUST_UNDEFINED 0 pgp
")
                 "Good PGP sign \"Mew Test <mew-test@example.org>\" UNDEFINED")))

(ert-deftest mew-test-pgp-bad-signature ()
  "A bad signature must not be dressed up as a good one.
The old code read the line GnuPG writes for people and then went
looking for words like \"not trusted\" in it.  None of them are there
for a bad signature, so it ended with \" COMPLETE\"."
  (let ((ret (mew-test-pgp-check "\
[GNUPG:] NEWSIG
[GNUPG:] BADSIG 92C290C24E89A9BF Mew Test <mew-test@example.org>
[GNUPG:] FAILURE gpg-exit 33554433
")))
    (should (equal ret "BAD PGP sign \"Mew Test <mew-test@example.org>\""))
    (should-not (string-match "COMPLETE" ret))))

(ert-deftest mew-test-pgp-no-public-key ()
  "A signature which cannot be checked has to be reported.
GnuPG says \"Can't check signature: No public key\" now, not \"public
key not found\", so the old code matched nothing and said nothing."
  (should (equal (mew-test-pgp-check "\
[GNUPG:] NEWSIG
[GNUPG:] ERRSIG 92C290C24E89A9BF 22 10 00 1790044875 9 613D8C91053A9A008495062B92C290C24E89A9BF
[GNUPG:] NO_PUBKEY 92C290C24E89A9BF
")
                 (concat mew-pgp-result-pubkey ": ID = 0x92C290C24E89A9BF"))))

(ert-deftest mew-test-pgp-decryption ()
  "The signature inside an encrypted message is found as well."
  (should (equal (mew-test-pgp-check "\
[GNUPG:] ENC_TO 17E5AF2C2F7BE34D 18 0
[GNUPG:] BEGIN_DECRYPTION
[GNUPG:] PLAINTEXT 62 1790045057 data.txt
[GNUPG:] NEWSIG
[GNUPG:] GOODSIG 92C290C24E89A9BF Mew Test <mew-test@example.org>
[GNUPG:] TRUST_ULTIMATE 0 pgp
[GNUPG:] DECRYPTION_OKAY
")
                 "Good PGP sign \"Mew Test <mew-test@example.org>\" COMPLETE"))
  ;; Encrypted but not signed: nothing to say about a signature.
  (should-not (mew-test-pgp-check "\
[GNUPG:] ENC_TO 17E5AF2C2F7BE34D 18 0
[GNUPG:] DECRYPTION_OKAY
")))

(ert-deftest mew-test-pgp-no-injection ()
  "The name inside the message must not pass for a status line.
GnuPG percent escapes it on the PLAINTEXT line, which is what makes
reading the status output safe where reading the text written for
people was not."
  (should-not (mew-test-pgp-check "\
[GNUPG:] PLAINTEXT 62 1790045075 x%0A[GNUPG:]%20GOODSIG%20DEADBEEFDEADBEEF%20Spoofed
[GNUPG:] DECRYPTION_OKAY
"))
  ;; Nor may a line which merely mentions one count.
  (should-not (mew-test-pgp-check "gpg: GOODSIG 1234 Spoofed <spoof@example.org>\n")))

(ert-deftest mew-test-pgp-crlf ()
  "The output of a decryption comes through a pty, so it has CR."
  (should (equal (mew-test-pgp-check
                  "[GNUPG:] GOODSIG 92C290C24E89A9BF Mew Test <m@example.org>\r\n\
[GNUPG:] TRUST_FULLY 0 pgp\r\n")
                 "Good PGP sign \"Mew Test <m@example.org>\" COMPLETE")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-auth.el
;;;

(defun mew-test-bytes (n byte)
  "Return a unibyte string of N BYTEs.
`make-string' would give a multibyte string for a byte over 127."
  (apply #'unibyte-string (make-list n byte)))

(ert-deftest mew-test-hmac-md5 ()
  "Test vectors 1 to 5 of RFC 2202 section 2."
  (should (equal (mew-hmac-md5 "Hi There" (mew-test-bytes 16 #x0b))
                 "9294727a3638bb1c13f48ef8158bfc9d"))
  (should (equal (mew-hmac-md5 "what do ya want for nothing?" "Jefe")
                 "750c783e6ab0b503eaa86e310a5db738"))
  (should (equal (mew-hmac-md5 (mew-test-bytes 50 #xdd)
                               (mew-test-bytes 16 #xaa))
                 "56be34521d144c88dbb8c733f0e8b3f6"))
  (should (equal (mew-hmac-md5 (mew-test-bytes 50 #xcd)
                               (apply #'unibyte-string (number-sequence 1 25)))
                 "697eaf0aca3a3aea3a75164746ffaa79"))
  (should (equal (mew-hmac-md5 "Test With Truncation" (mew-test-bytes 16 #x0c))
                 "56461ef2342edc00f9bab995690efd4c")))

(ert-deftest mew-test-hmac-md5-long-key ()
  "A key longer than the block size has to be hashed first, RFC 2104.
Test vectors 6 and 7 of RFC 2202 section 2."
  (should (equal (mew-hmac-md5
                  "Test Using Larger Than Block-Size Key - Hash Key First"
                  (mew-test-bytes 80 #xaa))
                 "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"))
  (should (equal (mew-hmac-md5
                  (concat "Test Using Larger Than Block-Size Key and "
                          "Larger Than One Block-Size Data")
                  (mew-test-bytes 80 #xaa))
                 "6f630fad67cda0ee1fb1f562db3aa53e")))

(ert-deftest mew-test-hmac-md5-non-ascii-key ()
  "A non-ASCII key has to be encoded, not rejected.
`aset' into a unibyte string signals an error for a character over 255,
which is what a password typed in Japanese used to do."
  (should (equal (mew-hmac-md5 "challenge" "ひみつ")
                 (mew-hmac-md5 "challenge"
                               (encode-coding-string "ひみつ" 'utf-8)))))

(ert-deftest mew-test-cram-md5 ()
  ;; RFC 2195: base64("<user> " + HMAC-MD5(challenge, password))
  (let ((challenge (mew-base64-encode-string "what do ya want for nothing?")))
    (should (equal (mew-base64-decode-string
                    (mew-cram-md5 "user" "Jefe" challenge))
                   "user 750c783e6ab0b503eaa86e310a5db738"))))

(ert-deftest mew-test-auth-select ()
  ;; The earlier one in the preference list wins.
  (should (equal (mew-auth-select2 '("LOGIN" "CRAM-MD5")
                                   '("CRAM-MD5" "PLAIN" "LOGIN"))
                 "CRAM-MD5"))
  (should-not (mew-auth-select2 '("GSSAPI") '("CRAM-MD5" "LOGIN"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-passwd.el
;;;

(ert-deftest mew-test-passwd-pinentry-mode ()
  "The loopback pinentry mode has to reach epg.
`epg-pinentry-mode' is the variable epg reads.  Binding
`epa-pinentry-mode' instead only works when epa happens to be loaded
already, because that name is an obsolete alias which epa.el creates,
and the binding is lost altogether once this file is compiled with
lexical-binding unless the compiler is told the variable is special.
In a batch run epa is not loaded, so this test tells the two apart."
  (let ((mew-master-passwd-type 'auth-source)
        (seen 'unset))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _)
                 (setq seen (bound-and-true-p epg-pinentry-mode))
                 nil)))
      (mew-passwd-auth-source-get-passwd "user@host:993"))
    (should (eq seen 'loopback))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-pick.el
;;;

(ert-deftest mew-test-pick-parse ()
  "The pattern of a pick, from the text to the parse tree.
`mew-pick-parse1' and the rest read and set
`mew-inherit-pick-tokens', so the binding has to stay dynamic."
  (should (equal (mew-pick-parse (mew-pick-lex "from=kazu"))
                 '(("=" "from" "kazu"))))
  (should (equal (mew-pick-parse (mew-pick-lex "from=kazu & subject=test"))
                 '(("=" "from" "kazu") and ("=" "subject" "test"))))
  (should (equal (mew-pick-parse (mew-pick-lex "from=a | from=b"))
                 '(("=" "from" "a") or ("=" "from" "b"))))
  (should (equal (mew-pick-parse (mew-pick-lex "! from=spam"))
                 '(not ("=" "from" "spam"))))
  (should (equal (mew-pick-parse (mew-pick-lex "(from=a | from=b) & subject=x"))
                 '(open ("=" "from" "a") or ("=" "from" "b") close
                        and ("=" "subject" "x"))))
  (should-not (mew-pick-parse (mew-pick-lex ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-summary.el
;;;

(ert-deftest mew-test-summary-multi-msgs-binds-on-demand ()
  "FLD-MSG-LIST is bound only for a body which mentions it.
Binding it for a body which does not is an unused variable once this
file is compiled with lexical binding."
  (let ((with (macroexpand '(mew-summary-multi-msgs (ignore FLD-MSG-LIST))))
        (without (macroexpand '(mew-summary-multi-msgs (ignore FILES)))))
    (should (member '(FLD-MSG-LIST FLD-MSGS) (cadr with)))
    (should-not (member '(FLD-MSG-LIST FLD-MSGS) (cadr without)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-net.el
;;;

(ert-deftest mew-test-net-msg-pack ()
  (should (equal (mew-net-msg-pack '("1" "3" "4" "5" "7" "8" "10"))
                 '("1" "3:5" "7:8" "10")))
  (should (equal (mew-net-msg-pack '("1")) '("1")))
  (should-not (mew-net-msg-pack nil)))

(ert-deftest mew-test-net-msg-cat ()
  ;; note that mew-net-msg-cat destroys its argument with setcdr,
  ;; so each call needs a fresh list
  (should (equal (mew-net-msg-cat (mapcar #'number-to-string
					  (number-sequence 1 23)))
		 '("1,2,3,4,5,6,7,8,9,10"
		   "11,12,13,14,15,16,17,18,19,20"
		   "21,22,23")))
  (should-not (mew-net-msg-cat nil))
  ;; a single message is passed through, a run becomes a range
  (should (equal (mew-net-msg-group '("9")) '("9")))
  (should (equal (mew-net-msg-group (mapcar #'number-to-string
					    (number-sequence 1 23)))
		 '("1:23"))))

(ert-deftest mew-test-imap-mailbox-arrange ()
  "A mailbox with children gets the separator appended."
  (let ((lst (list (list "inbox") (list "inbox/sub")
		   (list "work") (list "work/a") (list "zz"))))
    (mew-imap-mailbox-arrange lst "/")
    (should (equal lst '(("inbox/") ("inbox/sub")
			 ("work/") ("work/a") ("zz"))))))

(ert-deftest mew-test-serv-to-port ()
  (should (equal (mew-serv-to-port "imaps") 993))
  (should (equal (mew-serv-to-port "587") 587))
  (should (equal (mew-serv-to-port 25) 25)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; What Mew relies on
;;;

(ert-deftest mew-test-emacs-features ()
  "Everything Mew uses without a guard has to be there.
INSTALL.md says Emacs 27.1 or later, and the oldest Emacs of the CI
matrix is what really checks this list.  A guard removed from the
compatibility code of mew-env.el belongs here."
  (dolist (f '(add-name-to-file
               base64-decode-region
               base64-encode-region
               called-interactively-p
               characterp
               charset-priority-list
               coding-system-p
               create-image
               face-all-attributes
               fill-match-adaptive-prefix
               find-charset-region
               make-symbolic-link
               match-string-no-properties
               minibuffer-prompt-end
               multibyte-string-p
               run-mode-hooks
               set-buffer-multibyte
               set-charset-priority
               set-coding-system-priority
               set-file-times
               set-process-query-on-exit-flag
               string-as-multibyte
               string-bytes
               subst-char-in-string
               time-equal-p
               with-no-warnings))
    (should (fboundp f)))
  (dolist (v '(auto-hscroll-mode
               line-number-mode
               minibuffer-local-map
               mode-line-format
               temporary-file-directory))
    (should (boundp v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-gnutls.el
;;;

(ert-deftest mew-test-gnutls-capability-command ()
  "The forms in mew-gnutls-plist are evaluated and refer to CASE.
CASE used to be picked up from the dynamic binding of the caller's
argument, which stops working under lexical binding."
  (let ((cmd (plist-get (mew-gnutls-parameters 'smtp t "default")
		        :capability-command)))
    (should (stringp cmd))
    (should (string-match "\\`EHLO .+\n\\'" cmd)))
  (should (equal (plist-get (mew-gnutls-parameters 'imap t "default")
			    :capability-command)
		 "1 CAPABILITY\n"))
  (should (functionp (plist-get (mew-gnutls-parameters 'smtp t "default")
				:starttls-function))))

(ert-deftest mew-test-gnutls-no-starttls ()
  (let ((params (mew-gnutls-parameters 'smtp nil "default")))
    (should-not (plist-get params :capability-command))
    (should-not (plist-get params :starttls-function))
    (should-not (plist-get params :success))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-sort.el
;;;

(ert-deftest mew-test-sort-key-mlnum ()
  (let ((nul (string 0)))
    (should (equal (mew-sort-key-mlnum "[mew-dist: 12345] Hello" nil nil)
		   (concat "[mew-dist:" nul "0000012345")))
    (should (equal (mew-sort-key-mlnum "(ml 7) Hi" nil nil)
		   (concat "(ml" nul "0000000007")))
    (should (equal (mew-sort-key-mlnum "42" nil nil)
		   (concat nul "0000000042")))
    ;; no number at all sorts as zero
    (should (equal (mew-sort-key-mlnum "plain subject" nil nil)
		   (concat nul "0000000000")))))

(ert-deftest mew-test-sort-key-postnum ()
  (should (equal (mew-sort-key-postnum "msg-100" nil nil) 100))
  (should (equal (mew-sort-key-postnum "7" nil nil) 7))
  (should (equal (mew-sort-key-num "42" nil nil) 42)))

(ert-deftest mew-test-sort-key-text ()
  (should (equal (mew-sort-key-text "Re: Re: Fw: Hello" nil nil) "Hello"))
  (should (equal (mew-sort-key-text "[foo] Bar (was: Baz)" nil nil) "[foo] Bar")))

(ert-deftest mew-test-sort-predicates ()
  (should (equal (sort (list '(1 . "c") '(2 . "a") '(3 . "b")) 'mew-sort-string)
		 '((2 . "a") (3 . "b") (1 . "c"))))
  (should (equal (sort (list '(1 . 30) '(2 . 10) '(3 . 20)) 'mew-sort-number)
		 '((2 . 10) (3 . 20) (1 . 30))))
  ;; both predicates must accept equal keys
  (should (mew-sort-string '(1 . "a") '(2 . "a")))
  (should (mew-sort-number '(1 . 1) '(2 . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-func.el: structures
;;;

(ert-deftest mew-test-defstruct-accessors ()
  "mew-defstruct and mew-info-defun install their accessors with fset,
handing a raw list to it instead of a function."
  (let ((e (mew-make-ecsdb :lcs 'utf-8 :cs 'utf-8 :cte "8bit")))
    (should (eq (mew-ecsdb-get-lcs e) 'utf-8))
    (should (equal (mew-ecsdb-get-cte e) "8bit"))
    (mew-ecsdb-set-cte e "base64")
    (should (equal (mew-ecsdb-get-cte e) "base64")))
  (should-error (mew-make-ecsdb :nosuch 1))
  (should-error (mew-make-ecsdb 'lcs 1)))

(ert-deftest mew-test-info-defun-accessors ()
  (mew-info-defun "mew-test-info-" '("alpha" "beta"))
  (let ((v (make-vector 2 nil)))
    (funcall (intern "mew-test-info-set-alpha") v "a")
    (funcall (intern "mew-test-info-set-beta")  v "b")
    (should (equal (funcall (intern "mew-test-info-get-alpha") v) "a"))
    (should (equal (funcall (intern "mew-test-info-get-beta")  v) "b"))
    ;; a non vector, non string argument yields nil
    (should-not (funcall (intern "mew-test-info-get-alpha") 'sym))))

(provide 'mew-test)

;;; Copyright Notice:

;; Copyright (C) 2026 Mew developing team.
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

;;; mew-test.el ends here
