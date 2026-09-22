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
  (should (equal (mew-remove-single-quote "'a'b'") "ab"))
  (should (equal (mew-remove-single-quote "ab") "ab")))

(ert-deftest mew-test-replace-white-space ()
  (should (equal (mew-replace-white-space "a \t\n  b") "a b"))
  (should (equal (mew-replace-white-space2 "a \t\r\n b") "a_b")))

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
  ;; A two digit year is 19xx or 20xx.
  (should (string-prefix-p "20" (mew-time-rfc-to-sortkey
                                 "26 Jul 00 12:00:00 +0000")))
  (should (string-prefix-p "19" (mew-time-rfc-to-sortkey
                                 "26 Jul 99 12:00:00 +0000")))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-encode.el
;;;

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
;;; mew-net.el
;;;

(ert-deftest mew-test-net-msg-pack ()
  (should (equal (mew-net-msg-pack '("1" "3" "4" "5" "7" "8" "10"))
                 '("1" "3:5" "7:8" "10")))
  (should (equal (mew-net-msg-pack '("1")) '("1")))
  (should-not (mew-net-msg-pack nil)))

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
               coding-system-p
               create-image
               face-all-attributes
               fill-match-adaptive-prefix
               make-symbolic-link
               match-string-no-properties
               minibuffer-prompt-end
               multibyte-string-p
               run-mode-hooks
               set-buffer-multibyte
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
