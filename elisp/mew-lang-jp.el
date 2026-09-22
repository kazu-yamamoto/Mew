;;; -*- lexical-binding: t; -*-
;;-*-coding:iso-2022-7bit;-*-
;;; mew-lang-jp.el --- Japanese specific stuff for Mew

;; Author:  Mew developing team
;; Created: Sep 08, 1997

;;; Code:

;; from jisx0201.el
(defvar mew-katakana-alist
  '(( 161 . "ｧ" )
    ( 162 . "ｱ" )
    ( 163 . "ｨ" )
    ( 164 . "ｲ" )
    ( 165 . "ｩ" )
    ( 166 . "ｳ" )
    ( 167 . "ｪ" )
    ( 168 . "ｴ" )
    ( 169 . "ｫ" )
    ( 170 . "ｵ" )
    ( 171 . "ｶ" )
    ( 172 . "ｶﾞ" )
    ( 173 . "ｷ" )
    ( 174 . "ｷﾞ" )
    ( 175 . "ｸ" )
    ( 176 . "ｸﾞ" )
    ( 177 . "ｹ" )
    ( 178 . "ｹﾞ" )
    ( 179 . "ｺ" )
    ( 180 . "ｺﾞ" )
    ( 181 . "ｻ" )
    ( 182 . "ｻﾞ" )
    ( 183 . "ｼ" )
    ( 184 . "ｼﾞ" )
    ( 185 . "ｽ" )
    ( 186 . "ｽﾞ" )
    ( 187 . "ｾ" )
    ( 188 . "ｾﾞ" )
    ( 189 . "ｿ" )
    ( 190 . "ｿﾞ" )
    ( 191 . "ﾀ" )
    ( 192 . "ﾀﾞ" )
    ( 193 . "ﾁ" )
    ( 194 . "ﾁﾞ" )
    ( 195 . "ｯ" )
    ( 196 . "ﾂ" )
    ( 197 . "ﾂﾞ" )
    ( 198 . "ﾃ" )
    ( 199 . "ﾃﾞ" )
    ( 200 . "ﾄ" )
    ( 201 . "ﾄﾞ" )
    ( 202 . "ﾅ" )
    ( 203 . "ﾆ" )
    ( 204 . "ﾇ" )
    ( 205 . "ﾈ" )
    ( 206 . "ﾉ" )
    ( 207 . "ﾊ" )
    ( 208 . "ﾊﾞ" )
    ( 209 . "ﾊﾟ" )
    ( 210 . "ﾋ" )
    ( 211 . "ﾋﾞ" )
    ( 212 . "ﾋﾟ" )
    ( 213 . "ﾌ" )
    ( 214 . "ﾌﾞ" )
    ( 215 . "ﾌﾟ" )
    ( 216 . "ﾍ" )
    ( 217 . "ﾍﾞ" )
    ( 218 . "ﾍﾟ" )
    ( 219 . "ﾎ" )
    ( 220 . "ﾎﾞ" )
    ( 221 . "ﾎﾟ" )
    ( 222 . "ﾏ" )
    ( 223 . "ﾐ" )
    ( 224 . "ﾑ" )
    ( 225 . "ﾒ" )
    ( 226 . "ﾓ" )
    ( 227 . "ｬ" )
    ( 228 . "ﾔ" )
    ( 229 . "ｭ" )
    ( 230 . "ﾕ" )
    ( 231 . "ｮ" )
    ( 232 . "ﾖ" )
    ( 233 . "ﾗ" )
    ( 234 . "ﾘ" )
    ( 235 . "ﾙ" )
    ( 236 . "ﾚ" )
    ( 237 . "ﾛ" )
    ( 239 . "ﾜ" ) ; ﾜ -> ワ に変換するように
    ( 238 . "ﾜ" ) ; ワとヮの順番が交換してある。
    ( 240 . "ｨ" )
    ( 241 . "ｪ" )
    ( 242 . "ｦ" )
    ( 243 . "ﾝ" )
    ( 244 . "ｳﾞ" )
    ( 245 . "ｶ" )
    ( 246 . "ｹ" )))

(defvar mew-katakana-kigou-alist
  '(( 162 . "､" )
    ( 163 . "｡" )
    ( 166 . "･" )
    ( 171 . "ﾞ" )
    ( 172 . "ﾟ" )
    ( 188 . "ｰ" )
    ( 214 . "｢" )
    ( 215 . "｣" )))

(defvar mew-dakuon-list
  '( ?カ ?キ ?ク ?ケ ?コ
     ?サ ?シ ?ス ?セ ?ソ
     ?タ ?チ ?ツ ?テ ?ト
     ?ハ ?ヒ ?フ ?ヘ ?ホ))

(defvar mew-handakuon-list (memq ?ハ mew-dakuon-list))

(defun mew-hankaku-code (ch)
  (let* ((str (char-to-string ch))
	 (ent (rassoc str mew-katakana-alist)))
    (if ent (car ent))))

(defun mew-hankaku-kigou-code (ch)
  (let* ((str (char-to-string ch))
	 (ent (rassoc str mew-katakana-kigou-alist)))
    (if ent (car ent))))

(defun mew-zenkaku-katakana-region (beg end)
  (let (ch wk)
    (save-restriction
      (narrow-to-region beg end)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "\\ck" nil t)
	  (setq ch (preceding-char))
	  (setq wk nil)
	  (cond
	   ((= ch ?ﾞ)
	    (save-excursion
	      (backward-char 1)
	      (setq wk (preceding-char)))
	    (cond
	     ((= wk ?ウ)
	      (delete-char -2)
	      (insert "ヴ"))
	     ((setq wk (memq wk mew-dakuon-list))
	      (delete-char -2)
	      (insert (1+ (car wk))))
	     (t
	      (delete-char -1)
	      (insert "゛"))))
	   ((= ch ?ﾟ)
	    (save-excursion
	      (backward-char 1)
	      (setq wk (preceding-char)))
	    (if (setq wk (memq wk mew-handakuon-list))
		(progn
		  (delete-char -2)
		  (insert (+ 2 (car wk))))
	      (delete-char -1)
	      (insert "゜")))
	   ((setq wk (mew-hankaku-code ch))
	    (delete-char -1)
	    (insert (make-char mew-lc-jp ?\245 wk)))
	   ((setq wk (mew-hankaku-kigou-code ch))
	    (delete-char -1)
	    (insert (make-char mew-lc-jp ?\241 wk)))))))))

;;

(defvar mew-thread-indent-strings ["┣" "┗" "┃" "　"])

(defvar mew-highlight-body-regex-cite
  "^\\(\\([ \t]\\{,7\\}\\([>:|〉＞》≫：｜]\\|\\w+\\([._-]+\\w+\\)*>+\\)\\)+\\).*")

;; mew-regex-url is defined in mew-vars.el. This duplication is intentional.
;; This definition includes full-width space characters.
(defvar mew-regex-url
  (concat
   "\\b\\("
   "\\(\\(file\\|news\\|mailto\\):\\)"
   "\\|"
   "\\(\\(s?https?\\|ftp\\|gopher\\|telnet\\|wais\\)://\\)"
   "\\)"
   "[^ 　\t\n>)\"]*"
   "[^] 　\t\n>,:)\"]+"))

(provide 'mew-lang-jp)

;;; Copyright Notice:

;; Copyright (C) 1997-2023 Mew developing team.
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

;;; mew-lang-jp.el ends here
