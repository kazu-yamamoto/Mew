;;; -*- lexical-binding: nil; -*-
;; this must be macro. If implemented as a function, its behavior
;; is changed.
(defmacro mew-called-interactively-p ()
  '(called-interactively-p 'interactive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Suppressing warnings at compile
;;;

(defmacro mew-no-warning-defvar (var-name)
  `(unless (boundp ',var-name) (defvar ,var-name nil)))

(defmacro mew-no-warning-defun (func-name)
  `(unless (fboundp ',func-name) (defun ,func-name(&rest _args) nil)))

(provide 'mew-env0)
