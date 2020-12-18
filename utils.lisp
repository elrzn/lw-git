;;;; lw-git.lisp

(in-package #:lw-git)

(defun string-first-line (string)
  (car (uiop:split-string string :separator '(#\Newline))))
