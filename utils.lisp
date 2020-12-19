;;;; lw-git.lisp

(in-package #:lw-git)

(defun shorten-commit (commit)
  (subseq commit 0 8))

(defun string-first-line (string)
  (car (uiop:split-string string :separator '(#\Newline))))

(defun dup (x) `(,x ,x))
