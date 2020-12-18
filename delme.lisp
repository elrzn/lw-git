;;;; delme.lisp

(in-package #:lw-git)

(defparameter *project* "/Users/elrzn/common-lisp/lw-git"
  "This is a dummy project for testing purposes. If you are seeing
this, it's bad news.")

(defparameter *repository*
  (make-instance 'legit:repository :location *project*))

(defun foo ()
  (capi:display (make-instance 'ui-status :repository *repository*)))
