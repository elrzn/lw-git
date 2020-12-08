;;;; lw-git.lisp

(in-package #:lw-git)

;; DELME
(defparameter *project* "/Users/elrzn/common-lisp/lw-git"
  "This is a dummy project for testing purposes. If you are seeing
this, it's bad news.")

;; DELME
(defparameter *repository*
  (make-instance 'legit:repository :location *project*))

(defclass ui-base (capi:interface)
  ((path :initarg :path
         :reader path
         :type string ; shall this be a directory instead?
         :documentation
         "The path provides the context for the git action.")
   (repository :initarg :repository
               :accessor repository
               :type legit:repository
               :initform nil)))

(defmethod initialize-instance :after ((obj ui-base) &key)
  (with-slots (path repository)
      obj
    ;; Instantiate REPOSITORY with the provided PATH if it's not set.
    (unless repository
      (when path
        (setf repository (make-instance 'legit:repository :location path))))))

(defclass display-pane-horizontal (capi:display-pane)
  ()
  (:default-initargs
   :title-position :left
   :background :transparent))

(capi:define-interface ui-overview (ui-base)
  ()
  (:panes
   (head-pane display-pane-horizontal :title "Head")
   (merge-pane display-pane-horizontal :title "Merge")
   (tags-pane display-pane-horizontal :title "Tags"))
  (:layouts (main capi:column-layout '(head-pane merge-pane tags-pane))))

(defmethod initialize-instance :after ((obj ui-overview) &key)
  (with-slots (repository head-pane merge-pane tags-pane)
      obj
    (let* ((branch (legit:current-branch repository))
           (current-message (legit:current-message repository)))
      (setf (capi:display-pane-text head-pane)
            (format nil "~a ~a"
                    branch
                    current-message))
      (setf (capi:display-pane-text merge-pane)
            (format nil "~a ~a"
                    "TODO"
                    current-message))
      (setf (capi:display-pane-text tags-pane)
            (format nil "~a ~a"
                    "TODO"
                    "TODO")))))