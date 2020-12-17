;;;; lw-git.lisp

(in-package #:lw-git)

;; DELME
(defparameter *project* "/Users/elrzn/common-lisp/lw-git"
  "This is a dummy project for testing purposes. If you are seeing
this, it's bad news.")

;; DELME
(defparameter *repository*
  (make-instance 'legit:repository :location *project*))

(defun shorten-commit (commit)
  (subseq commit 0 8))

(defclass ui-base (capi:interface)
  ((path :initarg :path
         :reader path
         :type string
         :initform nil
         :documentation
         "The path provides the context for the git action.")
   (repository :initarg :repository
               :accessor repository
               :type legit:repository
               :initform nil)))

(defmethod initialize-instance :after ((obj ui-base) &key)
  (with-slots (path repository)
      obj
    (unless (or repository path)
      (error "Either :PATH or :REPOSITORY initargs are mandatory."))
    ;; Instantiate REPOSITORY with the provided PATH if it's not set.
    (unless repository
      (when path
        (setf repository (make-instance 'legit:repository :location path))))))

(defclass display-pane-horizontal (capi:display-pane)
  ()
  (:default-initargs
   :title-position :left
   :background :transparent))

(capi:define-interface ui-status (ui-base)
  ;; It is handy to re-define REPOSITORY here although it is already
  ;; part of UI-BASE, so that it is available as part of the
  ;; WITH-SLOTS body created by the DEFINE-INTERFACE macro.
  (repository)
  (:panes
   (head-pane display-pane-horizontal :title "Head")
   (merge-pane display-pane-horizontal :title "Merge")
   (tags-pane display-pane-horizontal :title "Tags")
   (recent-commits capi:list-panel
                   :alternating-background t
                   :items (legit:commits repository)
                   :print-function #'shorten-commit
                   :action-callback #'(lambda (commit list-panel)
                                        (capi:display (make-instance 'ui-git-commit
                                                                     :commit commit
                                                                     :repository repository)))))
  (:layouts
   (main capi:column-layout '(overview recent-commits-layout))
   (overview capi:column-layout '(head-pane merge-pane tags-pane))
   (recent-commits-layout capi:column-layout '(recent-commits)
                          :title "Recent commits"
                          :title-position :frame))
  (:default-initargs
   :title "TODO Name of the project"
   :best-width 480
   :best-height 640))

(defmethod initialize-instance :after ((obj ui-status) &key)
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

(capi:define-interface ui-git-commit (ui-base)
  (path
   (commit :initarg :commit))
  (:panes
   (commit-author-pane display-pane-horizontal :title "Author")
   (commit-date-pane display-pane-horizontal :title "Date")
   (commit-refs-pane display-pane-horizontal :title "Refs"))
  (:layouts
   (main capi:column-layout '(head))
   (head capi:column-layout '(commit-author-pane
                              commit-date-pane
                              ;commit-refs-pane
                              )
         :title (format nil "Commit ~a" commit)
         :title-position :frame)))

(defmethod initialize-instance :after ((obj ui-git-commit) &key)
  (with-slots (path commit commit-author-pane commit-date-pane)
      obj
    (setf (capi:display-pane-text commit-author-pane) "Eric Lorenzana")
    (setf (capi:display-pane-text commit-date-pane) "TODAY")))
