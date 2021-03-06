;;;; lw-git.lisp

(in-package #:lw-git)

(defparameter *monospace-font*
  (gp:make-font-description :stock :system-fixed-font)
  "A simple stock monospace font.")

(defparameter *default-horizontal-separator*
  '(:external-min-width 50)
  "The default separator used to separate the title and content of
titled objects.")

(defmethod project-name ((repository legit:repository))
  (car (reverse (uiop/utility:split-string (simple-inferiors:location *repository*)
                                           :separator `(,(uiop/pathname:directory-separator-for-host))))))

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

(defclass display-pane-transparent (capi:display-pane)
  ()
  (:default-initargs :background :transparent))

(defclass display-pane-horizontal (display-pane-transparent)
  ()
  (:default-initargs :title-position :left))

(capi:define-interface ui-status (ui-base)
  ;; It is handy to re-define REPOSITORY here although it is already
  ;; part of UI-BASE, so that it is available as part of the
  ;; WITH-SLOTS body created by the DEFINE-INTERFACE macro.
  ((repository)
   (max-count :initarg :max-count :type fixnum :initform 10))
  (:panes
   (head-pane capi:title-pane
              :title "Head"
              :title-args *default-horizontal-separator*)
   (merge-pane capi:title-pane
               :title "Merge"
               :title-args *default-horizontal-separator*)
   (tags-pane display-pane-horizontal
              :title "Tags"
              :title-args *default-horizontal-separator*)
   ;; TODO Untracked files.
   ;; TODO Unstaged changes.
   ;; This is a bit too slow, specially since the addition of
   ;; :PRINT-FUNCTION, consider optimising this by fetching all
   ;; interface data in bulk.
   (recent-commits capi:multi-column-list-panel
                   :alternating-background t
                   :columns '((:title "Hash" :width (character 8))
                              (:title "Message" :width (character 72)))
                   :font *monospace-font*
                   :items (mapcar #'dup (legit:commits repository :max-count max-count))
                   :item-print-functions (list
                                          #'shorten-commit
                                          #'(lambda (commit)
                                              (legit:commit-message repository commit)))
                   :action-callback #'(lambda (commit-dup list-panel)
                                        (declare (ignore list-panel))
                                        (capi:display (make-instance 'ui-git-commit
                                                                     :commit (car commit-dup)
                                                                     :repository repository))))
   (refresh-button capi:push-button
                   :text "Refresh"
                   :callback (lambda (data interface)
                               (declare (ignore data interface))
                               (capi:display-message "Not yet implemented."))))
  (:layouts
   (main capi:column-layout '(overview recent-commits-layout refresh-button))
   (overview capi:column-layout '(head-pane merge-pane tags-pane))
   (recent-commits-layout capi:column-layout '(recent-commits)
                          :title (format nil "~d recent commits" max-count)
                          :title-position :frame))
  (:default-initargs
   :best-width 630
   :best-height 360))

(defmethod initialize-instance :after ((obj ui-status) &key)
  (with-slots (repository head-pane merge-pane tags-pane)
      obj
    (let* ((branch (legit:current-branch repository))
           (current-message (legit:current-message repository))
           (headline (string-first-line current-message)))
      (setf (capi:title-pane-text head-pane)
            (format nil "~a ~a" branch headline))
      (setf (capi:title-pane-text merge-pane)
            (format nil "~a ~a" "TODO" headline))
      (setf (capi:display-pane-text tags-pane)
            (format nil "~a ~a" "TODO" "TODO"))
      (setf (capi:interface-title obj) (project-name repository)))))

(capi:define-interface ui-git-commit (ui-base)
  ((repository)
   (commit :initarg :commit))
  (:panes
   (commit-author-pane capi:title-pane
                       :title "Author"
                       :text (legit:commit-author repository commit)
                       :title-args *default-horizontal-separator*)
   (commit-date-pane capi:title-pane
                     :title "Date"
                     :text (legit:commit-date repository commit)
                     :title-args *default-horizontal-separator*)
   (commit-refs-pane display-pane-horizontal :title "Refs")
   (commit-message-pane display-pane-transparent
                        :title "Message"
                        :font *monospace-font*
                        :text (legit:commit-message repository commit)))
  (:layouts
   (main capi:column-layout '(head))
   (head capi:column-layout '(commit-author-pane
                              commit-date-pane
                            ; commit-refs-pane
                              commit-message-pane)
         :title commit
         :title-position :frame))
  (:default-initargs
   :best-width 480
   :best-height 640))

(defmethod initialize-instance :after ((obj ui-git-commit) &key)
  (with-slots (commit)
      obj
    ;; COMMIT is not available on :DEFAULT-INITARGS so let's set the
    ;; title here.
    (setf (capi:interface-title obj) (format nil "Commit ~a" (shorten-commit commit)))))
