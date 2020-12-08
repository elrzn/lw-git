;;;; lw-git.lisp

(in-package #:lw-git)

(defclass ui-base (capi:interface)
  ((path :initarg :path
         :reader path
         :type string ; shall this be a directory instead?
         :documentation
         "The path provides the context for the git action.")))

(defclass display-pane-horizontal (capi:display-pane)
  ()
  (:default-initargs
   :title-position :left
   :background :transparent))

(capi:define-interface ui-overview (ui-base)
  ()
  (:panes
   (head-pane display-pane-horizontal :title "Head" :text "This is the head.")
   (merge-pane display-pane-horizontal :title "Merge" :text "This is the merge.")
   (tags-pane display-pane-horizontal :title "Tags" :text "These are the tags."))
  (:layouts (main capi:column-layout '(head-pane merge-pane tags-pane))))