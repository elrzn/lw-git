;;;; lw-git.asd

(asdf:defsystem #:lw-git
  :description "Describe lw-git here"
  :author "Eric Lorenzana"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:legit)
  :components ((:file "package")
               (:file "lw-git")))
