;;;; lw-git.asd

(asdf:defsystem #:lw-git
  :description "A Git Porcelain inside LispWorks"
  :author "Eric Lorenzana"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:legit)
  :components ((:file "package")
               (:file "utils")
               (:file "lw-git")))