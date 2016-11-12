(in-package :asdf-user)

(defsystem :pacnews
  :description "archlinux news reader."
  :author "Danny YUE <sheepduke@gmail.com>"
  :licence "public domain"
  :depends-on (:alexandria
               :drakma
               :cl-ppcre
               :uiop)
  :serial t
  :components ((:file "package")
               (:file "specials")
               (:file "news")
               (:file "sync")
               (:file "main")))
