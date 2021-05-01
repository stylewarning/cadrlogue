(defsystem #:cadrlogue
  :author "Robert Smith <robert@stylewarning.com>"
  :description "Book and media cataloguing system"
  :depends-on (#:drakma #:yason #:flexi-streams)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "cadrlogue")))
