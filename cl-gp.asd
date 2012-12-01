;;;; cl-gp.asd

(asdf:defsystem #:cl-gp
  :serial t
  :depends-on (:closer-mop)
  :components ((:file "package")
               (:file "cl-gp")))

