;;;; cl-gp.asd

(asdf:defsystem #:cl-gp
  :serial t
  :depends-on (hu.dwim.defclass-star)
  :components ((:file "package")
               (:file "cl-gp")))

