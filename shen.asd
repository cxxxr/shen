(defsystem "shen"
  :depends-on (:named-readtables)
  :components ((:file "package")
               (:file "primitives")
               (:file "backend")
               (:file "prelude")))
