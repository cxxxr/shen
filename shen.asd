(defsystem "shen"
  :serial t
  :components ((:file "package")
               (:file "primitives")
               (:file "backend")
               (:file "install")))

(defsystem "shen-test"
  :depends-on (:shen)
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :shen-test :test)))
