(defpackage :shen-test
  (:use :cl :shen.prelude)
  (:export :test))
(in-package :shen-test)

(defun test ()
  (init)
  (with-shen ()
    (shen::|load| "testsuite.shen")))
