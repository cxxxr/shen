(defpackage :shen-test
  (:use :cl :shen.misc)
  (:export :test))
(in-package :shen-test)

(defun test ()
  (shen.install:prelude)
  (with-shen ()
    (shen::|load| "testsuite.shen")))
