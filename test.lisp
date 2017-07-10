(defpackage :shen-test
  (:use :cl)
  (:export :test))

(in-package :shen-test)

(defun test ()
  (shen.install:prelude)
  (let ((*package* (find-package :shen))
        (sb-ext:*muffled-warnings* t))
    (shen::|load| "testsuite.shen")))
