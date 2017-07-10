(defpackage :shen
  (:use :cl))

(defpackage :shen.readtable
  (:use :cl)
  (:export :*shen-readtable*))

(defpackage :shen.prelude
  (:use :cl)
  (:export :install
           :init
           :with-shen))


(in-package :shen.readtable)

(defvar *shen-readtable*
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :preserve)
    readtable))
