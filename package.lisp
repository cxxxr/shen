(defpackage :shen
  (:use :cl))

(defpackage :shen.install
  (:use :cl)
  (:export))

(defpackage :shen.prelude
  (:use :cl)
  (:export :prelude))

(defpackage :shen.readtable
  (:use :cl)
  (:export :*shen-readtable*))


(in-package :shen.readtable)

(defvar *shen-readtable*
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :preserve)
    readtable))
