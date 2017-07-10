(in-package :shen.install)

(defvar *kernel-version* "20.1")
(defvar *url-root* "https://github.com/Shen-Language/shen-sources/releases/download")
(defvar *release-name* (format nil "shen-~A" *kernel-version*))
(defvar *nested-folder-name* (format nil "ShenOSKernel-~A" *kernel-version*))
(defvar *file-name* (format nil "ShenOSKernel-~A.tar.gz" *kernel-version*))

(defun shen-get ()
  (uiop:run-program (format nil "wget '~A/~A/~A'" *url-root* *release-name* *file-name*))
  (uiop:run-program (format nil "tar xf ~A" *file-name*))
  (uiop:run-program (format nil "rm -f ~A" *file-name*))
  (uiop:run-program (format nil "mv ~A kernel" *nested-folder-name*)))

(defun shen-compile ()
  (compile-kl "toplevel")
  (compile-kl "core")
  (compile-kl "sys")
  (compile-kl "sequent")
  (compile-kl "yacc")
  (compile-kl "reader")
  (compile-kl "prolog")
  (compile-kl "track")
  (compile-kl "load")
  (compile-kl "writer")
  (compile-kl "macros")
  (compile-kl "declarations")
  (compile-kl "types")
  (compile-kl "t-star"))

(defun compile-kl (file)
  (let ((kl-file (merge-pathnames (format nil "kernel/klambda/~A.kl" file)
                                  (asdf:system-source-directory :shen)))
        (lisp-file (merge-pathnames (format nil "SHEN/~A.lisp" file)
                                    (asdf:system-source-directory :shen))))
    (ensure-directories-exist lisp-file)
    (let ((*readtable* shen.readtable:*shen-readtable*)
          (*package* (find-package :shen)))
      (write-file lisp-file (translate-kl (read-kl-file kl-file))))))

(defun read-kl-file (file)
  (with-open-file (in file :direction :input)
    (let ((cleanedcode (clean-kl (read-char in nil nil) in nil nil)))
      (read-from-string (format nil "(~A)" (coerce cleanedcode 'string))))))

(defun clean-kl (char in chars insidequote)
  (if (null char)
      (reverse chars)
      (clean-kl
       (read-char in nil nil)
       in
       (if (and (not insidequote) (member char '(#\: #\; #\,) :test 'char-equal))
           (list* #\| char #\| chars)
           (cons char chars))
       (if (char-equal char #\")
           (not insidequote)
           insidequote))))

(defun translate-kl (klcode)
  (mapcar #'(lambda (x) (shen::|shen.kl-to-lisp| nil x)) klcode))

(defun write-file (file code)
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (terpri out)
    (dolist (x code)
      (format out "~S~%~%" x))))
