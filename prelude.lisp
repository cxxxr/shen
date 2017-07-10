(in-package :shen.prelude)

(defvar shen::|*language*| "Common Lisp")
(defvar shen::|*port*| 2.1)
(defvar shen::|*porters*| "Mark Tarver")
(defvar shen::|*implementation*| (LISP-IMPLEMENTATION-TYPE))
(defvar shen::|*release*| (LISP-IMPLEMENTATION-VERSION))
(defvar shen::|*os*| (OR #+WIN32 "Windows"
                         #+LINUX "Linux"
                         #+DARWIN "macOS"
                         #+UNIX "Unix"
                         "Unknown"))

(defvar *kernel-version* "20.1")
(defvar *url-root* "https://github.com/Shen-Language/shen-sources/releases/download")
(defvar *release-name* (format nil "shen-~A" *kernel-version*))
(defvar *nested-folder-name* (format nil "ShenOSKernel-~A" *kernel-version*))
(defvar *file-name* (format nil "ShenOSKernel-~A.tar.gz" *kernel-version*))

(defvar *shen-files* '("toplevel"
                       "core"
                       "sys"
                       "sequent"
                       "yacc"
                       "reader"
                       "prolog"
                       "track"
                       "load"
                       "writer"
                       "macros"
                       "declarations"
                       "types"
                       "t-star"))

(defvar *native-path* (merge-pathnames "native/" (asdf:system-source-directory :shen)))

(defun install ()
  (fetch-source)
  (build))

(defun fetch-source ()
  (uiop:run-program (format nil "wget '~A/~A/~A'" *url-root* *release-name* *file-name*))
  (uiop:run-program (format nil "tar xf ~A" *file-name*))
  (uiop:run-program (format nil "rm -f ~A" *file-name*))
  (uiop:run-program (format nil "mv ~A kernel" *nested-folder-name*)))

(defun build ()
  (dolist (file *shen-files*)
    (compile-kl file)))

(defun compile-kl (file)
  (let ((kl-file (merge-pathnames (format nil "kernel/klambda/~A.kl" file)
                                  (asdf:system-source-directory :shen)))
        (lisp-file (merge-pathnames (format nil "~A.lisp" file) *native-path*)))
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


(defun init ()
  (load-shen-files)
  (load-file (make-pathname :name "overwrite"
                            :type "lisp"
                            :defaults (asdf:system-source-directory :shen)))
  (load-platform))

(defun load-platform ()
  (let ((*readtable* shen.readtable:*shen-readtable*)
        (*package* (find-package :shen))
        (sb-ext:*muffled-warnings* t))
    (uiop:symbol-call :shen '|load|
                      (merge-pathnames "platform.shen"
                                       (asdf:system-source-directory :shen)))))

(defun load-shen-files ()
  (dolist (name *shen-files*)
    (let ((pathname (make-pathname :name name :type "lisp" :defaults *native-path*)))
      (load-file pathname))))

(defun load-file (pathname)
  (let ((*readtable* shen.readtable:*shen-readtable*)
        (*package* (find-package :shen))
        (sb-ext:*muffled-warnings* t))
    (load pathname)))


(defmacro with-shen (() &body body)
  `(let ((*package* (find-package :shen))
         (*readtable* shen.readtable:*shen-readtable*)
         (sb-ext:*muffled-warnings* t))
     ,@body))

(defun toplevel ()
  (with-shen ()
    (shen::|shen-cl.toplevel|)))
