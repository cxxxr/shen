(in-package :shen.prelude)

(defvar shen::|*language*| "Common Lisp")
(defvar shen::|*port*| 2.1)
(defvar shen::|*porters*| "Mark Tarver")
(defvar shen::|*implementation*| (LISP-IMPLEMENTATION-TYPE))
(defvar shen::|*release*| (LISP-IMPLEMENTATION-VERSION))
(defvar shen::|*os*| (OR #+WIN32 "Windows" #+LINUX "Linux" #+DARWIN "macOS" #+UNIX "Unix" "Unknown"))

(defun load-file (pathname)
  (let ((*readtable* shen.readtable:*shen-readtable*)
        (*package* (find-package :shen))
        (sb-ext:*muffled-warnings* t))
    (load pathname)))

(defun load-shen-files (base-dir)
  (setf base-dir
        (merge-pathnames "SHEN/" base-dir))
  (dolist (name '("toplevel"
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
    (let ((pathname (make-pathname :name name :type "lsp" :defaults base-dir)))
      (load-file pathname))))

(defun load-platform ()
  (let ((*readtable* shen.readtable:*shen-readtable*)
        (*package* (find-package :shen))
        (sb-ext:*muffled-warnings* t))
    (uiop:symbol-call :shen '|load| "platform.shen")))

(defun prelude ()
  (let ((dir (asdf:system-source-directory :shen)))
    (load-shen-files dir)
    (load-file (make-pathname :name "overwrite" :type "lisp" :defaults dir))
    (load-platform)))
