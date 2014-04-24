(in-package :common-lisp-user)

(require :asdf)

(declaim (optimize (speed 3) (space 3) (safety 3)))

;;; code adapted from cl-launch http://www.cliki.net/cl-launch
(defun exit-lisp (return)
  #+allegro
  (excl:exit return)
  #+clisp
  (ext:quit return)
  #+(or cmu scl)
  (unix:unix-exit return)
  #+ecl
  (si:quit return)
  #+gcl
  (lisp:quit return)
  #+lispworks
  (lispworks:quit :status return :confirm nil :return nil :ignore-errors-p t)
  #+(or openmcl mcl)
  (ccl::quit return)
  #+mkcl
  (mk-ext:quit :exit-code return)
  #+sbcl #.(let ((exit (find-symbol "EXIT" :sb-ext))
                 (quit (find-symbol "QUIT" :sb-ext)))
             (cond
               (exit `(,exit :code return :abort t))
               (quit `(,quit :unix-status return :recklessly-p t))))
  #+(or abcl xcl)
  (ext:quit :status return)
  (error "Don't know how to quit Lisp; wanting to use exit code ~a" return))

(asdf:load-system "asdf")
;; (asdf:initialize-output-translations)
;; (asdf:initialize-source-registry)


(setf asdf::*compile-file-failure-behaviour* :error)
(setf asdf::*compile-file-warnings-behaviour* :warn)

(defvar *build-warning* nil)
(defvar *build-error* nil)

(handler-bind ((warning #'(lambda (x) (declare (ignore x)) 
                            (setf *build-warning* t)))
               (error #'(lambda (x) (declare (ignore x))
                          (setf *build-error* t))))
  (asdf:load-system "shop2" :force :all))

(cond
  (*build-error*
   (format t "SHOP2 build failed with an error.~%")
   (exit-lisp 1))
  (*build-warning*
   (format t "SHOP2 build failed with a warning.~%")
   (exit-lisp 2))
  (t
   (format t "SHOP2 build successful.~%")
   (exit-lisp 0)))






