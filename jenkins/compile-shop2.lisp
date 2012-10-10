(defpackage compile-shop2-package
    (:use :common-lisp))

(in-package :compile-shop2-package)

(or (ignore-errors (require :asdf))
    (load "asdf"))

(declaim (optimize (speed 3) (space 3)))

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

(defun leave-lisp (message return)
  (fresh-line *error-output*)
  (when message
    (format *error-output* message)
    (terpri *error-output*))
  (finish-output *error-output*)
  (finish-output *standard-output*)
  (exit-lisp return))

(defmacro quit-on-error (&body body)
  `(call-quitting-on-error (lambda () ,@body)))

(defun call-quitting-on-error (thunk)
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (handler-bind
      ((error (lambda (c)
                (format *error-output* "~&~a~&" c)
                (cond
                  ((ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_ASDF_TEST"))
                   (break))
                  (t
                   (finish-output *standard-output*)
                   (finish-output *trace-output*)
                   (format *error-output* "~&ABORTING:~% ~S~%" c)
                   #+sbcl (sb-debug:backtrace 69)
                   #+clozure (ccl:print-call-history :count 69 :start-frame-number 1)
                   #+clisp (system::print-backtrace)
                   (format *error-output* "~&ABORTING:~% ~S~%" c)
                   (finish-output *error-output*)
                   (leave-lisp "~&Script failed~%" 1))))))
    (funcall thunk)
    (leave-lisp "~&Script succeeded~%" 0)))


(quit-on-error
 (asdf:compile-system "shop2"))


