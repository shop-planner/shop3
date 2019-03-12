(defpackage testing-shop3
    (:use common-lisp))

(in-package :testing-shop3)

(require :asdf)

(defun leave-lisp (message return)
  (fresh-line *error-output*)
  (when message
    (format *error-output* message)
    (terpri *error-output*))
  (finish-output *error-output*)
  (finish-output *standard-output*)
  (uiop:quit return))

(defmacro quit-on-error (&body body)
  `(call-quitting-on-error (lambda () ,@body)))

(defun call-quitting-on-error (thunk)
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (flet ((quit (c desc)
           (format *error-output* "~&Encountered ~a during test.~%~a~%" desc c)
           (cond
            ;; decline to handle the error.
            ((ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_ASDF_TEST"))
             (format t "~&Interactive mode (DEBUG_ASDF_TEST) -- Invoke debugger.~%")
             (invoke-debugger c))
            (t
             (finish-output *standard-output*)
             (finish-output *trace-output*)
             (format *error-output* "~&ABORTING:~% ~S~%" c)
             (uiop:print-condition-backtrace c)
             (format *error-output* "~&ABORTING:~% ~S~%" c)
             (finish-output *error-output*)
             (leave-lisp "~&Script failed~%" 1)))))
    (handler-bind
        ((error (lambda (c)
                  (quit c  "ERROR")))
         (storage-condition
          (lambda (c) (quit c "STORAGE-CONDITION")))
         (serious-condition (lambda (c)
                              (quit c "Other SERIOUS-CONDIITON"))))
      (funcall thunk)
      (format t "~&Script succeeded~%")
      t)))

(quit-on-error
 (format t "~&;;; Testing standard SHOP3.~%")
 (asdf:test-system "shop3"))

(uiop:quit 0)
