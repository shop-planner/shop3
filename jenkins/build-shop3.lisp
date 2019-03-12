(in-package :common-lisp-user)

(require :asdf)

(declaim (optimize (speed 3) (space 3) (safety 3)))

(asdf:load-system "asdf")
;; (asdf:initialize-output-translations)
;; (asdf:initialize-source-registry)


(setf asdf::*compile-file-failure-behaviour* :error)
;;(setf asdf::*compile-file-warnings-behaviour* :warn)

(defvar *build-warning* nil)
(defvar *build-error* nil)

(handler-bind ((warning #'(lambda (x)
                            ;; this is necessary because on SBCL
                            ;; there's an EXTERNAL handler for some
                            ;; uninteresting warnings.
                            (signal x)
                            (push x *build-warning*)))
               (error #'(lambda (x)
                          (declare (ignore x))
                          (setf *build-error* t))))
  (asdf:load-system "shop3" :force :all))

(cond
  (*build-error*
   (uiop:die 1 "SHOP3 build failed with an error: ~a.~%" *build-error*))
  (*build-warning*
   (uiop:die 2 "SHOP3 build failed with warnings:~%~{~t~a~%~}" *build-warning*))
  (t
   (format t "SHOP3 build successful.~%")
   (uiop:quit 0)))
