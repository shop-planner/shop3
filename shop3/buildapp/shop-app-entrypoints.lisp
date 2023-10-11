;;;---------------------------------------------------------------------------
;;; Copyright Robert P. Goldman and Smart Information Flow Technologies,
;;; d/b/a SIFT, LLC
;;;
;;; This file made available under the license terms of the SHOP3 system.
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;  A file that gives entry points to a command-line application that runs the
;;;  SHOP3 planner on an input domain and problem.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2023/10/11:rpg] Created.
;;;
;;;---------------------------------------------------------------------------
(defpackage :shop-app
  (:use :shop3 :iterate :common-lisp))
(in-package :shop-app)

(defun usage (progname)
  (format t "~&~a: file1 [file2]~%" progname)
  (format t "~%Runs the SHOP planner on the problem, and prints the result to standard output.~%")
  (format t "~%If only one file is given, it should contain both domain and problem definitions.~%~
            If two are given, the first should be the domain file and the second the problem file~%~
            (although in fact, the ordering is not critical).~%~%~
            The output plan is wrapped between two lines of equal signs (=), and is printed ~%~
            in three columns, step number, a colon (:), step, a colon (:), and cost rounded to~%2 decimal places.~%"))

(defun print-plan (plan)
  (let ((*print-length* nil)
        (*print-right-margin* 10000)
        ;; best guess at package for output
        (*package* (symbol-package (shop::problem-name shop::*problem*))))
    (flet ((print-separator ()
             (format t "~&======================================================================~%")))
     (print-separator)
     (iter (for (step cost . nil) on plan by 'cddr)
       (as i from 1)
       (format t "~3d:~t~a:~t~,2f~%"
               i step cost))
      (print-separator))))

(defun main (argv &key (plan-fun #'shop:find-plans))
  (when (member "--help" (rest argv) :test #'string=)
    (usage (first argv))
    (uiop:quit 0))
  (handler-bind ((error
                   (lambda (x)
                     (format *error-output* "~a" x)
                     (uiop:quit 1))))
   (iter (for x in (rest argv))
     (unless (load x :if-does-not-exist t)
       (error "File ~a failed to load." x)))
    (multiple-value-bind (plans time trees)
        (funcall plan-fun shop::*problem*)
      (declare (ignore time trees))     ; at least for now...
      (unless plans
        (error "Unable to find a plan for problem ~a"
               (shop::problem-name shop::*problem*)))
      (print-plan (first plans))))
  (uiop:quit 0))

(defun ess-main (argv)
  (when (member "--help" (rest argv) :test #'string=)
    (usage (first argv))
    (uiop:quit 0))
  (main argv :plan-fun #'shop:find-plans-stack))
