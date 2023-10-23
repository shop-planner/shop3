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

(defvar *interactive* t
  "Flag variable that can be set true for interactive debugging in REPL.")

(defun usage (progname)
  (format t "~&~a: file1 [file2]~%" progname)
  (format t "~%Runs the SHOP planner on the problem, and prints the result to standard output.~%")
  (format t "~%If only one file is given, it should contain both domain and problem definitions.~%~
            If two are given, the first should be the domain file and the second the problem file~%~
            (although in fact, the ordering is not critical).~%~%~
            The output plan is wrapped between two lines of equal signs (=), and is printed ~%~
            in three columns, step number, a colon (:), step, a colon (:), and cost rounded to~%2 decimal places.~%"))

(defun print-separator (&optional (stream t))
  (format stream "~&======================================================================~%"))

(defun print-plan (plan)
  (let ((*print-length* nil)
        (*print-right-margin* 10000)
        ;; best guess at package for output
        (*package* (symbol-package (shop::problem-name shop::*problem*))))
    (print-separator)
    (iter (for (step cost . nil) on plan by 'cddr)
      (as i from 1)
      (format t "~3d:~t~a:~t~,2f~%"
              i step cost))
    (print-separator)))

(defun print-ess-tree (tree)
  (let ((*print-length* nil)
        ;; (*print-right-margin* 10000)
        ;; best guess at package for output
        (*package* (symbol-package (shop::problem-name shop::*problem*))))
    (pprint (plan-tree:plan-tree->sexp tree))
    (print-separator)))

(defun print-classic-tree (tree)
  (let ((*print-length* nil)
        ;; (*print-right-margin* 10000)
        ;; best guess at package for output
        (*package* (symbol-package (shop::problem-name shop::*problem*))))
    (pprint tree)
    (print-separator)))

(defun common/options ()
  (list
   (clingon:make-option
    :flag
    :description "Print plan tree as well as plan."
    :key :plan-tree
    :long-name "tree")))

(defun ess/options ()
  (common/options))

(defun classic/options ()
  (common/options))

(defun ess/handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (plan-tree (clingon:getopt cmd :plan-tree)))
    (handler-bind ((error
                     (lambda (x)
                       (unless *interactive*
                         (format *error-output* "~a" x)
                         (uiop:quit 1)))))
      (iter (for x in args)
        (unless (load x :if-does-not-exist t)
          (error "File ~a failed to load." x)))

      (let ((retvals
              (find-plans-stack shop::*problem* :plan-tree plan-tree :unpack-returns nil)))
        (unless retvals
          (error "Unable to find a plan for problem ~a"
                 (shop::problem-name shop::*problem*)))
        (print-plan (shop:plan (first retvals)))
        (print-ess-tree (tree (first retvals)))))))

(defun classic/handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (plan-tree (clingon:getopt cmd :plan-tree)))
    (handler-bind ((error
                     (lambda (x)
                       (unless *interactive*
                         (format *error-output* "~a" x)
                         (uiop:quit 1)))))
      (iter (for x in args)
        (unless (load x :if-does-not-exist t)
          (error "File ~a failed to load." x)))
      (multiple-value-bind (plans time trees)
          (find-plans shop::*problem* :plan-tree plan-tree)
        (declare (ignore time))     ; at least for now...
        (unless plans
          (error "Unable to find a plan for problem ~a"
                 (shop::problem-name shop::*problem*)))
        (print-plan (first plans))
        (print-classic-tree (first trees))))))

(defun ess/command ()
  (clingon:make-command
   :name "ESS SHOP3"
   :description "Run Explicit Stack Search (ESS) SHOP3 planner"
   :version "0.1.0"
   :authors '("Robert P. Goldman <rpgoldman@sift.net")
   :license "BSD 2-Clause"
   :options (ess/options)
   :handler #'ess/handler))

(defun classic/command ()
  (clingon:make-command
   :name "Classic SHOP3"
   :description "Run Classic (Process Stack) SHOP3 planner"
   :version "0.1.0"
   :authors '("Robert P. Goldman <rpgoldman@sift.net")
   :license "BSD 2-Clause"
   :options (classic/options)
   :handler #'classic/handler))

(defun main (argv &key (planner :classic))
  (let ((app
          (ecase planner
            (:classic (classic/command))
            (:ess (ess/command)))))
    (clingon:run app (rest argv)))
  (uiop:quit 0))

(defun ess-main (argv)
  (when (member "--help" (rest argv) :test #'string=)
    (usage (first argv))
    (uiop:quit 0))
  (main argv :planner :ess))

(defun classic-main (argv)
  (when (member "--help" (rest argv) :test #'string=)
    (usage (first argv))
    (uiop:quit 0))
  (main argv :planner :classic))
