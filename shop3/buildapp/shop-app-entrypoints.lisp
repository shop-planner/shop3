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
  (:use :shop3 :iterate :common-lisp)
  (:import-from #:shop-hddl #:hddl-plan))
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

(defun print-plan (plan &optional (stream t))
  (let ((*print-length* nil)
        (*print-right-margin* 10000)
        ;; best guess at package for output
        (*package* (symbol-package (shop::problem-name shop::*problem*))))
    (when (eq stream t)
     (print-separator stream))
    (iter (for (step cost . nil) on plan by 'cddr)
      (as i from 1)
      (format stream "~3d:~t~a:~t~,2f~%"
              i step cost))
    (finish-output stream)
    (when (eq stream t)
      (print-separator stream))))

(defun print-ess-tree (tree &optional (stream-arg t))
  (let ((*print-length* nil)
        ;; (*print-right-margin* 10000)
        ;; best guess at package for output
        (*package* (symbol-package (shop::problem-name shop::*problem*)))
        (stream (if (eq stream-arg t) *standard-output* stream-arg)))
    ;; print functions
    (pprint (plan-tree:plan-tree->sexp tree) stream)
    (terpri stream)
    (finish-output stream)
    (when (eq stream-arg t)
      (print-separator stream))))

(defun print-classic-tree (tree &optional (stream-arg t))
  (let ((*print-length* nil)
        ;; (*print-right-margin* 10000)
        ;; best guess at package for output
        (*package* (symbol-package (shop::problem-name shop::*problem*)))
        (stream (if (eq stream-arg t) *standard-output* stream-arg)))
    (pprint tree stream)
    (terpri stream)
    (finish-output stream)
    (when (eq stream-arg t)
      (print-separator stream))))

(defun print-hddl-plan (plan tree &optional (stream-arg t))
  (let ((hddl-plan (hddl-plan plan tree)))
    ;; (terpri *error-output*)
    ;; (pprint hddl-plan *error-output*)
    ;; (terpri *error-output*)
    (let ((*print-length* nil)
          ;; (*print-right-margin* 10000)
          ;; best guess at package for output
          (*package* (symbol-package (shop::problem-name shop::*problem*))))
      (shop-hddl:print-hddl-plan hddl-plan (if (eq stream-arg t) *standard-output* stream-arg)))))


(defun common/options ()
  (list
   (clingon:make-option
    :flag
    :description "Print plan tree as well as plan."
    :key :plan-tree
    :long-name "tree")
   (clingon:make-option
    :counter
    :description "Verbose output."
    :key :verbose
    :short-name #\v
    :initial-value 0
    :long-name "verbose")
   (clingon:make-option
    :string
    :description "Print plan to file."
    :key :plan-file
    :required nil
    :long-name "plan-file")
   (clingon:make-option
    :string
    :description "Print plan tree to file."
    :key :tree-file
    :required nil
    :long-name "tree-file")))

(defun ess/options ()
  (append
   (list
    (clingon:make-option
    :flag
    :description "Print HDDL output (plan and tree)."
    :key :hddl
    :long-name "hddl")
    (clingon:make-option
    :string
    :description "Print HDDL output to file."
    :key :hddl-file
    :required nil
    :long-name "hddl-file"))
   (common/options)))

(defun classic/options ()
  (common/options))

(defun tree-compare/options ()
  nil)

(defun load-shop-file (filename)
  (let ((*package* (find-package :shop-user)))
    (unless (load filename :if-does-not-exist t)
      (error "File ~a failed to load." filename))))

;; (eval-when (:load-toplevel :execute)
;;   (trace shop-hddl::plan-tree->decompositions)
;;   (trace shop-hddl::forest-roots))

(defun ess/handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (plan-tree (or (clingon:getopt cmd :plan-tree)
                       (clingon:getopt cmd :tree-file)))
        (hddl (or (clingon:getopt cmd :hddl)
                  (clingon:getopt cmd :hddl-file)))
        (verbosity  (clingon:getopt cmd :verbose))
        (shop::*define-silently* (zerop (clingon:getopt cmd :verbose))))
    (handler-bind ((error
                     (lambda (x)
                       (unless *interactive*
                         (format *error-output* "~&ESS-SHOP ERROR:~a~%" x)
                         #+sbcl(sb-debug:print-backtrace :stream *error-output*)
                         (uiop:quit 1)))))
      (iter (for x in args)
        (load-shop-file x))

      (let ((retvals
              (find-plans-stack shop::*problem* :plan-tree (or plan-tree hddl) :unpack-returns nil :verbose verbosity)))
        (unless retvals
          (error "Unable to find a plan for problem ~a"
                 (shop::problem-name shop::*problem*)))

        ;; print the plan sequence
        (unless hddl
         (if (clingon:getopt cmd :plan-file)
             (let ((plan-stream (open (clingon:getopt cmd :plan-file) :direction :output :if-exists :supersede)))
               (unwind-protect
                    (print-plan (shop:plan (first retvals)) plan-stream)
                 (unless (eq plan-stream t) (close plan-stream))))
             (print-plan (shop:plan (first retvals)) t))

         ;; print the plan-tree (if appropriate)
         (when plan-tree
           (if (clingon:getopt cmd :tree-file)
               (let ((stream (open (clingon:getopt cmd :tree-file) :direction :output :if-exists :supersede)))
                 (unwind-protect
                      (print-ess-tree (tree (first retvals)) stream)
                   (close stream)))
               (print-ess-tree (tree (first retvals)) t))))

        ;; print the HDDL, if appropriate
        (when hddl
          (let ((plan (plan (first retvals)))
                (tree (tree (first retvals))))
           (if (clingon:getopt cmd :hddl-file)
               (let ((stream
                       (open (clingon:getopt cmd :hddl-file) :direction :output :if-exists :supersede)))
                 (unwind-protect
                      (print-hddl-plan plan tree stream)
                   (unless (eq stream t) (close stream))))
               (print-hddl-plan plan tree t))))))))

(defun classic/handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (plan-tree (or (clingon:getopt cmd :plan-tree)
                       (clingon:getopt cmd :tree-file)))
        (shop::*define-silently* (zerop (clingon:getopt cmd :verbose))))
    (handler-bind ((error
                     (lambda (x)
                       (unless *interactive*
                         (format *error-output* "~a" x)
                         (uiop:quit 1)))))
      (iter (for x in args)
        (load-shop-file x))
      (multiple-value-bind (plans time trees)
          (find-plans shop::*problem* :plan-tree plan-tree :verbose (clingon:getopt cmd :verbose))
        (declare (ignore time))     ; at least for now...
        (unless plans
          (error "Unable to find a plan for problem ~a"
                 (shop::problem-name shop::*problem*)))
        (let ((plan-stream (alexandria:if-let ((plan-path (clingon:getopt cmd :plan-file)))
                             (open plan-path :direction :output :if-exists :supersede)
                             t)))
          (unwind-protect
               (print-plan (first plans) plan-stream)
            (unless (eq plan-stream t) (close plan-stream))))
        (when plan-tree
          (let ((stream (alexandria:if-let ((plan-path (clingon:getopt cmd :tree-file)))
                          (open plan-path :direction :output :if-exists :supersede)
                          t)))
            (unwind-protect
                 (print-classic-tree (first trees) stream)
              (unless (eq stream t) (close stream)))))))))

(defun tree-compare/handler (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (handler-bind ((error
                     (lambda (x)
                       (unless *interactive*
                         (format *error-output* "TREE-COMPARE ERROR: ~a" x)
                         (uiop:quit 2)))))
      (flet ((load-file (filename)
               (uiop:with-input-file (str filename :if-does-not-exist :error)
                 (let ((*package* (find-package :shop-user)))
                   (read str)))))
        (unless (= (length args) 2)
          (error "Wrong number of arguments to ~a: ~s"
                 cmd args))
        (let ((tree1 (load-file (first args)))
              (tree2 (load-file (second args))))
          (setf tree1 (shop::canonically-order-plan-tree tree1)
                tree2 (shop::canonically-order-plan-tree tree2))
          (cond ((equalp tree1 tree2)
                 (format t "~&Trees match.~%")
                 (uiop:quit 0))
                (t (format t "~&Trees DO NOT match.~%")
                   (uiop:quit 1))))))))

(defun ess/command ()
  (clingon:make-command
   :name "ESS SHOP3"
   :description "Run Explicit Stack Search (ESS) SHOP3 planner"
   ;; :version "0.1.0"
   :authors '("Robert P. Goldman <rpgoldman@sift.net")
   :license "BSD 2-Clause"
   :options (ess/options)
   :handler #'ess/handler))

(defun classic/command ()
  (clingon:make-command
   :name "Classic SHOP3"
   :description "Run Classic (Process Stack) SHOP3 planner"
   ;; :version "0.1.0"
   :authors '("Robert P. Goldman <rpgoldman@sift.net")
   :license "BSD 2-Clause"
   :options (classic/options)
   :handler #'classic/handler))

(defun tree-compare/command ()
  (clingon:make-command
   :name "Compare SHOP3 trees"
   :description "Compare two files containing SHOP3 plan trees."
   ;; :version "0.1.0"
   :authors '("Robert P. Goldman <rpgoldman@sift.net")
   :license "BSD 2-Clause"
   :options (tree-compare/options)
   :handler #'tree-compare/handler))

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

(defun tree-compare-main (argv)
  (clingon:run (tree-compare/command) (rest argv)))
