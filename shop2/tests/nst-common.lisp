;;; -*- Mode: common-lisp; package: shop2-user; -*-

(in-package :shop2-user)

(nst:def-value-check (:no-plan () (plan-list runtime))
    `(declare (ignorable runtime))
    `(cond
       ((null plan-list) (nst::make-check-result))
       (t (nst:emit-failure
           :format "~@<Expected no plans but found ~d:~{~_ ~s~}~:>"
           :args (list (length plan-list) plan-list)))))

(nst:def-value-check (:found-plan () (plan-list runtime))
    `(declare (ignorable runtime))
    `(cond
       ((null plan-list) (nst:emit-failure :format "No plans generated"))
       (t (nst::make-check-result))))

(defun remove-plan-costs (plan-and-costs)
  "The SHOP2 plans come with the operators interspersed with their
costs.  This function just throws away the costs."
  (loop with planlist = plan-and-costs
        while planlist
        for (operator cost . rest) = planlist
        do (assert (numberp cost))
        collect operator
        do (setf planlist rest)))

(nst:def-value-check (:correct-plan (target-plan)
                                    (plan-list runtime))
  `(declare (ignorable runtime))
  `(let ((plan (remove-plan-costs (first plan-list))))
     (cond
      ((equal ',target-plan plan)  (nst::make-check-result))
      (t  (nst:emit-failure :format "Unexpected plan ~s"
                            :args (list plan))))))

(defun plan-quietly (problem &rest args)
  (apply #'find-plans problem :which :first :verbose 0 args))

(nst:def-check-alias (:plan-problem criterion)
  `(:apply plan-quietly ,criterion))

(nst:def-check-alias (:primary-result-plan target-plan)
    `(:plan-problem (:all :found-plan
                          (:correct-plan ,target-plan))))

(nst:def-check-alias (:no-result-plans)
    `(:plan-problem :no-plan))

