;;; -*- Mode: common-lisp; package: shop2-user; -*-

(in-package :shop2-user)


(defun remove-plan-costs (plan-and-costs)
  "The SHOP2 plans come with the operators interspersed with their
costs.  This function just throws away the costs."
  (loop with planlist = plan-and-costs
        while planlist
        for (operator cost . rest) = planlist
        do (assert (numberp cost))
        collect operator
        do (setf planlist rest)))


(defun plan-quietly (problem &rest args)
  (apply #'find-plans problem :which :first :verbose 0 :gc t args))
