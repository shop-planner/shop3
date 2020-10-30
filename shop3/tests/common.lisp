
(in-package :shop-user)


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

(defun ess-plan-quietly (problem &rest args)
  (flet ((find-plans (problem  &rest rest &key which verbose gc state-type)
           (declare (ignore gc))
           (remf rest :which)
           (remf rest :verbose)
           (remf rest :gc)
           (assert (eq which :first))
           (let ((state-type-arg
                   (when state-type
                     `(:state-type ,state-type))))
             (remf rest :state-type)
             (when rest
               (error "Can't handle rest arguments for FIND-PLANS-STACK: ~s" rest))
             (apply #'find-plans-stack problem :verbose verbose state-type-arg))))
    (apply #'find-plans problem :which :first :verbose 0 :gc t args)))


