(in-package :shop)

(defun plan-states (plan &key domain problem)
  "Return a trajectory of states corresponding to the plan-sequence.  The
trajectory will be a list of states in which each state will be a list of
atoms.

  DOMAIN is a domain designator.  PROBLEM is a SHOP problem designator.
  The PROBLEM  keyword argument is *mandatory*.  It must also be possible to
find a domain, but this can be done either by supplying it directly, or taking
it from the problem. "
  (let ((final-state (plan-final-state plan :domain domain :problem problem)))
    (values (shop.common::state-trajectory (copy-state final-state))
            final-state)))

(defun plan-final-state (plan &key domain problem initial-state)
  (let* ((problem (if problem (find-problem problem t)
                      (or initial-state (error "Must supply either PROBLEM or INITIAL-STATE argument."))))
         (domain (or (find-domain domain nil)
                     (and problem (domain-name problem))
                     (error "Must have domain to generate state sequence for plans.")))
         (state
           (etypecase initial-state
             (null
              (make-initial-state domain (default-state-type domain)
                                  (state-atoms problem)))
             (state
              (copy-state initial-state))
             (list
              (make-initial-state domain (default-state-type domain)
                                      initial-state))))
        (*domain* domain))
    (declare (special *domain*))
    ;; state will be updated by side-effects...
    (iter (for action in plan)
      (as act = (operator domain (first action)))
      (as ret =
          (etypecase act
            (operator (apply-operator domain state action act nil 1 nil))
            (pddl-action (apply-action domain state action act nil 1 nil))))
      (when (eq ret 'fail)
        (error "Plan defective: action ~a cannot be applied." action)))
    state))
