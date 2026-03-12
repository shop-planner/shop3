(in-package :shop)

(defun plan-states (plan &key domain problem final-state)
  "Return a trajectory of states corresponding to the plan-sequence.  The
trajectory will be a list of states in which each state will be a list of
atoms.

  DOMAIN is a domain designator.  PROBLEM is a SHOP problem designator.
  The PROBLEM  keyword argument is *mandatory*.  It must also be possible to
find a domain, but this can be done either by supplying it directly, or taking
it from the problem. "
  (let ((final-state (or final-state (plan-final-state plan :domain domain :problem problem))))
    (values (shop.common::state-trajectory (copy-state final-state))
            final-state)))

(defun plan-final-state (plan &key domain problem initial-state map-to-actions)
  "Return a SHOP.COMMON::TAGGED-STATE that represents the final state after
executing the plan.  This state has, implicitly, the full state trajectory.

If MAP-TO-ACTIONS is non-NIL, construct the hash tables needed for SHOP.COMMON::TAG-FOR-ACTION
to work properly.  This is required for SHOP:LAST-ESTABLISHER to work."
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
    (unless (or (not map-to-actions) (typep state 'shop.common::tagged-state))
      (error "Default state type for this domain is not a TAGGED-STATE. Unless the initial state
is tagged, we cannot compute the information needed to map state tags to actions and vice versa."))
    ;; state will be updated by side-effects...
    (when map-to-actions
      (shop.common:prepare-state-tag-decoder))
    (iter (for action in plan)
      (as act = (operator domain (first action)))
      (as ret =
          (etypecase act
            (operator (apply-operator domain state action act nil 1 nil))
            (pddl-action (apply-action domain state action act nil 1 nil))))
      (when map-to-actions
        (let ((tag (shop.common::tagged-state-tags-info-tag state)))
          (make-tag-map tag action action)))
      (when (eq ret 'fail)
        (error "Plan defective: action ~a cannot be applied." action)))
    (if map-to-actions
     (let ((state-tag-map shop.common::*state-tag-map*)
           (action-to-tag-map shop.common::*action-to-tag-map*))
       (delete-state-tag-decoder)
       (values state state-tag-map action-to-tag-map))
     state)))
