(defpackage shop-replan-tests
  ;; watch out -- this shadows fiveam:fail
  (:shadowing-import-from #:shop2 #:fail)
  (:import-from #:shop2-pddl-helpers #:validate-replan)
  (:import-from #:shop2-openstacks #:divergence
           #:os-sequencedstrips-p5_1i)
  (:use #:common-lisp #:iterate #:fiveam #:shop2))

(in-package :shop-replan-tests)

(defun executed-prefix (last-action plan)
  (let ((pos (position last-action plan :test 'equalp)))
    (subseq plan 0
            (if (some 'numberp plan)
                ;; there are costs -- keep them
                (+ 2 pos)
                (1+ pos)))))

(in-package :shop2-openstacks)
;;; work around bug in CCL reader -- at least for the version I have.
(defparameter shop-replan-tests::*sample-failed-action*
  '(!make-product p4))
(defparameter shop-replan-tests::*sample-divergence*
  '((:delete (made p4)) (:add (waiting o4)) (:delete (started o4))))
(in-package :shop-replan-tests)


(defun test-replan (&key (problem 'shop2-openstacks::os-sequencedstrips-p5_1i) (on-failure :error))
  (let ((r (make-initial-plan :problem problem)))
    (destructuring-bind ((plan) (plan-tree) (plan-tree-hash) search-state)
        r
      ;; (shop-trace :tasks :states)
      (let* ((executed (executed-prefix *sample-failed-action* plan))
             (domain (shop2::find-domain (shop2::domain-name problem)))
             (divergence *sample-divergence*)
             (repaired (unwind-protect
                            (shop2:repair-plan domain plan plan-tree executed divergence search-state :plan-tree-hash plan-tree-hash)
                         (shop-untrace))))
        
        
        ;;(list executed plan)
        (values
         (validate-replan repaired :shop-domain domain :package :shop2-openstacks
                                   :pddl-domain (asdf:system-relative-pathname "shop2" "examples/openstacks-adl/domain.pddl")
                                   :pddl-problem (asdf:system-relative-pathname "shop2" "examples/openstacks-adl/p01.pddl")
                                   :on-failure on-failure)

         repaired
         executed
         domain
         divergence)))))

(defun make-initial-plan (&key (problem 'shop2-openstacks::os-sequencedstrips-p5_1i)
                            (problem-file (asdf:system-relative-pathname "shop2" "examples/openstacks-adl/p01-manual.lisp")))
  (load problem-file)
  (let ((r (multiple-value-list (find-plans-stack
                                 problem :verbose 0 :plan-tree t :repairable t))))
    (unless (first r) (error "Failed to generate a plan for openstacks problem."))
    r))


;;; never used.  But useful....
#+ignore (defun temp-file-pathname ()
  (uiop:with-temporary-file (:pathname pddl-plan-name :keep t)
    pddl-plan-name))

(def-suite* test-plan-repair)
(test test-simple-openstacks-repair
  (is-true (test-replan)))