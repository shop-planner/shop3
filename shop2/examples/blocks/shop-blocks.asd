(defpackage :shop-blocks-asd
    (:use :common-lisp :asdf)
    )
(in-package :shop-blocks-asd)

(defsystem :shop-blocks
    :class shop2-asd::shop-tester
    :depends-on (:shop2)
    :in-order-to ((test-op (load-op :shop-blocks)))
    :components ((:file "block2")
		 (:file "problem100" :depends-on ("block2"))
		 (:file "problem200" :depends-on ("block2"))
		 (:file "problem300" :depends-on ("block2"))
		 (:file "plans" :depends-on ("problem100" "problem200" "problem300"))))
    
(defconstant +shop-package+ :common-lisp-user)
(defmethod perform :after ((op test-op) (component (eql (find-system :shop-blocks))))
  (multiple-value-bind (success failed-test-names)
      (eval (list (intern "BW-TESTS" +shop-package+)))
    (if success t
      (progn
	(warn "Failed the following blocks world tests: ~S" failed-test-names)
	;; this seems like The Wrong Thing --- there should be a
	;; specific test-failed error... [2006/05/09:rpg]
	(cerror "Continue and return nil from perfoming test-op."
		(make-condition 'operation-error
		  :component component
		  :operation op))
	nil))))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system :shop-blocks))))
  (values nil))