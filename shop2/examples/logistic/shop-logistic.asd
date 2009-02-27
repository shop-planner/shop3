(defpackage :shop-logistic-asd
    (:use :common-lisp :asdf)
    )
(in-package :shop-logistic-asd)

(defsystem :shop-logistic
    :class shop2-asd::shop-tester
    :depends-on (:shop2)
    :components ((:file "logistic")
		 (:file "Log_ran_problems_15" :depends-on ("logistic"))
		 (:file "Log_ran_problems_20" :depends-on ("logistic"))
		 (:file "Log_ran_problems_25" :depends-on ("logistic"))
		 (:file "Log_ran_problems_30" :depends-on ("logistic"))
		 (:file "Log_ran_problems_35" :depends-on ("logistic"))
		 (:file "Log_ran_problems_40" :depends-on ("logistic"))
		 (:file "Log_ran_problems_45" :depends-on ("logistic"))
		 (:file "Log_ran_problems_50" :depends-on ("logistic"))
		 (:file "Log_ran_problems_55" :depends-on ("logistic"))
		 (:file "Log_ran_problems_60" :depends-on ("logistic"))
		 (:file "plans")
		 )
    :in-order-to ((test-op (load-op :shop-logistic))))
    
(defconstant +shop-package+ :common-lisp-user)
;;; needs to be changed to check the results we find.
(defmethod perform :after ((op test-op) (component (eql (find-system :shop-logistic))))
  (eval `(,(intern "TEST-LOGISTICS-PLANS" +shop-package+))))

;;; make sure we don't do this only once...
(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system :shop-logistic))))
  (values nil))

