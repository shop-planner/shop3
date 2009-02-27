(defpackage :shop-umt-asd
    (:use :common-lisp :asdf)
    )
(in-package :shop-umt-asd)

(defsystem :shop-umt
    :class shop2-asd::shop-tester
    :depends-on (:shop2)
    :in-order-to ((test-op (load-op :shop-umt)))
    :components ((:file "UMT2")
		 (:file "pfile1" :depends-on ("UMT2"))
		 (:file "pfile2" :depends-on ("UMT2"))
		 ;; interestingly, pfile3 does not seem solvable.
		 ;; Haven't checked to see why [2006/05/10:rpg]
		 (:file "pfile3" :depends-on ("UMT2"))
		 (:file "plans")
		 ))
    
(defconstant +shop-package+ :common-lisp-user)
;;; needs to be changed to check the results we find.
(defmethod perform :after ((op test-op) (component (eql (find-system :shop-umt))))
  (eval `(,(intern "test-umt-shop" +shop-package+))))


(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system :shop-umt))))
  (values nil))

