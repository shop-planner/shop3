(defpackage :shop-depots-asd
    (:use :common-lisp :asdf)
    )
(in-package :shop-depots-asd)

(defsystem :shop-depots
    :class shop2-asd::shop-tester
    :depends-on (:shop2)
    :in-order-to ((test-op (load-op :shop-depots)))
    :components ((:file "depots")
		 (:file "pfile1" :depends-on ("depots"))
		 (:file "pfile2" :depends-on ("depots"))
		 (:file "pfile3" :depends-on ("depots"))
		 (:file "pfile4" :depends-on ("depots"))
		 (:file "pfile5" :depends-on ("depots"))
		 (:file "pfile6" :depends-on ("depots"))
		 (:file "pfile7" :depends-on ("depots"))
		 (:file "pfile8" :depends-on ("depots"))
		 (:file "pfile9" :depends-on ("depots"))
		 (:file "pfile10" :depends-on ("depots"))
		 (:file "pfile11" :depends-on ("depots"))
		 (:file "pfile12" :depends-on ("depots"))
		 (:file "pfile13" :depends-on ("depots"))
		 (:file "pfile14" :depends-on ("depots"))
		 (:file "pfile15" :depends-on ("depots"))
		 (:file "pfile16" :depends-on ("depots"))
		 (:file "pfile17" :depends-on ("depots"))
		 (:file "pfile18" :depends-on ("depots"))
		 (:file "pfile19" :depends-on ("depots"))
		 (:file "pfile20" :depends-on ("depots"))
		 (:file "pfile21" :depends-on ("depots"))
		 (:file "pfile22" :depends-on ("depots"))
		 (:file "plans")))
    

    
(defconstant +shop-package+ :common-lisp-user)
;;; needs to be changed to check the results we find.
(defmethod perform :after ((op test-op) (component (eql (find-system :shop-depots))))
  (eval (list (intern "TEST-SHOP-DEPOTS" +shop-package+))))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system :shop-depots))))
  (values nil))