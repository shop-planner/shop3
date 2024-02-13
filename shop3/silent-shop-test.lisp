(defpackage :silent-shop-test
  (:use :asdf :common-lisp)
  (:export #:shop-fiveam-tester))

(in-package #:silent-shop-test)


(defclass shop-tester-mixin ()
     ()
  (:documentation "Mixin that adds silent functioning of SHOP3."))

(defclass shop-fiveam-tester (shop-tester-mixin fiveam-tester-system) ())

(defmethod perform :around ((op test-op)
                                 (component shop-tester-mixin))
  (progv (list (uiop:intern* '#:*silent* '#:shop))
    (call-next-method)))
