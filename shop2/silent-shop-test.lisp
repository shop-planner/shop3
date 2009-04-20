
(defmethod asdf:perform :around ((op asdf:test-op)
                                 (component shop2-asd::shop-nst-testable))
  (let ((shop::*silent* t))
    (declare (special shop::*silent*))
    (call-next-method)))
