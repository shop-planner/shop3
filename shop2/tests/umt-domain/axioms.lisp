(in-package :shop-user)

(defdomain umt-translog-axioms
  (
   ;; state axioms
   (:- (different ?x ?y) ((not (same ?x ?y))))
   (:- (same ?x ?x) nil)))


