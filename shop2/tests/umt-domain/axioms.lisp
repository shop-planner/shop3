(in-package :shop2-user)

(defdomain umt-translog-axioms
  (
   ;; state axioms
   (:- (different ?x ?y) ((not (same ?x ?y))))
   (:- (same ?x ?x) nil)))


