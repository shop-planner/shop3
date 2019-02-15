;;;; Unification test cases for the SHOP2 unifier taken from Eli
;;;; Bendersky.  See
;;;; https://github.com/eliben/code-for-blog/tree/master/2018/unif

(in-package :common-lisp-user)

(defpackage :shop-unifier-tests
  (:shadowing-import-from #:shop.unifier #:fail)
  (:use common-lisp fiveam shop.unifier))

(in-package :shop-unifier-tests)

(defun binding-list-equiv (bl1 bl2)
  (alexandria:set-equal bl1 bl2 :test 'equalp))

(def-suite* test-shop-unifier)
(test test-basic-var
  (is (eq 'fail (unify '(v) 't)))
  (is (equalp (make-binding-list '(?v) '(t))
             (unify '(?v) '(t))))
  (is (equalp (make-binding-list '(?v) '(t))
             (unify '(t) '(?v))))
  (is (equalp (make-binding-list '(?t) '(?v))
             (unify '(?t) '(?v)))))

(test test-basic-app
  (is (eq 'fail (unify '(f v) '(g v))))
  (is (equalp (make-binding-list nil nil)
              (unify '(f v) '(f v))))
  (is (equalp (make-binding-list '(?v) '(v))
              (unify '(f ?v) '(f v))))
  (is (equalp (make-binding-list '(?v) '(v))
              (unify '(f v) '(f ?v))))
  (is (equalp (make-binding-list '(?v) '(v))
              (unify '(f v x) '(f ?v x))))
  (is (equalp (make-binding-list '(?v) '(v))
              (unify '(f x v) '(f x ?v))))
  (is (eq 'fail (unify '(f v x) '(f ?v y))))
  (is (eq 'fail (unify '(f y v) '(f x ?v))))
  (is (eq 'fail (unify '(f y v) 'y)))
  (is (equalp (make-binding-list '(?y) '((f v)))
              (unify '?y '(f v))))
  (is (equalp (make-binding-list '(?y) '((f y v)))
              (unify '(f y v) '?y)))
  (is (equalp (make-binding-list '(?x) '(?y))
              (unify '(f ?x ?x) '(f ?y ?y))))
  (is (equalp (make-binding-list '(?y) '(?x))
              (unify '(f ?y ?y) '(f ?x ?x))))
  (is (equalp (make-binding-list '(?y) '(?x))
              (unify '(f ?y ?x) '(f ?x ?y))))
  (is (binding-list-equiv (make-binding-list '(?x ?a) '(?y ?y))
              (unify '(f ?x ?y ?a) '(f ?y ?x ?x))))
  (is (equalp (make-binding-list '(?z) '((g ?x ?y ?a)))
              (unify '(f (g ?x ?y ?a) (g ?x ?y ?a)) '(f ?z ?z))))
  (is (binding-list-equiv (make-binding-list '(?x ?y) '(p p))
              (unify '(f p ?x ?y) '(f ?x ?y ?x))))
  (is (binding-list-equiv (make-binding-list '(?x ?y) '(p p))
              (unify '(f ?y ?x ?y) '(f ?x ?y p))))
  (is (binding-list-equiv (make-binding-list '(?x ?w ?y) '((g ?z) (h (g ?z)) ?z))
                          (unify '(f ?x (h ?x) ?y (g ?y)) '(f (g ?z) ?w ?z ?x))))
  (is (binding-list-equiv (make-binding-list '(?x ?y ?z) '((g a b c) c b))
                          (unify '(f ?x ?x) '(f (g a b ?y) (g a ?z c))))))

(test test-apply-substitution
  (flet ((assert-unifier (s1 s2 result)
           (let ((subst (unify s1 s2)))
             (and (is-false (eq subst 'fail))
                  (let ((sub1 (apply-substitution s1 subst))
                        (sub2 (apply-substitution s2 subst)))
                    (is (equalp sub1 sub2))
                    (is (equalp sub1 result)))))))
    (assert-unifier '(f ?x) '(f t) '(f t))
    (assert-unifier '(f t) '(f ?x) '(f t))
    (assert-unifier '(f ?x (h ?x) ?y (g ?y)) '(f (g a) ?w a ?x)
                    '(f (g a) (h (g a)) a (g a)))
    (assert-unifier '(f ?x (h ?x) ?y (g ?y)) '(f (g ?z) ?w ?z ?x)
                    '(f (g ?z) (h (g ?z)) ?z (g ?z)))))

