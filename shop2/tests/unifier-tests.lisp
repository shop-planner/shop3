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
  (is (equalp (make-binding-list '(?x ?a) '(?y ?y))
              (unify '(f ?x ?y ?a) '(f ?y ?x ?x))))
  (is (equalp (make-binding-list '(?z) '((g ?x ?y ?a)))
              (unify '(f (g ?x ?y ?a) (g ?x ?y ?a)) '(f ?z ?z))))
  (is (equalp (make-binding-list '(?x ?y) '(p p))
              (unify '(f p ?x ?y) '(f ?x ?y ?x))))
  (is (binding-list-equiv (make-binding-list '(?x ?y) '(p p))
              (unify '(f ?y ?x ?y) '(f ?x ?y p))))


  )

#|
    def test_basic_app(self):

        self.assertUnifyResult('f(X, h(X), Y, g(Y))', 'f(g(Z), W, Z, X)',
                {'X': App('g', (Var('Z'),)), 'W': App('h', (Var('X'),)), 'Y': Var('Z')})

        self.assertUnifyResult('f(X, X)', 'f(g(a, b, Y), g(a, Z, c))',
                {'X': App('g', (Const('a'), Const('b'), Var('Y'))),
                 'Y': Const('c'),
                 'Z': Const('b')})


class TestApplyUnifier(unittest.TestCase):

    def assertUnifier(self, s1, s2, result):
        """Asserts that the unifier term of s1 and s2 is result.
        All arguments are string representations of terms.
        """
        subst = unify(parse_term(s1), parse_term(s2), {})
        if subst is None:
            self.fail('expected {} and {} to unify'.format(s1, s2))
        unified_s1 = apply_unifier(parse_term(s1), subst)
        unified_s2 = apply_unifier(parse_term(s2), subst)
        self.assertEqual(unified_s1, unified_s2)
        self.assertEqual(unified_s1, parse_term(result))

    def test_unifier(self):
        self.assertUnifier('f(X)', 'f(t)', 'f(t)')
        self.assertUnifier('f(t)', 'f(X)', 'f(t)')
        self.assertUnifier('f(X, h(X), Y, g(Y))', 'f(g(a), W, a, X)',
                           'f(g(a),h(g(a)),a,g(a))')
        self.assertUnifier('f(X, h(X), Y, g(Y))', 'f(g(Z), W, Z, X)',
                           'f(g(Z),h(g(Z)),Z,g(Z))')
|#
