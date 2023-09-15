;;; [mpelican:20230911.1026CDT] Derived from search-tests and uses same domain and problem.
;;; Poss this should be a suite within search-tests?

(defpackage :plan-num-limit-tests
  (:shadow #:fail)
  (:import-from :alexandria #:set-equal #:setp)
  (:use :shop3 :common-lisp :fiveam))

(in-package :plan-num-limit-tests)

(def-suite* plan-num-limit-tests)

(def-fixture sort-by-domain ()
  (let ((shop:*define-silently* t))
   (defdomain sort-by
       ((:method (sorting)
          (:sort-by ?v
                    (alt ?x ?v))
          (!choose-alt ?x))

        (:op (!choose-alt ?x)
         :add ((chosen ?x))))))
  (&body))

(def-fixture simple-sort-by-problem ()
  (let ((shop:*define-silently* t))
    ;; for some reason the following isn't quashing the warnings [2022/11/01:rpg]
    (#+allegro excl:without-redefinition-warnings
     #-allegro progn
     (defproblem (simple-sort-by :redefine-ok t)
         ((alt a 1)
          (alt b 2)
          (alt c 3))
       (sorting))))
  (&body))

;;; :all returns three, in order.
(test sort-all
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by
                          :domain 'sort-by
                          :which :all
                          :verbose 0)))))
    (is-true plans)
    (is (= 3 (length plans)))
    (is (equalp '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                (mapcar #'shorter-plan plans)))))

(test sort-all-ess
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans-stack 'simple-sort-by
                          :domain 'sort-by
                          :which :all
                          :verbose 0)))))
    (is-true plans)
    (is (= 3 (length plans)))
    (is (equalp '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                (mapcar #'shorter-plan plans)))))

;;; :first returns one, in order.
(test sort-first-default
      (let ((plans
              (with-fixture sort-by-domain ()
                (with-fixture simple-sort-by-problem ()
                  (find-plans 'simple-sort-by
                              :domain 'sort-by
                              :which :first
                              :verbose 0)))))
        (is-true plans)
        (is (= 1 (length plans)))
        (is (equalp '((!choose-alt a))
                    (shorter-plan (first plans))))))


(test sort-first-default-ess
      (let ((plans
              (with-fixture sort-by-domain ()
                (with-fixture simple-sort-by-problem ()
                  (find-plans-stack 'simple-sort-by
                              :domain 'sort-by
                              :which :first
                              :verbose 0)))))
        (is-true plans)
        (is (= 1 (length plans)))
        (is (equalp '((!choose-alt a))
                    (shorter-plan (first plans))))))

;;; :random returns one, any of three possible.
;;; NOTE: :random is not yet implemented in ESS
(test sort-random-default
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by
                          :domain 'sort-by
                          :which :random
                          :verbose 0)))))
    (is-true plans)
    (is (= 1 (length plans)))
    (is (member (shorter-plan (first plans))
                '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                :test #'equalp))))

;;; With :plan-num-limit 2, :first returns 2, in order.
(test sort-first-limit-2
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by
                          :domain 'sort-by
                          :which :first
                          :plan-num-limit 2
                          :verbose 0)))))
    (is-true plans)
    (is (= 2 (length plans)))
    (is (equalp '(((!choose-alt a)) ((!choose-alt b)))
                (mapcar #'shorter-plan plans)))))

(test sort-first-limit-2-ess
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans-stack 'simple-sort-by
                          :domain 'sort-by
                          :which :first
                          :plan-num-limit 2
                          :verbose 0)))))
    (is-true plans)
    (is (= 2 (length plans)))
    (is (equalp '(((!choose-alt a)) ((!choose-alt b)))
                (mapcar #'shorter-plan plans)))))

;;; With :plan-num-limit 2, :random returns 2, any two (different) of three possible.
(test sort-random-limit-2
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by
                          :domain 'sort-by
                          :which :random
                          :plan-num-limit 2
                          :verbose 0)))))
    (is-true plans)
    (is (= 2 (length plans)))
    (let ((shorter-plans (mapcar #'shorter-plan plans)))
      (setp shorter-plans :test #'equalp)
      (is (subsetp shorter-plans
                   '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                   :test #'equalp)))))

;;; With :plan-num-limit 5, :first returns 3, in order.
(test sort-first-limit-5
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by
                          :domain 'sort-by
                          :which :first
                          :plan-num-limit 5
                          :verbose 0)))))
    (is-true plans)
    (is (= 3 (length plans)))
    (is (equalp '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                (mapcar #'shorter-plan plans)))))

(test sort-first-limit-5-ess
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans-stack 'simple-sort-by
                          :domain 'sort-by
                          :which :first
                          :plan-num-limit 5
                          :verbose 0)))))
    (is-true plans)
    (is (= 3 (length plans)))
    (is (equalp '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                (mapcar #'shorter-plan plans)))))

;;; With :plan-num-limit 5, :random returns 3, in any order.
(test sort-random-limit-5
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by
                          :domain 'sort-by
                          :which :random
                          :plan-num-limit 5
                          :verbose 0)))))
    (is-true plans)
    (is (= 3 (length plans)))
    (let ((shorter-plans (mapcar #'shorter-plan plans)))
      (setp shorter-plans :test #'equalp)
      (is (set-equal shorter-plans
                     '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                     :test 'equalp)))))
