(defpackage :search-tests
  (:shadow #:fail)
  (:import-from :alexandria #:set-equal)
  (:use :shop3 :common-lisp :fiveam))

(in-package :search-tests)

(def-suite* search-tests)

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
    (is (set-equal '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                   (mapcar #'shorter-plan plans)
                   :test 'equalp))))


(test sort-all-stack
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans-stack 'simple-sort-by
                          :domain 'sort-by
                          :which :all
                          :verbose 0)))))
    (is-true plans)
    (is (= 3 (length plans)))
    (is (set-equal '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                   (mapcar #'shorter-plan plans)
                   :test 'equalp))))

(test sort-all-iterative-deepening
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by
                          :domain 'sort-by
                          :which :id-all
                          :verbose 0)))))
    (is-true plans)
    (is (= 3 (length plans)))
    (is (set-equal '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                   (mapcar #'shorter-plan plans)
                   :test 'equalp))))
