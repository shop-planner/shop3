(in-package :arity-test)

(fiveam:def-suite* test-sort-by :in all-shop3-internal-tests)

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

(def-fixture sort-by-highest-domain ()
  (let ((shop:*define-silently* t))
    (defdomain sort-by-highest
        ((:method (sorting)
           (:sort-by ?v #'>
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
     (defproblem simple-sort-by
         ((alt a 1)
          (alt b 2)
          (alt c 3))
       (sorting))))
  (&body))

(test sort-lowest-first
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by :domain 'sort-by)))))
    (is-true plans)
    (is (equalp '((!choose-alt a))
                (shorter-plan (first plans))))))

(test sort-highest-first
  (let ((plans
          (with-fixture sort-by-highest-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by :domain 'sort-by-highest)))))
    (is-true plans)
    (is (equalp '((!choose-alt c))
                (shorter-plan (first plans))))))

(test sort-lowest-first-all
  (let ((plans
          (with-fixture sort-by-domain ()
            (with-fixture simple-sort-by-problem ()
              (find-plans 'simple-sort-by :domain 'sort-by :which :all)))))
    (is-true plans)
    (is (= 3 (length plans)))
    (is (set-equal '(((!choose-alt a)) ((!choose-alt b)) ((!choose-alt c)))
                   (mapcar #'shorter-plan plans)
                   :test 'equalp))))
