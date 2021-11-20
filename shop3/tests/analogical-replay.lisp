(in-package :common-lisp-user)
(defpackage analogical-replay-tests
  ;; maybe just shadow this instead of preferring the SHOP binding.
  (:shadow #:fail)
  (:import-from #:alexandria #:set-equal)
  (:use #:common-lisp #:shop #:fiveam))

(in-package :analogical-replay-tests)

(def-suite analogical-replay-tests)
(in-suite analogical-replay-tests)

(def-fixture ar-test-domain ()
  (let (*domain*                        ; protect the old value of *domain*
        (*define-silently* t)
        )
    (defdomain (ar-test-domain :redefine-ok t)
      ((:method (drive ?a ?b)
         take-shortcut
         (not (bridge-out ?a ?b))
         (!cross-bridge ?a ?b))

       (:method (drive ?a ?b)
         go-the-long-way
         ()
         (!go-long-way ?a ?b))

       (:op (!cross-bridge ?a ?b)
        :precond (and (not (bridge-out ?a ?b))
                      (at ?a))
        :delete ((at ?a))
        :add ((at ?b)))
       (:op (!go-long-way ?a ?b)
        :precond (at ?a)
        :delete ((at ?a))
        :add ((at ?b))
        :cost 10.0)))
    (defproblem (bridge-out :domain ar-test-domain :redefine-ok t)
      ((at home)
       (bridge-out home work))
      (drive home work))
    (defproblem (bridge-not-out :domain ar-test-domain :redefine-ok t)
      ((at home))
      (drive home work))
    (&body)))

(test check-domain
  (with-fixture ar-test-domain ()
    (let ((plans (find-plans 'bridge-out :which :all :verbose 0)))
      (is (= 1 (length plans)))
      (is (= 10 (second (first plans)))))
    (let ((plans (find-plans-stack 'bridge-out :which :first :verbose 0)))
      (is-true plans)
      (when plans
        (is (= 1 (length plans)))
        (is (= 10 (second (first plans))))))
    (let* ((prs (find-plans-stack 'bridge-out
                                   :unpack-returns nil
                                   :which :first :verbose 0))
           plans)
      (is-true prs)
      (setf plans (mapcar #'shop::plan prs))
      (when plans
        (is (= 1 (length plans)))
        (is (= 10 (second (first plans))))))
    (let ((plans (find-plans-stack 'bridge-out :which :all :verbose 0)))
      (is-true plans)
      (when plans
        (is (= 1 (length plans)))
        (is (= 10 (second (first plans))))))
    (let* ((plans (find-plans 'bridge-not-out :which :all :verbose 0))
           (costs (mapcar #'second plans)))
      (is (= 2 (length plans)))
      (is (set-equal (list 1 10) costs :test '=)))
    (let* ((plans (find-plans-stack 'bridge-not-out :which :all :verbose 0))
           (costs (mapcar #'second plans)))
      (is (= 2 (length plans)))
      (is (set-equal (list 1 10) costs :test '=)))))

(test check-analogical-replay
  (with-fixture ar-test-domain ()
    (let ((prs (find-plans-stack 'bridge-out :unpack-returns nil
                                             :which :first :verbose 0))
         replay-table)
      (let ((plans (mapcar #'shop::plan prs)))
        (is-true plans)
        (when plans
          (is (= 1 (length plans)))
          (is (= 10 (second (first plans))))))
      (setf replay-table (shop::replay-table (first prs)))
      (let ((prs (find-plans-stack 'bridge-not-out
                                   :unpack-returns nil
                                   :analogical-replay replay-table
                                   :which :first :verbose 0)))
        (let ((plans (mapcar #'shop::plan prs)))
          (is-true plans)
          (when plans
            (is (= 1 (length plans)))
            (is (= 10 (second (first plans)))))))
      (let ((prs (find-plans-stack 'bridge-not-out
                                   :unpack-returns nil
                                   :which :first :verbose 0)))
        (let ((plans (mapcar #'shop::plan prs)))
          (is-true plans)
          (when plans
            (is (= 1 (length plans)))
            (is (= 1 (shop::plan-cost (first plans))))))))))
