;;;---------------------------------------------------------------------------
;;; Copyright 2010 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;;
;;;  This file made available together with the SHOP2 system, according to the
;;;  SHOP2 system's license
;;;
;;;---------------------------------------------------------------------------
;;;
;;; Created [2010/05/19:rpg]
;;; File Description:
;;;
;;;    This file is intended to supply a number of unit tests to
;;;    determine whether or not SHOP2's processing of definitions is working.
;;;
;;;--------------------------------------------------------------------------

(in-package :arity-test)

(fiveam:def-suite io-tests :in all-shop3-internal-tests)
(fiveam:in-suite io-tests)

(def-fixture empty-domain ()
  (let ((*domain* (make-instance 'domain)))
    (&body)))

(def-fixture op-def-domain ()
  (let ((domain (make-instance 'domain)))
    (setf (slot-value domain 'shop2::operators)
          (make-hash-table :test 'eq))
    (&body)))


(def-fixture method-def ()
  (let ((meth '(:method (achieve-goals ?goals)
          ()
          ((assert-goals ?goals nil)
           (find-nomove) (add-new-goals) (find-movable) (move-block)))))
    (&body)))

(def-fixture complex-method-def (&optional named)
  (let ((meth `(:method ,@(when named '(find-movable-meth)) (find-movable)
                (:first (clear ?x) (not (dont-move ?x))
                 (goal (on-table ?x)) (not (put-on-table ?x)))
                                        ; Decomposition
                ((!assert ((put-on-table ?x))) (find-movable))

                (:first (clear ?x) (not (dont-move ?x)) (goal (on ?x ?y))
                 (not (stack-on-block ?x ?y)) (dont-move ?y) (clear ?y))
                                        ;Decomposition
                ((!assert ((stack-on-block ?x ?y))) (find-movable))

                nil
                nil)))
    (&body)))

(test method-tests
  (with-fixture empty-domain ()
    (with-fixture method-def ()
      (is (equal (let ((meth-def (shop2::process-method *domain* meth)))
                   ;; there will be a gensym in the third position -- the name that is
                   ;; automatically supplied
                   (setf (nth 2 meth-def) 'placeholder)
                   meth-def)
                 '(:method (achieve-goals ?goals)
                   placeholder
                   ()
                   '(:ordered (:task assert-goals ?goals nil)
                     (:task find-nomove) (:task add-new-goals) (:task find-movable) (:task move-block))))))
    (with-fixture complex-method-def ()
      (is
       (equal (let ((meth-def (shop2::process-method *domain* meth)))
                ;; replace all the gensyms
                (subst-if 'placeholder
                          #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                          meth-def))
              '(:method (find-movable)
                placeholder
                (:first (clear ?x) (not (dont-move ?x)) (goal (on-table ?x)) (not (put-on-table ?x)))
                '(:ordered (:task !assert ((put-on-table ?x))) (:task find-movable))
                placeholder
                (:first (clear ?x) (not (dont-move ?x)) (goal (on ?x ?y)) (not (stack-on-block ?x ?y)) (dont-move ?y) (clear ?y))
                '(:ordered (:task !assert ((stack-on-block ?x ?y))) (:task find-movable))
                placeholder nil '(:ordered (:task shop2::!!inop)))))))
  (with-fixture empty-domain ()
   (with-fixture complex-method-def (t)
     (multiple-value-bind (meth-def meth-id)
         (shop2::process-method *domain* meth)
       (is
        (equal 
         ;; replace all the gensyms
         (subst-if 'placeholder
                   #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                   meth-def)
         '(:method (find-movable)
           placeholder
           (:first (clear ?x) (not (dont-move ?x)) (goal (on-table ?x)) (not (put-on-table ?x)))
           '(:ordered (:task !assert ((put-on-table ?x))) (:task find-movable))
           placeholder
           (:first (clear ?x) (not (dont-move ?x)) (goal (on ?x ?y)) (not (stack-on-block ?x ?y)) (dont-move ?y) (clear ?y))
           '(:ordered (:task !assert ((stack-on-block ?x ?y))) (:task find-movable))
           placeholder nil '(:ordered (:task shop2::!!inop)))))
       (is (eq 'find-movable-meth meth-id))))))

(test test-method-recording
  (with-fixture complex-method-def (t)
    (let* ((temp-domain-desig (gentemp (symbol-name '#:domain) :arity-test))
           (domain (progn 
                     (eval `(defdomain ,temp-domain-desig
                                (,meth)))
                     (find-domain temp-domain-desig)))
           (translated (subst-if 'placeholder
                         #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                         (with-fixture empty-domain ()
                           (shop::process-method *domain* meth)))))
      
      (fiveam:is-true (typep domain 'domain))
      (is (equalp translated
                  (subst-if 'placeholder
                         #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                         (gethash 'find-movable-meth (shop::domain-name-to-method-table domain)))))
      (is (eq 'find-movable-meth
              (gethash
               (gethash 'find-movable-meth (shop::domain-name-to-method-table domain))
               (shop::domain-method-to-name-table domain)))))))

(test test-singleton-method-recording
  (let* ((meth `(:method (find-movable)
                  find-movable-meth
                  (:first (clear ?x) (not (dont-move ?x))
                          (goal (on-table ?x)) (not (put-on-table ?x)))
                                        ; Decomposition
                  ((!assert ((put-on-table ?x))) (find-movable))))
         (temp-domain-desig (gentemp (symbol-name '#:domain) :arity-test))
         (domain (progn 
                   (eval `(defdomain ,temp-domain-desig
                              (,meth)))
                   (find-domain temp-domain-desig)))
         (translated (subst-if 'placeholder
                               #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                               (shop::process-method domain meth))))
    
    (fiveam:is-true (typep domain 'domain))
    (is (equalp translated
                (subst-if 'placeholder
                          #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                          (gethash 'find-movable-meth (shop::domain-name-to-method-table domain)))))
    (is (eq 'find-movable-meth
            (gethash
             (gethash 'find-movable-meth (shop::domain-name-to-method-table domain))
             (shop::domain-method-to-name-table domain))))))

(test test-null-precondition-method-recording
  (let ((shop::*ignore-singleton-variables* t)) ;don't need these warnings.
   (let* ((meth `(:method (find-movable)
                   ()
                   ((!assert ((put-on-table ?x))) (find-movable))))
          (temp-domain-desig (gentemp (symbol-name '#:domain) :arity-test))
          (domain (progn 
                    (eval `(defdomain ,temp-domain-desig
                               (,meth)))
                    (find-domain temp-domain-desig)))
          (translated (subst-if 'placeholder
                                #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                                (shop::process-method domain meth))))
    
     (fiveam:is-true (typep domain 'domain))
     (is (= 1 (length (alexandria:hash-table-keys (shop::domain-name-to-method-table domain)))))
     (let ((lookup-key (first (alexandria:hash-table-keys (shop::domain-name-to-method-table domain)))))
       (is (equalp translated
                   (subst-if 'placeholder
                             #'(lambda (x) (and x (symbolp x) (null (symbol-package x))))
                             (gethash lookup-key (shop::domain-name-to-method-table domain)))))
       (is (eq
            lookup-key
            (gethash
             (gethash lookup-key (shop::domain-name-to-method-table domain))
             (shop::domain-method-to-name-table domain))))))))

(test check-operator-definitions
  ;; FIXME: it's possibly wrong to be depending on the accidental return of the operator object
  (let ((op (with-fixture op-def-domain ()
              (shop2::parse-domain-item domain :operator '(:operator (!!delete-truck ?truck)
                                                           ()
                                                           ((typevp ?truck truck))
                                                           ())))))
    (is (equal (shop2::operator-head op) '(!!delete-truck ?truck)))
    (is (null (shop2::operator-preconditions op)))
    (is (null (shop2::operator-additions op)))
    (is (equal (shop2::operator-deletions op) '((typevp ?truck truck))))
    (is (= (shop2::operator-cost-fun op) 1.0)))
  (let ((op (with-fixture op-def-domain ()
              (shop2::parse-domain-item domain :op '(:op (!!delete-truck ?truck)
                                                           :delete
                                                           ((typevp ?truck truck)))))))
    (is (equal (shop2::operator-head op) '(!!delete-truck ?truck)))
    (is (null (shop2::operator-preconditions op)))
    (is (null (shop2::operator-additions op)))
    (is (equal (shop2::operator-deletions op) '((typevp ?truck truck))))
    (is (= (shop2::operator-cost-fun op) 1.0)))
  ;; here's a big one
  (let ((op (with-fixture op-def-domain ()
              (shop2::parse-domain-item domain :operator
                                        '(:operator (!takeoff ?p ?flight-alt ?earliest-start ?start ?end)
                                          ;; preconditions
                                          (
                                           (at ?p (pos ?north ?east ?alt)) ; a/c starts at alt == 0
                                           (= 0 ?alt)
                                           (fuel ?p ?fuel)
                                           (assign ?fuel-cost (takeoff-fuel-cost ?flight-alt))
                                           (assign ?fuel-remaining (- ?fuel ?fuel-cost))
                                           (call >= ?fuel-remaining 0)
                                           ;; uninformed hack FIXME
                                           (assign ?duration 10)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?t-write-at)
                                           (read-time (at ?p) ?t-read-at)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?t-write-fuel)
                                           (read-time (fuel ?p) ?t-read-fuel)
                                           
                                           (assign ?start (max ?earliest-start ?t-write-at ?t-read-at ?t-write-fuel ?t-read-fuel))
                                           (assign ?end (+ ?start ?duration))
                                           )
                                          ;; deletes
                                          (
                                           ;; update fuel and position
                                           (at ?p (pos ?north ?east ?alt))
                                           (fuel ?p ?fuel)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?t-write-at)
                                           (read-time (at ?p) ?t-read-at)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?t-write-fuel)
                                           (read-time (fuel ?p) ?t-read-fuel)
                                           )
                                          
                                          ;; adds
                                          (
                                           ;; update fuel and position
                                           (at ?p (pos ?north ?east ?flight-alt))
                                           (fuel ?p ?fuel-remaining)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?end)
                                           (read-time (at ?p) ?end)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?end)
                                           (read-time (fuel ?p) ?end)
                                           )
                                          ;; cost
                                          0)))))
    (is (equal (shop2::operator-head op) '(!takeoff ?p ?flight-alt ?earliest-start ?start ?end)))
    (is (equal (shop2::operator-preconditions op)
        '(
                                           (at ?p (pos ?north ?east ?alt)) ; a/c starts at alt == 0
                                           (= 0 ?alt)
                                           (fuel ?p ?fuel)
                                           (assign ?fuel-cost (takeoff-fuel-cost ?flight-alt))
                                           (assign ?fuel-remaining (- ?fuel ?fuel-cost))
                                           (call >= ?fuel-remaining 0)
                                           ;; uninformed hack FIXME
                                           (assign ?duration 10)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?t-write-at)
                                           (read-time (at ?p) ?t-read-at)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?t-write-fuel)
                                           (read-time (fuel ?p) ?t-read-fuel)
                                           
                                           (assign ?start (max ?earliest-start ?t-write-at ?t-read-at ?t-write-fuel ?t-read-fuel))
                                           (assign ?end (+ ?start ?duration))
                                           )))
    (is (equal (shop2::operator-additions op)
               '(
                                           ;; update fuel and position
                                           (at ?p (pos ?north ?east ?flight-alt))
                                           (fuel ?p ?fuel-remaining)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?end)
                                           (read-time (at ?p) ?end)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?end)
                                           (read-time (fuel ?p) ?end)
                                           )))
    (is (equal (shop2::operator-deletions op) '(
                                           ;; update fuel and position
                                           (at ?p (pos ?north ?east ?alt))
                                           (fuel ?p ?fuel)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?t-write-at)
                                           (read-time (at ?p) ?t-read-at)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?t-write-fuel)
                                           (read-time (fuel ?p) ?t-read-fuel)
                                           )))
    (is (= (shop2::operator-cost-fun op) 0)))
  (let ((op (with-fixture op-def-domain ()
              (shop2::parse-domain-item domain :op
                                        '(:op (!takeoff ?p ?flight-alt ?earliest-start ?start ?end)
                                          :precond
                                          (
                                           (at ?p (pos ?north ?east ?alt)) ; a/c starts at alt == 0
                                           (= 0 ?alt)
                                           (fuel ?p ?fuel)
                                           (assign ?fuel-cost (takeoff-fuel-cost ?flight-alt))
                                           (assign ?fuel-remaining (- ?fuel ?fuel-cost))
                                           (call >= ?fuel-remaining 0)
                                           ;; uninformed hack FIXME
                                           (assign ?duration 10)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?t-write-at)
                                           (read-time (at ?p) ?t-read-at)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?t-write-fuel)
                                           (read-time (fuel ?p) ?t-read-fuel)
                                           
                                           (assign ?start (max ?earliest-start ?t-write-at ?t-read-at ?t-write-fuel ?t-read-fuel))
                                           (assign ?end (+ ?start ?duration))
                                           )
                                          :delete
                                          (
                                           ;; update fuel and position
                                           (at ?p (pos ?north ?east ?alt))
                                           (fuel ?p ?fuel)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?t-write-at)
                                           (read-time (at ?p) ?t-read-at)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?t-write-fuel)
                                           (read-time (fuel ?p) ?t-read-fuel)
                                           )
                                          
                                          :add
                                          (
                                           ;; update fuel and position
                                           (at ?p (pos ?north ?east ?flight-alt))
                                           (fuel ?p ?fuel-remaining)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?end)
                                           (read-time (at ?p) ?end)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?end)
                                           (read-time (fuel ?p) ?end)
                                           )
                                          :cost
                                          0)))))
    (is (equal (shop2::operator-head op) '(!takeoff ?p ?flight-alt ?earliest-start ?start ?end)))
    (is (equal (shop2::operator-preconditions op)
        '(
                                           (at ?p (pos ?north ?east ?alt)) ; a/c starts at alt == 0
                                           (= 0 ?alt)
                                           (fuel ?p ?fuel)
                                           (assign ?fuel-cost (takeoff-fuel-cost ?flight-alt))
                                           (assign ?fuel-remaining (- ?fuel ?fuel-cost))
                                           (call >= ?fuel-remaining 0)
                                           ;; uninformed hack FIXME
                                           (assign ?duration 10)
                                           ;; timelines for at update
                                           (write-time (at ?p) ?t-write-at)
                                           (read-time (at ?p) ?t-read-at)
                                           ;; timelines for fuel update
                                           (write-time (fuel ?p) ?t-write-fuel)
                                           (read-time (fuel ?p) ?t-read-fuel)
                                           (assign ?start (max ?earliest-start ?t-write-at ?t-read-at ?t-write-fuel ?t-read-fuel))
                                           (assign ?end (+ ?start ?duration))
                                           )))
    (is (equal (shop2::operator-additions op)
               '(
                 ;; update fuel and position
                 (at ?p (pos ?north ?east ?flight-alt))
                 (fuel ?p ?fuel-remaining)
                 ;; timelines for at update
                 (write-time (at ?p) ?end)
                 (read-time (at ?p) ?end)
                 ;; timelines for fuel update
                 (write-time (fuel ?p) ?end)
                 (read-time (fuel ?p) ?end)
                 )))
    (is (equal (shop2::operator-deletions op)
               '(
                 ;; update fuel and position
                 (at ?p (pos ?north ?east ?alt))
                 (fuel ?p ?fuel)
                 ;; timelines for at update
                 (write-time (at ?p) ?t-write-at)
                 (read-time (at ?p) ?t-read-at)
                 ;; timelines for fuel update
                 (write-time (fuel ?p) ?t-write-fuel)
                 (read-time (fuel ?p) ?t-read-fuel)
                 )))
    (is (= (shop2::operator-cost-fun op) 0))))

    
           



(test check-problem-deletion
  (make-problem 'problem-for-deletion-test
                '((foo x) (bar y))
                '(achieve (bar x)))
  (fiveam:is-true (find-problem 'problem-for-deletion-test))
  (delete-problem 'problem-for-deletion-test)
  (fiveam:is-false (find-problem 'problem-for-deletion-test nil)))

(in-package :shop-user)
(defparameter arity-test::*expected-umt-plan*
           '((!!ASSERT
           ((GOAL (CLEAR)) (GOAL (DELIVERED PACKAGE2 LOCATION5))
            (GOAL (DELIVERED PACKAGE1 LOCATION4))
            (GOAL (DELIVERED PACKAGE0 LOCATION1))))
          (!!CHECK PACKAGE2) (!!ADD-PACKAGE-LOCAL PACKAGE2)
          (!!ADD-PACKAGE-NN PACKAGE2 LOCATION3 LOCATION3)
          (!!ADD-PACKAGE-NN PACKAGE2 LOCATION3 LOCATION2)
          (!!ADD-PACKAGE-NN PACKAGE2 LOCATION2 LOCATION3)
          (!!ADD-PACKAGE-NN PACKAGE2 LOCATION2 LOCATION2) (!!CHECK PACKAGE1)
          (!!ADD-PACKAGE-ROAD PACKAGE1 ROAD_ROUTE1)
          (!!ADD-PACKAGE-NN PACKAGE1 LOCATION3 LOCATION1)
          (!!ADD-PACKAGE-NN PACKAGE1 LOCATION3 LOCATION0)
          (!!ADD-PACKAGE-NN PACKAGE1 LOCATION2 LOCATION1)
          (!!ADD-PACKAGE-NN PACKAGE1 LOCATION2 LOCATION0) (!!CHECK PACKAGE0)
          (!!ADD-PACKAGE-LOCAL PACKAGE0) (!COLLECT-FEES PACKAGE0)
          (!!ADD-NEXT TRUCK3 LOCATION4) (!!EXP-WEIGHT-SET TRUCK3 CITY0 13)
          (!MOVE-VEHICLE-LOCAL-ROAD-ROUTE3 TRUCK3 LOCATION0 LOCATION4 CITY0)
          (!!DELETE-PROTECTION (NEXT TRUCK3 LOCATION4)) (!!DEL-NEXT TRUCK3 LOCATION4)
          (!!ADD-NEXT TRUCK3 LOCATION1) (!CONNECT-CHUTE TRUCK3)
          (!FILL-HOPPER PACKAGE0 TRUCK3 LOCATION4) (!COLLECT-FEES PACKAGE1)
          (!!ADD-NEXT TRUCK0 LOCATION5) (!!EXP-WEIGHT-SET TRUCK0 ROAD_ROUTE1 12)
          (!MOVE-VEHICLE-LOCAL-ROAD-ROUTE3 TRUCK0 LOCATION3 LOCATION5 CITY1)
          (!!DELETE-PROTECTION (NEXT TRUCK0 LOCATION5)) (!!DEL-NEXT TRUCK0 LOCATION5)
          (!!ADD-NEXT TRUCK0 LOCATION4) (!CONNECT-CHUTE TRUCK0)
          (!FILL-HOPPER PACKAGE1 TRUCK0 LOCATION5) (!DELIVER PACKAGE2 LOCATION5)
          (!DISCONNECT-CHUTE TRUCK3) (!DISCONNECT-CHUTE TRUCK0)
          (!MOVE-VEHICLE-LOCAL-ROAD-ROUTE2 TRUCK3 LOCATION4 LOCATION1 CITY0)
          (!!DELETE-PROTECTION (NEXT TRUCK3 LOCATION1))
          (!!EXP-WEIGHT-CLEAR TRUCK3 CITY0) (!CONNECT-CHUTE TRUCK3)
          (!EMPTY-HOPPER PACKAGE0 TRUCK3 LOCATION1)
          (!MOVE-VEHICLE-ROAD-ROUTE-CROSSCITY TRUCK0 LOCATION5 LOCATION4 CITY1 CITY0
                                              ROAD_ROUTE1)
          (!!DELETE-PROTECTION (NEXT TRUCK0 LOCATION4))
          (!!EXP-WEIGHT-CLEAR TRUCK0 ROAD_ROUTE1) (!CONNECT-CHUTE TRUCK0)
          (!EMPTY-HOPPER PACKAGE1 TRUCK0 LOCATION4) (!DISCONNECT-CHUTE TRUCK3)
          (!DELIVER PACKAGE0 LOCATION1) (!DISCONNECT-CHUTE TRUCK0)
          (!DELIVER PACKAGE1 LOCATION4) (!CLEAN-DOMAIN)))
(in-package :arity-test)
  
;;; FIXME: probably should undefine the problem and domain here.
(test test-include-directive
  (shop-user::define-partitioned-umt-domain)
  (fiveam:is
   (equalp
    (shop-user::remove-plan-costs
     (first
      (find-plans
             'shop-user::umt-partitioned.pfile1
             :which :first
             :verbose 0)))
    *expected-umt-plan*)))


(in-package :fiveam)

(defmacro arity-test::warns (condition-spec
                   &body body)
  "Generates a pass if BODY signals a warning of type
CONDITION. BODY is evaluated in a block named NIL, CONDITION is
not evaluated.
  Is like SIGNALS, but does NOT abort the execution of BODY upon the signal
being raised."
  (let ((block-name (gensym))
        (signaled-p (gensym)))
    (destructuring-bind (condition &optional reason-control reason-args)
        (ensure-list condition-spec)
      `(let ((,signaled-p nil))
         (block ,block-name
           (handler-bind ((,condition (lambda (c)
                                        (unless (typep c 'warning)
                                          (error "Cannot use FiveAM \"warns\" check for non-warning conditions."))
                                        ;; ok, body threw condition
                                        (add-result 'test-passed
                                                    :test-expr ',condition)
                                        (setf ,signaled-p t)
                                        (muffle-warning c))))
             (block nil
               ,@body))
           (when ,signaled-p (return-from ,block-name t))
           (process-failure
            ',condition
            ,@(if reason-control
                  `(,reason-control ,@reason-args)
                  `("Failed to signal a ~S" ',condition)))
           (return-from ,block-name nil))))))

(in-package :arity-test)

#|
(fiveam:def-suite* test-implicit-conjunction-warning)
(test (implicit-conj-singleton-op :suite test-implicit-conjunction-warning)
  (let ((shop:*define-silently* t))
   (ignore-errors
    (shop3::delete-domain 'implicit-conjunction-singleton))
    (warns shop3::implicit-conjunction-warning
      (defdomain implicit-conjunction-singleton
          ((:op (!op1)
            :precond ((c))
            :add ((a) (b)))))))
  (let* ((dom (shop::find-domain 'implicit-conjunction-singleton nil))
         (op (progn (is-true (typep dom 'shop::domain) "Couldn't find definition of implicit-conjunction-singleton first domain.")
                    (shop::operator dom '!op1))))
    (is (equalp '(c)
                (shop::operator-preconditions op)))))

(test (implicit-conj-singleton-meth :suite test-implicit-conjunction-warning)
  (ignore-errors
    (shop3::delete-domain 'implicit-conjunction-singleton))
  (warns shop3::implicit-conjunction-warning
    (let ((shop:*define-silently* t))
     (defdomain implicit-conjunction-singleton
         ((:method (task2)
            m2
            ((a))
            (:ordered (:task4) (task5))))))
    (let* ((dom (shop::find-domain 'implicit-conjunction-singleton))
           (meths (progn (is-true (typep dom 'shop::domain) "Couldn't find definition of implicit-conjunction-singleton second domain.")
                         (shop::methods dom 'task2)))
           (meth (progn (is (eql 1 (length meths))) (first meths)))
           (body (progn (is (eql 5 (length meth))) (cddr meth)))
           (pre (progn (is (eql 3 (length body))) (second body)))) ;first is name, second is precond, third is task net
    (is (equalp '(a) pre)))))


(test (implicit-conj-conjunction-op :suite test-implicit-conjunction-warning)
  (ignore-errors
   (shop3::delete-domain 'implicit-conjunction-conjunction))
  (warns shop3::implicit-conjunction-warning
    (let ((shop:*define-silently* t))
     (defdomain implicit-conjunction-conjunction
         ((:op (!op1)
           :precond ((c) (d))
           :add ((a) (b)))))))
  (let* ((dom (shop::find-domain 'implicit-conjunction-conjunction nil))
         (op (progn (is-true (typep dom 'shop::domain) "Couldn't find definition of implicit-conjunction-conjunction first domain.")
                    (shop::operator dom '!op1))))
    (is (equalp '(and (c) (d))
                (shop::operator-preconditions op)))))


(test (implicit-conj-conjunction-meth :suite test-implicit-conjunction-warning)
  (ignore-errors
   (shop3::delete-domain 'implicit-conjunction-conjunction))
  (warns shop3::implicit-conjunction-warning
    (let ((shop:*define-silently* t))
     (defdomain implicit-conjunction-conjunction
         ((:method (task2)
            m2
            ((a) (b))
            (:ordered (:task4) (task5)))))))
    (let* ((dom (shop::find-domain 'implicit-conjunction-conjunction))
           (meths (progn (is-true (typep dom 'shop::domain) "Couldn't find definition of implicit-conjunction-conjunction second domain.")
                         (shop::methods dom 'task2)))
           (meth (progn (is (eql 1 (length meths))) (first meths)))
           (body (progn (is (eql 5 (length meth))) (cddr meth)))
           (pre (progn (is (eql 3 (length body))) (second body)))) ;first is name, second is precond, third is task net
    (is (equalp '(and (a) (b)) pre))))
|#
