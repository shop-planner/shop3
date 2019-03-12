;;; -*- mode: common-lisp; coding: unix; -*-
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

(fiveam:def-suite io-tests)
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

(def-fixture complex-method-def ()
  (let ((meth '(:method (find-movable)
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
                placeholder nil '(:ordered (:task shop2::!!inop))))))))

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



