(in-package :shop-user)

(fiveam:def-suite umt-domain-tests)
(fiveam:in-suite umt-domain-tests)

(fiveam:def-fixture umt-domain ()
  (progn 
    (define-umt-domain)
    (&body)))

(defun test-plan-quietly (problem &rest args)
  (remove-plan-costs
   (first
    (apply #'find-plans problem :which :first :verbose 0 args))))

(defun ess-test-plan-quietly (problem &rest args)
  (flet ((find-plans (problem  &rest rest &key which verbose gc)
           (declare (ignore gc))
           (remf rest :which)
           (remf rest :verbose)
           (remf rest :gc)
           (assert (eq which :first))
           (when rest
             (error "Can't handle rest arguments for FIND-PLANS-STACK: ~s" rest))
           (find-plans-stack problem :verbose verbose)))
    (remove-plan-costs
     (first
      (apply #'find-plans problem :which :first :verbose 0 :gc t args)))))

(defparameter *pfile1-plan*
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

(defparameter *pfile2-plan*
  '((!!ASSERT
     ((GOAL (CLEAR)) (GOAL (DELIVERED PACKAGE2 LOCATION5))
      (GOAL (DELIVERED PACKAGE1 LOCATION0))
      (GOAL (DELIVERED PACKAGE0 LOCATION2))))
    (!!CHECK PACKAGE2) (!!ADD-PACKAGE-ROAD PACKAGE2 ROAD_ROUTE0)
    (!!ADD-PACKAGE-NN PACKAGE2 LOCATION2 LOCATION0) (!!CHECK PACKAGE1)
    (!!ADD-PACKAGE-LOCAL PACKAGE1) (!!CHECK PACKAGE0)
    (!!ADD-PACKAGE-LOCAL PACKAGE0) (!DELIVER PACKAGE0 LOCATION2)
    (!DELIVER PACKAGE1 LOCATION0) (!COLLECT-FEES PACKAGE2)
    (!!ADD-NEXT TRUCK4 LOCATION4) (!!EXP-WEIGHT-SET TRUCK4 ROAD_ROUTE0 5)
    (!!ADD-PROTECTION (AT-VEHICLE TRUCK4 LOCATION4))
    (!!DELETE-PROTECTION (NEXT TRUCK4 LOCATION4)) (!!DEL-NEXT TRUCK4 LOCATION4)
    (!!ADD-NEXT TRUCK4 LOCATION5) (!LOWER-RAMP TRUCK4)
    (!LOAD-CARS PACKAGE2 TRUCK4 LOCATION4) (!RAISE-RAMP TRUCK4)
    (!MOVE-VEHICLE-ROAD-ROUTE-CROSSCITY TRUCK4 LOCATION4 LOCATION5 CITY1 CITY0
     ROAD_ROUTE0)
    (!!DELETE-PROTECTION (NEXT TRUCK4 LOCATION5))
    (!!EXP-WEIGHT-CLEAR TRUCK4 ROAD_ROUTE0) (!LOWER-RAMP TRUCK4)
    (!UNLOAD-CARS PACKAGE2 TRUCK4 LOCATION5) (!RAISE-RAMP TRUCK4)
    (!DELIVER PACKAGE2 LOCATION5) (!CLEAN-DOMAIN)))

(fiveam:test umt-tests
  (fiveam:with-fixture umt-domain ()
    (fiveam:is
     (equal *pfile1-plan*
         (test-plan-quietly 'umt.pfile1)))
    (fiveam:is
     (equal *pfile1-plan*
         (ess-test-plan-quietly 'umt.pfile1)))
    (fiveam:is
     (equal *pfile2-plan*
         (test-plan-quietly 'umt.pfile2)))
    (fiveam:is
     (null (find-plans 'umt.pfile3 :which :first :verbose 0)))))

(fiveam:test ess-umt-tests
  (fiveam:with-fixture umt-domain ()
    (fiveam:is
     (equal *pfile1-plan*
         (ess-test-plan-quietly 'umt.pfile1)))
    (fiveam:is
     (equal *pfile1-plan*
         (ess-test-plan-quietly 'umt.pfile1)))
    (fiveam:is
     (equal *pfile2-plan*
         (test-plan-quietly 'umt.pfile2)))
    (fiveam:is
     (null (find-plans-stack 'umt.pfile3 ;; :which :first
                                         :verbose 0)))))
