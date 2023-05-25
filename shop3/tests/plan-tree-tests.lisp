(defpackage :plan-tree-tests
  (:shadow #:fail)
  (:import-from #:shop-user #:log-ran-15-1)
  (:use :common-lisp :fiveam :shop3))

(in-package plan-tree-tests)

(def-suite* plan-tree-tests)

(test plan-tree
  (multiple-value-bind (plans ignore trees)
      (find-plans 'log-ran-15-1 :plan-tree t :verbose 0)
    (declare (ignore ignore))
    (is (equalp
         '((((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE1 SHOP3-USER::LOC8-1)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK8-1
               SHOP3-USER::PACKAGE1 SHOP3-USER::LOC8-3 SHOP3-USER::LOC8-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-3)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1
                                          SHOP3-USER::LOC8-3)
                0))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE1 SHOP3-USER::TRUCK8-1
                SHOP3-USER::LOC8-3)
               1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-2
                                          SHOP3-USER::LOC8-1)
                22))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE1
                SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1)
               23)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE3 SHOP3-USER::LOC2-3)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK2-1
               SHOP3-USER::PACKAGE3 SHOP3-USER::LOC2-2 SHOP3-USER::LOC2-3)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1
                                          SHOP3-USER::LOC2-2)
                2))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE3 SHOP3-USER::TRUCK2-1
                SHOP3-USER::LOC2-2)
               3)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-3)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-2
                                          SHOP3-USER::LOC2-3)
                25))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE3
                SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-3)
               26)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE4 SHOP3-USER::LOC6-2)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK6-1
               SHOP3-USER::PACKAGE4 SHOP3-USER::LOC6-3 SHOP3-USER::LOC6-2)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1
                                          SHOP3-USER::LOC6-3)
                4))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE4 SHOP3-USER::TRUCK6-1
                SHOP3-USER::LOC6-3)
               5)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3
                                          SHOP3-USER::LOC6-2)
                27))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE4
                SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-2)
               28)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE6 SHOP3-USER::LOC6-2)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK2-1
               SHOP3-USER::PACKAGE6 SHOP3-USER::LOC2-3 SHOP3-USER::LOC2-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-3)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-2
                                          SHOP3-USER::LOC2-3)
                6))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE6 SHOP3-USER::TRUCK2-1
                SHOP3-USER::LOC2-3)
               7)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-3
                                          SHOP3-USER::LOC2-1)
                30))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE6
                SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1)
               31))
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE6
               SHOP3-USER::LOC2-1 SHOP3-USER::LOC6-1)
              (1.0
               (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC5-1
                SHOP3-USER::LOC2-1)
               50)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE6
                SHOP3-USER::PLANE3 SHOP3-USER::LOC2-1)
               57)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC6-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC1-1
                                           SHOP3-USER::LOC6-1)
                76))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE6
                SHOP3-USER::PLANE3 SHOP3-USER::LOC6-1)
               85))
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK6-1
               SHOP3-USER::PACKAGE6 SHOP3-USER::LOC6-1 SHOP3-USER::LOC6-2)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3
                                          SHOP3-USER::LOC6-1)
                97))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE6 SHOP3-USER::TRUCK6-1
                SHOP3-USER::LOC6-1)
               98)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3
                                          SHOP3-USER::LOC6-2)
                107))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE6
                SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-2)
               108)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE7 SHOP3-USER::LOC6-3)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK1-1
               SHOP3-USER::PACKAGE7 SHOP3-USER::LOC1-2 SHOP3-USER::LOC1-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK1-1 SHOP3-USER::LOC1-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK1-1 SHOP3-USER::LOC1-1
                                          SHOP3-USER::LOC1-2)
                8))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE7 SHOP3-USER::TRUCK1-1
                SHOP3-USER::LOC1-2)
               9)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK1-1 SHOP3-USER::LOC1-1)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK1-1 SHOP3-USER::LOC1-2
                                          SHOP3-USER::LOC1-1)
                32))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE7
                SHOP3-USER::TRUCK1-1 SHOP3-USER::LOC1-1)
               33))
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE7
               SHOP3-USER::LOC1-1 SHOP3-USER::LOC6-1)
              (0
               (SHOP3-USER::!ADD-PROTECTION
                (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE1 SHOP3-USER::LOC1-1))
               51)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE7
                SHOP3-USER::PLANE1 SHOP3-USER::LOC1-1)
               58)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE1 SHOP3-USER::LOC6-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE1 SHOP3-USER::LOC6-1))
                66))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE7
                SHOP3-USER::PLANE1 SHOP3-USER::LOC6-1)
               73))
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK6-1
               SHOP3-USER::PACKAGE7 SHOP3-USER::LOC6-1 SHOP3-USER::LOC6-3)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1))
                81))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE7 SHOP3-USER::TRUCK6-1
                SHOP3-USER::LOC6-1)
               82)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1
                                          SHOP3-USER::LOC6-3)
                94))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE7
                SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3)
               95)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE8 SHOP3-USER::LOC1-1)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK6-1
               SHOP3-USER::PACKAGE8 SHOP3-USER::LOC6-3 SHOP3-USER::LOC6-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3))
                10))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE8 SHOP3-USER::TRUCK6-1
                SHOP3-USER::LOC6-3)
               11)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-2
                                          SHOP3-USER::LOC6-1)
                34))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE8
                SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1)
               35))
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE8
               SHOP3-USER::LOC6-1 SHOP3-USER::LOC1-1)
              (1.0
               (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE1 SHOP3-USER::LOC1-1
                SHOP3-USER::LOC6-1)
               59)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE8
                SHOP3-USER::PLANE1 SHOP3-USER::LOC6-1)
               67)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE1 SHOP3-USER::LOC1-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE1 SHOP3-USER::LOC2-1
                                           SHOP3-USER::LOC1-1)
                84))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE8
                SHOP3-USER::PLANE1 SHOP3-USER::LOC1-1)
               96)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE9 SHOP3-USER::LOC4-2)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK5-1
               SHOP3-USER::PACKAGE9 SHOP3-USER::LOC5-2 SHOP3-USER::LOC5-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK5-1 SHOP3-USER::LOC5-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK5-1 SHOP3-USER::LOC5-1
                                          SHOP3-USER::LOC5-2)
                12))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE9 SHOP3-USER::TRUCK5-1
                SHOP3-USER::LOC5-2)
               13)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK5-1 SHOP3-USER::LOC5-1)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK5-1 SHOP3-USER::LOC5-2
                                          SHOP3-USER::LOC5-1)
                36))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE9
                SHOP3-USER::TRUCK5-1 SHOP3-USER::LOC5-1)
               37))
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE9
               SHOP3-USER::LOC5-1 SHOP3-USER::LOC4-1)
              (1.0
               (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC2-1
                SHOP3-USER::LOC5-1)
               62)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE9
                SHOP3-USER::PLANE3 SHOP3-USER::LOC5-1)
               68)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC4-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC3-1
                                           SHOP3-USER::LOC4-1)
                101))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE9
                SHOP3-USER::PLANE3 SHOP3-USER::LOC4-1)
               113))
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK4-1
               SHOP3-USER::PACKAGE9 SHOP3-USER::LOC4-1 SHOP3-USER::LOC4-2)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK4-1 SHOP3-USER::LOC4-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK4-1 SHOP3-USER::LOC4-1))
                120))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE9 SHOP3-USER::TRUCK4-1
                SHOP3-USER::LOC4-1)
               121)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK4-1 SHOP3-USER::LOC4-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK4-1 SHOP3-USER::LOC4-1
                                          SHOP3-USER::LOC4-2)
                122))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE9
                SHOP3-USER::TRUCK4-1 SHOP3-USER::LOC4-2)
               123)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE11 SHOP3-USER::LOC3-2)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK2-1
               SHOP3-USER::PACKAGE11 SHOP3-USER::LOC2-3 SHOP3-USER::LOC2-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-3)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-3))
                14))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE11
                SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-3)
               15)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1))
                39))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE11
                SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1)
               40))
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE11
               SHOP3-USER::LOC2-1 SHOP3-USER::LOC3-1)
              (0
               (SHOP3-USER::!ADD-PROTECTION
                (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE3 SHOP3-USER::LOC2-1))
               53)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE11
                SHOP3-USER::PLANE3 SHOP3-USER::LOC2-1)
               60)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC3-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC6-1
                                           SHOP3-USER::LOC3-1)
                86))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE11
                SHOP3-USER::PLANE3 SHOP3-USER::LOC3-1)
               99))
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK3-1
               SHOP3-USER::PACKAGE11 SHOP3-USER::LOC3-1 SHOP3-USER::LOC3-2)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1))
                109))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE11
                SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1)
               110)
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE11
                SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-2)
               115)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-2)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-2))
                116))))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE12 SHOP3-USER::LOC3-3)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK2-1
               SHOP3-USER::PACKAGE12 SHOP3-USER::LOC2-2 SHOP3-USER::LOC2-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-3
                                          SHOP3-USER::LOC2-2)
                16))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE12
                SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-2)
               17)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1))
                41))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE12
                SHOP3-USER::TRUCK2-1 SHOP3-USER::LOC2-1)
               42))
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE12
               SHOP3-USER::LOC2-1 SHOP3-USER::LOC3-1)
              (0
               (SHOP3-USER::!ADD-PROTECTION
                (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE3 SHOP3-USER::LOC2-1))
               54)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE12
                SHOP3-USER::PLANE3 SHOP3-USER::LOC2-1)
               61)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC3-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE3 SHOP3-USER::LOC3-1))
                87))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE12
                SHOP3-USER::PLANE3 SHOP3-USER::LOC3-1)
               100))
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK3-1
               SHOP3-USER::PACKAGE12 SHOP3-USER::LOC3-1 SHOP3-USER::LOC3-3)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1))
                111))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE12
                SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1)
               112)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-3)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-2
                                          SHOP3-USER::LOC3-3)
                118))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE12
                SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-3)
               119)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE13 SHOP3-USER::LOC3-2)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK7-1
               SHOP3-USER::PACKAGE13 SHOP3-USER::LOC7-2 SHOP3-USER::LOC7-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK7-1 SHOP3-USER::LOC7-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK7-1 SHOP3-USER::LOC7-1
                                          SHOP3-USER::LOC7-2)
                18))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE13
                SHOP3-USER::TRUCK7-1 SHOP3-USER::LOC7-2)
               19)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK7-1 SHOP3-USER::LOC7-1)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK7-1 SHOP3-USER::LOC7-2
                                          SHOP3-USER::LOC7-1)
                43))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE13
                SHOP3-USER::TRUCK7-1 SHOP3-USER::LOC7-1)
               44))
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE13
               SHOP3-USER::LOC7-1 SHOP3-USER::LOC3-1)
              (0
               (SHOP3-USER::!ADD-PROTECTION
                (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE2 SHOP3-USER::LOC7-1))
               55)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE13
                SHOP3-USER::PLANE2 SHOP3-USER::LOC7-1)
               63)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC3-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC6-1
                                           SHOP3-USER::LOC3-1)
                80))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE13
                SHOP3-USER::PLANE2 SHOP3-USER::LOC3-1)
               92))
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK3-1
               SHOP3-USER::PACKAGE13 SHOP3-USER::LOC3-1 SHOP3-USER::LOC3-2)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1))
                104))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE13
                SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1)
               105)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-1
                                          SHOP3-USER::LOC3-2)
                114))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE13
                SHOP3-USER::TRUCK3-1 SHOP3-USER::LOC3-2)
               117)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE15 SHOP3-USER::LOC5-1)
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK8-1
               SHOP3-USER::PACKAGE15 SHOP3-USER::LOC8-2 SHOP3-USER::LOC8-1)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-2)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-3
                                          SHOP3-USER::LOC8-2)
                20))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE15
                SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-2)
               21)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1))
                46))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE15
                SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1)
               47))
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE15
               SHOP3-USER::LOC8-1 SHOP3-USER::LOC5-1)
              (1.0
               (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC7-1
                SHOP3-USER::LOC8-1)
               64)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE15
                SHOP3-USER::PLANE2 SHOP3-USER::LOC8-1)
               70)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC5-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC3-1
                                           SHOP3-USER::LOC5-1)
                93))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE15
                SHOP3-USER::PLANE2 SHOP3-USER::LOC5-1)
               106)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE2 SHOP3-USER::LOC2-1)
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE2
               SHOP3-USER::LOC1-1 SHOP3-USER::LOC2-1)
              (0
               (SHOP3-USER::!ADD-PROTECTION
                (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE1 SHOP3-USER::LOC1-1))
               24)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE2
                SHOP3-USER::PLANE1 SHOP3-USER::LOC1-1)
               48)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE1 SHOP3-USER::LOC2-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE1 SHOP3-USER::LOC6-1
                                           SHOP3-USER::LOC2-1)
                74))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE2
                SHOP3-USER::PLANE1 SHOP3-USER::LOC2-1)
               83)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE5 SHOP3-USER::LOC1-1)
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE5
               SHOP3-USER::LOC5-1 SHOP3-USER::LOC1-1)
              (0
               (SHOP3-USER::!ADD-PROTECTION
                (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE3 SHOP3-USER::LOC5-1))
               29)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE5
                SHOP3-USER::PLANE3 SHOP3-USER::LOC5-1)
               49)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC1-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE3 SHOP3-USER::LOC5-1
                                           SHOP3-USER::LOC1-1)
                69))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE5
                SHOP3-USER::PLANE3 SHOP3-USER::LOC1-1)
               75)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE10 SHOP3-USER::LOC8-3)
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE10
               SHOP3-USER::LOC7-1 SHOP3-USER::LOC8-1)
              (1.0
               (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC4-1
                SHOP3-USER::LOC7-1)
               38)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE10
                SHOP3-USER::PLANE2 SHOP3-USER::LOC7-1)
               52)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC8-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE2 SHOP3-USER::LOC8-1))
                65))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE10
                SHOP3-USER::PLANE2 SHOP3-USER::LOC8-1)
               71))
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK8-1
               SHOP3-USER::PACKAGE10 SHOP3-USER::LOC8-1 SHOP3-USER::LOC8-3)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1))
                77))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE10
                SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1)
               78)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-3)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1
                                          SHOP3-USER::LOC8-3)
                88))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE10
                SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-3)
               89)))
            ((SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE14 SHOP3-USER::LOC6-3)
             ((SHOP3-USER::AIR-DELIVER-OBJ SHOP3-USER::PACKAGE14
               SHOP3-USER::LOC7-1 SHOP3-USER::LOC6-1)
              (0
               (SHOP3-USER::!ADD-PROTECTION
                (SHOP3-USER::AIRPLANE-AT SHOP3-USER::PLANE2 SHOP3-USER::LOC7-1))
               45)
              (1.0
               (SHOP3-USER::!LOAD-AIRPLANE SHOP3-USER::PACKAGE14
                SHOP3-USER::PLANE2 SHOP3-USER::LOC7-1)
               56)
              ((SHOP3-USER::FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC6-1)
               (1.0
                (SHOP3-USER::!FLY-AIRPLANE SHOP3-USER::PLANE2 SHOP3-USER::LOC8-1
                                           SHOP3-USER::LOC6-1)
                72))
              (1.0
               (SHOP3-USER::!UNLOAD-AIRPLANE SHOP3-USER::PACKAGE14
                SHOP3-USER::PLANE2 SHOP3-USER::LOC6-1)
               79))
             ((SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK6-1
               SHOP3-USER::PACKAGE14 SHOP3-USER::LOC6-1 SHOP3-USER::LOC6-3)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1)
               (0
                (SHOP3-USER::!ADD-PROTECTION
                 (SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1))
                90))
              (1.0
               (SHOP3-USER::!LOAD-TRUCK SHOP3-USER::PACKAGE14
                SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1)
               91)
              ((SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3)
               (1.0
                (SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-1
                                          SHOP3-USER::LOC6-3)
                102))
              (1.0
               (SHOP3-USER::!UNLOAD-TRUCK SHOP3-USER::PACKAGE14
                SHOP3-USER::TRUCK6-1 SHOP3-USER::LOC6-3)
               103)))))
         trees))
    (let* ((tree (first trees))
          (root (first tree)))
      (is-true (complex-node-p root))
      (is (equalp
           '(SHOP3-USER::OBJ-AT SHOP3-USER::PACKAGE1 SHOP3-USER::LOC8-1)
           (complex-node-task (first tree))))
      (is (equalp (tree-node-task (first tree))
                  (complex-node-task (first tree))))
      (let ((first-subtask
              (first (complex-node-children (first tree)))))
        (is-true (complex-node-p first-subtask))
        (is (equalp '(SHOP3-USER::IN-CITY-DELIVERY SHOP3-USER::TRUCK8-1
                      SHOP3-USER::PACKAGE1 SHOP3-USER::LOC8-3 SHOP3-USER::LOC8-1)
                    (tree-node-task first-subtask)))
        (let ((truck-at (first (complex-node-children first-subtask))))
          (is-true (complex-node-p truck-at))
          (is (equalp '(SHOP3-USER::TRUCK-AT SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-3)
                     (complex-node-task truck-at)))
          (is (= 1 (length (complex-node-children truck-at))))
          (let ((prim (first (complex-node-children truck-at))))
            (is-true (primitive-node-p prim))
            (is (equalp '(SHOP3-USER::!DRIVE-TRUCK SHOP3-USER::TRUCK8-1 SHOP3-USER::LOC8-1
                          SHOP3-USER::LOC8-3)
                        (primitive-node-task prim)))
            (is (= (primitive-node-position prim) 0))
            (is (= (primitive-node-cost prim) 1))))))))
