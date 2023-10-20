(defpackage shop-hddl-tests
  (:use common-lisp iterate fiveam hddl-translator)
  (:import-from #:shop #:find-plans #:find-plans-stack)
  )
(in-package :shop-hddl-tests)

(def-suite* hddl-plan-tests)

(defmacro load-log-problem ()
  `(progn
     (unless (shop:find-domain 'shop-user::logistics nil)
      (load (asdf:system-relative-pathname "shop3"
                                           "examples/logistic/logistic.lisp")))
     (unless (shop:find-problem 'shop-user::log-ran-15-1 nil)
       (load (asdf:system-relative-pathname "shop3"
                                           "examples/logistic/Log_ran_problems_15.lisp")))))

(defvar *plan*)
(defvar *tree*)

(defmacro with-plan-and-tree ((planvar treevar &key ess) &body body)
  `(,(if ess
         'with-ess-plan-and-tree
         'with-shop2-plan-and-tree)
    (,planvar ,treevar) ,@body))

(defmacro with-shop2-plan-and-tree ((planvar treevar) &body body)
  (let ((plans (gensym))
        (trees (gensym))
        (ignore (gensym)))
    `(multiple-value-bind (,plans ,ignore ,trees)
        (find-plans 'shop-user::log-ran-15-1 :plan-tree t :verbose 0)
       (declare (ignore ,ignore))
       (unless (and ,plans ,trees) (error "Expected a plan and tree for test case."))
       (let ((,planvar (first ,plans))
             (,treevar (first ,trees)))
         ,@body))))

(defmacro with-ess-plan-and-tree ((planvar treevar) &body body)
  (let ((returns (gensym)))
    `(let ((,returns (find-plans-stack 'shop-user::log-ran-15-1 :plan-tree t :verbose 0 :unpack-returns nil)))
       (unless ,returns (error "Expected successful planning"))
       (let ((,planvar (shop::plan (first ,returns)))
             (,treevar (shop::tree (first , returns))))
         ,@body))))

#+ignore
(test plan-test
  (load-log-problem)
  (with-plan-and-tree (plan tree)
    (is (equalp expected-plan plan))
    (is (equalp expected-tree tree))))

(test plan-test-ess
  (load-log-problem)
  (with-plan-and-tree (plan tree :ess t)
      (is (equalp expected-plan plan))
    ;; (is (equalp expected-tree tree))
    ))

#+ignore
(test translate-tree
  (load-log-problem)
  (with-plan-and-tree (plan tree)
    (let ((hddl-plan-sexp
            (hddl-plan plan tree)))
      (is-true hddl-plan-sexp)
      (let ((max-act 124)
            (act-alist (getf (rest hddl-plan-sexp) :actions))
            (root-list (getf (rest hddl-plan-sexp) :roots)))
        (is (= max-act (length act-alist)))
        (is (= 15 (length root-list)))
        (is (equalp (alexandria:iota 15 :start (1+ max-act))
                    root-list))))))

;;; this works do far...
(test translate-tree-ess
  (load-log-problem)
  (with-plan-and-tree (plan tree :ess t)
    (let ((hddl-plan-sexp
            (hddl-plan plan tree)))
      (is-true hddl-plan-sexp)
      (let ((max-act 124)
            (act-alist (getf (rest hddl-plan-sexp) :actions))
            (root-list (getf (rest hddl-plan-sexp) :roots)))
        (is (= max-act (length act-alist)))
        (is (= 15 (length root-list)))
        (is (equalp (alexandria:iota 15 :start (1+ max-act))
                    root-list))))))


(in-package :shop-user)
(defparameter shop-hddl-tests::expected-plan
  '((!DRIVE-TRUCK TRUCK8-1 LOC8-1
   LOC8-3)
  1.0
  (!LOAD-TRUCK PACKAGE1 TRUCK8-1
   LOC8-3)
  1.0
  (!DRIVE-TRUCK TRUCK2-1 LOC2-1
   LOC2-2)
  1.0
  (!LOAD-TRUCK PACKAGE3 TRUCK2-1
   LOC2-2)
  1.0
  (!DRIVE-TRUCK TRUCK6-1 LOC6-1
   LOC6-3)
  1.0
  (!LOAD-TRUCK PACKAGE4 TRUCK6-1
   LOC6-3)
  1.0
  (!DRIVE-TRUCK TRUCK2-1 LOC2-2
   LOC2-3)
  1.0
  (!LOAD-TRUCK PACKAGE6 TRUCK2-1
   LOC2-3)
  1.0
  (!DRIVE-TRUCK TRUCK1-1 LOC1-1
   LOC1-2)
  1.0
  (!LOAD-TRUCK PACKAGE7 TRUCK1-1
   LOC1-2)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK6-1 LOC6-3))
  0
  (!LOAD-TRUCK PACKAGE8 TRUCK6-1
   LOC6-3)
  1.0
  (!DRIVE-TRUCK TRUCK5-1 LOC5-1
   LOC5-2)
  1.0
  (!LOAD-TRUCK PACKAGE9 TRUCK5-1
   LOC5-2)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK2-1 LOC2-3))
  0
  (!LOAD-TRUCK PACKAGE11 TRUCK2-1
   LOC2-3)
  1.0
  (!DRIVE-TRUCK TRUCK2-1 LOC2-3
   LOC2-2)
  1.0
  (!LOAD-TRUCK PACKAGE12 TRUCK2-1
   LOC2-2)
  1.0
  (!DRIVE-TRUCK TRUCK7-1 LOC7-1
   LOC7-2)
  1.0
  (!LOAD-TRUCK PACKAGE13 TRUCK7-1
   LOC7-2)
  1.0
  (!DRIVE-TRUCK TRUCK8-1 LOC8-3
   LOC8-2)
  1.0
  (!LOAD-TRUCK PACKAGE15 TRUCK8-1
   LOC8-2)
  1.0
  (!DRIVE-TRUCK TRUCK8-1 LOC8-2
   LOC8-1)
  1.0
  (!UNLOAD-TRUCK PACKAGE1 TRUCK8-1
   LOC8-1)
  1.0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE1 LOC1-1))
  0
  (!DRIVE-TRUCK TRUCK2-1 LOC2-2
   LOC2-3)
  1.0
  (!UNLOAD-TRUCK PACKAGE3 TRUCK2-1
   LOC2-3)
  1.0
  (!DRIVE-TRUCK TRUCK6-1 LOC6-3
   LOC6-2)
  1.0
  (!UNLOAD-TRUCK PACKAGE4 TRUCK6-1
   LOC6-2)
  1.0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE3 LOC5-1))
  0
  (!DRIVE-TRUCK TRUCK2-1 LOC2-3
   LOC2-1)
  1.0
  (!UNLOAD-TRUCK PACKAGE6 TRUCK2-1
   LOC2-1)
  1.0
  (!DRIVE-TRUCK TRUCK1-1 LOC1-2
   LOC1-1)
  1.0
  (!UNLOAD-TRUCK PACKAGE7 TRUCK1-1
   LOC1-1)
  1.0
  (!DRIVE-TRUCK TRUCK6-1 LOC6-2
   LOC6-1)
  1.0
  (!UNLOAD-TRUCK PACKAGE8 TRUCK6-1
   LOC6-1)
  1.0
  (!DRIVE-TRUCK TRUCK5-1 LOC5-2
   LOC5-1)
  1.0
  (!UNLOAD-TRUCK PACKAGE9 TRUCK5-1
   LOC5-1)
  1.0
  (!FLY-AIRPLANE PLANE2 LOC4-1
   LOC7-1)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK2-1 LOC2-1))
  0
  (!UNLOAD-TRUCK PACKAGE11 TRUCK2-1
   LOC2-1)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK2-1 LOC2-1))
  0
  (!UNLOAD-TRUCK PACKAGE12 TRUCK2-1
   LOC2-1)
  1.0
  (!DRIVE-TRUCK TRUCK7-1 LOC7-2
   LOC7-1)
  1.0
  (!UNLOAD-TRUCK PACKAGE13 TRUCK7-1
   LOC7-1)
  1.0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE2 LOC7-1))
  0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK8-1 LOC8-1))
  0
  (!UNLOAD-TRUCK PACKAGE15 TRUCK8-1
   LOC8-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE2 PLANE1
   LOC1-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE5 PLANE3
   LOC5-1)
  1.0
  (!FLY-AIRPLANE PLANE3 LOC5-1
   LOC2-1)
  1.0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE1 LOC1-1))
  0
  (!LOAD-AIRPLANE PACKAGE10 PLANE2
   LOC7-1)
  1.0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE3 LOC2-1))
  0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE3 LOC2-1))
  0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE2 LOC7-1))
  0
  (!LOAD-AIRPLANE PACKAGE14 PLANE2
   LOC7-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE6 PLANE3
   LOC2-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE7 PLANE1
   LOC1-1)
  1.0
  (!FLY-AIRPLANE PLANE1 LOC1-1
   LOC6-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE11 PLANE3
   LOC2-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE12 PLANE3
   LOC2-1)
  1.0
  (!FLY-AIRPLANE PLANE3 LOC2-1
   LOC5-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE13 PLANE2
   LOC7-1)
  1.0
  (!FLY-AIRPLANE PLANE2 LOC7-1
   LOC8-1)
  1.0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE2 LOC8-1))
  0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE1 LOC6-1))
  0
  (!LOAD-AIRPLANE PACKAGE8 PLANE1
   LOC6-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE9 PLANE3
   LOC5-1)
  1.0
  (!FLY-AIRPLANE PLANE3 LOC5-1
   LOC1-1)
  1.0
  (!LOAD-AIRPLANE PACKAGE15 PLANE2
   LOC8-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE10
   PLANE2 LOC8-1)
  1.0
  (!FLY-AIRPLANE PLANE2 LOC8-1
   LOC6-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE7 PLANE1
   LOC6-1)
  1.0
  (!FLY-AIRPLANE PLANE1 LOC6-1
   LOC2-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE5 PLANE3
   LOC1-1)
  1.0
  (!FLY-AIRPLANE PLANE3 LOC1-1
   LOC6-1)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK8-1 LOC8-1))
  0
  (!LOAD-TRUCK PACKAGE10 TRUCK8-1
   LOC8-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE14
   PLANE2 LOC6-1)
  1.0
  (!FLY-AIRPLANE PLANE2 LOC6-1
   LOC3-1)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK6-1 LOC6-1))
  0
  (!LOAD-TRUCK PACKAGE7 TRUCK6-1
   LOC6-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE2 PLANE1
   LOC2-1)
  1.0
  (!FLY-AIRPLANE PLANE1 LOC2-1
   LOC1-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE6 PLANE3
   LOC6-1)
  1.0
  (!FLY-AIRPLANE PLANE3 LOC6-1
   LOC3-1)
  1.0
  (!ADD-PROTECTION
   (AIRPLANE-AT PLANE3 LOC3-1))
  0
  (!DRIVE-TRUCK TRUCK8-1 LOC8-1
   LOC8-3)
  1.0
  (!UNLOAD-TRUCK PACKAGE10 TRUCK8-1
   LOC8-3)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK6-1 LOC6-1))
  0
  (!LOAD-TRUCK PACKAGE14 TRUCK6-1
   LOC6-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE13
   PLANE2 LOC3-1)
  1.0
  (!FLY-AIRPLANE PLANE2 LOC3-1
   LOC5-1)
  1.0
  (!DRIVE-TRUCK TRUCK6-1 LOC6-1
   LOC6-3)
  1.0
  (!UNLOAD-TRUCK PACKAGE7 TRUCK6-1
   LOC6-3)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE8 PLANE1
   LOC1-1)
  1.0
  (!DRIVE-TRUCK TRUCK6-1 LOC6-3
   LOC6-1)
  1.0
  (!LOAD-TRUCK PACKAGE6 TRUCK6-1
   LOC6-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE11
   PLANE3 LOC3-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE12
   PLANE3 LOC3-1)
  1.0
  (!FLY-AIRPLANE PLANE3 LOC3-1
   LOC4-1)
  1.0
  (!DRIVE-TRUCK TRUCK6-1 LOC6-1
   LOC6-3)
  1.0
  (!UNLOAD-TRUCK PACKAGE14 TRUCK6-1
   LOC6-3)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK3-1 LOC3-1))
  0
  (!LOAD-TRUCK PACKAGE13 TRUCK3-1
   LOC3-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE15
   PLANE2 LOC5-1)
  1.0
  (!DRIVE-TRUCK TRUCK6-1 LOC6-3
   LOC6-2)
  1.0
  (!UNLOAD-TRUCK PACKAGE6 TRUCK6-1
   LOC6-2)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK3-1 LOC3-1))
  0
  (!LOAD-TRUCK PACKAGE11 TRUCK3-1
   LOC3-1)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK3-1 LOC3-1))
  0
  (!LOAD-TRUCK PACKAGE12 TRUCK3-1
   LOC3-1)
  1.0
  (!UNLOAD-AIRPLANE PACKAGE9 PLANE3
   LOC4-1)
  1.0
  (!DRIVE-TRUCK TRUCK3-1 LOC3-1
   LOC3-2)
  1.0
  (!UNLOAD-TRUCK PACKAGE11 TRUCK3-1
   LOC3-2)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK3-1 LOC3-2))
  0
  (!UNLOAD-TRUCK PACKAGE13 TRUCK3-1
   LOC3-2)
  1.0
  (!DRIVE-TRUCK TRUCK3-1 LOC3-2
   LOC3-3)
  1.0
  (!UNLOAD-TRUCK PACKAGE12 TRUCK3-1
   LOC3-3)
  1.0
  (!ADD-PROTECTION
   (TRUCK-AT TRUCK4-1 LOC4-1))
  0
  (!LOAD-TRUCK PACKAGE9 TRUCK4-1
   LOC4-1)
  1.0
  (!DRIVE-TRUCK TRUCK4-1 LOC4-1
   LOC4-2)
  1.0
  (!UNLOAD-TRUCK PACKAGE9 TRUCK4-1
   LOC4-2)
  1.0))

(defparameter shop-hddl-tests::expected-tree
  '(((OBJ-AT PACKAGE1 LOC8-1)
   SAME-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK8-1
      PACKAGE1 LOC8-3 LOC8-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK8-1 LOC8-3)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK8-1
          LOC8-1 LOC8-3)
         0)))
      (1.0
       (!LOAD-TRUCK PACKAGE1
        TRUCK8-1 LOC8-3)
       1)
      ((TRUCK-AT TRUCK8-1 LOC8-1)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK8-1
          LOC8-2 LOC8-1)
         22)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE1
        TRUCK8-1 LOC8-1)
       23)))))
  ((OBJ-AT PACKAGE3 LOC2-3)
   SAME-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK2-1
      PACKAGE3 LOC2-2 LOC2-3)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK2-1 LOC2-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK2-1
          LOC2-1 LOC2-2)
         2)))
      (1.0
       (!LOAD-TRUCK PACKAGE3
        TRUCK2-1 LOC2-2)
       3)
      ((TRUCK-AT TRUCK2-1 LOC2-3)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK2-1
          LOC2-2 LOC2-3)
         25)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE3
        TRUCK2-1 LOC2-3)
       26)))))
  ((OBJ-AT PACKAGE4 LOC6-2)
   SAME-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK6-1
      PACKAGE4 LOC6-3 LOC6-2)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK6-1 LOC6-3)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK6-1
          LOC6-1 LOC6-3)
         4)))
      (1.0
       (!LOAD-TRUCK PACKAGE4
        TRUCK6-1 LOC6-3)
       5)
      ((TRUCK-AT TRUCK6-1 LOC6-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK6-1
          LOC6-3 LOC6-2)
         27)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE4
        TRUCK6-1 LOC6-2)
       28)))))
  ((OBJ-AT PACKAGE6 LOC6-2)
   DIFFERENT-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK2-1
      PACKAGE6 LOC2-3 LOC2-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK2-1 LOC2-3)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK2-1
          LOC2-2 LOC2-3)
         6)))
      (1.0
       (!LOAD-TRUCK PACKAGE6
        TRUCK2-1 LOC2-3)
       7)
      ((TRUCK-AT TRUCK2-1 LOC2-1)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK2-1
          LOC2-3 LOC2-1)
         30)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE6
        TRUCK2-1 LOC2-1)
       31)))
    ((AIR-DELIVER-OBJ PACKAGE6
      LOC2-1 LOC6-1)
     NIL
     ((1.0
       (!FLY-AIRPLANE PLANE3 LOC5-1
        LOC2-1)
       50)
      (1.0
       (!LOAD-AIRPLANE PACKAGE6
        PLANE3 LOC2-1)
       57)
      ((FLY-AIRPLANE PLANE3 LOC6-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE3
          LOC1-1 LOC6-1)
         76)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE6
        PLANE3 LOC6-1)
       85)))
    ((IN-CITY-DELIVERY TRUCK6-1
      PACKAGE6 LOC6-1 LOC6-2)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK6-1 LOC6-1)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK6-1
          LOC6-3 LOC6-1)
         97)))
      (1.0
       (!LOAD-TRUCK PACKAGE6
        TRUCK6-1 LOC6-1)
       98)
      ((TRUCK-AT TRUCK6-1 LOC6-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK6-1
          LOC6-3 LOC6-2)
         107)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE6
        TRUCK6-1 LOC6-2)
       108)))))
  ((OBJ-AT PACKAGE7 LOC6-3)
   DIFFERENT-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK1-1
      PACKAGE7 LOC1-2 LOC1-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK1-1 LOC1-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK1-1
          LOC1-1 LOC1-2)
         8)))
      (1.0
       (!LOAD-TRUCK PACKAGE7
        TRUCK1-1 LOC1-2)
       9)
      ((TRUCK-AT TRUCK1-1 LOC1-1)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK1-1
          LOC1-2 LOC1-1)
         32)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE7
        TRUCK1-1 LOC1-1)
       33)))
    ((AIR-DELIVER-OBJ PACKAGE7
      LOC1-1 LOC6-1)
     NIL
     ((0
       (!ADD-PROTECTION
        (AIRPLANE-AT PLANE1
         LOC1-1))
       51)
      (1.0
       (!LOAD-AIRPLANE PACKAGE7
        PLANE1 LOC1-1)
       58)
      ((FLY-AIRPLANE PLANE1 LOC6-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (AIRPLANE-AT PLANE1
           LOC6-1))
         66)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE7
        PLANE1 LOC6-1)
       73)))
    ((IN-CITY-DELIVERY TRUCK6-1
      PACKAGE7 LOC6-1 LOC6-3)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK6-1 LOC6-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK6-1
           LOC6-1))
         81)))
      (1.0
       (!LOAD-TRUCK PACKAGE7
        TRUCK6-1 LOC6-1)
       82)
      ((TRUCK-AT TRUCK6-1 LOC6-3)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK6-1
          LOC6-1 LOC6-3)
         94)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE7
        TRUCK6-1 LOC6-3)
       95)))))
  ((OBJ-AT PACKAGE8 LOC1-1)
   DIFFERENT-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK6-1
      PACKAGE8 LOC6-3 LOC6-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK6-1 LOC6-3)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK6-1
           LOC6-3))
         10)))
      (1.0
       (!LOAD-TRUCK PACKAGE8
        TRUCK6-1 LOC6-3)
       11)
      ((TRUCK-AT TRUCK6-1 LOC6-1)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK6-1
          LOC6-2 LOC6-1)
         34)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE8
        TRUCK6-1 LOC6-1)
       35)))
    ((AIR-DELIVER-OBJ PACKAGE8
      LOC6-1 LOC1-1)
     NIL
     ((1.0
       (!FLY-AIRPLANE PLANE1 LOC1-1
        LOC6-1)
       59)
      (1.0
       (!LOAD-AIRPLANE PACKAGE8
        PLANE1 LOC6-1)
       67)
      ((FLY-AIRPLANE PLANE1 LOC1-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE1
          LOC2-1 LOC1-1)
         84)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE8
        PLANE1 LOC1-1)
       96)))))
  ((OBJ-AT PACKAGE9 LOC4-2)
   DIFFERENT-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK5-1
      PACKAGE9 LOC5-2 LOC5-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK5-1 LOC5-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK5-1
          LOC5-1 LOC5-2)
         12)))
      (1.0
       (!LOAD-TRUCK PACKAGE9
        TRUCK5-1 LOC5-2)
       13)
      ((TRUCK-AT TRUCK5-1 LOC5-1)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK5-1
          LOC5-2 LOC5-1)
         36)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE9
        TRUCK5-1 LOC5-1)
       37)))
    ((AIR-DELIVER-OBJ PACKAGE9
      LOC5-1 LOC4-1)
     NIL
     ((1.0
       (!FLY-AIRPLANE PLANE3 LOC2-1
        LOC5-1)
       62)
      (1.0
       (!LOAD-AIRPLANE PACKAGE9
        PLANE3 LOC5-1)
       68)
      ((FLY-AIRPLANE PLANE3 LOC4-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE3
          LOC3-1 LOC4-1)
         101)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE9
        PLANE3 LOC4-1)
       113)))
    ((IN-CITY-DELIVERY TRUCK4-1
      PACKAGE9 LOC4-1 LOC4-2)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK4-1 LOC4-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK4-1
           LOC4-1))
         120)))
      (1.0
       (!LOAD-TRUCK PACKAGE9
        TRUCK4-1 LOC4-1)
       121)
      ((TRUCK-AT TRUCK4-1 LOC4-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK4-1
          LOC4-1 LOC4-2)
         122)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE9
        TRUCK4-1 LOC4-2)
       123)))))
  ((OBJ-AT PACKAGE11 LOC3-2)
   DIFFERENT-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK2-1
      PACKAGE11 LOC2-3 LOC2-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK2-1 LOC2-3)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK2-1
           LOC2-3))
         14)))
      (1.0
       (!LOAD-TRUCK PACKAGE11
        TRUCK2-1 LOC2-3)
       15)
      ((TRUCK-AT TRUCK2-1 LOC2-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK2-1
           LOC2-1))
         39)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE11
        TRUCK2-1 LOC2-1)
       40)))
    ((AIR-DELIVER-OBJ PACKAGE11
      LOC2-1 LOC3-1)
     NIL
     ((0
       (!ADD-PROTECTION
        (AIRPLANE-AT PLANE3
         LOC2-1))
       53)
      (1.0
       (!LOAD-AIRPLANE PACKAGE11
        PLANE3 LOC2-1)
       60)
      ((FLY-AIRPLANE PLANE3 LOC3-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE3
          LOC6-1 LOC3-1)
         86)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE11
        PLANE3 LOC3-1)
       99)))
    ((IN-CITY-DELIVERY TRUCK3-1
      PACKAGE11 LOC3-1 LOC3-2)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK3-1 LOC3-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK3-1
           LOC3-1))
         109)))
      (1.0
       (!LOAD-TRUCK PACKAGE11
        TRUCK3-1 LOC3-1)
       110)
      (1.0
       (!UNLOAD-TRUCK PACKAGE11
        TRUCK3-1 LOC3-2)
       115)
      ((TRUCK-AT TRUCK3-1 LOC3-2)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK3-1
           LOC3-2))
         116)))))))
  ((OBJ-AT PACKAGE12 LOC3-3)
   DIFFERENT-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK2-1
      PACKAGE12 LOC2-2 LOC2-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK2-1 LOC2-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK2-1
          LOC2-3 LOC2-2)
         16)))
      (1.0
       (!LOAD-TRUCK PACKAGE12
        TRUCK2-1 LOC2-2)
       17)
      ((TRUCK-AT TRUCK2-1 LOC2-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK2-1
           LOC2-1))
         41)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE12
        TRUCK2-1 LOC2-1)
       42)))
    ((AIR-DELIVER-OBJ PACKAGE12
      LOC2-1 LOC3-1)
     NIL
     ((0
       (!ADD-PROTECTION
        (AIRPLANE-AT PLANE3
         LOC2-1))
       54)
      (1.0
       (!LOAD-AIRPLANE PACKAGE12
        PLANE3 LOC2-1)
       61)
      ((FLY-AIRPLANE PLANE3 LOC3-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (AIRPLANE-AT PLANE3
           LOC3-1))
         87)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE12
        PLANE3 LOC3-1)
       100)))
    ((IN-CITY-DELIVERY TRUCK3-1
      PACKAGE12 LOC3-1 LOC3-3)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK3-1 LOC3-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK3-1
           LOC3-1))
         111)))
      (1.0
       (!LOAD-TRUCK PACKAGE12
        TRUCK3-1 LOC3-1)
       112)
      ((TRUCK-AT TRUCK3-1 LOC3-3)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK3-1
          LOC3-2 LOC3-3)
         118)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE12
        TRUCK3-1 LOC3-3)
       119)))))
  ((OBJ-AT PACKAGE13 LOC3-2)
   DIFFERENT-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK7-1
      PACKAGE13 LOC7-2 LOC7-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK7-1 LOC7-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK7-1
          LOC7-1 LOC7-2)
         18)))
      (1.0
       (!LOAD-TRUCK PACKAGE13
        TRUCK7-1 LOC7-2)
       19)
      ((TRUCK-AT TRUCK7-1 LOC7-1)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK7-1
          LOC7-2 LOC7-1)
         43)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE13
        TRUCK7-1 LOC7-1)
       44)))
    ((AIR-DELIVER-OBJ PACKAGE13
      LOC7-1 LOC3-1)
     NIL
     ((0
       (!ADD-PROTECTION
        (AIRPLANE-AT PLANE2
         LOC7-1))
       55)
      (1.0
       (!LOAD-AIRPLANE PACKAGE13
        PLANE2 LOC7-1)
       63)
      ((FLY-AIRPLANE PLANE2 LOC3-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE2
          LOC6-1 LOC3-1)
         80)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE13
        PLANE2 LOC3-1)
       92)))
    ((IN-CITY-DELIVERY TRUCK3-1
      PACKAGE13 LOC3-1 LOC3-2)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK3-1 LOC3-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK3-1
           LOC3-1))
         104)))
      (1.0
       (!LOAD-TRUCK PACKAGE13
        TRUCK3-1 LOC3-1)
       105)
      ((TRUCK-AT TRUCK3-1 LOC3-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK3-1
          LOC3-1 LOC3-2)
         114)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE13
        TRUCK3-1 LOC3-2)
       117)))))
  ((OBJ-AT PACKAGE15 LOC5-1)
   DIFFERENT-CITY-DELIVER
   (((IN-CITY-DELIVERY TRUCK8-1
      PACKAGE15 LOC8-2 LOC8-1)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK8-1 LOC8-2)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK8-1
          LOC8-3 LOC8-2)
         20)))
      (1.0
       (!LOAD-TRUCK PACKAGE15
        TRUCK8-1 LOC8-2)
       21)
      ((TRUCK-AT TRUCK8-1 LOC8-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK8-1
           LOC8-1))
         46)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE15
        TRUCK8-1 LOC8-1)
       47)))
    ((AIR-DELIVER-OBJ PACKAGE15
      LOC8-1 LOC5-1)
     NIL
     ((1.0
       (!FLY-AIRPLANE PLANE2 LOC7-1
        LOC8-1)
       64)
      (1.0
       (!LOAD-AIRPLANE PACKAGE15
        PLANE2 LOC8-1)
       70)
      ((FLY-AIRPLANE PLANE2 LOC5-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE2
          LOC3-1 LOC5-1)
         93)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE15
        PLANE2 LOC5-1)
       106)))))
  ((OBJ-AT PACKAGE2 LOC2-1)
   DIFFERENT-CITY-DELIVER
   (((AIR-DELIVER-OBJ PACKAGE2
      LOC1-1 LOC2-1)
     NIL
     ((0
       (!ADD-PROTECTION
        (AIRPLANE-AT PLANE1
         LOC1-1))
       24)
      (1.0
       (!LOAD-AIRPLANE PACKAGE2
        PLANE1 LOC1-1)
       48)
      ((FLY-AIRPLANE PLANE1 LOC2-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE1
          LOC6-1 LOC2-1)
         74)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE2
        PLANE1 LOC2-1)
       83)))))
  ((OBJ-AT PACKAGE5 LOC1-1)
   DIFFERENT-CITY-DELIVER
   (((AIR-DELIVER-OBJ PACKAGE5
      LOC5-1 LOC1-1)
     NIL
     ((0
       (!ADD-PROTECTION
        (AIRPLANE-AT PLANE3
         LOC5-1))
       29)
      (1.0
       (!LOAD-AIRPLANE PACKAGE5
        PLANE3 LOC5-1)
       49)
      ((FLY-AIRPLANE PLANE3 LOC1-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE3
          LOC5-1 LOC1-1)
         69)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE5
        PLANE3 LOC1-1)
       75)))))
  ((OBJ-AT PACKAGE10 LOC8-3)
   DIFFERENT-CITY-DELIVER
   (((AIR-DELIVER-OBJ PACKAGE10
      LOC7-1 LOC8-1)
     NIL
     ((1.0
       (!FLY-AIRPLANE PLANE2 LOC4-1
        LOC7-1)
       38)
      (1.0
       (!LOAD-AIRPLANE PACKAGE10
        PLANE2 LOC7-1)
       52)
      ((FLY-AIRPLANE PLANE2 LOC8-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (AIRPLANE-AT PLANE2
           LOC8-1))
         65)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE10
        PLANE2 LOC8-1)
       71)))
    ((IN-CITY-DELIVERY TRUCK8-1
      PACKAGE10 LOC8-1 LOC8-3)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK8-1 LOC8-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK8-1
           LOC8-1))
         77)))
      (1.0
       (!LOAD-TRUCK PACKAGE10
        TRUCK8-1 LOC8-1)
       78)
      ((TRUCK-AT TRUCK8-1 LOC8-3)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK8-1
          LOC8-1 LOC8-3)
         88)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE10
        TRUCK8-1 LOC8-3)
       89)))))
  ((OBJ-AT PACKAGE14 LOC6-3)
   DIFFERENT-CITY-DELIVER
   (((AIR-DELIVER-OBJ PACKAGE14
      LOC7-1 LOC6-1)
     NIL
     ((0
       (!ADD-PROTECTION
        (AIRPLANE-AT PLANE2
         LOC7-1))
       45)
      (1.0
       (!LOAD-AIRPLANE PACKAGE14
        PLANE2 LOC7-1)
       56)
      ((FLY-AIRPLANE PLANE2 LOC6-1)
       NIL
       ((1.0
         (!FLY-AIRPLANE PLANE2
          LOC8-1 LOC6-1)
         72)))
      (1.0
       (!UNLOAD-AIRPLANE PACKAGE14
        PLANE2 LOC6-1)
       79)))
    ((IN-CITY-DELIVERY TRUCK6-1
      PACKAGE14 LOC6-1 LOC6-3)
     TRUCK-ACROSS-TOWN
     (((TRUCK-AT TRUCK6-1 LOC6-1)
       NIL
       ((0
         (!ADD-PROTECTION
          (TRUCK-AT TRUCK6-1
           LOC6-1))
         90)))
      (1.0
       (!LOAD-TRUCK PACKAGE14
        TRUCK6-1 LOC6-1)
       91)
      ((TRUCK-AT TRUCK6-1 LOC6-3)
       NIL
       ((1.0
         (!DRIVE-TRUCK TRUCK6-1
          LOC6-1 LOC6-3)
         102)))
      (1.0
       (!UNLOAD-TRUCK PACKAGE14
        TRUCK6-1 LOC6-3)
       103)))))))
