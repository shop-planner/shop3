;;;---------------------------------------------------------------------------
;;; Contributed by Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; to the SHOP2 project.
;;;
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;; Contains test files to demonstrate and test the function of the
;;; ASSIGN* operator added to the SHOP2 language.
;;;    
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2003/11/24:rpg] Created.
;;;
;;;---------------------------------------------------------------------------
(in-package :shop2-user)
(defdomain basic-example
    (
     (:operator (!pickup ?a) ((on-table ?a)) () ((have ?a)))
     (:operator (!pickup2 ?a) ((choose ?a) (on-table ?a)) () ((have ?a)))
     (:method (pickup ?a) ((on-table ?a)) ((!pickup ?a)))
     (:method (pickup2 ?a) ((on-table ?a)) ((!pickup2 ?a)))
     (:method (pickup3 ?a) ((on-table ?a)) ((!pickup2 ?a) (!check ?a)))
     (:operator (!check ?a) ((have ?a) (different ?a banjo)) () ((checked ?a)))
     (:method (print-current-state) ((eval (print-current-state))) ())

     (:- (choose ?a)
	 (
	  (assign* ?a (list 'banjo 'ukelele 'grass-skirt))
	  )
	 )
     (:- (different ?x ?y) ((not (same ?x ?y))))
     (:- (same ?x ?x) nil)

     ))

(defproblem get-something basic-example
  ((on-table banjo))
  ((!pickup banjo)))

(defproblem shouldnt-work basic-example
  ((on-table banjo))
  ((!pickup  kiwi)))

(defproblem get-something-with-choose basic-example
  ((on-table banjo))
  ((!pickup2 banjo)))

(defproblem get-many-things-with-choose basic-example
  ((on-table banjo) (on-table ukelele))
  ((!pickup2 ?a)))

(defproblem get-many-things-no-choose basic-example
  ((on-table banjo) (on-table ukelele))
  ((!pickup ?a)))

(defproblem get-many-things-no-choose1 basic-example
  ((on-table banjo) (on-table ukelele))
  ((!pickup banjo)))

(defproblem get-many-things-no-choose2 basic-example
  ((on-table banjo) (on-table ukelele))
  ((!pickup ukelele)))

(defproblem get-many-things-no-var basic-example
  ((on-table banjo) (on-table ukelele))
  ((!pickup banjo) (!pickup ukelele)))

(defproblem get-something-method basic-example
  ((on-table banjo))
  ((pickup banjo)))

(defproblem get-many-things-no-choose-method basic-example
  ((on-table banjo) (on-table ukelele))
  ((pickup ?a)))

(defproblem get-many-things-choose-method basic-example
  ((on-table banjo) (on-table ukelele))
  ((pickup2 ?a)))

;;; the banjo binding should be rejected.
;;; [2003/11/24:rpg]
(defproblem need-second basic-example
  ((on-table banjo) (on-table ukelele))
  ((pickup3 ?a)(print-current-state)))


