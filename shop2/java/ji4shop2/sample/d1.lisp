(defdomain ab (
  (:operator (!a1) () () ((foo)))
  (:operator (!a2) () ((bar)) ())
  (:operator (!b1) () () ())
  (:operator (!b2) () () ())

  (:method (main-goal)
    ()
    (:unordered (a) (b)))
 
  (:method (a)
	   ()
	   (:ordered (!a1) (!a2)))
	   
  (:method (b)
	   ((foo))
	   (:ordered (!b1) (!b2)))))
