(in-package :shop-user)

(defdomain test-immediate
    (
     (:method (t1)
	      ()
	      (:task :immediate !!op1))

     (:method (t2)
	      ()
	      (:task :immediate !!op2))

     (:operator (!!op1)
		((op2-fluent))
		()
		())

     (:operator (!!op2)
		()
		()
		((op2-fluent)))))

(make-problem 'test-immediate
	      '()
	      '(:unordered
		(t1)
		(t2)))

(make-problem 'test-immediate-2
	      '()
	      '(:unordered
		(t2)
		(t1)))


		
