(defproblem os-sequencedstrips-p5_1 
  openstacks-sequencedstrips-ADL
  ((count n0)
   (count n1)
   (count n2)
   (count n3)
   (count n4)
   (count n5)
   (order o1)
   (order o2)
   (order o3)
   (order o4)
   (order o5)
   (product p1)
   (product p2)
   (product p3)
   (product p4)
   (product p5)

   (next-count n0 n1) (next-count n1 n2) (next-count n2 n3) (next-count n3 n4) (next-count n4 n5) 
   (stacks-avail n0)

   (waiting o1)
   (includes o1 p2)

   (waiting o2)
   (includes o2 p1)(includes o2 p2)

   (waiting o3)
   (includes o3 p3)

   (waiting o4)
   (includes o4 p3)(includes o4 p4)

   (waiting o5)
   (includes o5 p5)


   

   (:goal
    (and
     (shipped o1)
     (shipped o2)
     (shipped o3)
     (shipped o4)
     (shipped o5)
     )))

  (do-orders)

  )