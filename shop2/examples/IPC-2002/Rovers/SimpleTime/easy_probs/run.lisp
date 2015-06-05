(format t "*** Assume SHOP2 has been loaded ***~%~%")

(load "../../../solution-converter")
(load "../../mm-graph")
(load "../../shortest-path")
(load "../STRover")


(defun solve-domain (pname)
  (load (concatenate 'string pname ".lisp"))
  (multiple-value-bind (sol soltime) (find-plans (read-from-string pname))
    (if sol
      (progn
        (format t "~%*** Plan found in ~A seconds ***~%" soltime)
        (solution-converter sol soltime "../STRover.pddl"
                            (concatenate 'string pname ".soln") t)
      )
      (format t "~%*** No Plan ***~%"))
  )
)

(solve-domain "pfile1")
(solve-domain "pfile2")
(solve-domain "pfile3")
(solve-domain "pfile4")
(solve-domain "pfile5")
(solve-domain "pfile6")
(solve-domain "pfile7")
(solve-domain "pfile8")
(solve-domain "pfile9")
(solve-domain "pfile10")
(solve-domain "pfile11")
(solve-domain "pfile12")
(solve-domain "pfile13")
(solve-domain "pfile14")
(solve-domain "pfile15")
(solve-domain "pfile16")
(solve-domain "pfile17")
(solve-domain "pfile18")
(solve-domain "pfile19")
(solve-domain "pfile20")

