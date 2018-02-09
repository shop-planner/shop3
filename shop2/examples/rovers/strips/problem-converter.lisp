(defpackage shop2-rovers
  (:use common-lisp shop2)
  (:intern
   #:communicated_image_data
   #:communicated_rock_data
   #:communicated_soil_data

   ;; rewrite for goals
   #:communicate_image_data
   #:communicate_rock_data
   #:communicate_soil_data

   ;; key predicates
   #:rover
   #:waypoint
   #:can_traverse

   ;; top level goal
   #:achieve-goals
   ))

(defpackage rovers-problem-converter
  (:use common-lisp iterate)
  (:import-from #:shop2 #:defproblem)
  (:import-from #:shop2-rovers
                #:communicated_image_data
                #:communicated_rock_data
                #:communicated_soil_data

                ;; rewrite for goals
                #:communicate_image_data
                #:communicate_rock_data
                #:communicate_soil_data

                ;; key predicates
                #:rover
                #:waypoint
                #:can_traverse
                
                ;; top level goal
                #:achieve-goals))

(in-package rovers-problem-converter)

;;;
;;; *** Problem Converter ***
;;;
;;; This is a converter which translates ROVER PDDL problems to ROVER SHOP2 problems.
;;; The main function is problem-converter, which takes two parameters:
;;;
;;;  (1) the name of the input file containing the PDDL problem
;;;  (2) the name of the output file containing the SHOP2 problem
;;;
;;; It mainly adds the the time lines for rovers domain.
;;; One for each rover, one for each camera and one for each storage and one for the lander.
;;;
;;; *** USAGE NOTE ***
;;; It may not be a good idea to have all characters capitalized in the
;;; output file.  If you want to avoid that, you should run this program
;;; in Allegro's mlisp rather than alisp.
;;;
;;;*** update ***
;;; This version support typed PDDL file and will change (object - typename) 
;;; to (typename object)
;;;
;;; This version also change function (= (property ?variable) ?value) 
;;; to (property ?variable ?value)

(defparameter *debug-mode* t)

#+nil
(convert-problems (directory (asdf:system-relative-pathname "shop2" "examples/rovers/strips/p??.pddl"))
                  :output-defaults
                  (make-pathname :directory (pathname-directory (asdf:system-relative-pathname "repair-experiments"
                                                                            "rovers-plan-repair/"))
                                 :type "lisp"))

(defun convert-problems (L &key output-defaults)
  (dolist (fn L)
    (problem-converter fn (make-pathname :name (pathname-name fn) :defaults output-defaults))))

(defun translate-objects (objects-list)
  (let (objects
        previous
        object-type
        facts)
    (dolist (ss objects-list)
      (cond ((and (not (eql ss '-))
                  (eql previous nil))
             (cond ((eql objects nil)
                    (setf objects (list ss)))
                   ((not (eql objects nil))      
                    (nconc objects (list ss)))))
            ((eql ss '-)
             (setf previous t))
            ((and (not (eql ss '-))
                  (eql previous t))
             (setf object-type ss)
             (dolist (s objects)
               (push `(,object-type ,s) facts))
             (setf objects nil)
             (setf previous nil))))
    facts))

(defparameter +goal-keywords+
  '(
    communicate_image_data
    communicate_rock_data
    communicate_soil_data))

(defun translate-goals (expr)
  (if (eq (car expr) 'and)
      (mapcar #'translate-goal (rest expr))
      (list (translate-goal expr))))

(defun translate-goal (goal-list)
  (let ((goal-keyword 
          (ecase (first goal-list)
            (communicated_image_data
             'communicate_image_data)
            (communicated_rock_data
             'communicate_rock_data)
            (communicated_soil_data
             'communicate_soil_data))))
    (cons goal-keyword (rest goal-list))))

;;; from Norvig's PAIP
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))


(defun write-SHOP2-problem (shop2-problem-filename
                            problem-name
                            package
                            domain-name
                            objects-list
                            init-list
                            goal-list)
  (let* ((object-facts
           (translate-objects objects-list))
         #+distance-table(waypoints (mapcar #'second
                                            (find-all 'waypoint  object-facts :key #'first)))
         #+distance-table(rovers (mapcar #'second
                                         (find-all 'rover  object-facts :key #'first)))
         #+distance-table (graph-facts (make-all-pair-shortest-path rovers waypoints init-list))
         (problem 
           `(defproblem ,problem-name ,domain-name
              (,@object-facts
               ,@init-list
               #+distance-table ,@graph-facts
               ,@(translate-goals goal-list))
              (:task achieve-goals))))
    (with-open-file (outfile shop2-problem-filename :direction :output
                                                    :if-exists :supersede)

      (let ((*print-readably* t)
            (*package* (find-package package)))
        (print `(in-package ,package) outfile)
        (print problem outfile)))))

(defun problem-converter (pddl-problem-filename shop2-problem-filename
                          &key (package :shop2-rovers))
  (with-open-file (infile pddl-problem-filename :direction :input)
     #+ignore problem-name
    (let* ((pddl-problem (let ((*package* (find-package package))) (read infile)))
           problem-name
            domain-name
            objects-list
            init-list
            goal-list
            ;; metric-list
           )
      ;;;
      ;;; read the PDDL problem
      ;;;
      (dolist (s pddl-problem)
        (when (and (listp s) (equal (symbol-name (first s)) (symbol-name '#:problem)))
         (setf problem-name (second s)))
        (when (and (listp s) (eql (first s) :domain))
          (setf domain-name (second s)))
        (when (and (listp s) (eql (first s) :objects))
          (setf objects-list (cdr s)))
        (when (and (listp s) (eql (first s) :init))
          (setf init-list (cdr s)))
        (when (and (listp s) (eql (first s) :goal))
          (setf goal-list (second s)))

        #+ignore
        (when (and (listp s) (eql (first s) :metric))
          (setf metric-list (second s)))
      )

      ;;;
      ;;; Override default values
      ;;;
      ;; (setf domain-name  "domain-name")

      ;;;
      ;;; Generate the SHOP2 problem
      ;;;
      (write-SHOP2-problem shop2-problem-filename
                           problem-name
                           package
                           domain-name
                           objects-list
                           init-list
                           goal-list
                           ;; metric-list
                           ))))

(defun make-all-pair-shortest-path (rovers vertices init-list)
  (let ((can_traverse-list (find-all 'can_traverse init-list :key #'first)))

    (flet ((make-rover-table (rover)
             (let (edges)
              (dolist (can_traverse-atom can_traverse-list)
                (when (eql (second can_traverse-atom) rover)
                  (push (list (third can_traverse-atom) (fourth can_traverse-atom) 1
                              (list (third can_traverse-atom) (fourth can_traverse-atom)))
                        edges)))
               (find-all-pair-shortest-path (build-weighted-graph vertices edges)))))
      (alexandria:mappend #'make-rover-table rovers))))

;;; ---------------------------------------------------------------------
;;; Build graph
;;; ---------------------------------------------------------------------

(defun build-weighted-graph (vertice-names edges)
  (let* ((N (length vertice-names))
         (vertice-ht (make-hash-table))
         (dists (make-array (list N N) :initial-element most-positive-fixnum))
         (hooks (make-array (list N N) :initial-element nil))
        )

    ;;; --- set up the hastable of vertice ---
    (let ((i 0))
      (dolist (vn vertice-names)
        (setf (gethash vn vertice-ht) i)
        (setf i (1+ i))))

    ;;; --- initialize the dists and hooks ---
    (do ((i 0 (+ i 1))) ((>= i N))
        (setf (aref dists i i) 0)
        (setf (aref hooks i i) nil)
    )

    (let (v1 v2)
      (dolist (e edges)
        (setf v1 (gethash (first e) vertice-ht))
        (setf v2 (gethash (second e) vertice-ht))
        (setf (aref dists v1 v2) (third e))
        (setf (aref hooks v1 v2) (fourth e))
      )
    )
    (list N vertice-ht dists hooks)
  )
)



;;; ---------------------------------------------------------------------
;;; All pair shortest path
;;; ---------------------------------------------------------------------

(defun find-all-pair-shortest-path (graph)
  (let*  ((N     (first graph))
          (dists (third graph))
          (D     (make-array (list N N) :initial-element nil))
          (links (make-array (list N N) :initial-element nil)))
    (do ((i 0 (+ i 1))) ((>= i N)) 
      (do ((j 0 (+ j 1))) ((>= j N)) 
        (setf (aref D i j) (aref dists i j))
        (if (< (aref dists i j) most-positive-fixnum)
           (setf (aref links i j) j)
        )
      )
    )
    (do ((k 0 (+ k 1))) ((>= k N)) 
      (do ((i 0 (+ i 1))) ((>= i N)) 
        (do ((j 0 (+ j 1))) ((>= j N)) 
          (when (< (+ (aref D i k) (aref D k j)) (aref D i j))
            (setf (aref D i j) (+ (aref D i k) (aref D k j)))
            (setf (aref links i j) (aref links i k))
          )
        )
      )
    )
    (list links D graph)
  )
)

(defun extract-shortest-path-cost (all-paths
                                   from-vertex-name to-vertex-name)
  (let* ((graph       (third all-paths))
         (vertice-ht  (second graph))
         (from-vertex (gethash from-vertex-name vertice-ht))
         (to-vertex   (gethash to-vertex-name vertice-ht))
         (D (second all-paths)))
    (aref D from-vertex to-vertex)
  )
)


(defun extract-shortest-path (all-paths
                              from-vertex-name to-vertex-name)
  (let* ((graph       (third all-paths))
         (vertice-ht  (second graph))
         (from-vertex (gethash from-vertex-name vertice-ht))
         (to-vertex   (gethash to-vertex-name vertice-ht))
         (hooks (fourth graph))
         (links (first all-paths))
         (solution))
    (setf solution (extract-shortest-path-recursive
                    links hooks from-vertex to-vertex))
    (if solution
        solution
        nil
    )
  )
)

  ;;; auxiliary function

(defun extract-shortest-path-recursive (links hooks from-vertex to-vertex)
  (if (equal from-vertex to-vertex)
    nil
    (let ((mid (aref links from-vertex to-vertex))
          left-list
          right-list)
      (if mid
        (if (equal mid to-vertex)
          (list (aref hooks from-vertex to-vertex))
          (progn
            (setf left-list (extract-shortest-path-recursive
                             links hooks from-vertex mid))
            (setf right-list (extract-shortest-path-recursive
                              links hooks mid to-vertex))
            (if (and left-list right-list)
                (append left-list right-list)
                nil
            )
          )
        )
        nil
      )
    )
  )
)

;;; ---------------------------------------------------------------------
;;; Testing
;;; ---------------------------------------------------------------------

; (setf g (build-weighted-graph '(v0 v1 v2 v3 v4)
;  '((v1 v0  3 e1)
;    (v2 v0  8 e2)
;    (v4 v0 -4 e3)
;    (v3 v1  1 e4)
;    (v4 v1  7 e5)
;    (v1 v2  4 e6)
;    (v0 v3  2 e7)
;    (v2 v3 -5 e8)
;    (v3 v4  6 e9))))
; 
; (setf s (find-all-pair-shortest-path g))
; (setf p (extract-shortest-path s 'v2 'v4))
; (setf c (extract-shortest-path-cost s 'v2 'v4))
; 
; (format t "path = ~A~%" p)
; (format t "cost = ~A~%" c)


;;; ---------------------------------------------------------------------
;;; On demand shortest path
;;; ---------------------------------------------------------------------

(defun find-on-demand-shortest-path (graph)
  (let*  ((N     (first graph))
          (costs (make-array (list N N) :initial-element nil))
          (paths (make-array (list N N) :initial-element nil)))
    (do ((i 0 (+ i 1))) ((>= i N))
      (setf (aref costs i i) 0)
      (setf (aref paths i i) nil)
    )
    (list costs paths graph)
  )
)

(defun update-on-demand-shortest-path2
       (N dists hooks bound i j visited) ;; assume i!=j, i j \in visited
  (let* ((min-cost bound)
         (min-path nil)
         d1
         sp
        )
    (when (< (aref dists i j) bound)
      (setf min-cost (aref dists i j))
      (setf min-path (list (aref hooks i j)))
    )

    (do* ((k 0 (1+ k))) ((>= k N))
      (when (not (member k visited))
        (setf d1 (aref dists i k))
        (when (< d1 min-cost)
          (setf sp (update-on-demand-shortest-path2 N dists hooks
                     (- min-cost d1) k j (cons k visited)))
          (when sp
            (setf min-cost (+ d1 (first sp)))
            (setf min-path (cons (aref hooks i k) (second sp)))
          )
        )
      )
    )

    (if (eq min-path nil)
        nil
        (list min-cost min-path))
  )
)


(defun update-on-demand-shortest-path (all-paths i j)
  (let* ((costs (first all-paths))
         (paths (second all-paths))
         (graph (third all-paths))
         (N (first graph))
         (dists (third graph))
         (hooks (fourth graph))
         (sp (update-on-demand-shortest-path2 N dists hooks
               most-positive-fixnum i j (list i j)))
        )
    (if sp
      (progn 
        (setf (aref costs i j) (first sp))
        (setf (aref paths i j) (second sp))
      )
      (error "Internal error in update-on-demand-shortest-path.")
    )
  )
)


(defun extract-on-demand-shortest-path-cost (all-paths from-v to-v)
  (let* ((costs (first all-paths))
         (paths (second all-paths))
         (vertex-ht (second (third all-paths)))
         (i (gethash from-v vertex-ht))
         (j (gethash to-v vertex-ht)))
    (declare (ignorable paths))
    (when (eql (aref costs i j) nil)
      (update-on-demand-shortest-path all-paths i j))
    (aref costs i j)))

(defun extract-on-demand-shortest-path (all-paths from-v to-v)
  (let* ((costs (first all-paths))
         (paths (second all-paths))
         (vertex-ht (second (third all-paths)))
         (i (gethash from-v vertex-ht))
         (j (gethash to-v vertex-ht)))
    (when (eql (aref costs i j) nil)
      (update-on-demand-shortest-path all-paths i j)
    )
    (aref paths i j)
  )
)



;;; ---------------------------------------------------------------------
;;; Testing
;;; ---------------------------------------------------------------------

; (setf g (build-weighted-graph '(v1 v2 v3 v4 v5)
;  '((v1 v2  3 (v1 v2))
;    (v1 v3  8 (v1 v3))
;    (v1 v5 -4 (v1 v5))
;    (v2 v5  7 (v2 v5))
;    (v2 v4  1 (v2 v4))
;    (v3 v2  4 (v3 v2))
;    (v4 v1  2 (v4 v1))
;    (v4 v3 -5 (v4 v5))
;    (v5 v4  6 (v5 v4))
;   ))
; )
; 
; (setf s (find-on-demand-shortest-path g))
; (setf p (extract-on-demand-shortest-path s 'v3 'v5))
; (setf c (extract-on-demand-shortest-path-cost s 'v3 'v5))
; 
; (format t "path = ~A~%" p)
; (format t "cost = ~A~%" c)

;;;; ---------------------------------------------------------------------
;;;; The End
;;;; ---------------------------------------------------------------------


