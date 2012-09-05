;;;  Logistic domain, implemented by Jason Yue Cao
;;;                   re-written to the next POMFOX syntax by swm
;;;
;;;
;;;
;;;  BASIC ATOMS needed to make the problem
;;;
;;;  (obj-at ?obj ?loc)
;;;  (truck-at ?truck ?loc)
;;;  (airplane-at ?airplane ?airport)
;;;
;;;  (in-truck ?obj ?truck)
;;;  (in-airplane ?obj ?truck)
;;;  (in-city ?loc ?city)
;;;
;;;  (city ?city)
;;;  (airport ?loc)
;;;
;;;  (obj ?obj)    :optional
;;;  (location ?loc)  :optional
;;;
;;;  (truck ?truck ?city)
;;;  (airplane ?airplane)
;;;
;;;  note:  I never use (obj ?obj) (location ?loc)
;;;         in my planning, if putted in, it will give you all the
;;;         answers, but it waste some time on those, so it better
;;;         not to include them.
;;;
;;;  Basic operators
;;;
;;;  (:operator (!load-truck ?obj ?truck ?loc))
;;;  (:operator (!unload-truck ?obj ?truck ?loc))
;;;  (:operator (!load-airplane ?obj ?airplane ?loc))
;;;  (:operator (!unload-airplane ?obj ?airplane ?loc))
;;;  (:operator (!drive-truck ?truck ?loc-from ?loc-to))
;;;  (:operator (!fly-airplane ?airplane ?loc-from ?loc-to))
;;;

;;;-------------problem set 2----------------------------------------
(in-package :shop2-user)

(defun logistics-domain ()

  (defdomain (logistics :redefine-ok t)
   (
    ;; basic operators

    (:operator (!load-truck ?obj ?truck ?loc)
               ((obj-at ?obj ?loc) (:protection (truck-at ?truck ?loc)))
               ((in-truck ?obj ?truck)))

    (:operator (!unload-truck ?obj ?truck ?loc)
               ((in-truck ?obj ?truck)
                (:protection (truck-at ?truck ?loc)))
               ((obj-at ?obj ?loc)))

    (:operator (!load-airplane ?obj ?airplane ?loc)
               ((obj-at ?obj ?loc)
                (:protection (airplane-at ?airplane ?loc)))
               ((in-airplane ?obj ?airplane)))

    (:operator (!unload-airplane ?obj ?airplane ?loc)
               ((in-airplane ?obj ?airplane)
                (:protection (airplane-at ?airplane ?loc)))
               ((obj-at ?obj ?loc)))

    (:operator (!drive-truck ?truck ?loc-from ?loc-to)
               ((truck-at ?truck ?loc-from))
               ((truck-at ?truck ?loc-to)
                (:protection (truck-at ?truck ?loc-to))))

    (:operator (!fly-airplane ?airplane ?airport-from ?airport-to)
               ((airplane-at ?airplane ?airport-from))
               ((airplane-at ?airplane ?airport-to)
                (:protection (airplane-at ?airplane ?airport-to))))


    ;; book-keeping methods & ops, to keep track of what needs to be done
    ;; !add-protection and !delete-protection are two special operators
    ;; that deal with the protection list instead of current state.
    (:operator (!add-protection ?g)
               ()
               ((:protection ?g))
               0)

    (:operator (!delete-protection ?g)
               ((:protection ?g))
               ()
               0)

    ;;;------------------------------------------------------------

    ;; actual AI planning methods

    ;; same city deliver
    (:method (obj-at ?obj ?loc-goal)
             same-city-deliver
             ((in-city ?loc-goal ?city-goal)
              (obj-at ?obj ?loc-now)
              (in-city ?loc-now ?city-goal)
              (truck ?truck ?city-goal)
              ;;(truck-at ?truck ?loc-truck)
              )
            ((:task in-city-delivery ?truck ?obj ?loc-now ?loc-goal))

            different-city-deliver
            ((in-city ?loc-goal ?city-goal)
             (obj-at ?obj ?loc-now)
             (in-city ?loc-now ?city-now)
             (different ?city-goal ?city-now)
             (truck ?truck-now ?city-now)
             (truck ?truck-goal ?city-goal)
             (airport ?airport-now) (in-city ?airport-now ?city-now)
             (airport ?airport-goal) (in-city ?airport-goal ?city-goal))
            (:ordered (:task in-city-delivery ?truck-now ?obj 
?loc-now ?airport-now)
                      (:task air-deliver-obj ?obj ?airport-now ?airport-goal)
                      (:task in-city-delivery ?truck-goal ?obj 
?airport-goal ?loc-goal)))

    ;;;-------------------------------------------------
    (:method (in-city-delivery ?truck ?obj ?loc-from ?loc-to)

             package-already-there
             ((same ?loc-from ?loc-to))
             ()

             truck-across-town
             ((in-city ?loc-from ?city)
              (truck ?truck ?city))
             (:ordered (:task truck-at ?truck ?loc-from)
                       (:task :immediate !load-truck ?obj ?truck ?loc-from)
                       (:task truck-at ?truck ?loc-to)
                       (:task :immediate !unload-truck ?obj ?truck ?loc-to)))


    ;;;-------------------------------------------------

    (:method (truck-at ?truck ?loc-to)

             truck-not-in-right-location
             ((truck-at ?truck ?loc-from)
              (different ?loc-from ?loc-to))
             ((:task :immediate !drive-truck ?truck ?loc-from ?loc-to))

             truck-in-right-location
             ((truck-at ?truck ?loc-from)
              (same ?loc-from ?loc-to))
             ((:task :immediate !add-protection (truck-at ?truck ?loc-to))))

    ;;; (:method head [name] conjunct tail [[name] conjunct tail])

    ;;;-------------------------------------------------
    (:method (air-deliver-obj ?obj ?airport-from ?airport-to)
             airplane-at-the-current-airport
             ((airplane-at ?airplane ?airport-from))
             (:ordered (:task :immediate !add-protection (airplane-at 
?airplane ?airport-from))
                       (:task !load-airplane ?obj ?airplane ?airport-from)
                       (:task fly-airplane ?airplane ?airport-to)
                       (:task !unload-airplane ?obj ?airplane ?airport-to))

             ;; no airplane at the current airport
             fly-airplane-from-any-other-airport
             ((airplane-at ?airplane ?any-airport))
             (:ordered (:task :immediate !fly-airplane ?airplane 
?any-airport ?airport-from)
                       (:task !load-airplane ?obj ?airplane ?airport-from)
                       (:task fly-airplane ?airplane ?airport-to)
                       (:task !unload-airplane ?obj ?airplane ?airport-to)))

    (:method (fly-airplane ?airplane ?airport-to)
             airplane-already-there
             ((airplane-at ?airplane ?airport-to))
             ((:task :immediate !add-protection (airplane-at ?airplane 
?airport-to)))

             fly-airplane-in
             ((airplane-at ?airplane ?airport-from))
             ((:task :immediate !fly-airplane ?airplane ?airport-from 
?airport-to)))


    ;;;-------------------------------------------------

    ;; state axioms

    (:- (same ?x ?x) nil)
    (:- (different ?x ?y) ((not (same ?x ?y))))


    )))

(eval-when (:load-toplevel)
  (logistics-domain))



;;;--------------------------------------------------------------











