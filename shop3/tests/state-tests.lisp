(defpackage test-states
  (:use #:common-lisp #:fiveam #:shop3.common #:iterate)
  (:import-from #:alexandria #:set-equal))

(in-package :test-states)

(def-suite* test-shop-states)

(def-fixture setup ()
  ;; this only needs to be done once...
  (shop-user::define-umt-domain)
  (load (asdf:system-relative-pathname "shop3" "examples/UMT2/pfile1.lisp"))
  (let* ((domain (shop:find-domain 'shop-user::um-translog-2))
         (problem (shop:find-problem 'shop-user::umt.pfile1))
         (facts (shop::problem-state problem)))
    (&body)))

(def-fixture state (encoding)
  (let ((state (make-initial-state domain encoding facts)))
    (&body)))

(test make-states
  (with-fixture setup ()
   (iter (for encoding in '(:list :hash :mixed :doubly-hashed :bit))
     (as state-type in '(shop3.common::list-state shop3.common::hash-state shop3.common::mixed-state shop3.common::doubly-hashed-state shop3.common::bit-state))
     (as state = (make-initial-state domain encoding facts))
     (is-true (typep state state-type))
     (let ((stored-facts (state-atoms state)))
       (is (set-equal facts stored-facts :test 'equalp)
           "Facts are not stored and recovered for state type ~a" encoding)))))

(test fill-states
  (with-fixture setup ()
   (iter (for encoding in '(:list :hash :mixed :doubly-hashed :bit))
     (as state = (make-initial-state domain encoding nil))
     (iter (for fact in facts)
       (add-atom-to-state fact state 0 '(fake operator)))
     (let ((stored-facts (state-atoms state)))
       (is (set-equal facts stored-facts :test 'equalp)
           "Facts are not stored and recovered for state type ~a" encoding)))))

(test states-with-singletons
  (with-fixture setup ()
   (iter (for encoding in '(:list :hash :mixed :doubly-hashed :bit))
     (as state-type in '(shop3.common::list-state shop3.common::hash-state shop3.common::mixed-state shop3.common::doubly-hashed-state shop3.common::bit-state))
     (as state = (make-initial-state domain encoding (cons '(singleton) facts)))
     (is-true (typep state state-type))
     (let ((stored-facts (state-atoms state)))
       (is (set-equal (cons '(singleton) facts) stored-facts :test 'equalp)
           "Facts are not stored and recovered for state type ~a" encoding)))))


(test test-empty-states
  (with-fixture setup ()
    (let ((facts (cons '(singleton) facts)))
      (iter (for encoding in '(:list :hash :mixed :doubly-hashed :bit))
        (as state-type in '(shop3.common::list-state shop3.common::hash-state shop3.common::mixed-state shop3.common::doubly-hashed-state shop3.common::bit-state))
        (as state = (make-initial-state domain encoding facts))
        (is-true (typep state state-type))
        (let ((stored-facts (state-atoms state)))
          (is (set-equal facts stored-facts :test 'equalp)
              "Facts are not stored and recovered for state type ~a" encoding))
        (iter (for fact in facts)
          (shop3.common::remove-atom fact state))
        (is (null (state-atoms state))
            "Facts are not correctly removed for state type ~a. After removing all, state is ~s" encoding (state-atoms state))))))

(in-package :shop-user)
(defparameter test-states::volume-load-facts '((VOLUME-LOAD-L LOCATION0 0)
                                               (VOLUME-LOAD-L LOCATION1 0)
                                               (VOLUME-LOAD-L LOCATION2 0)
                                               (VOLUME-LOAD-L LOCATION3 0)
                                               (VOLUME-LOAD-L LOCATION4 23)
                                               (VOLUME-LOAD-L LOCATION5 19)))

(defparameter test-states::volume-load-null-facts
  (remove 0 test-states::volume-load-facts :key 'third))

(defparameter test-states::volume-load-location3-facts
  (list (find 'location3 test-states::volume-load-facts :key 'second)))

(in-package :shop3.common)

(defparameter test-states::state-types
  '(list-state hash-state mixed-state doubly-hashed-state bit-state))


(in-package :test-states)


(test all-atoms-for-predicate
  (with-fixture setup ()
    (loop :for encoding :in '(:list :hash :mixed :doubly-hashed :bit)
      :as state-type :in state-types
          :do
             (with-fixture state (encoding)
               (is-true (typep state state-type))
               (shop3.common::insert-atom '(singleton) state)
               (is (equalp '((singleton)) (state-all-atoms-for-predicate state 'singleton)))
               (is (set-equal volume-load-facts
                              (state-all-atoms-for-predicate state 'shop-user::volume-load-l) :test 'equalp))))))


(test copy-states
  (with-fixture setup ()
   (iter (for encoding in '(:list :hash :mixed :doubly-hashed :bit))
     (as state-type in state-types)
     (as state = (make-initial-state domain encoding facts))
     (let ((new-state 
             (copy-state state)))
       (is-true (typep state state-type))
       (is-true (typep new-state state-type) "State of type ~a is copied to the wrong class: ~a" encoding (class-of new-state))
       (is (set-equal facts (state-atoms state) :test 'equalp)
           "Facts are not stored and recovered for state type ~a" encoding)
       (is (set-equal facts (state-atoms new-state) :test 'equalp)
           "Facts are not recovered after copying state type ~a" encoding)
       (is (set-equal (state-atoms new-state) (state-atoms state) :test 'equalp)
           "States are not identical after copying state type ~a" encoding)))))


(test candidate-atoms-for-goal
  (with-fixture setup ()
    (shop.unifier:set-variable-property domain '(shop-user::volume-load-l ?x ?y))
    (loop :for encoding :in '(:list :hash :mixed :doubly-hashed :bit)
          :as state-type :in state-types
          :do
             (with-fixture state (encoding)
               (is-true (typep state state-type))
               (shop3.common::insert-atom '(singleton) state)
               (is (equalp '((singleton)) (state-candidate-atoms-for-goal state '(singleton))))
               (let ((candidates (state-candidate-atoms-for-goal state '(shop-user::volume-load-l ?x ?y))))
                 (is (set-equal volume-load-facts candidates :test 'equalp)
                     "Unexpected state candidate atoms for goal for ~s encoding:~%~T~S" encoding))
               (let ((candidates (state-candidate-atoms-for-goal state '(shop-user::volume-load-l ?x 0))))
                 (is (set-equal volume-load-facts candidates :test 'equalp)
                     "Unexpected state candidate atoms for ~s for ~s encoding:~%~T~S"
                     '(shop-user::volume-load-l ?x 0)
                     encoding
                     candidates))
               (let* ((goal '(shop-user::volume-load-l shop-user::location3 0))
                      (candidates (state-candidate-atoms-for-goal state goal)))
                 (if (member encoding (list :doubly-hashed :mixed :hash))
                     (is (set-equal volume-load-location3-facts
                                    candidates :test 'equalp)
                         "Unexpected state candidate atoms for ~s for ~s encoding:~%~T~S"
                         goal
                         encoding
                         candidates)
                     (is (set-equal volume-load-facts
                                    candidates :test 'equalp)
                         "Unexpected state candidate atoms for ~s for ~s encoding:~%~T~S"
                         goal
                         encoding
                         candidates)))

                   (let ((candidates (state-candidate-atoms-for-goal state '(shop-user::volume-load-l shop-user::location3 ?y))))
                     (if (eq encoding :doubly-hashed)
                         (is (set-equal volume-load-location3-facts
                                  candidates :test 'equalp)
                             "Getting facts for location 3 gives unexpected results ~s for encoding ~s" candidates encoding)
                         (is (set-equal volume-load-facts candidates :test 'equalp)
                             "Getting facts for location 3 gives unexpected results ~s for encoding ~s" candidates encoding)))))))
