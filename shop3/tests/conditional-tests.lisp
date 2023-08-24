(defpackage #:shop3-conditionals-tests
  (:use #:cl
        #:fiveam)
  (:import-from :shop #:defdomain #:defproblem #:looping-domain))

(in-package #:shop3-conditionals-tests)

(def-suite conditionals-suite)
(in-suite conditionals-suite)

(defdomain (when-static-nil :type looping-domain :redefine-ok t)
    ((:method (top)
       nil
       (:ordered
        (:when
            (:cond (and nil))
          (:ordered (:task !task-1)))
        (:task !task-2)))
     (:op (!task-1)
      :precond nil
      :add nil
      :delete nil)
     (:op (!task-2)
      :precond nil
      :add nil
      :delete nil)))

(defdomain (when-static-t :type looping-domain :redefine-ok t)
    (
     (:method (top)
       nil
       (:ordered
        (:when
            (:cond (and t))
          (:ordered (:task !task-1)))
        (:task !task-2)))
     (:op (!task-1)
      :precond nil
      :add nil
      :delete nil)
     (:op (!task-2)
      :precond nil
      :add nil
      :delete nil)))

(defproblem when-static-nil when-static-nil
  nil
  (top))

(defproblem when-static-t when-static-t
  nil
  (top))

(defdomain (when-dynamic :type looping-domain :redefine-ok t)
    (
     (:method (top)
       (and (good-value ?value))
       (:ordered
        (:when
            (:cond (and (value-match ?value)))
          (:ordered (:task !task-1)))
        (:task !task-2)))
     (:op (!task-1)
      :precond nil
      :add nil
      :delete nil)
     (:op (!task-2)
      :precond nil
      :add nil
      :delete nil)))

(defproblem when-dynamic-pass when-dynamic
  ((good-value some-value) (value-match some-value))
  (top))

(defproblem when-dynamic-not-pass when-dynamic
  ((good-value some-value) (value-match some-other-value))
  (top))


(defdomain (when-existential :type looping-domain :redefine-ok t)
    (
     (:method (top)
       nil
       (:ordered
        (:when
            (:cond (and (value-match ?value)))
          (:ordered (:task !task-1)))
        (:task !task-2)))
     (:op (!task-1)
      :precond nil
      :add nil
      :delete nil)
     (:op (!task-2)
      :precond nil
      :add nil
      :delete nil)))

(defproblem when-existential-pass when-existential
  ((value-match some-value))
  (top))

(defproblem when-existential-not-pass when-existential
  nil
  (top))


(test when-static
  (is (equalp
       '(((!TASK-1) 1.0 (!TASK-2) 1.0))
       (shop3::find-plans-stack 'when-static-nil
                                :domain 'when-static-nil)))

  (is (equalp
       '(((!TASK-2) 1.0))
       (shop3::find-plans-stack 'when-static-t
                                :domain 'when-static-t))))

(test when-dynamic
  (is (equalp
       '(((!TASK-1) 1.0 (!TASK-2) 1.0))
       (shop3::find-plans-stack 'when-dynamic-pass
                                :domain 'when-dynamic)))

  (is (equalp
       '(((!TASK-2) 1.0))
       (shop3::find-plans-stack 'when-dynamic-not-pass
                                :domain 'when-dynamic))))

(test when-existential
  (is (equalp
       '(((!TASK-1) 1.0 (!TASK-2) 1.0))
       (shop3::find-plans-stack 'when-existential-pass
                                :domain 'when-existential)))

  (is (equalp
       '(((!TASK-2) 1.0))
       (shop3::find-plans-stack 'when-existential-not-pass
                                :domain 'when-existential))))


(defdomain (when-binding-inner :type looping-domain :redefine-ok t)
    ((:method (top)
       nil
       (:ordered
        (:task !task 1)
        (:when
            (:cond (and (bound-value ?value)))
          (:ordered (:task !task ?value)))
        (:task !task 3)))
     (:op (!task ?id)
      :precond nil
      :add nil
      :del nil)))

(defproblem when-binding-inner when-binding-inner
  ((bound-value testing))
  (top))

(test when-binding-inner
  (is (equalp
       '(((!TASK 1) 1.0 (!TASK TESTING) 1.0 (!TASK 3) 1.0))
       (shop3::find-plans-stack 'when-binding-inner
                                :domain 'when-binding-inner))))


(defdomain (when-rebind-inner :type looping-domain :redefine-ok t)
    ((:method (top)
       (and (some ?value))
       (:ordered
        (:task !task 1)
        (:when
            (:cond (and (bound-value ?value)))
          (:ordered (:task !task ?value)))
        (:task !task 3)))
     (:op (!task ?id)
      :precond nil
      :add nil
      :del nil)))

(defproblem when-rebind-inner when-rebind-inner
  ((some value) (bound-value testing))
  (top))

(test when-rebinding-inner
  (is (equalp
       '(((!TASK 1) 1.0 (!TASK 3) 1.0))
       (shop3::find-plans-stack 'when-rebind-inner
                                :domain 'when-rebind-inner))))

(defdomain (unless-rebind-inner :type looping-domain :redefine-ok t)
    ((:method (top)
       (and (some ?value))
       (:ordered
        (:task !task 1)
        (:unless
            (:cond (and (bound-value ?value)))
          (:ordered (:task !task ?value)))
        (:task !task 3)))
     (:op (!task ?id)
      :precond nil
      :add nil
      :del nil)))

(defproblem unless-rebind-inner unless-rebind-inner
  ((some value) (bound-value testing))
  (top))

(test unless-rebinding-inner
  (is (equalp
       '(((!TASK 1) 1.0 (!TASK VALUE) 1.0 (!TASK 3) 1.0))
       (shop3::find-plans-stack 'unless-rebind-inner
                                :domain 'unless-rebind-inner))))


(defdomain (if-rebind-inner :type looping-domain :redefine-ok t)
    ((:method (top)
       (and (some ?value))
       (:ordered
        (:task !task 1)
        (:if
         (:cond (and (bound-value ?value)))
         (:ordered (:task !task ?value))
         (:else
          (:ordered
           (:task !task ?value))))
        (:task !task 3)))
     (:op (!task ?id)
      :precond nil
      :add nil
      :del nil)))

(defproblem if-rebind-inner unless-rebind-inner
  ((some else) (bound-value testing))
  (top))

(test if-rebinding-inner
  (is (equalp
       '(((!TASK 1) 1.0 (!TASK ELSE) 1.0 (!TASK 3) 1.0))
       (shop3::find-plans-stack 'if-rebind-inner
                                :domain 'if-rebind-inner))))

(defdomain (if-bind-inner :type looping-domain :redefine-ok t)
    ((:method (top)
       nil
       (:ordered
        (:task !task 1)
        (:if
         (:cond (and (bound-value ?value)))
         (:ordered (:task !task ?value))
         (:else
          (:ordered
           (:task !task ?value))))
        (:task !task 3)))
     (:op (!task ?id)
      :precond nil
      :add nil
      :del nil)))

(defproblem if-bind-inner unless-bind-inner
  ((some else) (bound-value testing))
  (top))

(test if-binding-inner
  (is (equalp
       '(((!TASK 1) 1.0 (!TASK TESTING) 1.0 (!TASK 3) 1.0))
       (shop3::find-plans-stack 'if-bind-inner
                                :domain 'if-bind-inner))))


(defdomain (loop-cond :type looping-domain :redefine-ok t)
    ((:method (top)
       nil
       ((:loop
          (:cond
            (and (something ?a)
                 ))
          (:ordered
           (:task !task ?a)))))
     (:op (!task ?id)
      :precond nil
      :add nil
      :del nil)))

(defproblem loop-cond loop-cond
  ((something here) (something there))
  (top))

(test loop-cond
  (is (equalp
       '(((!TASK THERE) 1.0 (!TASK HERE) 1.0))
       (shop3::find-plans-stack 'loop-cond
                                :domain 'loop-cond))))
