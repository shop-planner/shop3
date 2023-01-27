(in-package :shop3.common)

;;;---------------------------------------------------------------------------
;;; Elnatan wrote the following:
;;;---------------------------------------------------------------------------
;;; Functions (other than constructors) that are called from shop2.lisp:

;;; state-atoms (from trace-print and print-current-state)
;;; state-all-atoms-for-predicate (from query-current-state)
;;; state-candidate-atoms-for-goal (from do-conjuct)
;;; copy-state (from store-plan!)
;;; tag-state (from apply-operator)
;;; retract-state-changes (from apply-operator and seek-plans-primitive)
;;; add-atom-to-state (from invoke-external-query and apply-operator)
;;; delete-atom-from-state (from apply-operator)

;;; I hope the preceding list is useful for clarifying the interface that states
;;; should present to the outside world.

;;; higher level operations on the state objects:
;;; find-satisfiers (in SHOP2) uses the above to answer queries on states
;;;---------------------------------------------------------------------------
;;; End of quote from Elnatan --- possibly this needs revision
;;;---------------------------------------------------------------------------

;;;---------------------------------------------------------------------------
;;; Additional interface functions
;;;---------------------------------------------------------------------------
;;; add-protection-to-state
;;; make-initial-state

;;;---------------------------------------------------------------------------
;;; Class that is the core of all domain objects
;;;---------------------------------------------------------------------------
(defclass domain-core ()
  ()
  (:documentation "Core class that is the parent of all domain classes, both
theorem-prover and SHOP3."))

;;;---------------------------------------------------------------------------
;;; Generic function declarations
;;;---------------------------------------------------------------------------
;; this is the new initial state creation function which dispatches
;; on the domain and the state encoding
(defgeneric make-initial-state (domain state-encoding atoms &key)
  (:documentation
   "Make a state according to a specified domain type and state encoding.
Always takes a list of atoms, for special domain types, may take
keyword arguments for additional state components."))

;;; The following generic functions should be implemented by every state

;;; Insert the atom into the state. The return value is undefined.
;;; This function destructively modifies its state argument.
(defgeneric insert-atom (atom state)
  (:documentation
   "Insert the atom into the state.  The return value is
   undefined. This function destructively modifies its state
   argument.
   Note that an atom is NOT a lisp atom --- it is actually a list of
   pred . args representing a first order logic positive literal."
   ))

;;; Remove the atom from the state. The return value is undefined.
;;; This function destructively modifies its state argument.
(defgeneric remove-atom (atom state)
  (:documentation
   "Delete the atom from the statebody.  The return value is
   undefined.  This function destructively modifies its state
   argument.
      Note that an atom is NOT a lisp atom --- it is actually a list of
   pred . args representing a first order logic positive literal."))

;;; Returns the atoms of the state as a list.
(defgeneric state-atoms (state)
  (:documentation "Return the atoms of the state in a plain list"))

;;; Returns nil iff the atom is not in the state.
(defgeneric atom-in-state-p (atom state)
  (:documentation "Is the atom in the state?"))

(defgeneric state-all-atoms-for-predicate (state pred)
  (:documentation "Return all atoms in STATE that concern PRED.  Used internally
by the theorem-prover.  Must be implemented when adding a new state structure type."))

(defgeneric state-candidate-atoms-for-goal (state goal)
  (:documentation "Return all atoms in STATE relevant to GOAL.  Used internally
by the theorem-prover.  Must be implemented when adding a new state structure type."))

; Returns a copy of the state.
(defgeneric copy-state (state)
  (:documentation "Return a copy of the state"))

(defgeneric tag-state (state &optional increment)
  (:documentation "Add a tag to a state; used to make tagged-states, which
provide information about how to backtrack over state updates."))

(defgeneric include-in-tag (action atom state)
  (:documentation "Add to the current tag a state update \(characterized by ACTION\)
performed with ATOM \(a literal\) as operand."))

(defgeneric retract-state-changes (state tag)
  (:documentation "Restore STATE to its contents *before* the
changes added by TAG.  Side-effecting function:  will undo
individual changes step-by-step.  Returns nothing of interest."))

;;; this function is designed this way for backward compatibility.
;;; Eventually, instead of doing eql dispatch on the keyword, we
;;; should just make add-type state-updates, delete-type
;;; state-updates, etc. [2008/01/25:rpg]
(defgeneric undo-state-update (state-update-keyword state-update state)
  (:documentation "Undo the state update instantiated in state-update.
Chooses how to do this based on state-update-keyword.  Side-effecting.
Used inside RETRACT-STATE-CHANGES."))

(defgeneric redo-state-update (state-update-keyword state-update state)
  (:documentation "Redo the state update instantiated in state-update.
Chooses how to do this based on state-update-keyword.  Side-effecting.
Used in plan repair."))

(defgeneric replay-state-changes (state update-list &optional stop-at))

(defgeneric add-atom-to-state (atom state depth operator)
  (:documentation "Destructively modifies STATE by adding ATOM
\(a positive literal\) to the state.  DEPTH and OPERATOR are
used only for debugging purposes.  Will update tag information
in state.")
  )

(defgeneric delete-atom-from-state (atom state depth operator)
    (:documentation "Destructively modifies STATE by removing ATOM
\(a positive literal\) from the state.  DEPTH and OPERATOR are
used only for debugging purposes.  Will update tag information
in state.")
  )

;;; this needs a better description of what a trajectory object is...
(defgeneric state-trajectory (state &key sorted)
  (:documentation "Any state in SHOP implicitly defines a trajectory ---
the sequence of states, starting from the initial state, and terminating at
this state.  This function returns a trajectory leading to STATE.")
  )

(defvar *state-tag-map* nil
  "Will be bound to a hash table to look up an operator/action instance
from a tag.")
(defvar *action-to-tag-map* nil
  "Will be bound to a hash table to look up a numerical tag from an operator/action
instance.")


(defgeneric last-establisher (state literal)
  (:documentation "Return the action that last established LITERAL
before STATE."))


(defgeneric state->state-type (state)
  (:documentation "Return the state-type keyword for STATE.")
  (:method (state)
    (error "No state type recorded for ~a." state)))

;;;---------------------------------------------------------------------------
;;; Structure and other type declarations
;;;---------------------------------------------------------------------------

;;; The "state" class

(defstruct (state (:constructor nil) (:copier nil))
  body)

(defstruct (tagged-state (:include state) (:constructor nil) (:copier nil))
  "The `tagged-state` type encapsulates the behaviors needed to support
backtracking and backjumping."
  (tags-info (list (list 0)))
  ;; from this point back, can't undo...
  (block-at 0 :type fixnum))

(deftype action-type () '(member add delete redundant-add redundant-delete))

(defstruct state-update
  (action 'add :type action-type)
  (literal nil :type list))

(defstruct (list-state (:include tagged-state)
                       (:constructor makeliststate)
                       (:copier nil)))

(defstruct (hash-state (:include tagged-state)
                       (:constructor makehashstate)
                       (:copier nil)))

(defstruct (mixed-state (:include tagged-state)
                        (:constructor makemixedstate)
                        (:copier nil))
  "A `:mixed` state is a singly-hashed state. I.e., the body is
represented as a hash table, hashed on the predicate, not on
the entire state atom (as in the `:hash` type state).")

(defstruct (doubly-hashed-state (:include tagged-state)
                        (:constructor makedoubly-hashedstate)
                        (:copier nil))
  "A `:doubly-hashed` state is like singly-hashed (`:mixed`) state. I.e., the body is
represented as a hash table, hashed on the predicate, not on
the entire state atom (as in the `:hash` type state).  *Unlike* a singly-hashed
state, the entry for a predicate will be a sub-hash table for the second argument.")

(defstruct (bit-state (:include tagged-state)
                      (:constructor %make-bit-state)
                      (:copier nil)))
