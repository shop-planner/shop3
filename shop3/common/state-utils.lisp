;;;
;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;; 
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;; 
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations under
;;; the License.
;;; 
;;; The Original Code is SHOP2.  
;;; 
;;; The Initial Developer of the Original Code is the University of
;;; Maryland. Portions created by the Initial Developer are Copyright (C)
;;; 2002,2003 the Initial Developer. All Rights Reserved.
;;;
;;; Additional developments made by Robert P. Goldman, John Maraist.
;;; Portions created by Drs. Goldman and Maraist are Copyright (C)
;;; 2004-2007 SIFT, LLC.  These additions and modifications are also
;;; available under the MPL/GPL/LGPL licensing terms.
;;; 
;;; 
;;; Alternatively, the contents of this file may be used under the terms of
;;; either of the GNU General Public License Version 2 or later (the "GPL"),
;;; or the GNU Lesser General Public License Version 2.1 or later (the
;;; "LGPL"), in which case the provisions of the GPL or the LGPL are
;;; applicable instead of those above. If you wish to allow use of your
;;; version of this file only under the terms of either the GPL or the LGPL,
;;; and not to allow others to use your version of this file under the terms
;;; of the MPL, indicate your decision by deleting the provisions above and
;;; replace them with the notice and other provisions required by the GPL or
;;; the LGPL. If you do not delete the provisions above, a recipient may use
;;; your version of this file under the terms of any one of the MPL, the GPL
;;; or the LGPL.
;;; ----------------------------------------------------------------------

;;; Smart Information Flow Technologies Copyright 2006-2007 Unpublished work
;;; 
;;; GOVERNMENT PURPOSE RIGHTS
;;; 
;;; Contract No.         FA8650-06-C-7606, 
;;; Contractor Name      Smart Information Flow Technologies, LLC
;;;                      d/b/a SIFT, LLC
;;; Contractor Address   211 N 1st Street, Suite 300
;;;                      Minneapolis, MN 55401
;;; Expiration Date      5/2/2011
;;; 
;;; The Government's rights to use, modify, reproduce, release,
;;; perform, display, or disclose this software are restricted by
;;; paragraph (b)(2) of the Rights in Noncommercial Computer Software
;;; and Noncommercial Computer Software Documentation clause contained
;;; in the above identified contract. No restrictions apply after the
;;; expiration date shown above. Any reproduction of the software or
;;; portions thereof marked with this legend must also reproduce the
;;; markings.
(in-package :shop2.common)




;;;

;;; The "state" class

(defstruct (state (:constructor nil) (:copier nil))
  body)

;; here for backward compatibility  -- don't use this  
(defun make-state (atoms &optional (state-encoding *state-encoding*))
  (warn "MAKE-STATE is deprecated and will be removed; you should be ~
using MAKE-INITIAL-STATE.")
  (ecase state-encoding
    (:list (make-list-state atoms))
    (:mixed (make-mixed-state atoms))
    (:hash (make-hash-state atoms))
    (:bit (make-bit-state atoms))))

;;; very large states can be difficult and time-consuming to print.
(defmethod print-object ((s state) str)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (s str :type t :identity t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The "tagged-state" class

;;; Tags-info is a list of tag-info entries.  Each tag-info is a list whose
;;; first element is a tag (represented by an integer) and whose remaining
;;; elements are a list of changes made to the state while that tag was active.
;;; The command tag-state activates a new tag and returns it.  The command
;;; retract-state-changes retracts all changes which were made while the given
;;; tag was active.  It is expected that retractions will typically involve the
;;; most recently added tag, but the system does allow older tags to be
;;; retracted instead.

(defstruct (tagged-state (:include state) (:constructor nil) (:copier nil))
  (tags-info (list (list 0)))
  ;; from this point back, can't undo...
  (block-at 0 :type fixnum))

(defun tagged-state-tags-info-tag (tagged-state)
  (caar (tagged-state-tags-info tagged-state)))

(deftype action-type () '(member add delete redundant-add redundant-delete))

(defstruct state-update
  (action 'add :type action-type)
  (literal nil :type list))

(defmethod tag-state ((st tagged-state) &optional (increment 2))
  ;; bumped this by TWO instead of one to permit special increments for plan repair.
  (let ((new-tag (+ increment (tagged-state-tags-info-tag st))))
    (push (list new-tag) (tagged-state-tags-info st))
    new-tag))

(defmethod last-establisher ((st tagged-state) literal)
  (let* ((negative (eq (first literal) 'not))
         (literal (if negative (second literal) literal)))
    (iter outer (for (tag . updates) in (tagged-state-tags-info st))
      (iter (for update in updates)
        (when (equalp (state-update-literal update) literal)
          (when (or (and negative (member (state-update-action update)
                                      '(delete redundant-delete) :test 'eq))
                    (member (state-update-action update) '(add redundant-add) :test 'eq))
            (if (zerop tag)
                ;; this case seems never to happen -- the zero tag all
                ;; seems empty.
                (return-from last-establisher :init)
                (return-from last-establisher (decode-tag tag)))))))
    ;; atom is established/deleted in the initial state...
    :init))

(defun decode-tag (tag)
  (let ((hash-val (gethash tag *state-tag-map*)))
       (if hash-val
           (values-list hash-val)            ;task
           (error "No action/operator instance stored for state update tag ~A" tag))))

(defun tag-for-action (action)
  (or (gethash action *action-to-tag-map*)
      (error "No tag instance stored for action ~A" action)))

(defun prepare-state-tag-decoder ()
  (setf *state-tag-map* (make-hash-table :test 'eq))
  (setf *action-to-tag-map* (make-hash-table :test 'eq)))

(defun delete-state-tag-decoder ()
  (setf *state-tag-map* nil
        *action-to-tag-map* nil))

(defun make-tag-map (tag task primitive)
  "Record association of TAG with operator/action instance OPERATOR."
  (setf (gethash tag *state-tag-map*) (list task primitive)
        (gethash primitive *action-to-tag-map*) tag))

(defun delete-tag-map (tag)
  "Erase association of TAG with its operator/action instance."
  (let ((prim (nth-value 1 (decode-tag tag))))
    (assert prim)
    (remhash tag *state-tag-map*)
    (remhash prim *action-to-tag-map*)
    (values)))



(defmethod include-in-tag (action atom (st tagged-state))
  (unless (typep action 'action-type)
    (error "Unacceptable action ~S" action))
  (push (make-state-update :action action :literal atom)
        (rest (first (tagged-state-tags-info st)))))

(defmethod retract-state-changes ((st tagged-state) tag)
  (multiple-value-bind (new-tags-info changes)
      (pull-tag-info (tagged-state-tags-info st) tag (tagged-state-block-at st))
    (setf (tagged-state-tags-info st) new-tags-info)
    (dolist (change changes)
      (undo-state-update (state-update-action change) change st)))
  (values))

(defmethod replay-state-changes ((st tagged-state) tags-info-list &optional stop-at)
  (catch 'stop-replay
    (dolist (tagged-updates tags-info-list)
      (destructuring-bind (tag . updates) tagged-updates
        (assert (> tag (tagged-state-tags-info-tag st)))
        (when (and stop-at (= stop-at tag))
          (throw 'stop-replay nil))
        (dolist (update updates)
          (redo-state-update (state-update-action update) update st)))
      (push tagged-updates (tagged-state-tags-info st))))
  (values))

(defmethod undo-state-update ((keyword (eql 'add)) change state)
  (remove-atom (state-update-literal change) state))

(defmethod undo-state-update ((keyword (eql 'delete)) change state)
  ;; FIXME: delete this!!!!
  (assert (not (member (state-update-literal change) (state-atoms state) :test 'equalp)))
  (insert-atom (state-update-literal change) state))

(defmethod undo-state-update ((keyword (eql 'redundant-add)) change state)
  (declare (ignorable keyword change state))
  (values))

(defmethod undo-state-update ((keyword (eql 'redundant-delete)) change state)
  (declare (ignorable keyword change state))
  (values))


;;; We REDO state updates when we are repairing a plan.  We have
;;; introduced a divergence into the plan, so at some point a
;;; precondition will fail, but we will stop redoing before we get to
;;; this point.
(defmethod redo-state-update ((keyword (eql 'add)) change state)
  (unless (atom-in-state-p (state-update-literal change) state)
    (insert-atom (state-update-literal change) state)))

(defmethod redo-state-update ((keyword (eql 'delete)) change state)
  (when (atom-in-state-p (state-update-literal change) state)
    (remove-atom (state-update-literal change) state)))

;;; The distinction between redundant modifications and stock
;;; modifications only matters when unwinding effects, because we need
;;; to avoid repeated undos.  But this doesn't matter when redoing,
;;; because redoing, unlike undoing is idempotent
(defmethod redo-state-update ((keyword (eql 'redundant-add)) change state)
  (redo-state-update 'add change state)
  (values))

(defmethod redo-state-update ((keyword (eql 'redundant-delete)) change state)
  (redo-state-update 'delete change state)
  (values))



(defmethod add-atom-to-state (atom (st tagged-state) depth operator)
;;;  (let ((shop2::state st))
;;;    ;; the above binding makes the trace-print work properly --- it references state [2006/12/06:rpg]
  (trace-print :effects (car atom) st
               "~2%Depth ~s, adding atom to current state~%      atom ~s~%  operator ~s"
               depth atom operator)
;;;  )
  (let ((in-state-p (atom-in-state-p atom st)))
    (cond ((and in-state-p *state-tag-map*)
           (include-in-tag 'redundant-add atom st))
          ((not in-state-p)
           (include-in-tag 'add atom st)
           (insert-atom atom st)))))

(defmethod delete-atom-from-state (atom (st tagged-state) depth operator)
;;;  (let ((shop2::state st))
;;;    ;; the above binding makes the trace-print work properly --- it references state [2006/12/06:rpg]
    (trace-print :effects (car atom) st
                 "~2%Depth ~s, deleting atom from current state~%      atom ~s~%  operator ~s"
                 depth atom operator)
;;;    )
  (cond ((atom-in-state-p atom st)
         (include-in-tag 'delete atom st)
         (remove-atom atom st))
        (*state-tag-map*
         (include-in-tag 'redundant-delete atom st))))

;;; TAGS-INFO is the tags-info list of a tagged-state, TAG is the
;;; state to roll back to, and STOP-AT can be used to record how much
;;; of the plan has been executed, because we can't roll back past
;;; this point.
(defun pull-tag-info (tags-info tag &optional (stop-at 0))
  (iter (for (first-info . rest-info) on tags-info)
    (as this-tag = (first first-info))
    (when (null first-info)
      (error "Attempt to retract to nonexistent tag ~d" tag))
    (until (or (< this-tag tag)
               (= this-tag stop-at)))
    (appending (rest first-info) into undone)
    (finally (return-from pull-tag-info
               (values (cons first-info rest-info) undone)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The "list-state" class

(defstruct (list-state (:include tagged-state)
                       (:constructor makeliststate)
                       (:copier nil)))

(defmethod state->state-type ((state list-state))
  :list)




(defmethod make-initial-state (domain (state-encoding (eql :list))  atoms &key)
  (declare (ignore domain))
  (make-list-state atoms)
  )


(defun make-list-state (atoms)
  (let ((st (makeliststate)))
    (setf (state-body st) nil)
    (dolist (atom atoms) (insert-atom atom st))
    st))


(defmethod insert-atom (atom (st list-state))
  (setf (state-body st) (LIST-insert-atom-into-statebody atom (state-body st))))

(defmethod remove-atom (atom (st list-state))
  (setf (state-body st) (LIST-remove-atom-from-statebody atom (state-body st))))

(defmethod state-atoms ((st list-state))
  (mapcan #'(lambda (entry) (copy-list (cdr entry))) (state-body st)))

(defmethod atom-in-state-p (atom (st list-state))
  (member atom (rest (assoc (first atom) (state-body st))) :test #'equal))

(defmethod state-all-atoms-for-predicate ((st list-state) pred)
  (rest (assoc pred (state-body st))))

(defmethod state-candidate-atoms-for-goal ((st list-state) goal)
  
  (state-all-atoms-for-predicate st (first goal)))

(defmethod copy-state ((st list-state))
  (let ((the-copy (make-list-state nil)))
    (setf (state-body the-copy) (copy-tree (state-body st)))
    (setf (tagged-state-tags-info the-copy)
          (copy-tree (tagged-state-tags-info st)))
    (setf (tagged-state-block-at the-copy)
     (tagged-state-block-at st))
    the-copy))

;;; Unlike for MIXED, HASH, and BIT encodings, LIST-insert-atom-into-statebody and
;;; LIST-remove-atom-from-statebody are recursive, requiring their arguments to be
;;; statebodies and not states. So until we redo the way these functions work,
;;; they have to stay.

;;; I think this code is going to be pretty inefficient, since it's not properly tail-recursive.  I don't think it would be terribly difficult to replace this with a properly tail-recursive program.  Alternatively, a simple destructive update using (setf (getf statebody (car atom)) ....) might work, but I don't know whether a destructive version of this operation would be acceptable. [2008-02-06: rpg
(defun LIST-insert-atom-into-statebody (atom statebody)
  ;; the statebody here is evidently implemented as an associative structure, indexed on the predicate, of cells whose cdr is a LIST of atoms
  (cond 
   ((null statebody)
    (list (list (car atom) atom)))
   ((string< (car atom) (caar statebody))
    (cons (list (car atom) atom) statebody))
   ((eq (car atom) (caar statebody))
    (cons
     (cons (caar statebody)
           (if (member atom (cdar statebody) :test #'equal)
               (cdar statebody)
             (cons atom (cdar statebody))))
     (cdr statebody)))
   (t (cons (car statebody)
            (LIST-insert-atom-into-statebody atom (cdr statebody))))))

(defun LIST-remove-atom-from-statebody (atom statebody)
  (cond ((null statebody) nil)
        ((string< (car atom) (caar statebody)) statebody)
        ((eq (car atom) (caar statebody))
         (let ((newval (remove atom (cdar statebody) :test #'equal)))
           (if newval
               (cons (cons (car atom) newval) (cdr statebody))
               ;; if there are no remaining propositions for this
               ;; predicate, we just drop the entry
               ;; altogether. [2006/08/02:rpg]
               (cdr statebody))))
        (t (cons (car statebody)
                 (LIST-remove-atom-from-statebody atom (cdr statebody))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The "hash-state" class

(defstruct (hash-state (:include tagged-state)
                       (:constructor makehashstate)
                       (:copier nil)))

(defmethod make-initial-state (domain (state-encoding (eql :hash)) atoms &key)
  (declare (ignore domain))
  (make-hash-state atoms))

(defmethod state->state-type ((state hash-state))
  :hash)


(defun make-hash-state (atoms)
  (let ((st (makehashstate)))
    (setf (state-body st) (make-hash-table :test #'equal))
    (dolist (atom atoms) (insert-atom atom st))
    st)
)

(defmethod insert-atom (atom (st hash-state))
  (setf (gethash atom (state-body st)) t))

(defmethod remove-atom (atom (st hash-state))
  (remhash atom (state-body st)))

(defmethod state-atoms ((st hash-state))
  (let ((statebody (state-body st))
        (acc nil))
    (maphash #'(lambda (key val)
                 (declare (ignore val)) (setf acc (cons key acc)))
             statebody)
    acc))

(defmethod atom-in-state-p (atom (st hash-state))
  (gethash atom (state-body st)))

(defmethod state-all-atoms-for-predicate ((st hash-state) pred)
 (remove-if-not
   #'(lambda (atom)
       (eq (first atom) pred))
   (state-atoms st)))

(defmethod state-candidate-atoms-for-goal ((st hash-state) goal)
  (cond
   ((find-if-not #'(lambda (term) (and (atom term) (not (variablep term))))
                 (rest goal))
    (state-all-atoms-for-predicate st (first goal)))
   ((atom-in-state-p goal st)
    (list goal))
   (t nil)))

(defmethod copy-state ((st hash-state))
  (let ((the-copy (make-hash-state nil)))
    (setf (state-body the-copy) (copy-hash-table (state-body st)))
    (setf (tagged-state-tags-info the-copy)
          (copy-tree (tagged-state-tags-info st)))
        (setf (tagged-state-block-at the-copy)
     (tagged-state-block-at st))
    the-copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The "mixed-state" class

(defstruct (mixed-state (:include tagged-state)
                        (:constructor makemixedstate)
                        (:copier nil))
  "A `:mixed` state is a singly-hashed state. I.e., the body is
represented as a hash table, hashed on the predicate, not on
the entire state atom (as in the `:hash` type state).")

(defmethod make-initial-state (domain (state-encoding (eql :mixed)) atoms &key)
  (declare (ignore domain))
   (make-mixed-state atoms))

(defmethod state->state-type ((state mixed-state))
  :mixed)

(defun make-mixed-state (atoms)
 (let ((st (makemixedstate)))
    (setf (state-body st) (make-hash-table :test #'eq))
    (dolist (atom atoms) (insert-atom atom st))
    st))

(defmethod insert-atom (atom (st mixed-state))
  (pushnew (rest atom) (gethash (first atom) (state-body st)) :test 'equalp))

(defmethod remove-atom (atom (st mixed-state))
  (let ((statebody (state-body st)))
    (setf
     (gethash (first atom) statebody)
     (delete
      (rest atom)
      (gethash (first atom) statebody)
      :test #'equal))))

(defmethod state-atoms ((st mixed-state))
  (let ((statebody (state-body st)))
    (let ((acc nil)) 
      (maphash #'(lambda (pred lis)
                   (setf acc
                         (append (mapcar #'(lambda (entry) (cons pred entry)) lis)
                                 acc)))
               statebody)
      acc)))

(defmethod atom-in-state-p (atom (st mixed-state))
  (member (rest atom) (gethash (first atom) (state-body st)) :test #'equal))

(defmethod state-all-atoms-for-predicate ((st mixed-state) pred)
  (let ((lis (gethash pred (state-body st))))
    (mapcar #'(lambda (entry) (cons pred entry)) lis)))

(defmethod state-candidate-atoms-for-goal ((st mixed-state) goal)
  ;(format t "state-body: ~A~%~%"  (state-atoms st))
  (cond
   ((find-if-not #'(lambda (term)
                     (and (atom term) (not (variablep term))))
                 (rest goal))
    (state-all-atoms-for-predicate st (first goal)))
   ((atom-in-state-p goal st) (list goal))
   (t nil)))

(defmethod copy-state ((st mixed-state))
  (let ((the-copy (make-mixed-state nil)))
    (setf (state-body the-copy) (copy-hash-table (state-body st)))
    (setf (tagged-state-tags-info the-copy)
          (copy-tree (tagged-state-tags-info st)))
    (setf (tagged-state-block-at the-copy)
     (tagged-state-block-at st))
    the-copy))

; If we don't trust that copy-hash-table copies a mixed-state correctly, we can
; replace the preceding function with:
; (defmethod copy-state ((st mixed-state))
;   (let ((the-copy (make-mixed-state (state-atoms st))))
;     (setf (tagged-state-tags-info the-copy)
;           (copy-tree (tagged-state-tags-info st)))
;     the-copy))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The "doubly-hashed-state" class
(defconstant +VARIABLE-TERM+ (uiop:find-symbol* '#:%variable% (find-package (symbol-name '%shop3-common-private%))))
(defconstant +SINGLETON-TERM+ (uiop:find-symbol* '#:%singleton% (find-package (symbol-name '%shop3-common-private%))))
(defstruct (doubly-hashed-state (:include tagged-state)
                        (:constructor makedoubly-hashedstate)
                        (:copier nil))
  "A `:doubly-hashed` state is like singly-hashed (`:mixed`) state. I.e., the body is
represented as a hash table, hashed on the predicate, not on
the entire state atom (as in the `:hash` type state).  *Unlike* a singly-hashed
state, the entry for a predicate will be a sub-hash table for the second argument.")

(defmethod make-initial-state (domain (state-encoding (eql :doubly-hashed)) atoms &key)
  (declare (ignore domain))
   (make-doubly-hashed-state atoms))

(defmethod state->state-type ((state doubly-hashed-state))
  :doubly-hashed)

(defun make-doubly-hashed-state (atoms)
 (let ((st (makedoubly-hashedstate)))
    (setf (state-body st) (make-hash-table :test #'eq))
    (dolist (atom atoms) (insert-atom atom st))
    st))

(defmethod insert-atom (atom (st doubly-hashed-state))
  (let ((subtable (gethash (first atom) (state-body st))))
    (unless subtable
       ;; because of the possibility of numbers, strings, etc.
      (setf subtable (make-hash-table :test #'equal))
      (setf (gethash (first atom) (state-body st)) subtable))
    (if (= (length atom) 1)
        (pushnew t (gethash +SINGLETON-TERM+ subtable))
        (pushnew (rest atom) (gethash (second atom) subtable) :test 'equal))))

(defmethod remove-atom (atom (st doubly-hashed-state))
  (let* ((statebody (state-body st))
         (subtable (gethash (first atom) statebody)) ; hash-table
         (sub-key (if (> (length atom) 1)
                      (second atom) 
                      +SINGLETON-TERM+))
         (subtable-entry (when subtable
                           (gethash sub-key subtable)))) ; list
    (cond ((null subtable) (values)) ;no-op
          ((null subtable-entry) (values))
          ((eq sub-key +SINGLETON-TERM+) (remhash sub-key subtable))
          (t
           (setf (gethash sub-key subtable)
                 (delete
                  (rest atom)
                  subtable-entry
                  :test #'equal))))))

(defmethod state-atoms ((st doubly-hashed-state))
  (let ((statebody (state-body st))     ; this is a hash-table of hash-tables
        (acc nil)) 
    (maphash #'(lambda (pred subtable)
                 (maphash #'(lambda (first-arg lis)
                              (if (eq first-arg +SINGLETON-TERM+)
                                  (push `(,pred) acc)
                                  (setf acc
                                        (append (mapcar #'(lambda (entry) (cons pred entry)) lis)
                                                acc))))
                          subtable))
             statebody)
    acc))

(defmethod atom-in-state-p (atom (st doubly-hashed-state))
  (if-let ((subhash (gethash (first atom) (state-body st))))
    (if-let (lst (gethash (second atom) subhash))
      (member (rest atom) lst :test #'equal))))

(defmethod state-all-atoms-for-predicate ((st doubly-hashed-state) pred)
  (let ((subhash (gethash pred (state-body st))))
    (when subhash
      (let ((acc nil))
        (maphash #'(lambda (subkey lst)
                     (if (eq subkey +singleton-term+)
                         (push (cons pred nil) acc)
                         ;; lst has the atoms with the predicate removed.
                         (mapc #'(lambda (tail)
                                   (push (cons pred tail) acc))
                               lst)))
                 subhash)
        acc))))

(defmethod state-candidate-atoms-for-goal ((st doubly-hashed-state) goal)
  ;(format t "state-body: ~A~%~%"  (state-atoms st))
  (destructuring-bind (pred . body) goal
    (if-let ((sub-table (gethash pred (state-body st))))
      (if (or (zerop (length body)) (variablep (first body)))
          (state-all-atoms-for-predicate st pred)
          (if-let ((lst (gethash (first body) sub-table)))
            (mapcar #'(lambda (x) (cons pred x)) lst))))))

(defmethod copy-state ((st doubly-hashed-state))
  (let ((the-copy (make-doubly-hashed-state nil)))
    (setf (state-body the-copy) (copy-hash-table (state-body st) #'copy-hash-table))
    (setf (tagged-state-tags-info the-copy)
          (copy-tree (tagged-state-tags-info st)))
    (setf (tagged-state-block-at the-copy)
     (tagged-state-block-at st))
    the-copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The "bit-state" class

(defstruct (bit-state (:include tagged-state)
                      (:constructor %make-bit-state)
                      (:copier nil)))




(defmethod make-initial-state (domain (state-encoding (eql :bit)) atoms &key)
  (declare (ignore domain))
  (make-bit-state atoms))

(defmethod state->state-type ((state bit-state))
  :bit)




(defun make-bit-state (atoms)
  (let ((st (%make-bit-state)))
    ;; The previous version of shop2.lisp did some strange initialization work
    ;; when making a new :bit statebody which I didn't understand.
    ;; This doesn't do that. It seems to me like this just makes bit-states into
    ;; list-states that carry around some useless empty hash tables. That is, I
    ;; don't think the hash tables in the statebody do anything in this
    ;; implementation.
    (setf (state-body st)
          (list (make-hash-table :test #'eq)
                (make-hash-table :test #'equal)
                (make-hash-table :test #'eq)
                nil))
    (dolist (atom atoms) (insert-atom atom st))
    st))

(defmethod insert-atom (atom (st bit-state))
  (let* ((statebody (state-body st))
         (pred-table (first statebody))
         (entity-table (second statebody))
         (extras (fourth statebody))
         (entities (rest atom))
         (types (mapcar #'(lambda (entity)
                            (first (gethash entity entity-table)))
                        entities))
         (entity-numbers (mapcar #'(lambda (entity)
                                     (second (gethash entity entity-table)))
                                 entities))
         (pred-entry (gethash (first atom) pred-table))
         (pred-types (first pred-entry))
         (pred-array (third pred-entry)))

    (if (and entities (equal types pred-types))
        (setf (apply #'aref pred-array entity-numbers) 1)
      (setf (fourth statebody)
            (LIST-insert-atom-into-statebody atom extras)))))

(defmethod remove-atom (atom (st bit-state))
  (let* ((statebody (state-body st))
         (pred-table (first statebody))
         (entity-table (second statebody))
         (extras (fourth statebody))
         (entities (rest atom))
         (types (mapcar #'(lambda (entity)
                            (first (gethash entity entity-table)))
                        entities))
         (entity-numbers (mapcar #'(lambda (entity)
                                     (second (gethash entity entity-table)))
                                 entities))
         (pred-entry (gethash (first atom) pred-table))
         (pred-types (first pred-entry))
         (pred-array (third pred-entry)))

    (if (and entities (equal types pred-types))
        (setf (apply #'aref pred-array entity-numbers) 0)
      (setf (fourth statebody)
            (LIST-remove-atom-from-statebody atom extras)))))

(defmethod state-atoms ((st bit-state))
  (let ((acc nil))
    (maphash #'(lambda (pred lis)
                 (declare (ignore lis))
                 (setf acc
                       (append
                        (state-all-atoms-for-predicate st pred)
                       acc)))
             (first (state-body st)))
    (remove-duplicates (append
                        acc 
                        (mapcan #'(lambda (entry) (copy-list (cdr entry)))
                                (fourth (state-body st)))))))

(defmethod atom-in-state-p (atom (st bit-state))
  (let* ((statebody (state-body st))
         (pred-table (first statebody))
         (entity-table (second statebody))
         (extras (fourth statebody))
         (entities (rest atom))
         (types (mapcar #'(lambda (entity)
                            (first (gethash entity entity-table)))
                        entities))
         (entity-numbers (mapcar #'(lambda (entity)
                                     (second (gethash entity entity-table)))
                                 entities))
         (pred-entry (gethash (first atom) pred-table))
         (pred-types (first pred-entry))
         (pred-array (third pred-entry)))

    (if (and entities (equal types pred-types))
        (= (apply #'aref pred-array entity-numbers) 1)
      (member atom (rest (assoc (first atom) extras)) :test #'equal))))

(defmethod state-all-atoms-for-predicate ((st bit-state) pred)
  (let* ((statebody (state-body st))
         (pred-table (first statebody))
         (type-table (third statebody))
         (extras (fourth statebody))
         (pred-entry (gethash pred pred-table))
         (pred-types (first pred-entry))
         (pred-type-counts (second pred-entry))
         (pred-array (third pred-entry)))

    (append
     (when pred-entry
       (mapcar #'(lambda (entities)
                   (cons pred entities))
               (BIT-statebody-search-array
                pred-array pred-type-counts
                (mapcar #'(lambda (type-name)
                            (second (gethash type-name type-table)))
                        pred-types)
                (mapcar #'(lambda (x) (declare (ignore x)) (list :variable 0))
                        pred-types))))
     (rest (assoc pred extras)))))

(defmethod state-candidate-atoms-for-goal ((st bit-state) goal)
  (let* ((statebody (state-body st))
         (pred-table (first statebody))
         (entity-table (second statebody))
         (type-table (third statebody))
         (extras (fourth statebody))
         (pred (first goal))
         (goal-terms (rest goal))
         (pred-entry (gethash pred pred-table))
         (pred-types (first pred-entry))
         (pred-type-counts (second pred-entry))
         (pred-array (third pred-entry)))



    (append
     (when (and pred-entry
                (= (length goal-terms) (length pred-types)))
       (let ((initial-counter
              (mapcar #'(lambda (entity pred-type)
                          (if (variablep entity)
                              (list :variable 0)
                            (let ((entry (gethash entity entity-table)))
                              (if (eq (first entry) pred-type)
                                  (list :fixed (second entry))
                                nil))))
                      goal-terms pred-types)))

         (unless (member nil initial-counter)
           (mapcar #'(lambda (entities)
                       (cons pred entities))
                   (BIT-statebody-search-array
                    pred-array pred-type-counts
                    (mapcar #'(lambda (type-name)
                                (second (gethash type-name type-table)))
                            pred-types)
                    initial-counter)))))
     (rest (assoc pred extras)))))

;;; This is very different from what was in state-utils before, but I'm pretty
;;; sure this does the job.
(defmethod copy-state ((st bit-state))
  (let ((the-copy (make-bit-state (state-atoms st))))
    (setf (tagged-state-tags-info the-copy)
          (copy-tree (tagged-state-tags-info st)))
    (setf (tagged-state-block-at the-copy)
          (tagged-state-block-at st))
    the-copy))

;;; I don't know what these next two functions do, so I left them as defuns
;;; rather than trying to define them as methods for the bit-state class.
(defun BIT-statebody-search-array
  (pred-array pred-type-counts entity-number-tables complex-position)
  (let ((position (mapcar #'second complex-position)))
    (cond
     ((null position)
      nil)
     ((= (apply #'aref pred-array position) 1)
      (cons
       (mapcar #'(lambda (num entity-number-table)
                   (gethash num entity-number-table))
               position entity-number-tables)
       (BIT-statebody-search-array
        pred-array pred-type-counts entity-number-tables
        (BIT-statebody-increment-position
         complex-position pred-type-counts))))
     (t
      (BIT-statebody-search-array
       pred-array pred-type-counts entity-number-tables
       (BIT-statebody-increment-position
        complex-position pred-type-counts))))))

(defun BIT-statebody-increment-position
  (position pred-type-counts)
  (cond
   ((null position) nil)
   ((eq :fixed (first (first position)))
    (if (BIT-statebody-increment-position
         (rest position) (rest pred-type-counts))
        position
      nil))
   (t
    (incf (second (first position)))
    (cond 
     ((< (second (first position)) (first pred-type-counts))
      position)
     ((null (rest position))
      nil)
     ((BIT-statebody-increment-position
       (rest position) (rest pred-type-counts))
      (setf (second (first position)) 0)
      position)
     (t nil)))))

(defun copy-hash-table (H1 &optional (copy-fn #'identity))
  ;; modified this to use the hash-table-test function, instead of always building
  ;; an "EQUAL" hash-table.  Also initialized to be the same size, avoiding
  ;; resizes in building, I hope. [2002/10/08:rpg]
  (let ((H2 (make-hash-table :size (hash-table-size H1) :test (hash-table-test H1))))
    (maphash #'(lambda (key val) (setf (gethash key H2) (funcall copy-fn val)))
             H1)
    H2))

(defun prop-sorter (p1 p2)
  (flet ((elem< (p1 p2)
           (cond ((numberp p1)
                  (if (numberp p2)
                      (< p1 p2)
                      t))
                 ((numberp p2)            ;only p2 is a number
                  nil)
                 ((symbolp p1)
                  (if (symbolp p2)
                      (cond 
                        ((string-lessp p1 p2) t)
                        ((string-lessp p2 p1) (values nil t))
                        (t (values nil nil)))
                      ;; p1 is a symbol and p2 is something weird; put p2 first
                      nil))
                 ;; arbitrary
                 (t t))))
    (cond ((and p1 p2)
           (multiple-value-bind (lessp known)
               (elem< (first p1) (first p2))
             (cond (lessp t)
                   (known nil)
                   (t
                    (prop-sorter (rest p1) (rest p2))))))
          (p1 nil)
          (t t))))

(defmethod state-trajectory ((st tagged-state) &key sorted)
  (let ((state (copy-state st)))
    (loop :with trajectory
          :for state-info :in (tagged-state-tags-info state)
          :for state-list = (state-atoms state)
          :when sorted
            :do (setf state-list (sort state-list 'prop-sorter))
          :do (push state-list trajectory)
              (retract-state-changes state (first state-info))
          :finally (return trajectory))))
