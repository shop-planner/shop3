;;;---------------------------------------------------------------------------
;;; Copyright (c) 2022 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; This code available under the Mozilla Public License.
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
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;;    Operations on the Tagged states.  This defines a protocol for backtracking
;;; and backjumping.
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2022/11/22:rpg] Created.
;;;
;;;---------------------------------------------------------------------------

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

(in-package :shop.common)

(defun tagged-state-tags-info-tag (tagged-state)
  (caar (tagged-state-tags-info tagged-state)))

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
