
(defun make-all-pair-shortest-path (rover)
  (let* ((waypoint-list (query-current-state 'Waypoint))
         (can_traverse-list (query-current-state 'can_traverse))
	 (visible-list (query-current-state 'visible))
	 (vertices nil)
	 (edges  nil)
	 L
	)
       (dolist (waypoint-atom waypoint-list)
          (push (second waypoint-atom) vertices))

       (dolist (can_traverse-atom can_traverse-list)
          (when (eql (second can_traverse-atom) rover)
            (push (list (third can_traverse-atom) (fourth can_traverse-atom) 1
                        (list (third can_traverse-atom) (fourth can_traverse-atom)))
                   edges)
          )
       )
       (find-all-pair-shortest-path (build-weighted-graph vertices edges))
   )
)
