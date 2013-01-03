(in-package :city)

;;
;; This version is slightly better than the original city because we
;; don't have any rule data hardcoded and we have "factored out"
;; repeated code.  However from a performance standpoint it still
;; could be better -- note how each building instance of a given type
;; will recompute its applicable-rules-data, which will be identical
;; for all buildings of a given type. This could be done once in the
;; parent object, and the applicable-rules-data passed into building
;; as an :input-slot. This is not an assignment but we will do it as a
;; group exercise.
;;

(define-object building (box)
  
  :input-slots (section proximity-rules)
  
  :computed-slots 
  (;;
   ;; catch-all color for any non-specific buildings or building sub-types 
   ;; with no color specified.
   ;;
   (color :black)
		   
   (display-controls (list :color (the color)
			   (if (the any-rules-violated?) 3 1)))
		   
   (neighbors (the section nearby-buildings))
		   
   ;; Returns non-nil if this object violates any rules
   ;;
   (any-rules-violated? (some #'(lambda(rule) (the-object rule violated?))
			      (list-elements (the rules))))
		   
   ;;
   ;; Returns a list of rules which apply to this type of object
   ;;
   (applicable-rules-data (remove-if-not #'(lambda(rule) (eql (first rule) (the type)))
					 (the proximity-rules)))
		   
		   
   (minimum-distance (let (result)
		       (dolist (rule (list-elements (the rules)) result)
			 (when (and (the-object rule minimum-distance)
				    (or (null result)
					(< (first (the-object rule minimum-distance))
					   (first result))))
			   (setq result (the-object rule minimum-distance)))))))
		   
  
  :hidden-objects 
  (
   ;; Now we have a child object to compute the rule, rather than just
   ;; a computed-slot. Pushing the work into a child object is similar
   ;; in concept to calling a function or subroutine in a procedural
   ;; language.
   ;;
   ;; This is defined as a sequence, in case more than one rule
   ;; matches for this building type.
   ;; 
   ;; In our current example only one rule will match for pub and one
   ;; for school, so the sequence will only have one element.
   ;;
   (rules :type 'proximity-rule
	  :sequence (:size (length (the applicable-rules-data)))
	  :pass-down (neighbors)
	  :rule-data (nth (the-child index) (the applicable-rules-data))
	  :target-type (second (the-child rule-data))
	  :distance (third (the-child rule-data))
	  :current-building self)))



(define-object school (building)
  :computed-slots
  ((color :blue)))


(define-object pub (building)
  :computed-slots
  ((color :red)))


(define-object proximity-rule ()

  :input-slots (current-building neighbors target-type distance)
  
  :computed-slots
  ((minimum-distance (let (result)
		       (dolist (building (the violated-buildings) result)
			 (let ((distance (3d-distance  
					  (the current-building center)
					  (the-object building center))))
			   (when (or (null result) (< distance (first result)))
			     (setq result (list distance building)))))))
   
   (violated? (not (null (the violated-buildings))))
     
   ;;
   ;;
   ;; FLAG -- fill in this value.
   ;;
   ;; This will end up as a list of other buildings which violate the  
   ;; proximity rule. It can also be nil, which is the same as the 
   ;; empty list, if no buildings are close enough to violate.
   ;;
   (violated-buildings  nil)))
