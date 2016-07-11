;;
;; Copyright 2016 Genworks International 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 

(in-package :surf)

#+(or allegro lispworks)
(#+allegro
 excl:without-package-locks #-allegro progn
 (#+allegro
  excl:without-redefinition-warnings #-allegro progn
  (define-object normally-projected-curve (curve)

    :input-slots (from-surface
		  curve to-surface
		  (number-of-points 200))


    :computed-slots ((sample-points (the curve uv-curve (equi-spaced-points (the number-of-points))))

		     (normals (mapcar #'(lambda(point) (the from-surface (normal (get-u point) (get-v point)))) (the sample-points)))

		     (projected-points (mapcar #'(lambda(point normal)
						   (the to-surface (projected-point (the from-surface (point (get-u point) (get-v point))) normal)))
					       (the sample-points) (the normals)))

		     (projected-points-3d (mapcar #'get-3d-point-of (the projected-points)))


		     (projected-points-uv (mapcar #'get-uv-point-of (the projected-points)))

		     (built-from (the fitted-3d)))

  
  
    :hidden-objects ((fitted :type 'fitted-curve
			     :points (the projected-points-3d))

		     (fitted-uv :type 'fitted-curve
				:points (mapcar #'(lambda(point) (make-point (get-x point) (get-y point) 0)) (the projected-points-uv)))

		     (approximated :type 'approximated-curve
				   :curve-in (the fitted-uv)
				   :tolerance 0.00001)
            
		     (fitted-3d :type 'b-spline-curve
				:knot-vector (the approximated knot-vector)
				:degree (the approximated degree)
				:weights (the approximated weights)
				:control-points (mapcar #'(lambda(point)
							    (the to-surface (point (get-u point) (get-v point))))
							(the approximated control-points)))))


  (define-object-amendment curve ()
    :computed-slots
    ((%curves-to-draw%
      (unless (or *curve-tessellation?*
		  (near-to? (the total-length) 0))
	(if (typep self 'arc-curve)
	    (call-next-method)
	    (if (or (the %decomposed?%) (= (the %decomposed% curves number-of-elements) 1))
		(unless (and (= (the degree) 1) (not (the rational?)))
		  (when (list-elements (the beziers))
		    (let (result)
		      (dolist (bezier 
				(if t ;;*chain-beziers-for-display?*
				    (chain-nurbs-curves (list-elements (the beziers)))
				    (list-elements (the beziers)))
			       result)
			(if (null result)
			    (setq result (the-object bezier %curves-to-draw%))
			    (nconc result (the-object bezier %curves-to-draw%)))))))
		(append-elements (the %decomposed% curves) 
				 (the-element %curves-to-draw%))))))))))


