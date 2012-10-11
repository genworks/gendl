;;
;; Copyright 2002-2011 Genworks International 
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

(in-package :geom-base)



(define-object global-filleted-polygon-projection-sample (global-filleted-polygon-projection)
    :computed-slots
    ((display-controls (list :color :blue-steel :transparency 0.3 :shininess 0.7 :spectral-color :white))
     (default-radius 5)
     (projection-depth 5)
     (vertex-list (list (make-point 0 0 0)
                        (make-point 10 10 0)
                        (make-point 30 10 0)
                        (make-point 40 0 0)
                        (make-point 30 -10 0)
                        (make-point 10 -10 0)
                        (make-point 0 0 0)))))

(define-object route-pipe (global-filleted-polyline-mixin outline-specialization-mixin)

  :documentation
  
  (:description "Defines an alternating set of cylinders and torus sections for the elbows"
                  
                  
                  :examples "<pre>

 (in-package :gdl-user)  

 (define-object route-pipe-sample (base-object)

  :objects

  ((pipe :type 'route-pipe
         :vertex-list (list #(410.36 436.12 664.68) 
                            #(404.21 436.12 734.97) 
                            #(402.22 397.48 757.72) 
                            #(407.24 397.48 801.12) 
                            #(407.24 448.0 837.0)
                            #(346.76 448.0 837.0))
         :default-radius 19
         :outer-pipe-radius 7
         :inner-pipe-radius nil
         :display-controls (list :color :blue-steel 
                                 :transparency 0.0 
                                 :shininess 0.7 
                                 :spectral-color :white))))

 
 (generate-sample-drawing :objects (the-object (make-object 'route-pipe-sample) pipe)
                          :projection-direction (getf *standard-views* :trimetric))
  


</pre>")
  
  :input-slots
  (("Number. Radius of the inner hollow part of the piping. NIL for a solid pipe."
    inner-pipe-radius  nil)
   
   "Number. Radius to the outer surface of the piping."
   outer-pipe-radius 
   
   "List of 3D Points. Same as for global-filleted-polyline (which is mixed in to this part)"
   vertex-list)
  
  :computed-slots
  ((%renderer-info% (list :vrml? t :view-default :trimetric))
   
   (outline-objects (append (list-elements (the cylinders))
                            (list-elements (the tori))))
   
   (bounding-box (let ((overage (the outer-pipe-radius)))
                   (let ((p1 (first (the polyline bounding-box)))
                         (p2 (second (the polyline bounding-box))))
                     (list (make-point (- (get-x p1) overage) (- (get-y p1) overage) (- (get-z p1) overage))
                           (make-point (+ (get-x p2) overage) (+ (get-y p2) overage) (+ (get-z p2) overage))))))
                                       
   
   (length (the bounding-bbox length))
   (width (the bounding-bbox width))
   (height (the bounding-bbox height))
   
   (orientation nil)
   
   (torus-types (mapcar #'(lambda(test)
                            (if (the-object test valid?) 'torus 'null-part))
                        (list-elements (the fillet-tests))))
   
   
   (pipe-parts (let ((cylinders (list-elements (the cylinders)))
                     (torii (let (result) (dotimes (n (the tori number-of-elements) (nreverse result))
                                            (push (the (tori n)) result)))))
                 (remove-if #'(lambda(part) (typep part 'null-part))
                            (append (apply #'append (mapcar #'list cylinders torii))
                                    (when (> (length cylinders) (length torii)) (last cylinders))))))
   
   )
   
  
  ;;:hidden-objects
  :objects
  ((polyline :type 'global-filleted-polyline
             :pass-down (vertex-list default-radius))
   
   
   (cylinders :type 'cylinder-with-straight
              :sequence (:size (length (the straights)))
              :straight (nth (the-child index) (the straights))
              :radius (the outer-pipe-radius)
              :inner-radius (the inner-pipe-radius)
              :length (* 1 (3d-distance (first (the-child straight))
                                        (second (the-child straight))) )
              :center (mid-point (the-child straight))
              :bottom-cap? (not (the-child first?))
              :display-controls (append (list :solid? t) (the display-controls))
              :top-cap? (not (the-child last?))
              :orientation (alignment :front (direction-vector (the-child straight))))
   
   (tori :type (:sequence (the torus-types))
         :sequence (:size (the fillets number-of-elements))
         :fillet (the (fillets (the-child index)))
          
         :minor-radius (the outer-pipe-radius)
          
         :major-radius (the-child fillet radius)

         :inner-minor-radius (when (the inner-pipe-radius)
                               (the inner-pipe-radius))

         :display-controls (append (list :solid? t) (the display-controls))
         
         :center (the-child fillet center)
         :orientation (alignment 
                       :top (the-child fillet (face-normal-vector :top))
                       :right (rotate-vector (the-child fillet (face-normal-vector :right))
                                             (the-child fillet start-angle-normalized)
                                             (the-child fillet (face-normal-vector :top))))
          
                        
         :arc (- (the-child fillet end-angle-normalized) (the-child fillet start-angle-normalized)))))



(define-object cylinder-with-straight (cylinder)
  :input-slots (straight))
