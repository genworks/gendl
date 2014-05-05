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

(define-object angular-dimension (linear-dimension)
  
  :documentation (:description "This dimensional object produces a clear and concise arc dimensional annotation."
                  :examples  "<pre>
 (in-package :gdl-user)

 (define-object angular-dimension-test (base-object) 
   
   :objects 
   ((arc :type 'arc
         :display-controls (list :color :green )
         :radius 30
         :end-angle (degrees-to-radians 90))
    
    
    (dimension :type 'angular-dimension
               :display-controls (list :color :blue )
               :leader-radius (+ (* 0.1 (the arc radius))(the arc radius))
               :arc-object (the arc))
    
    (explicit-dimension :type 'angular-dimension
                        :center-point (the arc center)
                        :start-point (the arc (point-on-arc (degrees-to-radians 10)))
                        :end-point (the arc (point-on-arc (degrees-to-radians 60))))))
 (generate-sample-drawing 
  :objects (list 
            (the-object (make-object 'angular-dimension-test) arc) 
            (the-object (make-object 'angular-dimension-test) dimension)
            (the-object (make-object 'angular-dimension-test) explicit-dimension))
  :projection-direction (getf *standard-views* :top))
 
  </pre>")

  :input-slots
  ("GDL object. The arc being measured."
   arc-object
   
   ("3D Point. The center of the arc being measured."
    center-point (the arc-object center))
   
   ("3D Point. The start point  of the arc being measured."
    start-point (the arc-object start))
   
   ("3D Point. The end point of the arc being measured."
    end-point (the arc-object end))
   
   ("Number. The radius for the leader-arc."
    leader-radius (+ (the witness-line-length)
                     (max (3d-distance (the center-point) (the start-point))
                          (3d-distance (the center-point) (the end-point)))))
   
   ("Number. Amount of padding above leader for text-along-leader? t. This is multiplied by the 
character-size to get the actual padding amount. Defaults to 1/3."
    text-along-leader-padding-factor 1/3)
   
   ("Boolean. Determines whether a witness line extends all the way from the start-point to the center. 
Defaults to nil."
    witness-1-to-center? nil) 
   
   ("Boolean. Determines whether a witness line extends all the way from the end-point to the center. 
Defaults to nil."
    witness-2-to-center? nil)
   
   ("3D Point. Determines where the text will start. Defaults to halfway along the arc, just beyond the radius."
    dim-text-start (translate-along-vector (the center-point)
                                           (rotate-vector (the witness-direction-vector-1)
                                                          (half (angle-between-vectors  
                                                                 (the witness-direction-vector-1)
                                                                 (the witness-direction-vector-2)
                                                                 (the (face-normal-vector :top))))
                                                          (the (face-normal-vector :top)))
                                           (if (the text-above-leader?) (the leader-radius)
                                             (the leader-radius)))))
   

  
  :computed-slots
  ((witness-direction-vector-1 (subtract-vectors (the start-point) (the center-point)))
   
   (witness-direction-vector-2 (subtract-vectors (the end-point) (the center-point)))
   
   (witness-line-length-1 (the witness-line-length))
   
   (witness-line-length-2 (the witness-line-length))
   

   (leader-start (translate-along-vector (the start-point) 
                                         (the witness-direction-vector-1)
                                         (+ (the witness-line-length-1)
                                            (the witness-line-gap))))

   (leader-end (let ((nominal (translate-along-vector (the end-point)
                                                      (the witness-direction-vector-2)
                                                      (+ (the witness-line-length-2)
                                                         (the witness-line-gap)))))
                 (if (and (not (the outside-leaders?)) (the full-leader-line-length))
                     (translate-along-vector (the leader-start)
                                             (subtract-vectors nominal (the leader-start))
                                             (the full-leader-line-length))
                   nominal)))
   
   (dimension-angle (angle-between-vectors (the witness-direction-vector-1)
                                           (the witness-direction-vector-2)))
   
   (dim-value (radians-to-degrees (the dimension-angle)))
   
   (%curves-to-draw% (the full-leader arc %curves-to-draw%))
   
   (%arcs% (the full-leader arc %arcs%)))


  
  :hidden-objects
  ((full-leader :type 'leader-arc
                :path-points (list (the leader-start)
                                   (the leader-end))
                :radius (the leader-radius)
                :pass-down (center-point
                            witness-direction-vector-1
                            witness-direction-vector-2
                            arrowhead-style arrowhead-style-2 
                            arrowhead-length arrowhead-width))
   




   (leader-1 :type 'null-part ;; FLAG -- fill in for conditions requiring this.
             )

   (leader-2 :type 'null-part ;; FLAG -- fill in for conditions requiring this.
             )

   
   (witness-line-1 :type  (if (the witness-line?) 'line 'null-part)
                   :start (if (the witness-1-to-center?)
                              (the center-point) (the start-point))
                   :end   (translate-along-vector (the center-point) (the witness-direction-vector-1) 
                                                  (+ (the leader-radius) (the witness-line-ext))))
   
   (witness-line-2 :type  (if (the witness-line-2?) 'line 'null-part)
                   :start (if (the witness-2-to-center?)
                            (the center-point) (the end-point))
                   :end   (translate-along-vector (the center-point) (the witness-direction-vector-2) 
                                                  (+ (the leader-radius) (the witness-line-ext))))
   
   (dimension-text :type 'general-note
                   
                   :pseudo-inputs (start-center)

                   :start-center (let ((point (add-vectors (the dim-text-start) (the dim-text-start-offset))))

                                    (translate-along-vector point 
                                                            (subtract-vectors point
                                                                              (the center-point))
                                                            (* (the text-along-leader-padding-factor)
                                                               (the character-size))))
                                    
                   
                   :start (translate-along-vector (the-child start-center)
                                                  (the-child (face-normal-vector :left))
                                                  (half (the-child width)))
           
                   


                   :strings (ensure-list (the dim-text))
                   
                   :orientation (if (the text-along-axis?)
                                    (alignment 
                                     :top (the (face-normal-vector :top))
                                     :right (cross-vectors (subtract-vectors (the-child start-center)
                                                                             (the center-point))
                                                           (the (face-normal-vector :top))))
                                  (the orientation))

                   :pass-down (font character-size))))




