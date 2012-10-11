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

(in-package :surf)

;;(eval-when (:compile-toplevel :load-toplevel :execute) ;;XXX
  
  
(define-object fitted-curve (curve)
  
  :documentation (:description "Fits a curve through a set of points with given degree 
and parameterization."
                  
                  :examples "<pre>
 (in-package :surf)

 (define-object test-fitted-curve (fitted-curve) 
  
   :computed-slots
   ((points (the circle (equi-spaced-points 20))))

   :hidden-objects ((circle :type 'circle :radius 10)
                    (spheres  :type 'sphere
                              :sequence (:size (length (the points)))
                              :radius 0.2
                              :center (nth (the-child :index) (the points))
                              :display-controls (list :color :blue-neon))))


 (generate-sample-drawing :objects (let ((self (make-object 'test-fitted-curve)))
                                     (cons self (list-elements (the spheres)))))


 </pre>")
                  
  
  :input-slots
  ("List of 3D Points. The points for fitting." 
   points 
   
   ("Integer. The desired degree of the resultant curve. Default is 3, unless there are fewer than four control point given,
in which case it one less than the number of control points." 
    degree (min (1- (length (the points))) 3))
   
   ("List of 3D Vectors. Optional list of vectors used to influence the fitting. Default is NIL." vectors nil) 
   
   ("Keyword symbol, one of <tt>:tangents</tt>, <tt>:normals</tt>, or <tt>:first-last</tt>. <tt>:Tangents</tt> indicates that the <tt>:vectors</tt> specify
a tangent vector at each point (there should be one vector for each point), 
<tt>:normals</tt> indicates that the <tt>:vectors</tt> specify a normal vector at each 
point (there should be one vector for each point), and 
<tt>:first-last</tt> indicates that the <tt>:vectors</tt> specify the starting and ending 
tangent (in this case there should be two vectors in the <tt>:vectors</tt> list. Default is <tt>:tangents</tt>.)" vector-type :tangents)
      
   
   ("Keyword symbol, one of :uniform, :chord-length, :centripetal. The parameterization to use in the resultant curve. 
Default is :centripetal. Note that the NLib documentation states that when specifying vectors and a vector-type of <tt>:tangents</tt>
or <tt>:first-last</tt>, <tt>:chord-length</tt> is a recommended value for parameterization. If vectors are used and the vector-type 
is <tt>:normals</tt>, this input has no effect. The default is <tt>:chord-length</tt>"
    parameterization :chord-length)
   
   
   ("Number or nil. The allowed tolerance for doing data reduction after the initial fitting. 
A <tt>nil</tt> value indicates that no data reduction is to be attempted. Defaults to nil."
    tolerance *3d-approximation-tolerance-default*)
   
   ("Boolean. Indicates whether the curve will interpolate the points. Defaults to T ." 
    interpolant? t)
   
   (knot-multiplicity :multiple)

   ("Boolean. Indicates whether the inputted control-points should be considered in local coordinate system of this object. Default is nil." 
    local? nil))
  
  :computed-slots
  ((effective-points (if (the local?)
			 (mapcar #'(lambda(point) (the (local-to-global point))) (the points))
			 (the points)))
   
   (effective-vectors (if (the local?)
			 (mapcar #'(lambda(vector) (the (local-to-global vector))) (the vectors))
			 (the vectors)))
   

   (native-curve
    (progn
      (when (and (the vectors)
                 (member (the vector-type) (list :tangents :normals))
                 (not (= (length (the points)) (length (the vectors)))))
        (error "For fitted-curve ~s, points and vectors must be lists of the same length.~%" 
               (cons 'the (reverse (the root-path)))))
      ;;
      ;; FLAG -- consider the case of 2 fit points but resulting curve should have more control-points
      ;; based on given vectors... maybe this conditional is not valid. 
      ;;
      (if (> (length (the points)) 2)
          (let ((degree-ok? (<= (the degree) (1- (length (the points))))))
            (unless degree-ok?
              (error "The degree of a fitted-curve must be less than or equal to the number of control points."))
            (interpolate-curve 
             *geometry-kernel* (the effective-points) (the degree) (the parameterization) :vectors (the effective-vectors) 
             :vector-type (the vector-type)  :interpolant? (the interpolant?)
             :knot-multiplicity (the knot-multiplicity)
             :tolerance (the tolerance)))
        
        (the linear-curve native-curve))))
   
   (%renderer-info% (list :vrml? t :view-default :top))
   
   )
  
  
  :hidden-objects
  ((linear-curve :type (if (= (length (the points)) 2) 'linear-curve 'null-part)
                 :start (first (the points))
                 :end (second (the points)))))

;;)



   
             
             

