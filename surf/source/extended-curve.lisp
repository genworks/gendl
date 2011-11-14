;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(define-object  extended-curve (curve)
  
  :documentation (:description "Creates an extended curve extending its start or end  (<tt>u1</tt> and <tt>u2</tt>)."
                  :examples "<pre>
  
 (in-package :gdl-user)

 (define-object test-extended-curve ()

   :objects
   ((b-spline-curve :type 'b-spline-curve
                    :display-controls (list :color :black :line-thickness 3.0)
                    :control-points (list (make-point -2.0 0.0 0.0)
                                          (make-point  0.0 1.0 0.0) 
                                          (make-point  1.0 0.0 0.0) 
                                          (make-point  0.0 -1.0 0.0))) 
                                           
    
    (extended-curve-G1 :type 'extended-curve
                       :curve-in (the b-spline-curve)
                       :distance 2.5
                       :distance-type :absolute
                       :extending-from :start
                       :continuity :g1
                       :display-controls (list :color :red))
    
    (extended-curve-G2 :type 'extended-curve
                       :curve-in (the b-spline-curve)
                       :distance 2.5
                       :distance-type :absolute
                       :extending-from :start
                       :continuity :g2
                       :display-controls (list :color :green))
    
    (extended-curve-Cmax :type 'extended-curve
                         :curve-in (the b-spline-curve)
                         :distance 2.5
                         :distance-type :absolute
                         :extending-from :start
                         :continuity :cmax
                         :display-controls (list :color :blue))))

 (generate-sample-drawing :object-roots (make-object 'test-extended-curve))

   </pre>" )
  
  :input-slots
  ("GDL Curve Object. The underlying curve from which to build this curve."
   curve-in
      
   ("Number. Specified the distance to which the curve is extended."
    distance) 
      
   ("Keyword. Specified if the distance is an absolute distance <tt>:absolute</tt> or the distance is scaled by the curve's arc length
     to yield the desired extension distance <tt>:relative</tt>. Defaults to the <tt>:absolute</tt>."
    distance-type :absolute)
   
   ("Keyword. Specified from which end the curve to be extended. If :start the curve is extended back from  its  start point.
    If :end the the curve is extended forward from its end point. Defaults to the <tt>:start</tt>."
    extending-from :start)
   
   ("Keyword. Specified the extention continuity. If :g1 the curve is extended  by a linear segment. Curve-in is at least G1(possibly C1) 
    where the extension joins the original curve. If :g2 the curve is extended by reflection,  yielding a G2 continuous extension. 
    If :cmax  the extension yields infinite(C) continuity at the join point(no knot there). Defaults to the <tt>:g2</tt>."
    continuity :g2)
   
   )
  
  :computed-slots ((native-curve (make-extend-curve *geometry-kernel* (the curve-in) 
                                               :distance (the distance) 
                                               :distance-type (the distance-type)
                                               :extending-from (the extending-from)
                                               :continuity (the continuity)))))
                                              
  


  (define-object test-extended-curve ()

   :objects
   ((b-spline-curve :type 'arc-curve
                    :center (make-point 0 0 0)
                    :radius 1.5 
                    :start-angle 0 
                    :end-angle  pi)
    
    
    (extended-curve-G1 :type 'extended-curve
                       :curve-in (the b-spline-curve)
                       :distance 3.0
                       :distance-type :absolute
                       :extending-from :start
                       :continuity :g1
                       :display-controls (list :color :red))
    
    (extended-curve-G2 :type 'extended-curve
                       :curve-in (the b-spline-curve)
                       :distance 2.5
                       :distance-type :absolute
                       :extending-from :start
                       :continuity :g2
                       :display-controls (list :color :green))
    
    (extended-curve-Cmax :type 'extended-curve
                         :curve-in (the b-spline-curve)
                         :distance 3.0
                         :distance-type :absolute
                         :extending-from :start
                         :continuity :cmax
                         :display-controls (list :color :blue))))
