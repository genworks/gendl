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

(in-package :geom-base)

(define-object global-polygon-projection (base-object ifs-output-mixin)
  
  :documentation (:description "A polygon ``extruded'' for a given distance along a single vector.
For planar polygons, the projection vector must not be orthogonal to the normal of the plane of
the polygon. The vertices and projection-vector are given in the global coordinate system, so
the local center and orientation do not affect the positioning or orientation of this part."
                  
                  
                                  :examples "<pre>

  (in-package :gdl-user)

  (define-object global-polygon-projection-sample (global-polygon-projection)
    :computed-slots
    ((display-controls (list :color :gold-old :transparency 0.3))
     (projection-depth 5)
     (vertex-list (list (make-point 0 0 0)
                        (make-point 10 10 0)
                        (make-point 30 10 0)
                        (make-point 40 0 0)
                        (make-point 30 -10 0)
                        (make-point 10 -10 0)
                        (make-point 0 0 0)))))

  (generate-sample-drawing :objects (make-object 'global-polygon-projection-sample)
                           :projection-direction (getf *standard-views* :trimetric))  

  </pre>")

  
  :input-slots
  ("Number. The resultant distance from the two end faces of the extrusion."
   projection-depth 
   
   "List of 3D points. The vertex list making up the polyline, same as the input for global-polyline."
   vertex-list
   
   ("The direction of extrusion with respect to the vertices in vertex-list and the projection-vector:
<ul>
<li><tt>:up</tt> Indicates to start from current location of vertices and move in the direction of 
the projection-vector.</li>
<li><tt>:down</tt> Indicates to start from current location of vertices and move in the direction opposite
the projection-vector.</li>
<li><tt>:center</tt> Indicates to start from current location of vertices and move in the direction of 
the projection-vector <i>and</i> opposite the projection-vector, going half the projection-depth in
each direction.</li>
</ul>"
    offset :up)
   
   ("3D Vector. Indicates the straight path along which the extrusion should occur."
    projection-vector (cross-vectors (subtract-vectors (second (the :vertex-list))
                                                       (first (the :vertex-list)))
                                     (subtract-vectors (third (the :vertex-list))
                                                       (first (the :vertex-list))))))
  :computed-slots
  ((polygon-type 'global-polyline)
   
   (%renderer-info% (list :vrml? t :view-default :trimetric))
   
   (%lines-to-draw% (append 
                     (the :polygon-original :%%lines-to-draw%%)
                     (the :polygon-1 :%%lines-to-draw%%)
                     (when (typep (the :polygon-2) (the :polygon-type))
                       (the :polygon-2 :%%lines-to-draw%%))
                     (the :projection-lines)))
   
   (projection-lines (mapcar #'list 
                             (butlast
                              (the-object
                               (if (typep (the :polygon-2) (the :polygon-type))
                                   (the :polygon-2) 
                                 (the :polygon-original)) :vertex-list))
                             (butlast (the :polygon-1 :vertex-list))))
   
   
   (bounding-box (bounding-box-from-points (append (the polygon-original vertex-list)
                                                   (the polygon-1 vertex-list))))
   
   
   (polygon-2-effective (if (typep (the polygon-2) (the polygon-type))
                            (the polygon-2) (the polygon-original)))
   
   (polygons-for-ifs (cons (the polygon-1 vertex-list)
                           (append
                            (mapcar #'(lambda(p1-v1 p1-v2 p2-v1 p2-v2)
                                        (list  p1-v1 p1-v2 p2-v2 p2-v1))
                                    (the polygon-1 vertex-list)
                                    (rest (the polygon-1 vertex-list))
                                    (the polygon-2-effective vertex-list)
                                    (rest (the polygon-2-effective vertex-list)))
                           
                            (list (the polygon-2-effective vertex-list))))))
  
  :hidden-objects
  ((polygon-original :type (the :polygon-type)
                     :pass-down (:radius-list :default-radius)
                     :vertex-list (if (coincident-point? (first (the :vertex-list))
                                                         (lastcar (the :vertex-list)))
                                      (the :vertex-list)(append (the :vertex-list)
                                                                (list (first (the :vertex-list))))))
   (polygon-1 :type (the :polygon-type)
              :pass-down (:radius-list :default-radius)
              :vertex-list (mapcar #'(lambda(vertex)
                                       (translate-along-vector 
                                        vertex (ecase (the :offset)
                                                 ((:up :center) (the :projection-vector))
                                                 (:down (reverse-vector (the :projection-vector))))
                                        (ecase (the :offset)
                                          ((:up :down) (the :projection-depth))
                                          (:center (half (the :projection-depth))))))
                                   (the :polygon-original :vertex-list)))
   
   (polygon-2 :type (ecase (the :offset) ((:up :down) 'null-part) (:center (the :polygon-type)))
              :pass-down (:radius-list :default-radius)
              :vertex-list (mapcar #'(lambda(vertex)
                                       (translate-along-vector
                                        vertex (reverse-vector (the :projection-vector))
                                        (half (the :projection-depth))))
                                   (the :polygon-original :vertex-list)))))



(define-object ifs-output-mixin ()
  
  :input-slots
  (polygons-for-ifs)
  
  :computed-slots
  ((ifs-vertex-ht (let ((ht (make-hash-table :test #'equalp))
                        (count -1))
                    (dolist (polygon (the polygons-for-ifs) ht)
                      (dolist (vertex polygon)
                        (let ((current (gethash vertex ht)))
                          (unless current 
                            (setf (gethash vertex ht) (incf count))))))))
   
   (ifs-array (let ((array (make-array (list (hash-table-count (the ifs-vertex-ht))))))
                (maphash #'(lambda(vertex index)
                             (setf (aref array index) vertex))
                         (the ifs-vertex-ht)) array))
   
   (ifs-indices (mapcar #'(lambda(polygon)
                            (mapcar #'(lambda(vertex)
                                        (gethash vertex (the ifs-vertex-ht)))
                                    polygon))
                        (the polygons-for-ifs)))))
  




