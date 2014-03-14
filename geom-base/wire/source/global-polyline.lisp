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

(define-object global-polyline-mixin (base-object)
  
  :input-slots
  ("List of 3D Points. The vertices (``corners'') of the polyline."
   vertex-list)
  
  :computed-slots
  ((%corners%  (bounding-box-from-points (the vertex-list)))

   (%lines-to-draw% (the :lines))
   (%%vertex-array%% (flatten-lines (the %lines-to-draw%)))
   (%%line-vertex-indices%% (let ((count -1))
                            (mapcar #'(lambda(line)
                                        (declare (ignore line))
                                        (list (incf count) (incf count)))
                                    (the %lines-to-draw%))))
   
   
   (path-info (append (list :move (first (the vertex-list)))
		      (mapcan #'(lambda(point) (list :line point))
			      (rest (the vertex-list)))))

   (%renderer-info% (list :vrml? t :view-default :top))
   
   ("List of pairs of 3D points. Each pair represents the start and end of each line segment in the polyline."
    lines (let ((vertex-list (the vertex-list)))
            (mapcar #'list vertex-list (rest vertex-list))))

   (vertex-list-local (mapcar #'(lambda(vertex) (the (global-to-local vertex))) (the vertex-list)))
   
   (invariant-component (let ((vertex-list (the vertex-list-local)))
                          (cond ((every #'near-to? (mapcar #'get-x vertex-list) 
                                        (mapcar #'get-x (rest vertex-list))) :x)
                                ((every #'near-to? (mapcar #'get-y vertex-list) 
                                        (mapcar #'get-y (rest vertex-list))) :y)
                                ((every #'near-to? (mapcar #'get-z vertex-list) 
                                        (mapcar #'get-z (rest vertex-list))) :z))))
   
   (profile-origin (when (the invariant-component)
                     (the (global-to-local 
                           (let ((normal (the (face-normal-vector (ecase (the invariant-component) 
                                                                    (:x :right) (:y :rear) (:z :top))))))
                             (inter-line-plane (the center) normal (first (the vertex-list)) normal))))))
   
   (bounding-box (bounding-box-from-points (the vertex-list))))

  
  :functions
  (
   ;;
   ;; FLAG -- generalize this
   ;;
   (interpolated-points-2d
    ()
    (let ((points (mapcar #'(lambda(point) (the (global-to-local point)))
                          (the (:interpolated-points)))))
      (let ((first-component (ecase (the :invariant-component) (:x #'get-y) ((:y :z) #'get-x)))
            (second-component (ecase (the :invariant-component) ((:x :y) #'get-z) (:z #'get-y))))
        (mapcar #'(lambda(point)
                    (make-point (funcall first-component point) (funcall second-component point))) 
                points))))
      
   (interpolated-points
    (&optional (curve-chords *curve-chords*))
    (declare (ignore curve-chords))
    (append (mapcar #'(lambda(line) (first line)) (the lines)) (last (lastcar (the lines)))))))


(define-object global-polyline (global-polyline-mixin)
  :documentation (:description "A sequence of points connected by straight line segments. Please see
global-polyline-mixin for documentation on the messages."
                  
                  :examples "<pre>
  (in-package :gdl-user)

  (define-object global-polyline-sample (global-polyline)
    :computed-slots
    ((vertex-list (list (make-point 0 0 0)
                        (make-point 10 10 0)
                        (make-point 30 10 0)
                        (make-point 40 0 0)
                        (make-point 30 -10 0)
                        (make-point 10 -10 0)
                        (make-point 0 0 0)))))
  
  (generate-sample-drawing :objects (make-object 'global-polyline-sample))

  </pre>"))




                             
