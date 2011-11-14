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

(define-object box (base-object)
  
  :documentation (:description "This represents a ``visible'' base-object -- a six-sided box with all the same
messages as base-object, which knows how to output itself in various formats."
                  
                  :examples "<pre>
 (in-package :gdl-user)

 (define-object box-sample (box)
   :computed-slots ((display-controls (list :color :blue-neon))
                    (length 10)
                    (width (* (the length) +phi+))
                    (height (* (the width) +phi+))))

 (generate-sample-drawing :objects (make-object 'box-sample)
                          :projection-direction (getf *standard-views* :trimetric))

</pre>")
                  
  :computed-slots 
  (("Number. Total volume of the box."
    volume (* (the length) (the width) (the height)))
   
   (%renderer-info% (list :vrml? t :view-default :trimetric))


   (%vertex-array% (make-array 8 :initial-contents (list (the (vertex :top :right :rear))
                                                         (the (vertex :top :left :rear))
                                                         (the (vertex :top :left :front))
                                                         (the (vertex :top :right :front))
                                                         (the (vertex :bottom :right :rear))
                                                         (the (vertex :bottom :left :rear))
                                                         (the (vertex :bottom :left :front))
                                                         (the (vertex :bottom :right :front)))))


   (%line-vertex-indices% (list (list 0 1) (list 1 2) (list 2 3) (list 3 0)
                                (list 0 4) (list 1 5) (list 2 6) (list 3 7)
                                (list 4 5) (list 5 6) (list 6 7) (list 7 4)))
   
   ;;
   ;; FLAG -- this should be able to be dumped at some point.
   ;;
   (%lines-to-draw% (let ((top-left-rear (the (vertex :top :left :rear)))
                          (top-right-rear (the (vertex :top :right :rear)))
                          (top-left-front (the (vertex :top :left :front)))
                          (top-right-front (the (vertex :top :right :front)))
                          (bottom-left-rear (the (vertex :bottom :left :rear)))
                          (bottom-right-rear (the (vertex :bottom :right :rear)))
                          (bottom-left-front (the (vertex :bottom :left :front)))
                          (bottom-right-front (the (vertex :bottom :right :front))))
                      (list (list top-left-rear top-right-rear)
                            (list top-right-rear top-right-front)
                            (list top-right-front top-left-front)
                            (list top-left-front top-left-rear)
                            (list top-left-rear bottom-left-rear)
                            (list top-right-rear bottom-right-rear)
                            (list top-right-front bottom-right-front)
                            (list top-left-front bottom-left-front)
                            (list bottom-left-rear bottom-right-rear)
                            (list bottom-right-rear bottom-right-front)
                            (list bottom-right-front bottom-left-front)
                            (list bottom-left-front bottom-left-rear)))))
  
  
  :functions
  ((in? 
    (point)
    
    (let ((x (get-x point))
          (y (get-y point))
          (z (get-z point)))
      (and (> (get-x (the (face-center :left))) x (get-x (the (face-center :right))))
           (> (get-y (the (face-center :front))) y (get-y (the (face-center :rear))))
           (> (get-z (the (face-center :bottom))) z (get-z (the (face-center :top)))))))))




(define-lens (obj box)()
  :output-functions
  ((cad-output
    ()
    (let ((vertex-array (the %vertex-array%)))
      ;;
      ;; FLAG -- replace format with something more efficent
      ;;
      (format *stream* "
v ~2,7f ~2,7f ~2,7f
v ~2,7f ~2,7f ~2,7f
v ~2,7f ~2,7f ~2,7f
v ~2,7f ~2,7f ~2,7f
v ~2,7f ~2,7f ~2,7f
v ~2,7f ~2,7f ~2,7f
v ~2,7f ~2,7f ~2,7f
v ~2,7f ~2,7f ~2,7f
f ~a ~a ~a ~a
f ~a ~a ~a ~a
l ~a ~a
l ~a ~a
l ~a ~a
l ~a ~a
"
              (get-x (aref vertex-array 0)) (get-y (aref vertex-array 0)) (get-z (aref vertex-array 0))
              (get-x (aref vertex-array 1)) (get-y (aref vertex-array 1)) (get-z (aref vertex-array 1))
              (get-x (aref vertex-array 2)) (get-y (aref vertex-array 2)) (get-z (aref vertex-array 2))
              (get-x (aref vertex-array 3)) (get-y (aref vertex-array 3)) (get-z (aref vertex-array 3))
              (get-x (aref vertex-array 4)) (get-y (aref vertex-array 4)) (get-z (aref vertex-array 4))
              (get-x (aref vertex-array 5)) (get-y (aref vertex-array 5)) (get-z (aref vertex-array 5))
              (get-x (aref vertex-array 6)) (get-y (aref vertex-array 6)) (get-z (aref vertex-array 6))
              (get-x (aref vertex-array 7)) (get-y (aref vertex-array 7)) (get-z (aref vertex-array 7))
              (+ *vertex-count* 1) (+ *vertex-count* 2)(+ *vertex-count* 3)(+ *vertex-count* 4)
              (+ *vertex-count* 5) (+ *vertex-count* 6)(+ *vertex-count* 7)(+ *vertex-count* 8)
              (+ *vertex-count* 1)(+ *vertex-count* 5)
              (+ *vertex-count* 2)(+ *vertex-count* 6)
              (+ *vertex-count* 3)(+ *vertex-count* 7)
              (+ *vertex-count* 4)(+ *vertex-count* 8)))
    
    (incf *vertex-count* 8)
    )))









