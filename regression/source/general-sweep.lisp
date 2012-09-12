;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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


(in-package :gdl-lift-tests)

#+nil
(define-object general-sweep-test (surf::general-sweep)
  
  :computed-slots
  ((profile-curve (the test-data-set (input-entities 1)))
   (guide-curve (the test-data-set (input-entities 0)))
   
   (v1-function #'(lambda(param)
                     (the guide-curve (tangent param))))
   
   (v2-function #'(lambda(param)
                    (cross-vectors
                     (the (face-normal-vector :right))
                     (the guide-curve (tangent param)))))
   
   (regression-test-data (multiple-value-list (the b-spline-data))))
  
  :objects
  ((test-data-set :type 'surf::nurbs-reader
                  :file-name (merge-pathnames "fuselage_fuselage-part_0_section-structure_frames_element_0.dat"
                                              gdl-lift-utils::*lift-data-directory*))))



#+nil
(register-test-definition 'general-sweep-test)


#+nil                    
(defvar *data* (merge-pathnames "../data/" #-lispworks *source-pathname* #+lispworks *load-truename*))

#+nil
(define-object test-guide-normal-on-surface-sweep ()
  :input-slots
  ((input-dump-file1 "fuselage_fuselage-part_0_section-structure_frames_element_0.dat")
   (input-dump-file2 "fuselage_fuselage-part_0_section-structure_frames_element_1.dat")
   (input-dump-file3 "fuselage_fuselage-part_0_section-structure_frames_element_21.dat")
   
   )
  
  :computed-slots
  ((input-path1 (merge-pathnames (the input-dump-file1) *data*))
   (input-path2 (merge-pathnames (the input-dump-file2) *data*))
   (input-path3 (merge-pathnames (the input-dump-file3) *data*))
   )
  
  :objects
  ((expected-surf-data1 :type 'surf::nurbs-reader
                        :file-name (the input-path1))
   
   #+nil
   (expected-surf-data2 :type 'surf::nurbs-reader
                        :file-name (the input-path2))
   
   #+nil
   (expected-surf-data3 :type 'surf::nurbs-reader
                        :file-name (the input-path3))
   
  #+nil
   (example-class-use
    :type 'guide-normal-on-surface-sweep
    :sequence (:size (the :no-of-sweeps))
    :guide-curve (nth (the-child :index) (the :guide-curves-to-use))
    :profile (nth (the-child :index) (the :profiles))
    :surface (the :surface)
    :display-controls (case (nth (the-child :index) (the :display-options))
                        ('a (merge-display-controls '(:color :yellow)))
                        ('b (merge-display-controls '(:color :green)))
                        ('c (merge-display-controls '(:color :blue)))
                        (otherwise (merge-display-controls '(:color :orange)))))
   
   
  ))
                    

(define-object filleted-triangle-curve (non-rational-curve)

  :computed-slots
  ((curve-in (the triangle)))
  
  :objects
  ((triangle :type 'global-filleted-polyline-curve
	     :vertex-list (list (make-point 0 0 0)
				(make-point -10 -5 0)
				(make-point -10 0 0)
				(make-point 0 0 0))
	     :default-radius 0.5)))


(define-object screw-spine (spiral-curve)
  
  :computed-slots
  ((height 110)
   (radius-1 20)
   (radius-2 20)
   (number-of-turns 8)))


(define-object screw-sweep (normal-sweep)

  :computed-slots
  ((display-controls (list :color :blue))
   (guide-curve (the screw-curve))
   (profile-curve (the triangle-curve))
   (end-caps-on-brep? t)
   
   ;;(screw-curve (the screw-curve-input (curves 0)))
   )
  
  :objects
  ((screw-curve :type 'screw-spine)

   #+nil
   (screw-curve-input :type 'native-reader
		      :file-name "/tmp/screw-curve.iwp")


   (triangle-curve :type 'filleted-triangle-curve)))


(define-object screw (subtracted-solid)

  :computed-slots
  ((error-on-invalid? nil)
   (brep (the cylinder))
   (other-brep (the cut-sweep brep))
   (display-controls (list :color :blue))
   )
  
  :objects
  ((cut-sweep :type 'screw-sweep)
   
   (cylinder :type 'cylinder-solid
	     :display-controls (list :color :red)
	     :length 110 
	     :radius 17.5
	     :orientation (alignment :rear (the (face-normal-vector :top)))
	     :center (translate (the center) :up (half (the-child length))))
	     ))