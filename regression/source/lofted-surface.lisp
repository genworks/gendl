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


(define-object lofted-surface-test (lofted-surface)
  
  :computed-slots
  ((curves (list (the curve-1) (the curve-2) (the curve-3) (the curve-4)))
   
   (regression-test-data (multiple-value-list (the b-spline-data))))
  
  :objects
  ((curve-1 :type 'b-spline-curve
	    :display-controls (list :color :red :line-thickness 3)
	    :control-points (list (make-point 0 0 0)
				  (make-point 1 1 0)
				  (make-point 0 1 0)
				  (make-point 0 0 0)))
   
   (curve-2 :type 'b-spline-curve
	    :display-controls (list :color :red :line-thickness 3)
	    :control-points (list (make-point 0 0 1)
				  (make-point -1 1 1)
				  (make-point 0 1 1)
				  (make-point 0 0 1)))
   
   (curve-3 :type 'b-spline-curve
	    :display-controls (list :color :red :line-thickness 3)
	    :control-points (list (make-point 0 0 2)
				  (make-point -1 -1 2)
				  (make-point 0 -1 2)
				  (make-point 0 0 2)))
   
   (curve-4 :type 'b-spline-curve
	    :display-controls (list :color :red :line-thickness 3)
	    :control-points (list (make-point 0 0 3)
				  (make-point 1 -1 3)
				  (make-point 0 -1 3)
				  (make-point 0 0 3)))))


(register-test-definition 'lofted-surface-test)


(define-object lofted-surface-test-2 (lofted-surface)
  :input-slots
  ((span 0.5 :settable)
   (profile-degree 3 :settable)
   (c-root-profile 0.3 :settable)
   (c-tip-profile 0.24 :settable)

   (airfoil-data '((1.0 0 0.0) 
		   (0.9994161 0 0.0013419) (0.9976658 0 0.001587) (0.9947532 0 0.0019938) 
		   (0.990685 0 0.0025595) (0.9854709 0 0.0032804)
		   (0.9791229 0 0.0041519) (0.9716559 0 0.0051685) (0.9630873 0 0.0063238) 
		   (0.9534372 0 0.0076108) (0.942728 0 0.0090217) (0.9309849 0 0.0105485)
		   (0.9182351 0 0.0121823) (0.9045085 0 0.0139143) (0.8898372 0 0.0157351) 
		   (0.8742554 0 0.0176353) (0.8577995 0 0.0196051) (0.8405079 0 0.0216347)
		   (0.8224211 0 0.0237142) (0.8035813 0 0.0258337) (0.7840324 0 0.0279828)
		   (0.7638202 0 0.0301515) (0.7429917 0 0.0323294) (0.7215958 0 0.0345058)
		   (0.6996823 0 0.03667) (0.6773025 0 0.0388109) (0.6545085 0 0.0409174) 
		   (0.6313537 0 0.0429778) (0.6078921 0 0.0449802) (0.5841786 0 0.0469124)
		   (0.5602683 0 0.0487619) (0.5362174 0 0.0505161) (0.5120819 0 0.052162) 
		   (0.4879181 0 0.0536866) (0.4637826 0 0.0550769) (0.4397317 0 0.05632)
		   (0.4158215 0 0.0574033) (0.3921079 0 0.0583145) (0.3686463 0 0.0590419)
		   (0.3454915 0 0.0595747) (0.3226976 0 0.0599028) (0.3003177 0 0.0600172)
		   (0.2784042 0 0.0599102) (0.2570083 0 0.0595755) (0.2361799 0 0.0590081)
		   (0.2159676 0 0.0582048) (0.1964187 0 0.057164) (0.1775789 0 0.0558856)
		   (0.1594921 0 0.0543715) (0.1422005 0 0.0526251) (0.1257446 0 0.0506513)
		   (0.1101628 0 0.0484567) (0.0954915 0 0.0460489) (0.0817649 0 0.0434371)
		   (0.0690152 0 0.040631) (0.057272 0 0.0376414) (0.0465628 0 0.0344792)
		   (0.0369127 0 0.0311559) (0.0283441 0 0.0276827) (0.0208771 0 0.0240706)
		   (0.0145291 0 0.02033) (0.0093149 0 0.0164706) (0.0052468 0 0.0125011) 
		   (0.0023342 0 0.0084289) (5.839e-4 0 0.0042603) (0.0 0 0.0)
		   (5.839e-4 0 -0.0042603 )(0.0023342 0 -0.0084289 )(0.0052468 0 -0.0125011 )
		   (0.0093149 0 -0.0164706 )(0.0145291 0 -0.02033 )(0.0208771 0 -0.0240706 )
		   (0.0283441 0 -0.0276827 )(0.0369127 0 -0.0311559 )(0.0465628 0 -0.0344792 )
		   (0.057272 0 -0.0376414 )(0.0690152 0 -0.040631 )(0.0817649 0 -0.0434371 )
		   (0.0954915 0 -0.0460489 )(0.1101628 0 -0.0484567 )(0.1257446 0 -0.0506513 )
		   (0.1422005 0 -0.0526251 )(0.1594921 0 -0.0543715 )(0.1775789 0 -0.0558856 )
		   (0.1964187 0 -0.057164 )(0.2159676 0 -0.0582048 )(0.2361799 0 -0.0590081 )
		   (0.2570083 0 -0.0595755 )(0.2784042 0 -0.0599102 )(0.3003177 0 -0.0600172 )
		   (0.3226976 0 -0.0599028 )(0.3454915 0 -0.0595747 )(0.3686463 0 -0.0590419 )
		   (0.3921079 0 -0.0583145 )(0.4158215 0 -0.0574033 )(0.4397317 0 -0.05632 )
		   (0.4637826 0 -0.0550769 )(0.4879181 0 -0.0536866 )(0.5120819 0 -0.052162 )
		   (0.5362174 0 -0.0505161 )(0.5602683 0 -0.0487619 )(0.5841786 0 -0.0469124 )
		   (0.6078921 0 -0.0449802 )(0.6313537 0 -0.0429778 )(0.6545085 0 -0.0409174 )
		   (0.6773025 0 -0.0388109 )(0.6996823 0 -0.03667 )(0.7215958 0 -0.0345058 )
		   (0.7429917 0 -0.0323294 )(0.7638202 0 -0.0301515 )(0.7840324 0 -0.0279828 )
		   (0.8035813 0 -0.0258337 )(0.8224211 0 -0.0237142 )(0.8405079 0 -0.0216347 )
		   (0.8577995 0 -0.0196051 )(0.8742554 0 -0.0176353 )(0.8898372 0 -0.0157351 )
		   (0.9045085 0 -0.0139143 )(0.9182351 0 -0.0121823 )(0.9309849 0 -0.0105485 )
		   (0.942728 0 -0.0090217 )(0.9534372 0 -0.0076108 )(0.9630873 0 -0.0063238 )
		   (0.9716559 0 -0.0051685 )(0.9791229 0 -0.0041519 )(0.9854709 0 -0.0032804 )
		   (0.990685 0 -0.0025595 )(0.9947532 0 -0.0019938 )(0.9976658 0 -0.001587 )
		   (0.9994161 0 -0.0013419 )(1.0 0 0.0 ))))

  
  :computed-slots
  ((airfoil-points (mapcar #'apply-make-point (the airfoil-data)))
   (regression-test-data (multiple-value-list (the b-spline-data)))
   ;;(synchronized? t)
   (tolerance (* (the span) 1.0e-5))
   (curves (list (the profile-root) (the profile-tip))))
  

  :objects
  (
   (p-curve-in  :type 'b-spline-curve 
		:hidden? t
		:degree (the profile-degree)
		:control-points (the airfoil-points))
   
   #+nil
   (p-curve-in  :type 'fitted-curve
		:hidden? t
		:degree (the profile-degree)
		:points (the airfoil-points)
		:interpolant? t

		)
   
   (profile-root :type 'boxed-curve
		 :hidden? t
		 :center (make-point 0 0 0 )
		 :scale (the c-root-profile)
		 :curve-in (the p-curve-in ))


   (profile-tip :type 'boxed-curve
		:hidden? t
		:center (translate (the center)
				   :right  (/ (-  (the c-root-profile) 
						  (the c-tip-profile))
					      2)
				   :rear (the span))
		:scale (the c-tip-profile)
		:curve-in (the p-curve-in))))


(register-test-definition 'lofted-surface-test-2)

;;
;; Memory leak results:: 2 MB for 1000 iterations.
;;
