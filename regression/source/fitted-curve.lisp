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


(define-object fitted-curve-test (fitted-curve) 
  
   :computed-slots
   ((points (the circle (equi-spaced-points 20)))

    (regression-test-data (multiple-value-list (the b-spline-data))))


   :hidden-objects
   ((circle :type 'circle :radius 10)))


(define-object fitted-curve-test-2 (fitted-curve)
  :computed-slots
  (
   (regression-test-data (multiple-value-list (the b-spline-data)))
   
   (offset-plane-normal (the (face-normal-vector :rear)))
   
   (offset-distance 50)
   
   (sample-parameters (the (equi-spaced-parameters (length (the points)))))

   (sample-points (mapcar #'(lambda(param)
			      (the (point param))) (the sample-parameters)))
   
   (sample-tangents (mapcar #'(lambda(param)
				(the (tangent param))) (the sample-parameters)))

   (offset-points (mapcar #'(lambda(point tangent)
			      (translate-along-vector point 
						      (cross-vectors (the offset-plane-normal)
								     tangent 
								     )
						      (the offset-distance)))
			  (the sample-points)
			  (the sample-tangents)))
   
   (interpolant? nil)
   
   (points
    (list #(-23.773313 1.924594 -12.071495) #(-22.841344380918503 1.759739783016406 -14.181323943024323)
	  #(-21.841344380918503 2.4122139947099095 -14.729992667370558)
	  #(-20.841344380918507 3.3536863929530267 -14.923734875318676)
	  #(-19.841344380918382 4.176489135579425 -15.06870580103053)
	  #(-18.8413443809184 4.933777500473312 -15.202185050851405)
	  #(-17.841344380918425 5.631369721976035 -15.325168730248224)
	  #(-16.84134438091845 6.273970454148179 -15.438491956009626)
	  #(-15.841344380918468 6.865530149027527 -15.542832621612929)
	  #(-14.841344380918478 7.4093395241001785 -15.638764660742744)
	  #(-13.841344380918416 7.90815438686246 -15.72678137237866)
	  #(-12.84134438091844 8.364367152996033 -15.807284787392605)
	  #(-11.841344380918452 8.77996681216584 -15.880637519214574)
	  #(-10.841344380918452 9.156701239361068 -15.94713447461761)
	  #(-9.841344380918448 9.496052643404102 -16.007034859594235)
	  #(-8.841344380918441 9.799271723250216 -16.060568901021437)
	  #(-7.841344380918437 10.06746531437129 -16.107919555112268)
	  #(-6.841344380918429 10.301560000602155 -16.149248415345042)
	  #(-5.841344380918427 10.502317266184134 -16.184700135961652)
	  #(-4.841344380918503 10.670411264401993 -16.214379697901453)
	  #(-3.8413443809185037 10.806360061789343 -16.23838592168945)
	  #(-2.8413443809185037 10.91058281577113 -16.256793898099733)
	  #(-1.841344380918504 10.98341556965735 -16.269652365713124)
	  #(-0.8413443809185032 11.025055813332797 -16.277008983500682)
	  #(0.15865561908149672 11.035644948815769 -16.278878956118103)
	  #(1.1586556190814967 11.015215162491632 -16.275267609672653)
	  #(2.1586556190814967 10.963686970735495 -16.266171675614164)
	  #(3.1586556190814967 10.88092426467907 -16.251556448859898)
	  #(4.158655619081497 10.766677044427743 -16.23137766334388)
	  #(5.158655619081498 10.62057414933642 -16.205582068129765)
	  #(6.158655619081422 10.442180676557317 -16.174081243108095)
	  #(7.158655619081425 10.230925408456665 -16.136776521368255)
	  #(8.158655619081431 9.98609937607083 -16.09355384390509)
	  #(9.158655619081435 9.706896947892174 -16.04426088401215)
	  #(10.158655619081443 9.392342681411849 -15.98872728569834)
	  #(11.158655619081445 9.041274709139891 -15.926760084999787)
	  #(12.158655619081443 8.652384488847156 -15.858115816863863)
	  #(13.158655619081426 8.224088375742841 -15.782532484457718)
	  #(14.158655619081479 7.754593734167767 -15.699684864521048)
	  #(15.158655619081467 7.241780261514546 -15.609202831094764)
	  #(16.158655619081458 6.683139763207802 -15.510659940970609)
	  #(17.15865561908144 6.0757727707709925 -15.4035344243332)
	  #(18.15865561908141 5.416172867974698 -15.287229141145774)
	  #(19.158655619081383 4.700197459553999 -15.161011513585837)
	  #(20.158655619081493 3.9228092499506992 -15.024003655356676)
	  #(21.158655619081504 2.8543291359267267 -14.907649406809197)
	  #(22.158655619081493 2.1945491375308506 -14.612153140548436)
	  #(23.158655619081493 1.3990454658479357 -13.81473853600493)
	  #(23.158655619081497 3.582334935707711 -10.01826248221973)
	  #(22.1586556190815 5.522048829120016 -7.611015079357786)
	  #(21.158655619081493 7.0164193259761785 -5.362185530249421)
	  #(20.158655619081365 8.186295010076309 -3.22911563667378)
	  #(19.158655619081497 9.101180043581415 -1.1872530565531378)
	  #(18.1586556190815 9.805345072581082 0.7790079766743399)
	  #(17.158655619081493 10.328869164179935 2.680255854374877) 
	  #(16.1586556190815 10.69311633295447 4.52399447020087)
	  #(15.158655619081497 10.913663395166884 6.315732365008114)
	  #(14.158655619081499 11.000833212266512 8.060440470107608)
	  #(13.158655619081499 10.962715532275286 9.761699723427306)
	  #(12.158655619081495 10.806046718198589 11.421633325908237)
	  #(11.158655619081497 10.535640414347894 13.041755498169548)
	  #(10.158655619081497 10.154740933650833 14.623073252618243)
	  #(9.158655619081497 9.665224654008608 16.166157476531936)
	  #(8.158655619081497 9.067708489894999 17.67118070125608) 
	  #(7.158655619081498 8.361632005149593 19.13789695167163)
	  #(6.158655619081496 7.545182671632559 20.565658128968195)
	  #(5.158655619081498 6.6151967040761805 21.95337139481127)
	  #(4.158655619081496 5.567000905705767 23.299412040811102)
	  #(3.1586556190814967 4.394063811437401 24.60154203551763)
	  #(2.1586556190814967 2.8004311295619044 25.952393206193605)
	  #(1.1586556190814967 1.7448809507641723 26.783832751360602)
	  #(0.1586556190814967 1.4072204669238992 26.97844965398033)
	  #(-0.8413443809185032 1.88320122929102 26.83937875640717)
	  #(-1.8413443809185033 2.6484030661557543 26.213462741853462)
	  #(-2.8413443809185033 3.9944419897158476 25.00508851939353)
	  #(-3.8413443809185037 5.208693300048525 23.717475631772473)
	  #(-4.841344380918503 6.295670402753672 22.385089223556385)
	  #(-5.841344380918503 7.262575712824631 21.01039818449651)
	  #(-6.841344380918502 8.114638840836841 19.595204406997734)
	  #(-7.841344380918503 8.855482099043869 18.14075745206532)
	  #(-8.841344380918503 9.487327344116311 16.647843748061646)
	  #(-9.841344380918501 10.011121551590394 15.116837084142482)
	  #(-10.841344380918505 10.42662999568028 13.547692459702091)
	  #(-11.841344380918503 10.732381125360016 11.939970034589686)
	  #(-12.841344380918505 10.92557571834092 10.292811263403342)
	  #(-13.841344380918503 11.00194351680579 8.604861209227394)
	  #(-14.841344380918505 10.955402384973151 6.8742080479809085)
	  #(-15.841344380918503 10.77804067000267 5.0979483240850385)
	  #(-16.841344380918507 10.460862535510797 3.2712297214141297)
	  #(-17.841344380918503 9.989881559101038 1.388927349335879)
	  #(-18.841344380918507 9.345800488074271 -0.5557314586957092)
	  #(-19.841344380918507 8.501745502438851 -2.5722043019827843)
	  #(-20.841344380918137 7.418783713681504 -4.67420987442744)
	  #(-21.841344380918503 6.037220810436056 -6.882831204671871)
	  #(-22.841344380918507 4.257093771052562 -9.233367166492508) 
	  #(-23.773313 1.924594 -12.071495))))
  
  :objects
  ((points-disp :type 'points-display
		:points (the points))
   
   (offset-points-disp :type 'points-display
		       :points (the offset-points))
   
   (our-offset :type 'fitted-curve
	       :interpolant? nil
	       :degree 2
	       :points (the offset-points))
	       
   
   (compatibles :type 'compatible-curves
		:curve-list (list self (the our-offset)))

   
   (loft :type 'lofted-surface
	 :curves (list-elements (the compatibles curves)))
   
   (offset :type 'planar-offset-curve 
	   :curve-in self
	   :distance 50
	   :plane-normal (the (face-normal-vector :rear))
	   
	   ;;:closure :closed
	   ;;:closed? t
	   ;:check-continuity t
	   )))



(define-object fitted-curve-test-3 (fitted-curve) 
  
   :computed-slots
   ((regression-test-data (multiple-value-list (the b-spline-data)))
    (points (the circle (equi-spaced-points 20))))

   :hidden-objects
   ((spheres  :type 'sphere
		       :sequence (:size (length (the points)))
		       :radius 0.2
		       :center (nth (the-child :index) (the points))
		       :display-controls (list :color :blue-neon))
    
    (circle :type 'circle :radius 10)
    
    (view :type 'base-view
	  :width (* 5 72) :height (* 5 72)
	  :objects (cons self (list-elements (the spheres))))))


(define-object fitted-curve-test-4 (fitted-curve) 
  
  :input-slots 
  ((degree 1))
  
  :computed-slots
  ((regression-test-data (multiple-value-list (the b-spline-data)))
   (smooth-corners? nil)
   ;;(parameterization :chordlength)
   
   (points (list (make-point -1 -2 0)
		 (make-point -1 2 0)
		 (make-point 1 2 0)
		 (make-point 1 -2 0)
		 (make-point -1 -2 0)))))



(dolist (symbol '(fitted-curve-test fitted-curve-test-2 fitted-curve-test-3 fitted-curve-test-4))
 (register-test-definition  symbol))


(define-object fitted-curve-test-local-global (base-object)

  :computed-slots ((child-orientation (alignment :rear (rotate-vector-d (the (face-normal-vector :rear))
									45
									(the (face-normal-vector :right))))))


  :objects
  ((local-degree-1 :type 'fitted-curve-test-4
		   :orientation (the child-orientation)
		   :local? t)

   (local-degree-3 :type 'fitted-curve-test-4
		   :orientation (the child-orientation)
		   :local? t
		   :degree 3)

   (global-degree-1 :type 'fitted-curve-test-4
		    :orientation (the child-orientation))

   (global-degree-3 :type 'fitted-curve-test-4
		    :orientation (the child-orientation)
		    :degree 3)))
