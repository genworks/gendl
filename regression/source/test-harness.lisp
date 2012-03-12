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

(in-package gdl-lift-tests)

(define-object test-harness (base-object)

  :input-slots
  ((hide-tests? t :settable))
  
  :objects
  ((curves :type 'curve-tests :hide-tests? (the hide-tests?))
   (surfaces :type 'surface-tests :hide-tests? (the hide-tests?))
   (solids :type 'solid-tests :hide-tests? (the hide-tests?))
   (readers :type 'reader-tests :hide-tests? (the hide-tests?))
   (writers :type 'writer-tests :hide-tests? (the hide-tests?))
   (customer-parts :type 'customer-part-tests :hide-tests? (the hide-tests?))))

(define-object curve-tests (base-object)
  
  :input-slots
  ((hide-tests? t :settable))
  
  :computed-slots
  ((arc-curve-test-result (gdl-lift-utils::test-object (the arc-curve-test)))
   (b-spline-curve-test-result (gdl-lift-utils::test-object (the b-spline-curve-test)))
   (boxed-curve-test-result (gdl-lift-utils::test-object (the boxed-curve-test)))
   (brep-intersect-test-result (gdl-lift-utils::test-object (the brep-intersect-test)))
   (compatible-curves-test-result (gdl-lift-utils::test-object (the compatible-curves-test)))
   (composed-curve-test-result (gdl-lift-utils::test-object (the composed-curve-test)))
   (composed-curves-test-result (gdl-lift-utils::test-object (the composed-curves-test)))
   (decomposed-curves-test-result (gdl-lift-utils::test-object (the decomposed-curves-test)))
   (dropped-curve-test-result (gdl-lift-utils::test-object (the dropped-curve-test)))
   (elliptical-curve-test-result (gdl-lift-utils::test-object (the elliptical-curve-test)))
   (fitted-curve-test-result (gdl-lift-utils::test-object (the fitted-curve-test)))
   (fitted-curve-test-2-result (gdl-lift-utils::test-object (the fitted-curve-test-2)))
   (fitted-curve-test-3-result (gdl-lift-utils::test-object (the fitted-curve-test-3)))
   (global-filleted-polyline-curves-test-result (gdl-lift-utils::test-object (the global-filleted-polyline-curves-test)))
   (iso-curve-test-result (gdl-lift-utils::test-object (the iso-curve-test)))
   (linear-curve-test-result (gdl-lift-utils::test-object (the linear-curve-test)))
   (planar-offset-curve-test-result (gdl-lift-utils::test-object (the planar-offset-curve-test)))
   (planar-section-curve-test-result (gdl-lift-utils::test-object (the planar-section-curve-test)))
   (planar-section-curves-test-result (gdl-lift-utils::test-object (the planar-section-curves-test)))
   (planar-section-curves-test-2-result  (gdl-lift-utils::test-object (the planar-section-curves-test-2)))
   (projected-curve-test-result (gdl-lift-utils::test-object (the projected-curve-test)))
   (reparameterized-curve-test-result (gdl-lift-utils::test-object (the reparameterized-curve-test)))
   (transformed-curve-test-result (gdl-lift-utils::test-object (the transformed-curve-test)))
   (trimmed-curve-test-result (gdl-lift-utils::test-object (the trimmed-curve-test)))

   (all-curve-tests (mapcar #'gdl-lift-utils::test-object (the objects)))

   (all-curve-tests-passed (every #'(lambda (test) (equal test t)) (the all-curve-tests)))

   (objects (mapcar #'(lambda (part-keyword)
			(the (evaluate part-keyword)))
		    (the %object-keywords%))))

  :objects
  ((arc-curve-test :type 'gdl-lift-tests::arc-curve-test :hidden? (the hide-tests?))
   (b-spline-curve-test :type 'gdl-lift-tests::b-spline-curve-test :hidden? (the hide-tests?))
   (boxed-curve-test :type 'gdl-lift-tests::boxed-curve-test :hidden? (the hide-tests?))
   (brep-intersect-test :type 'gdl-lift-tests::brep-intersect-test :hidden? (the hide-tests?))
   (compatible-curves-test :type 'gdl-lift-tests::compatible-curves-test :hidden? (the hide-tests?))
   (composed-curve-test :type 'gdl-lift-tests::composed-curve-test :hidden? (the hide-tests?))
   (composed-curves-test :type 'gdl-lift-tests::composed-curves-test :hidden? (the hide-tests?))
   (decomposed-curves-test :type 'gdl-lift-tests::decomposed-curves-test :hidden? (the hide-tests?))
   (dropped-curve-test :type 'gdl-lift-tests::dropped-curve-test :hidden? (the hide-tests?))
   (elliptical-curve-test :type 'gdl-lift-tests::elliptical-curve-test :hidden? (the hide-tests?))
   (fitted-curve-test :type 'gdl-lift-tests::fitted-curve-test :hidden? (the hide-tests?))
   (fitted-curve-test-2 :type 'gdl-lift-tests::fitted-curve-test-2 :hidden? (the hide-tests?))
   (fitted-curve-test-3 :type 'gdl-lift-tests::fitted-curve-test-3 :hidden? (the hide-tests?))
   (global-filleted-polyline-curves-test :type 'gdl-lift-tests::global-filleted-polyline-curves-test :hidden? (the hide-tests?))
   (iso-curve-test :type 'gdl-lift-tests::iso-curve-test :hidden? (the hide-tests?))
   (linear-curve-test :type 'gdl-lift-tests::linear-curve-test :hidden? (the hide-tests?))
   (planar-offset-curve-test :type 'gdl-lift-tests::planar-offset-curve-test :hidden? (the hide-tests?))
   (planar-section-curve-test :type 'gdl-lift-tests::planar-section-curve-test :hidden? (the hide-tests?))
   (planar-section-curves-test :type 'gdl-lift-tests::planar-section-curves-test :hidden? (the hide-tests?))
   (planar-section-curves-test-2 :type 'gdl-lift-tests::planar-section-curves-test-2 :hidden? (the hide-tests?))
   (projected-curve-test :type 'gdl-lift-tests::projected-curve-test :hidden? (the hide-tests?))
   (reparameterized-curve-test :type 'gdl-lift-tests::reparameterized-curve-test :hidden? (the hide-tests?))
   (transformed-curve-test :type 'gdl-lift-tests::transformed-curve-test :hidden? (the hide-tests?))
   (trimmed-curve-test :type 'gdl-lift-tests::trimmed-curve-test :hidden? (the hide-tests?))))


(define-object surface-tests (base-object)
  
  :input-slots
  ((hide-tests? t :settable))
  
  :computed-slots
  ((b-spline-surface-test-result (gdl-lift-utils::test-object (the b-spline-surface-test)))
   (boxed-surface-test-result (gdl-lift-utils::test-object (the boxed-surface-test)))
   (compatible-surfaces-test-result (gdl-lift-utils::test-object (the compatible-surfaces-test)))
   (dual-blend-surface-test-result (gdl-lift-utils::test-object (the dual-blend-surface-test)))
   (edge-blend-surface-test-result (gdl-lift-utils::test-object (the edge-blend-surface-test)))
   (fitted-surface-test-result (gdl-lift-utils::test-object (the fitted-surface-test)))
   (fitted-surface-test-2-result (gdl-lift-utils::test-object (the fitted-surface-test-2)))
   (fitted-surface-test-3-result (gdl-lift-utils::test-object (the fitted-surface-test-3)))
   (general-dual-blend-surface-test-result (gdl-lift-utils::test-object (the general-dual-blend-surface-test)))
   (gdl-lift-tests::general-sweep-test-result (gdl-lift-utils::test-object (the general-sweep-test)))
   (gdl-lift-tests::joined-surfaces-test-result (gdl-lift-utils::test-object (the joined-surfaces-test)))
   (lofted-surface-test-result (gdl-lift-utils::test-object (the lofted-surface-test)))
   (lofted-surface-test-2-result (gdl-lift-utils::test-object (the lofted-surface-test-2)))
   (planar-contour-surface-test-result (gdl-lift-utils::test-object (the planar-contour-surface-test)))
   (planar-surface-test-result (gdl-lift-utils::test-object (the planar-surface-test)))
   (rectangular-surface-test-result (gdl-lift-utils::test-object (the rectangular-surface-test)))
   (revolved-surface-test-result (gdl-lift-utils::test-object (the revolved-surface-test)))
   (revolved-surfaces-test-result (gdl-lift-utils::test-object (the revolved-surfaces-test)))
   (ruled-surface-test-result (gdl-lift-utils::test-object (the ruled-surface-test)))
   (spherical-surface-test-result (gdl-lift-utils::test-object (the spherical-surface-test)))
   (transformed-surface-test-result (gdl-lift-utils::test-object (the transformed-surface-test)))
   (trimmed-surface-test-result (gdl-lift-utils::test-object (the trimmed-surface-test)))
   
   (all-surface-tests (mapcar #'gdl-lift-utils::test-object (the objects)))
   
   (all-surface-tests-passed (every #'(lambda (test) (equal test t)) (the all-surface-tests)))

   (objects (mapcar #'(lambda (part-keyword)
			(the (evaluate part-keyword)))
		    (the %object-keywords%))))
  
  :objects
  ((b-spline-surface-test :type 'gdl-lift-tests::b-spline-surface-test :hidden? (the hide-tests?))
   (boxed-surface-test :type 'gdl-lift-tests::boxed-surface-test :hidden? (the hide-tests?))
   (compatible-surfaces-test :type 'gdl-lift-tests::compatible-surfaces-test :hidden? (the hide-tests?))
   (dual-blend-surface-test :type 'gdl-lift-tests::dual-blend-surface-test :hidden? (the hide-tests?))
   (edge-blend-surface-test :type 'gdl-lift-tests::edge-blend-surface-test :hidden? (the hide-tests?))
   (fitted-surface-test :type 'gdl-lift-tests::fitted-surface-test :hidden? (the hide-tests?))
   (fitted-surface-test-2 :type 'gdl-lift-tests::fitted-surface-test-2 :hidden? (the hide-tests?))
   (fitted-surface-test-3 :type 'gdl-lift-tests::fitted-surface-test-3 :hidden? (the hide-tests?))
   (general-dual-blend-surface-test :type 'gdl-lift-tests::general-dual-blend-surface-test :hidden? (the hide-tests?))
   (gdl-lift-tests::general-sweep-test :type 'gdl-lift-tests::general-sweep-test :hidden? (the hide-tests?))
   (gdl-lift-tests::joined-surfaces-test :type 'gdl-lift-tests::joined-surfaces-test :hidden? (the hide-tests?))
   (lofted-surface-test :type 'gdl-lift-tests::lofted-surface-test :hidden? (the hide-tests?))
   (lofted-surface-test-2 :type 'gdl-lift-tests::lofted-surface-test-2 :hidden? (the hide-tests?))
   (planar-contour-surface-test :type 'gdl-lift-tests::planar-contour-surface-test :hidden? (the hide-tests?))
   (planar-surface-test :type 'gdl-lift-tests::planar-surface-test :hidden? (the hide-tests?))
   (rectangular-surface-test :type 'gdl-lift-tests::rectangular-surface-test :hidden? (the hide-tests?))
   (revolved-surface-test :type 'gdl-lift-tests::revolved-surface-test :hidden? (the hide-tests?))
   (revolved-surfaces-test :type 'gdl-lift-tests::revolved-surfaces-test :hidden? (the hide-tests?))
   (ruled-surface-test :type 'gdl-lift-tests::ruled-surface-test :hidden? (the hide-tests?))
   (spherical-surface-test :type 'gdl-lift-tests::spherical-surface-test :hidden? (the hide-tests?))
   (transformed-surface-test :type 'gdl-lift-tests::transformed-surface-test :hidden? (the hide-tests?))
   (trimmed-surface-test :type 'gdl-lift-tests::trimmed-surface-test :hidden? (the hide-tests?))))

(define-object solid-tests (base-object)
  
  :input-slots
  ((hide-tests? t :settable))
  
  :computed-slots
  ((blended-solid-test-result (gdl-lift-utils::test-object (the blended-solid-test)))
   (boolean-test-result (gdl-lift-utils::test-object (the boolean-test)))
   (box-solid-test-result (gdl-lift-utils::test-object (the box-solid-test)))
   (cone-solid-test-result (gdl-lift-utils::test-object (the cone-solid-test)))
   (cylinder-solid-test-result (gdl-lift-utils::test-object (the cylinder-solid-test)))
   (extruded-solid-test-result (gdl-lift-utils::test-object (the extruded-solid-test)))
   (intersected-solid-test-result (gdl-lift-utils::test-object (the intersected-solid-test)))
   (merged-solid-test-result (gdl-lift-utils::test-object (the merged-solid-test)))
   (poly-brep-test-result (gdl-lift-utils::test-object (the poly-brep-test)))
   (regioned-solid-test-result (gdl-lift-utils::test-object (the regioned-solid-test)))
   (separated-solid-test-result (gdl-lift-utils::test-object (the separated-solid-test)))
   (sewn-solid-test-result (gdl-lift-utils::test-object (the sewn-solid-test)))
   (subtracted-solid-test-result (gdl-lift-utils::test-object (the subtracted-solid-test)))
   (swept-solid-test-result (gdl-lift-utils::test-object (the swept-solid-test)))
   (transformed-solid-test-result (gdl-lift-utils::test-object (the transformed-solid-test)))
   (united-solid-test-result (gdl-lift-utils::test-object (the united-solid-test)))
   
   (all-solid-tests (mapcar #'gdl-lift-utils::test-object (the objects)))
   
   (all-solid-tests-passed (every #'(lambda (test) (equal test t)) (the all-solid-tests)))

   (objects (mapcar #'(lambda (part-keyword)
			(the (evaluate part-keyword)))
		    (the %object-keywords%))))

  :objects
  ((blended-solid-test :type 'gdl-lift-tests::blended-solid-test :hidden? (the :hide-tests?))
   (boolean-test :type 'gdl-lift-tests::boolean-test :hidden? (the :hide-tests?))
   (box-solid-test :type 'gdl-lift-tests::box-solid-test :hidden? (the :hide-tests?))
   (cone-solid-test :type 'gdl-lift-tests::cone-solid-test :hidden? (the :hide-tests?))
   (cylinder-solid-test :type 'gdl-lift-tests::cylinder-solid-test :hidden? (the :hide-tests?))
   (extruded-solid-test :type 'gdl-lift-tests::extruded-solid-test :hidden? (the :hide-tests?))
   (intersected-solid-test :type 'gdl-lift-tests::intersected-solid-test :hidden? (the :hide-tests?))
   (merged-solid-test :type 'gdl-lift-tests::merged-solid-test :hidden? (the :hide-tests?))
   (poly-brep-test :type 'gdl-lift-tests::poly-brep-test :hidden? (the :hide-tests?))
   (regioned-solid-test :type 'gdl-lift-tests::regioned-solid-test :hidden? (the :hide-tests?))
   (separated-solid-test :type 'gdl-lift-tests::separated-solid-test :hidden? (the :hide-tests?))
   (sewn-solid-test :type 'gdl-lift-tests::sewn-solid-test :hidden? (the :hide-tests?))
   (subtracted-solid-test :type 'gdl-lift-tests::subtracted-solid-test :hidden? (the :hide-tests?))
   (swept-solid-test :type 'gdl-lift-tests::swept-solid-test :hidden? (the :hide-tests?))
   (transformed-solid-test :type 'gdl-lift-tests::transformed-solid-test :hidden? (the :hide-tests?))
   (united-solid-test :type 'gdl-lift-tests::united-solid-test :hidden? (the :hide-tests?))))

(define-object reader-tests (base-object)
  
  :input-slots
  ((hide-tests? t :settable))
  
  :computed-slots
  ((iges-reader-test-result (gdl-lift-utils::test-object (the iges-reader-test)))
   (native-reader-test-result (gdl-lift-utils::test-object (the native-reader-test)))
   (step-reader-test-result (gdl-lift-utils::test-object (the step-reader-test)))

   (all-reader-tests (mapcar #'gdl-lift-utils::test-object (the objects)))
   
   (all-reader-tests-passed (every #'(lambda (test) (equal test t)) (the all-reader-tests)))

   (objects (mapcar #'(lambda (part-keyword)
			(the (evaluate part-keyword)))
		    (the %object-keywords%))))
  :objects
  ((iges-reader-test :type 'gdl-lift-tests::iges-reader-test :hidden? (the :hide-tests?))
   (native-reader-test :type 'gdl-lift-tests::native-reader-test :hidden? (the :hide-tests?))
   (step-reader-test :type 'gdl-lift-tests::step-reader-test :hidden? (the :hide-tests?))))

(define-object writer-tests (base-object)

  :input-slots
  ((hide-tests? t :settable))

  :computed-slots
  ((iges-writer-test-result (gdl-lift-utils::test-object (the iges-writer-test)))
   (step-writer-test-result (gdl-lift-utils::test-object (the step-writer-test)))


   (all-writer-tests (mapcar #'gdl-lift-utils::test-object (the objects)))
   
   (all-writer-tests-passed (every #'(lambda (test) (equal test t)) (the all-writer-tests)))

   (objects (mapcar #'(lambda (part-keyword)
			(the (evaluate part-keyword)))
		    (the %object-keywords%))))

  :objects
  ((iges-writer-test :type 'gdl-lift-tests::iges-writer-test :hidden? (the :hide-tests?))
   (step-writer-test :type 'gdl-lift-tests::step-writer-test :hidden? (the :hide-tests?))))

(define-object customer-part-tests (base-object)
  
  :input-slots
  ((hide-tests? t :settable))
  
  :computed-slots
  ((pegasus-regioned-solid-test-result (gdl-lift-utils::test-object (the pegasus-regioned-solid-test)))
   (wing-assembly-test-result (gdl-lift-utils::test-object (the wing-assembly-test)))

   (all-customer-part-tests (mapcar #'gdl-lift-utils::test-object (the objects)))
   
   (all-customer-part-tests-passed (every #'(lambda (test) (equal test t)) (the all-customer-part-tests)))

   (objects (mapcar #'(lambda (part-keyword)
			(the (evaluate part-keyword)))
		    (the %object-keywords%))))

:objects
  ((pegasus-regioned-solid-test :type 'gdl-lift-tests::pegasus-regioned-solid-test :hidden? (the :hide-tests?))
   (wing-assembly-test :type 'gdl-lift-tests::wing-assembly-test :hidden? (the :hide-tests?))))



(define-object tasty-test-harness (tasty:assembly)  
  :computed-slots
  ((root-object-type 'test-harness)))


(gwl:publish-gwl-app "/test-harness" "gdl-user::tasty-test-harness")

