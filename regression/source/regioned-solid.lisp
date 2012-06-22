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

(define-object regioned-solid-test (base-object)
    :computed-slots
    ((points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
                    ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
                    ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
                    ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))

     (regression-test-data (mapcar #'(lambda(brep)
				       (append (multiple-value-list (the-object brep precise-properties))
					       (the-object brep %curves-to-draw%)
					       (the-object brep %lines-to-draw%)))
				   (list-elements (the regioned breps)))))
    
    :objects
    ((b-spline-capped-surface :type 'b-spline-surface
                              :control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) 
                                                      (the points-data))
			      :end-caps-on-brep? t
			      ;;:end-caps-on-brep? nil
                              :sew-and-orient-brep? t)

     (b-spline-solid :type 'brep
                     :%native-brep% (the b-spline-capped-surface brep %native-brep%))

     (rectangular-surface :type 'rectangular-surface
                          :width 10 :length 4 :height 0
                          :center (translate (the :center) :up (half (the b-spline-capped-surface :height))
                                             :right (half (the b-spline-capped-surface :width))))

     (subtracted :type 'subtracted-solid
		 :error-on-invalid? nil
                 :brep (the b-spline-solid)
                 :other-brep (the rectangular-surface))

     (regioned :type 'regioned-solid
               :brep (the subtracted))))

(register-test-definition 'regioned-solid-test)
