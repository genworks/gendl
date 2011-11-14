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

(define-object iso-curve (curve)
  
  :documentation (:description "Represents an exact iso curve on the given surface in given direction at given parameter value."
                  
                  :examples "<pre>
 (in-package :gdl-surf-user)

 (define-object iso-curve-example (base-object)
   :computed-slots
   ((parameter 0.5))
  
   :objects
   ((surface :type 'surf::test-b-spline-surface)
    (iso-curve :type 'iso-curve
               :strings-for-display \"Iso at 0.5\"
               :display-controls (list :color :red :line-thickness 3)
               :surface (the surface)
               :parameter (the parameter))))


  (generate-sample-drawing :object-roots (list (make-object 'iso-curve-example))
                           :projection-direction (getf *standard-views* :trimetric))

                  
 </pre>")
  
  :input-slots
  ("GDL object of type surface. The surface on which you want an iso curve." surface 
   
   "Number. The u or v will be fixed at this value. 
This should be between the min and max values for the surface in the given direction. Note that
you can check the min and max for a surface with (the u-min), (the u-max), (the v-min), or (the v-max)." 
   parameter 
 
   
   ("Keyword symbol, one of :u or :v. This is the parameter direction which will be fixed to the 
parameter value given.

Default is :u."
    fixed-parameter :v)
   
   
   ("Keyword symbol, one of :u or :v. This is the direction of the iso-curve. The other parameter will 
 be fixed on the surface at the given parameter value. 

:note The name of this input-slot is somewhat anti-intuitive. If you want to 
specify the U parameter, you give :v here, and if you want to specify 
the V parameter, you give :u here. For an intuitive input which matches
the parameter value that you are giving, use fixed-parameter instead of this input-slot.

Default is (ecase (the fixed-parameter) (:u :v) (:v :u))." 
    u-or-v (ecase (the fixed-parameter) (:u :v) (:v :u))))

  
  
  :computed-slots
  ((native-curve (funcall (read-from-string "smlib::get-iso-curve") (the surface) (the parameter) (the u-or-v)))
   
   ("List of GDL surfaces. For iso curve, this will contain the single surface on which the iso curve lies." 
    on-surfaces (list (the surface))))
  
  :hidden-objects
  ((""
    uv-curve :type 'uv-iso-curve
             :pass-down (surface parameter u-or-v))))


(define-object uv-iso-curve ()
  :input-slots (surface parameter u-or-v)
  
  :functions
  (("2D point. The UV surface representation at the given parameter value.

:arguments (parameter \"Number. The parameter value you want.\")"
    point 
    (parameter)
    (ecase (the u-or-v)
      (:u (make-point (the parameter) parameter))
      (:v (make-point parameter (the parameter)))))))



   
  
