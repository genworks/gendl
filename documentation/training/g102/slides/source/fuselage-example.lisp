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

(in-package :training-g102)

(define-object fuselage-example (slide-show-leaf)

  :computed-slots
  ((slide-data 
    `(
      (:title "Single cylinder" 
	      :bullet-points
	      ((:description 
		"Start with a single cylinder:"
		:examples 
		((:code (setq *brep-isos-default* '(:n-u 8 :n-v 8)))
		 (:define-object fuselage-cylinder)))
	       
	       
	       (:description 
		,(string-append "In ta2 with a trimetric view: "
				(with-cl-who-string ()
				  ((:img :src "/g102/images/fuselage-cylinder.png")))))))

      (:title "Single cone"
	      :bullet-points
	       ((:description 
		"Now add a single cone:"
		:examples 
		((:define-object fuselage-cone)))
	       
	       (:description 
		,(string-append "In ta2 with a trimetric view: "
				(with-cl-who-string ()
				  ((:img :src "/g102/images/fuselage-cone.png")))))))

      (:title "Excercise: Construct a fuselage using cylinder and cones" 
	      :bullet-points
	      ((:description 
		"Create a class that resembles an airliner's fuselage, using cylinder-solid and cone-solid. To do this, set up a define-object that has these primitives as children (in it's ':objects'-slot)" )
	      (:description 
		"Make use of the ':center' input-slots on both the cylinder-solid and cone-solid to position them in space" )
	    (:description 
	     "Add input-slots to your fuselage class that allow total length and diameter to be specified. Make sure that any possible combination of input values results in a proper fuselage (i.e. the individual children must always be connected)" )
	    (:description 
		,(string-append "You should end up with something like this:"
				(with-cl-who-string ()
				  ((:img :src "/g102/images/fuselage.png")))))
	      ))))))
      
