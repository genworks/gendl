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

(define-object tower-example (slide-show-leaf)

  :computed-slots
  ((slide-data 
    `(
      (:title "Single Box" :bullet-points
	      ((:description 
		"Start with a single box:"
		:examples 
		((:define-object tower-box)))
	       
	       (:description 
		,(string-append "In ta2 with a trimetric view: "
				(with-cl-who-string ()
				  ((:img :src "/g102/images/single-box.png")))))
	       
	       ))
      
      
      (:title "Sequence of Boxes" :bullet-points
	      ((:description 
		"Now make 25 of them:"
		:examples 
		((:define-object tower-boxes)))
	       
	       
	       (:description 
		,(string-append "In ta2 with a trimetric view: "
				(with-cl-who-string ()
				  ((:img :src "/g102/images/tower-boxes.png")))))
	       
	       ))
      
      (:title "Stack of Boxes" :bullet-points
	      ((:description 
		"Now stack them on top of each other:"
		:examples 
		((:define-object stacked-boxes)))
	       
	       (:description 
		,(string-append "In ta2 with a trimetric view: "
				(with-cl-who-string ()
				  ((:img :src "/g102/images/stacked-boxes.png")))))
	       
	       ))
      
      (:title "Twisted Stack" :bullet-points
	      ((:description 
		"Now apply a uniform twist, about the global top (Z) axis, to each one:"
		:examples 
		((:define-object twisted-stacked-boxes)))
	       
	       (:description 
		,(string-append "In ta2 with a trimetric view: "
				(with-cl-who-string ()
				  ((:img :src "/g102/images/twisted-stacked-boxes-trimetric.png")))))
	       
	       (:description 
		,(string-append "In ta2 with a top view: "
				(with-cl-who-string ()
				  ((:img :src "/g102/images/twisted-stacked-boxes-top.png")))))))
	       
      
      
      )))

	      
  :functions
  ((strings-for-display
    nil
    "Tower Example")))
