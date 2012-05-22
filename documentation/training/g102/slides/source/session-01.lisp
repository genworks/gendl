;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (Gendl).
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

(define-object introduction (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Introduction")
   
   (slide-data `((:title "Goals" :bullet-points
                         ((:description "Spark an interest in using a Generative approach for solving Engineering problems")
                          (:description "Qualify you all to Read, Understand, and Develop Gendl applications")
                          (:description "Give you the eyes to see situations where Generative technology and Gendl will be a good fit")
                          ))

                 (:title "Topics Covered in G102" :bullet-points
                  ((:description ,(with-cl-who-string 
				   ()
				   "Objects in Gendl and the " ((:span :class "gdl-operator") "define-object") " operator"
				   (:ul (:li "Debugging (basic)")
					(:li "Inspection & Visualization (tasty)"))))

                   (:description ,(with-output-to-string (ss)
                                    (html-stream 
                                     ss
                                     "Geometry and Coordinate Systems"
                                     (:ul (:li "Points")
					  (:li "Curves")
                                          (:li "Surfaces")
                                          (:li "Solids")))))
                   (:description "Custom User Interfaces (optional)")
                   (:description "Interacting with the Outside World")
                   (:description "Debugging and Performance (detailed)")))

                 (:title "What is Gendl?" :bullet-points
                  ((:description "A Dynamic, Declarative, Object-oriented 
language environment embedded in Common Lisp")
                   (:description "A technology which allows any type of engineer to define 
problems and their solutions using an intuitive, straightforward structure")
                   (:description "A cross-platform server solution for web application deployment")))
                 
                 
                 (:title "A Path of Discovery: Gendl as a Learning Tool"
                         :bullet-points 
                         ((:description "Humans learn best through Action and Discovery")
                          (:description "Applies to learning Gendl itself")
                          (:description "Applies to learning about your own engineering domain")))))))


