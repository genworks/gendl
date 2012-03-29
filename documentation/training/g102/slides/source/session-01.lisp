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

(define-object introduction (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Introduction")
   
   (slide-data `((:title "Goals" :bullet-points
                         ((:description "Spark an interest in using a Generative approach for solving Engineering problems")
                          (:description "Qualify you all to Read, Understand, and Develop GDL applications")
                          (:description "Give you the eyes to see situations where Generative technology and GDL will be a good fit")
                          ))

                 (:title "Topics Covered in G102" :bullet-points
                  ((:description "Objects in GDL and the <span class=gdl-operator>define-object</span> operator") 
                   (:description ,(with-output-to-string (ss)
                                    (html-stream 
                                     ss
                                     "Geometry and Coordinate Systems"
                                     (:ul (:li "Wireframe")
                                          (:li "Surfaces/Curves")
                                          (:li "Solids")))))
                   (:description "Custom User Interfaces")
                   (:description "Interacting with the Outside World")
                   (:description "Debugging and Performance")))

                 (:title "What is GDL?" :bullet-points
                  ((:description "A Dynamic, Declarative, Object-oriented 
language environment embedded in Common Lisp")
                   (:description "A technology which allows any type of engineer to define 
problems and their solutions using an intuitive, straightforward structure")
                   (:description "A cross-platform server solution for web application deployment")))
                 
                 (:title "What are Dynamic, Declarative, Object-oriented?" 
                         :bullet-points
                         ((:description "The system is <i>Dynamic</i> vs. <i>Static</i> on several levels:
<ul>
<li>Change code (definitions) and compile/load without restarting your model
<li>Define variables/slots without the need for predefined data types (dynamic vs. static typing)
<li>Modify slot values <i>and even object types</i> at runtime
</ul>")
                          (:description "Definitions in GDL have no \"begin\" or \"end\" - they are specified <i>declaratively</i>, in whatever order is convenient.")
                          (:description "All standard OO features plus several advanced ones:
<ul>
<li> Clear separation between the <i>definition</i> of an object and an <i>instance</i> (the object itself)</li>
<li> Multiple Inheritance: the ability for one definition to have several <i>superclasses</i></li>
<li> Message-passing (object-centric) and Generic Function (method-centric) styles
<li> Meta-object protocol (MOP): definitions themselves are also first-class objects</li>
<li> Reflection: ability for an object to generate documentation and source code for itself</li>
</ul>")))
                 
                 (:title "A Path of Discovery: GDL as a Learning Tool"
                         :bullet-points 
                         ((:description "Humans learn best through Action and Discovery")
                          (:description "Applies to learning GDL itself")
                          (:description "Applies to learning about your own engineering domain")))))))


