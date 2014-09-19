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

(define-object assembly (slide-show-node)
  :input-slots
  ((title "G102: Gendl Quickstart")
   (slide-package (find-package :training-g102))
   (image-base-url "/g102/images/")
   (images-path *images-path*)
   (style-url "/static/gwl/style/top.css"))

   
  
  :objects
  (
   (introduction :type 'introduction)
   (objects :type 'objects)
   (geometry :type 'geometry)
   (tower-example :type 'tower-example)
   (user-interface :type 'user-interface)
   (outside-world :type 'outside-world)
   (debugging :type 'debugging)
   (future :type 'future)))



#|

To Do:

x Change font to Courier for command-line examples. 

x Add make-self example.

x Change hello-jake example to aero function.

x get rid of keyword symbols on messages where not needed e.g. hairy-calc.

x check ordering example on hairy-calc.

o check Ackermann example

x check City example

o check Primi Plane example

o prepare VRML format for cone and cylinder.


|#
