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

(define-object session-06 (slide-show-leaf)

  :computed-slots
  ((slide-data '((:title "Exercises" :bullet-points
		  ((:description "Complete the provided <i>city</i> object
to accept a list of <i>zoning ordinances</i> which 
contain <i>proximity rules</i>, as in the skeleton code." )
		   (:description "Skeleton code is provided for download")
		   
		   (:description "<i>Extra Credit</i>:<br>

Plot a large <b>:red</b> sphere at the center of each violating neighbor")
		   
		   (:description "<i>Extra Credit 3</i>:<br>
Prepare a textual report listing the coordinates of the offending buildings and
other details of the violated rule."))))))

  :functions
  ((strings-for-display
    nil
    "Exercise with Proximity Rules")))
