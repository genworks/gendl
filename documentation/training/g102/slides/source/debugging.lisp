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

(define-object debugging (slide-show-leaf)

  :computed-slots
  ((strings-for-display "Debugging")
   
   (slide-data 
    `((:title 
       "Basic Tips"
       :bullet-points
       ((:description 
         "<a href=\"http://www.youtube.com/user/GDLTutorials\">GDLTutorials</a> 
video for Compile Errors")
        (:description "Stay tuned for video on Runtime Errors")
        (:description "Basic debugging technique is command-line evaluation")
        (:description "When some message or geometry object gives an error, 
evaluate the messages or objects on which it depends")
        (:description "Continue stepping backwards, doing evaluations, 
until you isolate the error")))))))


