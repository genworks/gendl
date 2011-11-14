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

(in-package :gdl-user)

(define-object font-drawing (base-drawing)
  
  :input-slots
  ((page-length 792) (page-width 612))
  
  :hidden-objects
  ((main-view :type 'base-view :objects (the font-test children)))
  
  :hidden-objects
  ((font-test :type 'font-test))
  
  :functions
  ((all () (the pdf) (the dxf))
   (pdf () (with-format (pdf "/tmp/try.pdf") (write-the-object self cad-output)))
   (dxf () (with-format (dxf "/tmp/lazydog.dxf") (write-the-object self cad-output)))))


(define-object font-test (base-object)
  
  :input-slots ((length (half 792)) (width 612) (character-size 50)
                (font "Courier"))
  
  :objects
  ((hey-now :type 'general-note 
            :text-x-scale 150
            :start (translate (the box (vertex :rear :top :left)) 
                              :front (the-child character-size))
            :character-size (the character-size) 
            :font (the font)
            :strings (list "hey now" "I'm Henk"
                           "The quick brown"
                           "fox jumps over"
                           "the lazy dog"
                           "mmmmmmmmmm"
                           "llllllllll"
                           "1111111111"
                           "nnnnnnnnnn"))

   (grid-lines-h :type 'line
                 :sequence (:size 5)
                 :start (translate (the box (vertex :rear :top :left)) :front
                                   (* (the-child index) (/ (the box length) 5)))
                 :end (translate (the-child start) :right (the box width)))
   
   (grid-lines-v :type 'line
                 :sequence (:size 5)
                 :start (translate (the box (vertex :rear :top :left)) :right
                                   (* (the-child index) (/ (the box width) 5)))
                 :end (translate (the-child start) :front (the box length)))
   
   (box :type 'box
        :length 700
        :width 500
        :center (translate (the center) :rear 450))))
   

(defun all () (the-object (make-object 'font-drawing) all))
