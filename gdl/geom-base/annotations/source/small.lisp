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


(define-object drawing-test (base-drawing)
  
  :input-slots (b-spline-surface paragraph
                
                view-width)
   
  :hidden-objects
  ((empty-view :type 'base-view
               :border-box? t)
    
    
   (paragraph-view :type 'base-view
                   :border-box? t
                   :length (the paragraph length)
                   :width (the paragraph width)
                   :corner (the surface-view (vertex :top :rear :right))
                   :front-margin 0
                   :left-margin 0
                   :objects (list (the paragraph)))
    
    
   (surface-view :type 'base-view
                 :length 300 
                 :width (the view-width)
                 :border-box? t
                 :projection-vector (getf *standard-views* :trimetric)
                 :corner (the (vertex :top :rear :left))
                 :objects (list (the b-spline-surface)))))



(define-object drawing-container (base-object)
  
  :input-slots
  ((view-width (half (the drawing width))))

  
  :objects
  ((drawing :type 'drawing-test
            :pass-down (b-spline-surface paragraph view-width)))
  
  :hidden-objects
  ((paragraph :type 'ron-paul-text
              :width (the view-width))

   
   (b-spline-surface :type 'surf::test-b-spline-surface)))

(define-object ron-paul-text (typeset-block)

  :functions
  ((content
    ()
    (typeset:compile-text
     ()
     (typeset:paragraph
      (:font-size 26)
      (typeset:put-string 
       "Congressman Ron Paul is the leading advocate for freedom in our nation's capital. 
As a member of the U.S. House of Representatives, Dr. Paul tirelessly works for limited 
constitutional government, low taxes, free markets, and a return to sound monetary policies. 
He is known among his congressional colleagues and his constituents for his consistent 
voting record. Dr. Paul never votes for legislation unless the proposed measure is expressly 
authorized by the Constitution. In the words of former Treasury Secretary William Simon, 
Dr. Paul is the one exception to the Gang of 535 on Capitol Hill."))))))




(define-object circle-drawing (base-drawing)
  
  :objects
  ((circle :type 'circle
           :radius 10
           :center (make-point 0 0 0))
   
   
   (main-view :type 'base-view
              :border-box? t
              :objects (list (the circle))))
  
  
  :functions
  ((pdf-out 
    ()
    (with-format (pdf "/tmp/try.pdf") (write-the cad-output)))
   
   (dxf-out
    ()
    (with-format (dxf "/tmp/try.dxf") (write-the cad-output)))))
