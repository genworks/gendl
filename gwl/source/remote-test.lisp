;;
;; Copyright 2002-2011 Genworks International 
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

(in-package :gwl-user)


(define-object remote-test (base-object)

  :computed-slots
  ((width 10 :settable)
   (length 20 :settable)
   (height 30 :settable)
   
   (remote-color (the remote-cylinder color))
   )
  
  :objects
  ((remote-cylinder :type 'remote-object
                    :remote-type 'cylinder
                    :host "localhost"
                    :port 9001
                    :radius 50
                    :length (the length))
   
   (cylinder :type 'cylinder
             :radius (the width))

   (remotes :type 'remote-object
            :sequence (:size 10)
            :remote-type 'box-with-cylinder
            :host "localhost"
            :port 9001
            :cylinder (the cylinder)
            :width (* (the width) (the-child index))
            :height (twice (the-child width))
            :length (the length))))

  

(define-object box-with-cylinder (box)
  
  :computed-slots
  ((color :blue :settable))
  
  :input-slots
  ((cylinder nil)))





(gwl:define-package :master)
(gwl:define-package :slave)

(in-package :master)

(define-object master ()
  :objects 
  ((slave :type 'remote-object
          :remote-type 'slave::slave
          :test-symbol 'gdl-user::heynow
          :host "localhost"
          :port 9000)))



(in-package :slave)

(define-object slave ()
  
  :input-slots (test-symbol)
  
  :computed-slots 
  ((message "hey now"))
  
  :functions
  ((test-print
    ()
    (print-messages test-symbol message))))
  
