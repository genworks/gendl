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


(in-package :gdl-user)

(defun dmapcar (function list)
  (dmapc function list)
  (mapcar function list))


(defun dmapc (function list)
  (let ((count -1))
    (mapc #'(lambda(element)
	      (let ((element element))
		(bt:make-thread function :name (format nil "thread ~a from dmapcar" (incf count)))))
	  list)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *compile-for-dgdl?* t))

(define-object dgdl-test (base-object)
  
  :input-slots
  ((length 10)
   (width 20)
   (height 30)
   (hole-radius 2 :settable)
   (hole-length 35)
   (quantity 3 :settable))
  
  
  :computed-slots 
  ((local-volumes (mapsend (list-elements (the local-drilled)) :volume))
   (remote-volumes (mapsend (list-elements (the remote-drilled)) :volume)))

  
  
  :objects
  (
   (remote-utils :type 'remote-object
		 :remote-type 'remote-utils
		 :sequence (:size (the remote-drilled number-of-elements))
		 :host (the (remote-drilled (the-child index)) host)
		 :port (the (remote-drilled (the-child index)) port))
   
   (local-drilled :type 'drilled-block
		  :sequence (:size (the quantity))
		  :pass-down (hole-length hole-radius))
   
   (remote-drilled :type 'remote-object
		   :sequence (:size (the quantity))
		   :remote-type 'drilled-block
		   :host "localhost"
		   :port (+ (the-child index) 9001)
		   ;;:port (if (oddp (the-child index)) 9001 9002)
		   :pass-down (length width height hole-length hole-radius)))
  
  :functions
  (
   
   (collect-volumes 
    (&key remote?)
    (dmapcar #'(lambda(drilled-block) (the-object drilled-block volume))
	     (list-elements (if remote? (the remote-drilled) (the local-drilled)))))
   
   
   (set-hole-radius! 
    (value)
    (the (set-slot! :hole-radius value)))))



(define-object remote-utils ()
  :functions
  ((clear-remotes 
    ()
    (clrhash gwl::*remote-objects-hash*))
   
   (global-gc
    ()
    (gc t))
   
   (reload 
    ()
    (gdl-user::g105))))



(define-object drilled-block (base-object)
  
  :input-slots 
  (length width height hole-radius hole-length
   
   (quantity 50 :settable))
  
  :computed-slots ((volume (sum-elements (the results) (the-element volume)))
		   
		   
		   (c-of-g (the results center-of-gravity))
		   
		   )
  
  :objects
  ((results :type 'subtracted-solid
	    ;;:sequence (:size (the quantity))
	    :pass-down (brep other-brep))
   
   (brep :type 'box-solid
	  :display-controls (list :color :green))

   
   (other-brep :type 'cylinder-solid
	       :radius (the hole-radius)
	       :length (the hole-length)
	       :display-controls (list :color :green)))
  
  :functions
  ((cl!
    ()
    (load (compile-file-if-needed "~/genworks/training/g105/examples/source/drilled-block.lisp")))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *compile-for-dgdl?* nil))
   

