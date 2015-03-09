;;
;; Copyright 2002-2015 Genworks International 
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

(in-package :surf)

(#+allegro 
 excl:without-package-locks #-allegro progn


 (define-object-amendment stitched-solid ()
  
   :input-slots
   (("Number. The tolerance to use for creating the brep and for sewing. Larger number is looser tolerance 
and more likely to lead to success. Default is 0.0 which uses the SMLib defaults."
     tolerance 0.0))

  
   :computed-slots
   ((%native-brep% (progn (when (or (the trimmed-surfaces) (the proper-faces))
			    (error "stitched-solid is currently only implemented for plain untrimmed surfaces"))
			  (make-stitched-solid-brep *geometry-kernel* 
						    :tolerance (the tolerance)
						    :surfaces (the plain-surfaces)
						    :proper-faces nil)))))

 (define-object-amendment boolean-merge ()

   :input-slots
   (("Boolean. Indicates whether we should try to sew and orient the
resulting brep. This defaults to t for merge operation and nil otherwise."  
     sew-and-orient? (eql (the operation) :merge)))

  
   :computed-slots
   ((%merge-and-brep 
     (progn
       (when (or (> (the first-brep regions number-of-elements) 2)
		 (not (every #'(lambda(number) (<= number 2))
			     (mapcar #'(lambda(object) (the-object object regions number-of-elements))
				     (the rest-breps)))))
        
	 (let ((message (format nil "~%~%in ~a -~%~%Attempting booleans where brep has ~~a non-infinite regions.~%"
				(cons 'the (reverse (the root-path))))))
	   (if (the allow-multiple-regions?)
	       (warn message 
		     (1- (the first-brep regions number-of-elements)))
	       (error message 
		      (1- (the first-brep regions number-of-elements))))))
      
       (let ((count -1) (length (length (the rest-breps)))
	     merge-container current-breps (current-brep (the first-brep %native-brep%)))
        
	 (dolist (other-brep (the rest-breps))
	   (incf count)
	   (when *debug?*
	     (print-variables (the root-path) 
			      (the first-brep root-path)
			      (the-object other-brep root-path)))
          
	   (when *debug?* (print-variables (the approximation-tolerance) (the angle-tolerance)))
          
	   (setq merge-container (make-merge-container *geometry-kernel* 
						       current-brep 
						       (the-object other-brep %native-brep%)
                                                      
						       (the approximation-tolerance) 
						       (the angle-tolerance)))
          
	   (ecase (the operation)
	     ((:difference :union :intersection)
	      (setq current-brep 
		    (do-boolean-merge-operation *geometry-kernel* merge-container 
						(the operation) (the make-manifold?)
						:sew-and-orient? (the sew-and-orient?))))
                          
	     ((:merge)
	     
	      (let ((try-manifold? (and (the make-manifold?) (= count (1- length)))))
		(setq current-brep (do-boolean-merge-operation  *geometry-kernel* merge-container
								(the operation) try-manifold?
								:make-manifold? try-manifold?
								:sew-and-orient? (the sew-and-orient?)))))
                           
	     (:extract_separate
	      (setq current-breps (do-boolean-separate-operation *geometry-kernel* 
				    merge-container (the make-manifold?) (the sew-and-orient?))))))

	 (ecase (the operation)
	   ((:difference :union :intersection :merge) 
	    (list :merge-container merge-container :native-brep current-brep))
                        
	   (:extract_separate (list :merge-container merge-container :native-breps current-breps)))))))))

