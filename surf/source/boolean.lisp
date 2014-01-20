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

(in-package :surf)


(define-object boolean-tolerance-mixin ()
  :input-slots
  (("Number. Defaults to *approximation-tolerance-factor*. This is multiplied by  the minimum of the 
adaptive-tolerance of any of the input breps to produce the approximation-tolerance-adaptive."
    approximation-tolerance-factor (or *approximation-tolerance-factor* 5))
   

   ("Number. Defaults to the minimum of the adaptive-tolerance of any of the input breps, 
multiplied by the approximation-tolerance-factor, rounded to nearest multiple of 
tenths (e.g. it will be 0.01, 0.001, 0.001), however if this evaluates as zerop, 
*3d-approximation-tolerance-default* will be used instead."
    approximation-tolerance-adaptive 
    (* (the approximation-tolerance-factor)
       (let ((first-tolerance 
              (the first-brep adaptive-tolerance)))
         (if (the rest-breps)
             (max first-tolerance 
                  (apply #'max
                         (mapsend (the rest-breps)
                                  :adaptive-tolerance)))
	     first-tolerance))))
   
   
   ("Number. Defaults to *3d-approximation-tolerance-default* if non-nil. If this value is nil,
then this defaults to the approximation-tolerance-adaptive." 
    approximation-tolerance (or *boolean-operation-tolerance-default*
                                (the approximation-tolerance-adaptive))))

  :computed-slots
  ((first-brep (let ((brep 
                      (cond ((the brep) (the brep))
                            ((consp (the other-brep))
                             (first (the other-brep)))
                            (t (error "If no brep is given, other-brep must be a list of breps.")))))
                 (the (ensure-brep brep))))
   
   
   (rest-breps (let ((breps (cond ((the brep)
                                   (ensure-list (the other-brep)))
                                  (t (rest (ensure-list (the other-brep)))))))
                 (mapcar #'(lambda(brep)
                             (the (ensure-brep brep))) breps))))

  :functions
  ((ensure-brep (brep)
		(if (typep brep 'brep)
		    brep
		    (let ((brep (ignore-errors (the-object brep brep))))
		      (if (typep brep 'brep)
			  brep
			  (error "Given brep must be of type brep, or contain an object of type brep.")))))))




(define-object boolean-merge (boolean-tolerance-mixin brep)

  :documentation (:description "Generalized Merge container for doing
boolean operations.  This is not to be used directly, but is mixed
into subtracted-solid, united-solid, intersected-solid, and
separated-solid. The specific operation is specified in the respective
subclass.")
  
  :input-slots
  ( 
   ("GDL Brep object or object containing a brep. First of the breps to be merged"
    brep nil)
   
   "GDL Brep object or object containing a brep, or list of brep
objects or object containing a brep.  Second (or rest) of the breps to
be merged into the given <tt>brep</tt>" 
   other-brep
   
   operation 
   
   
   ("Boolean. If set to non-nil, throw warning but not error if any of
 the input breps has more than one non-infinite region. Defaults to
 *boolean-allow-multiple-regions?*, which itself defaults to nil."
 allow-multiple-regions? *boolean-allow-multiple-regions?*)
   
   ("Boolean. If set to non-nil, we throw an error instead of a
warning if the resulting brep does not pass the built-in validation
test. If nil, we throw a warning but continue to return the resulting
brep.  Defaults to *boolean-error-on-invalid-brep?* which itself
defaults to t."  error-on-invalid? *boolean-error-on-invalid-brep?*)
   
   
   ("Number. Defaults to *angle-tolerance-radians-default*."
   angle-tolerance *angle-tolerance-radians-default*)
   
   
   ("Boolean. Indicates whether we should try to sew and orient the
resulting brep. Usually a good idea and this is defaulted to t, except
for merged-solid where we default this to nil."  sew-and-orient? t)
   
   (manifold? t))

  
  :computed-slots
  (
   
   

   (%merge-and-brep 
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
          
          ;;(the first-brep assert-valid)
          ;;(the-object other-brep assert-valid)
          
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
                                           (the operation) (the manifold?)
                                           :sew-and-orient? nil ;;(the sew-and-orient?)
                                           )))
                          
            ((:merge)
	     
	     (print-variables count length)
	     
	     (let ((try-manifold? (= count (1- length))))
	       (format t "Merging ~s into merge object ~s, ~a~%" 
		       (cons 'the (reverse (the-object other-brep root-path)))
		       (cons 'the (reverse (the root-path)))
		       (if try-manifold? ", trying to make manifold..." ""))
	       (setq current-brep (do-boolean-merge-operation  *geometry-kernel* merge-container
							      (the operation) try-manifold?
							      :make-manifold? try-manifold?
							      :sew-and-orient? (the sew-and-orient?)))))
                           
            (:extract_separate
             (setq current-breps (do-boolean-separate-operation *geometry-kernel* 
                                   merge-container (the manifold?) (the sew-and-orient?))))))

        ;;
        ;;(tag-edges *geometry-kernel* current-brep (get-long *geometry-kernel* current-brep))
        ;;
        (ecase (the operation)
          ((:difference :union :intersection :merge) 
           (list :merge-container merge-container :native-brep current-brep))
                        
          (:extract_separate (list :merge-container merge-container :native-breps current-breps))))))


   (%native-brep% 
    (progn
      (when (or (> (the first-brep regions number-of-elements) 2)
                (not (every #'(lambda(number) (<= number 2))
                            (mapcar #'(lambda(object) (the-object object regions number-of-elements))
                                    (the rest-breps)))))
        (let ((message (format nil "~%~%in ~a -~%~%Attempting booleans where brep has ~~a non-infinite regions.~%"
                               (cons 'the (reverse (the root-path))))))
          (if (the allow-multiple-regions?)
              (warn message (1- (the first-brep regions number-of-elements)))
            (error message (1- (the first-brep regions number-of-elements))))))
    
      (if (and (the first-brep) (null (the rest-breps)))
          (progn 
            (warn "Boolean operation invoked on a single brep, this has no effect and 
returns the original brep. 


This occured in:
 
  ~s~%~%"
                  (cons 'the (reverse (the root-path))))

            (the first-brep %native-brep%))
        
        (let ((result
               (closed-boolean-operation *geometry-kernel*
                                         ;;:object self
                                         :first-brep (the first-brep)
                                         :rest-breps (the rest-breps)
                                         :operation (the operation)
                                         :approximation-tolerance (the approximation-tolerance)
                                         :angle-tolerance (the angle-tolerance)
                                         :sew-and-orient? (the sew-and-orient?)
                                         :manifold? (the manifold?))))
          
          (brep-assert-valid *geometry-kernel* result :warn? (not (the error-on-invalid?)))

          result)))))


  :functions
  ((ensure-brep
    (brep)
    (if (typep brep 'brep)
        brep
      (let ((brep (ignore-errors (the-object brep brep))))
        (if (typep brep 'brep)
            brep
          (error "Given brep must be of type brep, or contain an object of type brep.")))))))




(define-object merged-solid (boolean-merge)
  :documentation (:description "Given two brep solids or a brep solid and an open face represented as a brep,
performs a merge operation. Optionally (with make-manifold? t) makes the result manifold by trimming 
and throwing away extra pieces of faces and edges.")
  
  :input-slots
  (("Boolean. Indicates whether the resulting brep should be made into a manifold brep, with one or more regions."
    make-manifold? nil)
   
   ("Boolean. Indicates whether we should try to sew and orient the resulting brep. Usually a good idea 
and this is defaulted to t, except for merged-solid where we default this to nil."
    sew-and-orient? nil))

  
  :computed-slots (;;(manifold? nil) 
                   
                   (%native-brep% (getf (the %merge-and-brep) :native-brep))
                   
                   (operation :merge)))
  


(define-object separated-solid-2 (base-object boolean-tolerance-mixin)

  :input-slots (brep other-brep)

  :objects ((breps :type 'brep
		   :sequence (:size (the regioned breps number-of-elements))
		   :display-controls (the-child built-from display-controls)
		   :built-from (the regioned (breps (the-child index)))))

  :hidden-objects ((merged :type 'merged-solid
			   :pass-down (brep other-brep approximation-tolerance))
		   
		   (regioned :type 'regioned-solid
			     :brep (the merged))))



(define-object separated-solid (boolean-merge)
  :documentation (:description "Given two brep solids or a brep solid and an open face represented as a brep,
performs a split operation")
  
  :input-slots
  (("Boolean. Indicates whether the resulting split pieces should be made into watertight solids (ends capped, etc)."
    cap-results? nil)
   
   
   ("List of Color Keywords. These indicate the colors for any child breps if the boolean operation results in
a separated solid. If the number of breps exceeds the length of this list, the colors will be 
repeated in order. Defaults to a list with keys: 
<ul>
 <li> :green </li>
 <li> :red </li>
 <li> :blue </li>
 <li> :purple-dark </li>
 <li> :violet </li>
 <li> :cyan. </li>
</ul>"
    section-colors (list  :red :orange :gold-old :green :blue :purple-dark :violet)))
  
  
  :computed-slots ((operation :extract_separate)
                   
                   (number-of-colors (length (the section-colors)))
                   
                   (
		    ;;native-array-and-breps 
		    native-breps
		    (closed-boolean-separate-operation *geometry-kernel*
						       :first-brep (the first-brep)
						       :other-brep (the other-brep%)
						       :approximation-tolerance 
						       (the approximation-tolerance)
						       :angle-tolerance
						       (the angle-tolerance)
						       :manifold? 
						       (the manifold?)))
                   
		   #+nil
                   (native-breps (second (the native-array-and-breps)))
                   
                   ;;
                   ;; FLAG -- following kept just for garbage finalization - not really needed as separate slot:
                   ;;
                   ;;(native-array (first (the native-array-and-breps)))
                   
                   (other-brep% (cond ((= (length (the rest-breps)) 1)
                                       (first (the rest-breps)))
                                      (t (the united-rest-breps)))))
  
  :objects
  (("Sequence of GDL brep objects. The resulting breps yielded from the separate operation. These are colored using section-colors."
    breps :type 'brep
    :sequence (:size (the open-breps number-of-elements))
    :pseudo-inputs (%native-brep% display-controls)
    :display-controls (append (the display-controls)
                              (list :color (nth (mod (the-child index)
                                                     (the number-of-colors))
                                                (the section-colors))))
    :%native-brep% (if (the cap-results?)
                       (the (capped-breps (the-child index)) %native-brep%)
                     (the (open-breps (the-child index)) %native-brep%))))
  
  :hidden-objects
  ((united-rest-breps :type 'united-solid
                      :pass-down (approximation-tolerance angle-tolerance)
                      :other-brep (the rest-breps))
   
   
   (open-breps :type 'brep
               :pseudo-inputs (display-controls %native-brep%)
               :display-controls (list :color (nth (mod (the-child index)
                                                           (the number-of-colors))
                                                   (the section-colors)))
               
               ;;:sequence (:size (length (getf (the %merge-and-brep) :native-breps)))
               ;;:%native-brep% (nth (the-child index) (getf (the %merge-and-brep) :native-breps))
               
               :sequence (:size (length (the native-breps)))
               :%native-brep% (nth (the-child index) (the native-breps))
               )
   
   
   (capped-breps :type 'united-solid
                 :sequence (:size (the open-breps number-of-elements))
                 :display-controls (list :color (nth (mod (the-child index)
                                                           (the number-of-colors))
                                                      (the section-colors)))
                 :allow-multiple-regions? (the allow-multiple-regions?)
                 :brep (the (open-breps (the-child index)))
                 :other-brep (the cap-face))

   
   (cap-face :type 'intersected-solid
             :allow-multiple-regions? (the allow-multiple-regions?)
             :brep (the brep)
             :other-brep (the other-brep))))
  
  
(define-object subtracted-solid (boolean-merge)
  
  :documentation (:description "Given two brep solids, performs the subtract Boolean of the other-brep from the brep")
  
  :computed-slots ((operation :difference)))


(define-object united-solid (boolean-merge)
  
  :documentation (:description "Given two brep solids, performs the union Boolean between the brep and the other-brep")
  
  :computed-slots ((operation :union)))

(define-object intersected-solid (boolean-merge)
  
  :documentation (:description "Given two brep solids, performs the intersect Boolean between the brep and the other-brep")
  
  :computed-slots ((operation :intersection)))
  

(define-object regioned-solid (base-object)
  :documentation (:description "Given a brep solid that contains multiple regions, splits the regions into separate breps")

  :input-slots
  ("GDL Brep object or object containing a brep. The multi-region brep to be split."
    brep

   (hide-faces? t)
   
   ("List of Color Keywords. These indicate the colors for any child breps if the regioning operation results in
multiple solids. Defaults to a repeating (circular) list with keys: 
<ul>
 <li> :green </li>
 <li> :red </li>
 <li> :blue </li>
 <li> :purple-dark </li>
 <li> :violet </li>
 <li> :cyan. </li> </ul> "
    section-colors (list  :green :red :blue :purple-dark :violet :cyan)))
  
  
  :computed-slots ((filled-regions (get-filled-regions *geometry-kernel* 
                                                       (the (ensure-brep (the brep)))))

                   (number-of-colors (length (the section-colors))))
  
  
  :objects
  ((breps :type 'brep
          :pass-down (hide-faces?)
          :sequence (:size (length (the filled-regions)))
	  :pseudo-inputs (hide-faces? display-controls %native-brep%)
          :display-controls (append (the display-controls)
                                    (list :color (nth (mod (the-child index)
                                                           (the number-of-colors))
                                                      (the section-colors))))
          :%native-brep% (make-brep-from-regions 
                          *geometry-kernel*
                          (the brep) (list (nth (the-child index) (the filled-regions))))))

  :functions
  ((ensure-brep
    (brep)
    (if (typep brep 'brep)
        brep
      (let ((brep (ignore-errors (the-object brep brep))))
        (if (typep brep 'brep)
            brep
          (error "Given brep must be of type brep, or contain an object of type brep.")))))))

                    

(define-object manifold-solid (brep)
  :input-slots ("GDL brep object. The brep to be represented as a manifold brep in this instance."
                brep

                ("Boolean. Indicates whether faces between two non-void regions should be kept.
Defaults to nil."
                 keep-internal-faces? nil))

  :computed-slots ((%native-brep%
                    (make-brep-manifold *geometry-kernel* (the brep %native-brep%)
                                        ;;
                                        ;; FLAG -- suspect finalization
                                        ;;
                                        :finalize-on self
                                        :regions-to-keep nil
                                        :keep-internal-faces? (the keep-internal-faces?)))))


(define-object brep-intersect (base-object)
  

  :documentation (:description "This primitive takes two brep objects and attempts
to intersect the faces of the one with the faces of the other, yielding a 
sequence of edges which also behave as curves.")
  
  :input-slots ("Object of type brep. The first brep for intersecting its faces." 
                brep 

                "Object of type brep. The other brep for intersecting faces." 
                other-brep    

                                                                                                
                ("Number. Defaults to the max of the adaptive-tolerance of any of 
the input breps." 
                 approximation-tolerance 
                 (let ((first-tolerance (the first-brep adaptive-tolerance)))
                   (if (the rest-breps)
                       (max first-tolerance (apply #'max
                                                   (mapsend (the rest-breps)
                                                            :adaptive-tolerance)))
                     first-tolerance)))
                

                ("Number. Defaults to (radians-to-degrees *angle-tolerance-radians-default*)." 
                 angle-tolerance (radians-to-degrees *angle-tolerance-radians-default*))

                ("Boolean. Should edges be children or hidden-children? 
Defaults to nil which makes them display as children."
                 hide-edges? nil)

                ("Boolean. Should points be children or hidden-children? 
Defaults to nil which makes them display as children."
                 hide-points? nil)

                )

  :computed-slots
  ((first-brep (the brep))
   
   (rest-breps (list (the other-brep)))
   
   (edges-and-points (multiple-value-list 
                      (brep-intersect *geometry-kernel*
                                      (the brep %native-brep%)
                                      (the other-brep %native-brep%)
                                      :tolerance (the approximation-tolerance)
                                      :angle-tolerance (the angle-tolerance)))))
  
  
  :objects ((edges :type 'curve
                   :hidden? (the hide-edges?)
                   :sequence (:size (length (first (the edges-and-points))))
                   :native-curve-iw (nth (the-child index) 
                                         (first (the edges-and-points))))
            
            (points :type 'point
                    :hidden? (the hide-points?)
                    :sequence (:size (length (second (the edges-and-points))))
                    ;;
                    ;; FLAG -- when is this really needed?
                    ;;
                    :pseudo-inputs (%native-edge%)
                    :%native-edge% (nth (the-child index) 
                                        (second (the edges-and-points))))))

