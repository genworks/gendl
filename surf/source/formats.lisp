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


;;
;; FLAG -- remove this package definition and move all low-level stuff to smlib layer. 
;;
(eval-when (:compile-toplevel :load-toplevel :execute) (defpackage :smlib (:use)))


(define-format nurbs (base-format)
  :slots ((assembly? nil)
	  (%assembly% nil)))


(define-format stl (nurbs)
  :slots ((tolerance nil) (breps-format :brep) 
          (units *output-units-default*) 
          (units-scale 1) 
          (analytic-curves? t)
          (objects nil))
  
  :functions
  ((objects-output
    ()
    (format *stream* "solid STL file created by Genworks GDL~%")
  
    (with-format-slots (objects)
        (dolist (object objects)
          (let ((buffer-file (namestring (glisp:temporary-file :extension "stl"))))
            (multiple-value-bind (result error)
                (ignore-errors (the-object object (write-stl-file buffer-file)))
              (declare (ignore result))
              (if (typep error 'error)
		  (warn "Could not write STL output for ~s (~s), skipping (error was ~a)..." 
			object (the-object object root-path) error)
                (progn
                  (with-open-file (in buffer-file)
                    (do ((line (read-line in nil nil) (read-line in nil nil)))
                        ((null line))
                      (unless (or (search "solid" line)
                                  (search "endsolid" line))
                        (write-string line *stream*)
                        (format *stream* "~%"))))
                  (delete-file buffer-file))))))
        
        (format *stream* "endsolid STL file created by Genworks GDL")))
   
   
   (finalize-output
    ()
    (write-env (objects-output))
    ;;(write-env (newline-out))
    )))
   
   
  
  
  

(define-format iges (nurbs)

  :slots ((tolerance nil) (breps-format :breps) (units *output-units-default*) (units-scale 1) (analytic-curves? t)
          (objects nil))

  :functions
  ((objects-output
    ()
    (with-format-slots (tolerance breps-format units units-scale analytic-curves? objects)
      (let ((leaves (reverse (ensure-list objects))))
        (let (entity-plist)
          (dolist (leaf leaves)
            
            (cond ((or (typep leaf 'trimmed-surface)(typep leaf 'face)) 
                   (push leaf (getf entity-plist :trimmed-surfaces)))
                  ((and (typep leaf 'brep) (member breps-format '(:surfaces :trimmed-surfaces)))
                   (push leaf (getf entity-plist :trimmed-surfaces)))
                  ((typep leaf 'surface) (push leaf (getf entity-plist :surfaces)))
                  ((typep leaf 'curve) (push leaf (getf entity-plist :curves)))
                  ((typep leaf 'point) (push leaf (getf entity-plist :points)))
                  ((and (typep leaf 'brep) (member breps-format '(:breps :solids))) 
                   (push leaf (getf entity-plist :solids)))
                  (t (let ((nurbs-reps (ensure-list (write-the-object leaf nurbs-reps))))
                       (dolist (nurbs nurbs-reps) 
                         (cond ((typep nurbs 'surface) (push nurbs (getf entity-plist :surfaces)))
                               ((typep nurbs 'curve) (push nurbs (getf entity-plist :curves)))
                               ((typep nurbs 'point) (push nurbs (getf entity-plist :points)))
                               (t (error "~s is an unrecognized NURBS representation.~%" nurbs))))))))
          (setq entity-plist (list :points (nreverse (getf entity-plist :points))
                                   :curves (nreverse (getf entity-plist :curves))
                                   :surfaces (nreverse (getf entity-plist :surfaces))
                                   :trimmed-surfaces (nreverse (getf entity-plist :trimmed-surfaces))
                                   :solids (nreverse (getf entity-plist :solids))))

          
          (let ((buffer-file (namestring (glisp:temporary-file))))
            (apply #'write-iges-file* *geometry-kernel* buffer-file 
                   :quiet? t :units units :units-scale units-scale :tolerance tolerance 
                   :write-analytic-curves? analytic-curves? entity-plist)
            (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
              (with-open-file (in buffer-file :element-type '(unsigned-byte 8))
                (do ((count (read-sequence buffer in) (read-sequence buffer in)))
                    ((<= count 0))
                  (write-sequence buffer *stream* :end count))))
            (delete-file buffer-file))))))

   (finalize-output
    ()
    (write-env (objects-output))
    ;;(write-env (a "%Finalized Output"))
    (write-env (newline-out)))))

(define-format step (nurbs)

  :slots ((tolerance nil) 
          (breps-format :breps) 
          (units *output-units-default*) 
          (units-scale 1) 
          (analytic-curves? t)
          (objects nil))

  :functions
  ((assembly-output
    ()
    (let ((buffer-file (namestring (glisp:temporary-file))))
      
      (with-format-slots (%assembly%)
	(smlib::write-hw-file-assembly *geometry-kernel* buffer-file %assembly%)
	)

      
      (let ((buffer (make-array 4096 
				:element-type '(unsigned-byte 8))))
	(with-open-file (in buffer-file :element-type '(unsigned-byte 8))
	  (do ((count (read-sequence buffer in) 
		      (read-sequence buffer in)))
	      ((<= count 0))
	    (write-sequence buffer *stream* :end count))))
      (delete-file buffer-file)))


   (objects-output
    ()
    (with-format-slots (tolerance breps-format units 
                                  units-scale analytic-curves? 
                                  objects)
      (let ((leaves (reverse (ensure-list objects))))
        (let (entity-plist)
          (dolist (leaf leaves)
            (cond ((or (typep leaf 'trimmed-surface)(typep leaf 'face)) 
                   (push leaf (getf entity-plist :trimmed-surfaces)))
                  ((and (typep leaf 'brep) 
                        (member breps-format 
                                '(:surfaces :trimmed-surfaces)))
                   (push leaf (getf entity-plist :trimmed-surfaces)))
                  ((typep leaf 'surface) 
                   (push leaf (getf entity-plist :surfaces)))
                  ((typep leaf 'curve) 
                   (push leaf (getf entity-plist :curves)))
                  ((typep leaf 'point) 
                   (push leaf (getf entity-plist :points)))
                  ((and (typep leaf 'brep) 
                        (member breps-format '(:breps :solids))) 
                   (push leaf (getf entity-plist :solids)))
                  (t (let ((nurbs-reps 
                            (ensure-list 
                             (write-the-object leaf nurbs-reps))))
                       (dolist (nurbs nurbs-reps) 
                         (cond ((typep nurbs 'surface) 
                                (push nurbs 
                                      (getf entity-plist :surfaces)))
                               ((typep nurbs 'curve) 
                                (push nurbs 
                                      (getf entity-plist :curves)))
                               ((typep nurbs 'point) 
                                (push nurbs 
                                      (getf entity-plist :points)))
                               (t (error "~s is an unrecognized NURBS representation.~%" nurbs))))))))
          (setq entity-plist 
            (list :points (nreverse (getf entity-plist :points))
                  :curves (nreverse (getf entity-plist :curves))
                  :surfaces (nreverse (getf entity-plist :surfaces))
                  :trimmed-surfaces (nreverse 
                                     (getf entity-plist 
                                           :trimmed-surfaces))
                  :solids (nreverse (getf entity-plist :solids))))

          (let ((buffer-file (namestring (glisp:temporary-file))))
            (apply #'write-step-file* *geometry-kernel* buffer-file 
                   :quiet? t :units units 
                   :units-scale units-scale :tolerance tolerance 
                   :write-analytic-curves? analytic-curves? 
                   entity-plist)
            (let ((buffer (make-array 4096 
                                      :element-type '(unsigned-byte 8))))
              (with-open-file (in buffer-file :element-type '(unsigned-byte 8))
                (do ((count (read-sequence buffer in) 
                            (read-sequence buffer in)))
                    ((<= count 0))
                  (write-sequence buffer *stream* :end count))))
            (delete-file buffer-file))))))

   (finalize-output
    ()
    (with-format-slots (assembly?)
      (if assembly?
	  (write-env (assembly-output))
	  (progn
	    (write-env (objects-output))
	    ;;(write-env (a "%Finalized Output"))
	    (write-env (newline-out))))))))



(define-format native (nurbs)

  :slots ((file-type :ascii) (objects nil))
  
  :functions ((finalize-output
               ()
               (write-env (objects-output))
               (write-env (a "// Finalized Output"))
               (write-env (newline-out)))
              
              (objects-output
               ()
               (with-format-slots (file-type objects)
                 (let ((leaves (ensure-list objects)))
                   (let (entity-plist)
                     (dolist (leaf leaves)
                       (cond ((typep leaf 'brep) (push leaf (getf entity-plist :breps)))
                             ((typep leaf 'surface) (push leaf (getf entity-plist :surfaces)))
                             ((typep leaf 'curve) (push leaf (getf entity-plist :curves)))
                  
                             (t (let ((nurbs-reps (ensure-list (write-the-object leaf nurbs-reps))))
                                  (dolist (nurbs nurbs-reps) 
                                    (cond ((typep nurbs 'surface) (push nurbs (getf entity-plist :surfaces)))
                                          ((typep nurbs 'curve) (push nurbs (getf entity-plist :curves)))
                                          (t (error "~s is an unrecognized NURBS representation.~%" nurbs))))))))
                     (setq entity-plist (list :curves (nreverse (getf entity-plist :curves))
                                              :surfaces (nreverse (getf entity-plist :surfaces))
                                              :breps (nreverse (getf entity-plist :breps))))

                     (let ((buffer-file (namestring (glisp:temporary-file))))
                       (apply #'write-native-file *geometry-kernel* buffer-file :quiet? t :file-type file-type entity-plist)
                       (if (and (glisp:featurep :excl)
                                (typep *stream* (read-from-string "excl:string-output-simple-stream")))
                           (with-open-file (in buffer-file)
                             (do ((line (read-line in nil nil) (read-line in nil nil)))
                                 ((null line))
                               (write-string line *stream*)
                               (write-char #\newline *stream*)))
                         (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
                           (with-open-file (in buffer-file :element-type '(unsigned-byte 8))
                             (do ((count (read-sequence buffer in) (read-sequence buffer in)))
                                 ((<= count 0))
                               (write-sequence buffer *stream* :end count)))))
                       (delete-file buffer-file))))))))



(defmethod encode-for-http ((self geometry-kernel-object-mixin))
  (if (typep (the %parent%) 'remote-object)
      (call-next-method)
    (encode-object-for-http :gdl-geometry-kernel-instance
                            (list :smlib-string (with-output-to-string(ss)(with-format (native ss) (write-the cad-output)))))))



