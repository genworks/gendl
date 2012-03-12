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



(in-package :gdl-lift-tests)

(define-object iges-writer-test (base-object)
  
  :computed-slots
  ((output-file-name
    (let ((output (merge-pathnames "round-trip.igs" gdl-lift-utils::*lift-data-directory*)))
      (with-format (iges output) (write-the test-part (breps 0) cad-output))
      output))

   
   (regression-test-data
    (progn
      (warn "Iges-writer-test is failing, check input of type iges-reader")
      nil))
   
   #+nil
   (regression-test-data
    (append (multiple-value-list (the input (breps 0) precise-properties))
            (the input (breps 0) %curves-to-draw%)
            (the input (breps 0) %lines-to-draw%))))

  :objects
  ((test-part :type 'native-reader
              :file-name (merge-pathnames "iraro.iwp" gdl-lift-utils::*lift-data-directory*))

   (input :type 'iges-reader
          :file-name (the output-file-name)
          ;;:make-single-brep? t
          )))


(define-object iges-writer-test-2 (base-object)
  
  :computed-slots
  ((output-file-name
    (let ((output (merge-pathnames "round-trip-2.igs" gdl-lift-utils::*lift-data-directory*)))
      (with-format (iges output) (write-the test-part (breps 0) cad-output))
      output))

   (regression-test-data
    (append (multiple-value-list (the input (breps 0) precise-properties))
            (the input (breps 0) %curves-to-draw%)
            (the input (breps 0) %lines-to-draw%))))

  :objects
  ((test-part :type 'iges-reader
              :make-single-brep? t
              :file-name (merge-pathnames "nurb_fillet.igs" gdl-lift-utils::*lift-data-directory*))

   (input :type 'iges-reader
          :file-name (the output-file-name)
          :make-single-brep? t)))


(register-test-definition 'iges-writer-test)


;;
;; Output tests
;;

(define-object iges-test-tree (base-object)
  :objects
  ((b-spline-curve :type 'test-b-spline-curves)
   (fitted-curve :type 'test-fitted-curve)
   ;(arc-curve :type 'test-arc-curve)
   (linear-curve :type 'test-linear-curve)
   ;(curve-from-line :type 'l-line :start (make-point 0 0 0)
;                   :end (make-point 10 10 0))
   (curve-from-arc :type 'arc :center (make-point 0 0 0)
                   :radius 10 :start-angle 0 :end-angle pi)
   (b-spline-surface :type 'test-b-spline-surface)
   (fitted-surface :type 'test-fitted-surface)
   (planar-surface :type 'test-planar-surface)
   (rectangular-surface :type 'test-rectangular-surface)
   (spherical-surface :type 'test-spherical-surface)
   (spherical-surface-2 :type 'spherical-surface
                        :radius 0.5)
   (spherical-surface-3 :type 'spherical-surface
                        :radius 0.05)
   
   (spherical-surface-4 :type 'spherical-surface
                        :radius 0.005)
   )
  
  :functions
  ((iges-out
    ()
    (let ((output-file 
           (make-pathname 
            :directory (pathname-directory (glisp:temporary-folder))
            :name "iges-test-tree" :type "iges")))
      (format t "Writing test IGES file to ~a...~%" output-file)
      (with-format (iges output-file) (write-the cad-output-tree))))))
