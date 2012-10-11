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

(define-object step-reader (base-object)
  
  :documentation (:description "This object will reflect the contents of a STEP file containing
points, curves, surfaces, and/or trimmed surfaces as sequences of GDL objects. Currently all
surfaces are treated as trimmed, where actual untrimmed surfaces have their natural outer 
boundaries as the result-island, i.e. no standalone surfaces will be produced by this part. 

This is a default option in the HarmonyWare STEP Translator which will be exposed in GDL
in a future release.

The HarmonyWare reader creates a log file in a temporary directory. The location of this log
file is printed on the console during the reading operation. Currently this log file is
not automatically deleted, and its name is determined by the system.

")
  
  :input-slots ("String or pathname. The location of the STEP file to be read."
                file-name
                
                ("Boolean. If true, treat all untrimmed surfaces in the file as if they are trimmed surfaces with 
the natural outer boundary of the surface as the trimming loop.  If used, no standalone IwSurface surface objects 
will ever be returned by the reader.  Default is nil." 
                 make-all-surfaces-trimmed? nil)
                
		#+nil
                ("Boolean. If true, treat all B-reps in the file as if they are collections of trimmed surfaces.  This loses
all B-rep connectivity contained in the file.  Its use is not recommended. Default is nil." 
                 break-up-breps-into-trimmed-surfaces? nil)
                
                ("Boolean. If true, group all trimmed surfaces in the file into one B-rep.  
If some trimmed surfaces are blanked, they are grouped into a second, blanked B-rep. Default is nil."
                 group-trimmed-surfaces-into-brep? nil)
                
                ("Boolean. If true, group all trimmed surfaces and B-reps in the file into one B-rep.  
If some trimmed surfaces or B-reps are blanked, they are grouped into a second, blanked B-rep. Default is nil."
                 make-single-brep? nil)
                
                ("Boolean. Indicates whether each resulting brep should have its faces sewn together. Default is (the make-single-brep?)." 
                 sew-brep-faces? (the make-single-brep?))
                
                )
  
  :computed-slots
  ((data (read-step-file* *geometry-kernel* (format nil "~a" (the file-name)) 
                          :finalize-on self
                          :make-all-surfaces-trimmed? (the make-all-surfaces-trimmed?)
                          ;;:break-up-breps-into-trimmed-surfaces? (the break-up-breps-into-trimmed-surfaces?)
                          :group-trimmed-surfaces-into-brep? (the group-trimmed-surfaces-into-brep?)
                          :make-single-brep? (the make-single-brep?))))
  
  :objects
  (("Sequence of GDL point objects. The points found in the STEP file."
    points :type 'point 
           :sequence (:size (length (getf (the data) :points)))
           :center (nth (the-child index) (getf (the data) :points)))
   
   ("Sequence of GDL curve objects. The curves found in the STEP file."
    curves 
    :type 'curve 
    :from-iges? t
    :sequence (:size (length (getf (the data) :curves)))
    :native-curve-iw (nth (the-child index) (getf (the data) :curves)))
   

   ("Sequence of GDL surface objects. The untrimmed ``standalone'' surfaces found in the STEP file."
    surfaces :type 'surface 
    :sequence (:size (length (getf (the data) :surfaces)))
    :native-surface (nth (the-child index) (getf (the data) :surfaces)))
   
   
   ("Sequence of GDL brep objects. The breps and trimmed surfaces (represented as breps) found in the STEP file."
    breps :type 'brep
    :from-iges? t
    :sequence (:size (length (getf (the data) :trimmed-surface-breps)))
    :%native-brep% (let ((brep (nth (the-child index) (getf (the data) :trimmed-surface-breps))))
                     
                     (when (the sew-brep-faces?) (iwbrep-sew-and-orient brep))
                     
                     (if brep brep (error "Brep is nil for some reason"))))))


;;
;; Input tests
;;

(define-object step-test-reader (step-reader)
  :computed-slots ((file-name "/tmp/as1-oc-214.stp")))

(define-object step-test-tree-reader (step-reader)
  :computed-slots ((file-name "/tmp/step-test-tree.stp")))


