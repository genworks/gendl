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

(in-package :surf)

(define-object native-reader (base-object)
  
  :documentation (:description "This object will reflect the contents of an iwp file containing
curves, surfaces, breps, and brep trees as sequences of GDL objects. 
")
  
  :input-slots (("String or pathname. The location of the IWP file to be read."
                 file-name (when (the smlib-string)
                             (let ((temp-file (funcall gdl::*make-temp-file-name-func*)))
                               (with-open-file (out temp-file 
                                                :direction :output
                                                :if-exists :supersede 
                                                :if-does-not-exist :create)
                                 (write-string (the smlib-string) out)) temp-file)))
                ("String. Contains output from a call to (with-format (native ...) (write-the cad-output))
for an SMLib object (e.g. curve, surface, brep). Defaults to nil. If you specify this as well as a 
file-name, the file-name will take precedence."
                 smlib-string nil))

  
  :computed-slots
  ((data (read-native-file *geometry-kernel* (format nil "~a" (translate-logical-pathname (the file-name)))))

   
   (breps-list (list-elements (the breps)))
   
   (surfaces-list (list-elements (the surfaces)))
   )
   
  
  :objects
  (
   
   ("Sequence of GDL curve objects. The curves found in the IWP file."
    curves 
    :type 'curve 
    :sequence (:size (length (getf (the data) :curves)))
    :native-curve-iw (nth (the-child index) (getf (the data) :curves))
           )
   
   ("Sequence of GDL surface objects. The untrimmed ``standalone'' surfaces found in the IWP file."
    surfaces 
    :type 'surface 
    :sequence (:size (length (getf (the data) :surfaces)))
    :native-surface-iw (nth (the-child index) (getf (the data) :surfaces)))
   
   ("Sequence of GDL brep objects. The breps found in the IGES file."
    breps :type 'brep
    :from-iges? t
    :sequence (:size (length (getf (the data) :breps)))
    :%native-brep% (nth (the-child index) (getf (the data) :breps)))))


