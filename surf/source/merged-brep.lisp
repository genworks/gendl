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


(define-object boolean-tree (brep)

  :input-slots (breps
                (operation :merge))

  :computed-slots (
                   (%native-brep% (merge-breps *geometry-kernel*
                                               :breps (the breps)
                                               :operation (the operation)))))
  
  

(define-object merged-brep (brep)
  
  :input-slots (breps
                (sew-tolerances (the sew-tolerances-default)))

  
  :computed-slots (
                   
                   (sew-tolerances-default 
                    (let ((brep-tolerances (mapsend (the breps) :adaptive-tolerance)))
                      (list (apply #'min brep-tolerances) (apply #'max brep-tolerances))))
                   
                   
                   (%native-brep% (let ((new-brep (brep-copy *geometry-kernel* (first (the breps)))))
                                    (dolist (brep (rest (the breps))
                                              (if (the sew-tolerances)
                                                  (sew-brep *geometry-kernel* 
                                                            :brep new-brep 
                                                            :tolerances (the sew-tolerances))
                                                new-brep))
                                      (merge-brep *geometry-kernel* :brep new-brep 
                                                  :brep-to-merge (the-object brep %native-brep%)))))))
                   

                   
                   
