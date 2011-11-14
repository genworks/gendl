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

(in-package :geom-base)

;;
;; FLAG -- check whether this is still being used.
;;

(defun print-document (type)
  (the-object (make-object type) print-pdf))

(define-object document (base-object)
  :computed-slots
  ((pages nil))
  
  :functions
  ((print-pdf
    (&key (output-file (format nil "/tmp/~s.pdf" (the type))))
    (with-format (pdf-multipage output-file)
      (write-the cad-output)))))


(define-lens (pdf document)()
  :output-functions 
  ((cad-output 
    ()
    (dolist (page (the pages))
      (let ((width (coerce (the-object page page-width) 'double-float)) 
            (length (coerce (the-object page page-length) 'double-float)))
        (pdf:with-page (:bounds (make-array 4 :initial-contents 
                                            (list 0 0 width length)))
          (pdf:with-saved-state 
              (pdf:translate (half (the-object page page-width))
                             (half (the-object page page-length)))
            (write-the-object page cad-output))))))))
