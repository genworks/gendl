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

(in-package :gdl)


(defmacro defaulting (form &optional default)
  "Lisp object. Returns a default value if the reference-chain is not handled.

:arguments (form \"Reference-chain with the or the-object\"
            default \"Lisp expression. Default value to return if reference-chain cannot be handled.\")"
  (let ((value '+value+ #+nil (gensym)) (error '+error+ #+nil (gensym)))
    `(multiple-value-bind (,value ,error) (ignore-errors ,form)
       (cond ((and ,error (typep ,error 'error)
                   (let ((string (glisp:replace-regexp (format nil "~a" ,error)
						       (format nil "~%") " ")))
                     (or (and (search "attempt to call" string)
                              (search "gdl-slots::" string))
                         (search "nor any of its ancestor instances could handle the" string)
                         (search "which is the root object, could not handle the" string)))) ,default)
             ((typep ,error 'error) (error ,error))
             (t ,value)))))
         
