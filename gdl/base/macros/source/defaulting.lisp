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

(in-package :gdl)


(defmacro defaulting (form &optional default)
  "Lisp object. Returns a default value if the reference-chain is not handled.

Note: Defaulting is currently implemented with ignore-errors, so it will return the default value
regardless of whether the reference-chain is actually not handled or throws some other error.

This will be updated in a future GDL release only to return the default if the reference-chain
is actually not handled, and to throw any other errors normally.

:arguments (form \"Reference-chain with the or the-object\"
            default \"Lisp expression. Default value to return if reference-chain cannot be handled.\")"
  (let ((value (gensym)) (error (gensym)))
    `(multiple-value-bind (,value ,error) (ignore-errors ,form)
       (cond ((and ,error (typep ,error 'error)
                   (let ((string (format nil "~a" ,error)))
                     (or (and (search "attempt to call" string)
                              (search "gdl-slots::" string))
                         (search "nor any of its ancestor instances could handle the" string)
                         (search "which is the root object, could not handle the" string)))) ,default)
             ((typep ,error 'error) (error ,error))
             (t ,value)))))
         
