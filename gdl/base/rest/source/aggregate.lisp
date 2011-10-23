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

(defmacro the-element (&rest args)
  "Lisp Object [Macro]. Acts similarly to <tt>the-object</tt> for each element of an aggregate,
within the context of a <tt>list-elements</tt>, <tt>append-elements</tt>, <tt>max-of-elements</tt>,
<tt>min-of-elements</tt>, <tt>sum-elements</tt>, or a query operator (query operators are not
yet documented).

:arguments (args \"(&rest). Standard reference chain applicable to the element.\")"
  `(the-object %object% ,@args))

(defmacro list-elements (aggregate &optional expression filter)
  "List of GDL Objects [Macro]. Returns a listing of the elements of an aggregate, 
with an optional <tt>the-element</tt> expression and filter. If an expression is given,
the list of results from the expressions is returned. If no expression is given, a list
of the objects themselves is returned.

:arguments (aggregate \"GDL aggregate object. (e.g. from a <tt>:sequence (:size ..)</tt> <tt>:object</tt> specification).\")
:&optional (expression \"Expression using <tt>the-element</tt>. Similar to a <tt>the-object</tt> reference.\"
            filter     \"Function of one argument. Can be used to filter the result list.\")"
  (cond ((and expression filter)
         `(let ((%objects% (if (typep ,aggregate 'quantification) (the-object ,aggregate :list-elements)
                             (the-object ,aggregate :children))))
            (mapcar #'(lambda(%object%) ,expression)
                    (remove-if-not #'(lambda(%object%) ,filter) %objects%))))
        (expression
         `(mapcar #'(lambda(%object%)
                      ,expression)
                  (if (typep ,aggregate 'quantification) (the-object ,aggregate :list-elements)
                    (the-object ,aggregate :children))))
        (t `(if (typep ,aggregate 'quantification) (the-object ,aggregate :list-elements)
              (the-object ,aggregate :children)))))

;;
;; FLAG -- rewrite the following without using apply so we don't risk
;;         hitting function argument limit.
;;
(defmacro append-elements (aggregate &optional expression filter)
  "List of Objects [Macro]. Returns an appended list of <tt>expression</tt> from each element of an aggregate, 
with an optional filter.

:arguments (aggregate \"GDL aggregate object. (e.g. from a <tt>:sequence (:size ..)</tt> <tt>:object</tt> specification).\")
:&optional (expression \"Expression using <tt>the-element</tt>. Similar to a <tt>the-object</tt> reference, which should return a list.\")"
  (cond ((and expression filter)
         `(let ((%objects% (the-object ,aggregate :list-elements)))
            (apply #'append
                   (mapcar #'(lambda(%object%) ,expression)
                           (remove-if-not #'(lambda(%object%) ,filter) %objects%)))))
        (expression
         `(apply #'append
                 (mapcar #'(lambda(%object%)
                             ,expression)
                         (the-object ,aggregate :list-elements))))
        (t (error "Macro append-elements requires an expression."))))


(defmacro max-of-elements (aggregate &optional expression filter)
  "Number [Macro]. Returns the maximum of <tt>expression</tt> from each element of an aggregate, 
with an optional filter.

:arguments (aggregate \"GDL aggregate object. (e.g. from a <tt>:sequence (:size ..)</tt> <tt>:object</tt> specification).\")
:&optional (expression \"Expression using <tt>the-element</tt>. Similar to a <tt>the-object</tt> reference, which should return a number.\")"
  (cond ((and expression filter)
         `(let ((%objects% (the-object ,aggregate :list-elements)))
            (apply #'max
                   (mapcar #'(lambda(%object%) ,expression)
                           (remove-if-not #'(lambda(%object%) ,filter) %objects%)))))
        (expression
         `(apply #'max
                 (mapcar #'(lambda(%object%)
                             ,expression)
                         (the-object ,aggregate :list-elements))))
        (t (error "Macro max-of-elements requires an expression."))))  

(defmacro min-of-elements (aggregate &optional expression filter)
  "Number [Macro]. Returns the minimum of <tt>expression</tt> from each element of an aggregate, 
with an optional filter.

:arguments (aggregate \"GDL aggregate object. (e.g. from a <tt>:sequence (:size ..)</tt> <tt>:object</tt> specification).\")
:&optional (expression \"Expression using <tt>the-element</tt>. Similar to a <tt>the-object</tt> reference, which should return a number.\")"
  (cond ((and expression filter)
         `(let ((%objects% (the-object ,aggregate :list-elements)))
            (apply #'min
                   (mapcar #'(lambda(%object%) ,expression)
                           (remove-if-not #'(lambda(%object%) ,filter) %objects%)))))
        (expression
         `(apply #'min
                 (mapcar #'(lambda(%object%)
                             ,expression)
                         (the-object ,aggregate :list-elements))))
        (t (error "Macro max-of-elements requires an expression."))))

(defmacro sum-elements (aggregate &optional expression filter)
  "Number [Macro]. Returns the sum of <tt>expression</tt> from each element of an aggregate, 
with an optional filter.

:arguments (aggregate \"GDL aggregate object. (e.g. from a <tt>:sequence (:size ..)</tt> <tt>:object</tt> specification).\")
:&optional (expression \"Expression using <tt>the-element</tt>. Similar to a <tt>the-object</tt> reference, which should return a number.\")"
  (cond ((and expression filter)
         `(let ((%objects% (the-object ,aggregate :list-elements)))
            (apply #'+
                   (mapcar #'(lambda(%object%) ,expression)
                           (remove-if-not #'(lambda(%object%) ,filter) %objects%)))))
        (expression
         `(apply #'+
                 (mapcar #'(lambda(%object%)
                             ,expression)
                         (the-object ,aggregate :list-elements))))
        (t (error "Macro sum-elements requires an expression."))))
