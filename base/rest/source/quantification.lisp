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


(define-object quantification ()
    :documentation (:description "A quantification is an aggregate created as a result of specifying <tt>:sequence (:size ...))</tt> or
<tt>:sequence (:indices ...))</tt> in an <tt>:objects</tt> specification. Usually, the elements of a quantified set are referenced by using
extra parentheses around the message in the reference chain and using the index number. But the aggregate itself also supports certain
messages, documented here. One message, <tt>number-of-elements</tt>, is not listed in the normal messages section because it is 
internal. It can be used, and returns an integer representing the cardinality of the aggregate.")

    :computed-slots
    ((index nil)

     ("GDL Object. Returns the first element of the aggregate."
      first nil)
   
     ("GDL Object. Returns the last element of the aggregate."
      last  nil)))
