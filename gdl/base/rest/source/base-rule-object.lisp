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

(define-object base-rule-object ()
  :documentation (:description "Encapsulates a basic computation, usually to be displayed to the user.
Typically this would be used as a mixin into a more sophisticated rule-object, but the type can be
 used to detect objects which should be processed as \"rules.\"")

  :input-slots
  (
   ("String. Title to be used with the rule object. Defaults to NIL."
    rule-title nil)
   
   ("String. Short description of the rule (generally one line). Defaults to NIL."
    rule-description nil)
   
   ("Boolean. Indicates whether this rule violates a standard condition."
    violated? nil)
   
   ("String. The basic return-value, or result, of evaluating the rule."
    rule-result nil)
   
   ("String. Verbose description of the purpose of the rule."
    rule-description-help nil)
   
   ("String. Verbose description of how the rule result is computed."
    rule-result-help nil)
   
   ("Boolean. Determines whether the rule is displayed by default in reports etc."
    :suppress-display? nil)
   
   ("String. Determines the rule's default name in various internal GDL contexts. Defaults to 
the <tt>rule-title</tt>, or \"Unnamed Rule\" if <tt>rule-title</tt> is NIL."
   strings-for-display (or (the :rule-title) "Unnamed Rule"))))



#|


Consider the following renaming scheme:


title ;; one-line title for linked display etc.  [static]

target-result ;; from a database lookup or user entry - target value or range. [quasi-static]

description ;; one-line verbose description maybe displayed on mouseover [static]

verbose-description ;; describes the purpose of the rule. [static]

verbose-result ;; natural language description of actual formulas in the rule. [static]


violated? ;; boolean or range  [evaluated at runtime]

result ;; Current result of "running" the rule, [evaluated at runtime]


|#
