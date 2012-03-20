;;
;; Copyright 2002-2011, 2012 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;

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

(in-package :com.genworks.lisp)

(glisp:without-package-variance-warnings
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:nicknames :glisp)
    (:export #:parse-xml)))


#+allegro (eval-when (:compile-toplevel :load-toplevel :execute) (require :pxml))
(defun parse-xml (xml-string &rest args)
  #+allegro (apply #'net.xml.parser:parse-xml xml-string args)
  #-allegro (declare (ignore xml-string))
  #-allegro (warn "No XML parser found - please implement compatible call to net.xml.parser:parse-xml for the currently running lisp.~%"))




