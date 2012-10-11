;;
;; Copyright 2002, 2009, 2012 Genworks International
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

(in-package :user)

(defparameter *assembly*
    `((:document :title   "Test Document"
                 :author  "David J. Cooper Jr."
                 :company "Genworks International"
                 :date    "January, 2001"
                 :textwidth 6.5
                 :textheight 8.5
                 :topmargin 0
                 :oddsidemargin 0
                 :evensidemargin 1.0
                 :class   :article)
      ((:section :title "Executive Summary")
       "This is a high-level "
       (:emph "executive summary")
       " of the whole paper.")
      
      ((:section :title "Sample List")
       ((:list :style :description)
	((:item :word "Scope and Requirements")
	 "We currently know only a small portion of the required functions of the
system. We will only discover the full range of requirements with some initial deployment 
and user feedback.")
	((:item :word "Technology Landscape")
	 "The IT landscape changes constantly, different solutions vendors, 
operating system platforms, programming languages, database systems, etc, constantly 
vying for advantage. Along with this, various Corporate-wide ``one size fits all'' 
initiatives promise far-reaching solutions but lack the flexibility to meet all 
specific needs in a timely and responsive manner."))
       
       ((:list :style :itemize)
	(:item "Do start building a prototype system.")
	(:item "Use only stable, well-defined technologies"))
       
       ((:list :style :enumerate)
	(:item "Do start building a prototype system.")
	(:item "Use only stable, well-defined technologies")))))
