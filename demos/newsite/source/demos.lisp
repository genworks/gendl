;;
;; Copyright 2002-2011, 2012 Genworks International
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


(in-package :www.genworks.com)


(define-object demos (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Demos")
   (link-title  "Demos")
   
)

  
  :objects
  ((wind :type 'wind-demo-launcher)

   (twenty-four :type 'assembly-24
		:pass-down (respondent))))




(define-object wind-demo-launcher (base-site-sheet)
  :computed-slots
  ((right-section-inner-html
    (with-cl-who-string ()
      "Please click "
      ((:a :href "/demo-1") "Here")
      " to visit the Wind Turbine demo."))))
