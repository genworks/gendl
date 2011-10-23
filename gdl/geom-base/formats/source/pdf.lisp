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

(define-format pdf (2d-output)
  :documentation (:author "David J Cooper Jr [Genworks]"
                  :description "PDF is the most developed output format in GDL currently. Using PDF you can produce wireframe
renderings for geometric objects, text output with specified fonts and sizes, and arbitrary PDF commands. Full-blown typeset 
output is coming later.")
  
  :functions
  ((postnet-bar 
    (digits)
    (let ((low-bar 3.6) (high-bar 9) (spacing 3.9))
      (pdf:with-saved-state (pdf:set-line-width 1.44)
        (pdf:move-to 0 0)
        (pdf:line-to 0 high-bar) (pdf:stroke)
        (pdf:translate spacing 0) (pdf:move-to 0 0)
        (dolist (digit digits)
          (write-env (postnet-digit digit high-bar low-bar spacing)))
        (pdf:line-to 0 high-bar) (pdf:stroke))))
   

   (postnet-digit 
    (digit high-bar low-bar spacing)
    
    (let ((bits (aref +postnet-bits+ digit)))
      (dolist (bit bits)
        (pdf:line-to 0 (if bit high-bar low-bar)) (pdf:stroke) 
        (pdf:translate spacing 0) (pdf:move-to 0 0))))))


