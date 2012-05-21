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


(in-package :training-g101)


(define-object assembly (slide-show-node)

  :input-slots
  ((title "G101: Common Lisp for GDL Developers")
   (slide-package (find-package :training-g101))
   (audio-base-url "/g101/mp3/")
   (image-base-url "/g101/images/")
   (style-url "/g101/style/top.css"))

  
  :objects
  ((introduction    :type 'introduction)
   (welcome         :type 'welcome)
   (lists           :type 'lists)
   (control         :type 'control)
   (functions       :type 'functions)
   (input-output    :type 'input-output)
   (numbers         :type 'numbers)
   (data-structures :type 'data-structures)
   ;;(macros          :type 'macros)
   (symbols         :type 'symbols)
   (conclusion      :type 'conclusion)
   ))
