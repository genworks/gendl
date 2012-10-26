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

(in-package :gendl-doc)

(defparameter *upgrade-notes*
    `((:chapter :title "This chapter lists the typical modifications you will want to 
consider for upgrading from GDL 1580 to Gendl 1581.")

      ((:list :style :itemize)
       (:item "(update-gdl ..) not yet available for 1581.")
       (:item "(register-asdf-systems no longer needed or available) - If you want your own asdf systems, 
                use (ql:quickload ...)  for 3rdpty files.")
       (:item "system-wide gdlinit.cl in application directory, personal one in home directory.")
       (:item "Slime debugging is different - refer to Slime intro")
       (:item "color-themes -- M-x color-theme-select")
       (:item "Gendl files can end with .lisp or .gdl"))))


