;;
;; Copyright 2013 Genworks International
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
(in-package :twenty-four)


(defparameter *cards-by-number* 
  (let ((array (make-array 13))
	(count -1)
	(number-cards-pairs 
	 (list (list "1" "2" "3" "4")
	       (list "49" "50" "51" "52")
	       (list "45" "46" "47" "48")
	       (list "41" "42" "43" "44")
	       (list "37" "38" "39" "40")
	       (list "33" "34" "35" "36")
	       (list "29" "30" "31" "32")
	       (list "25" "26" "27" "28")
	       (list "21" "22" "23" "24")
	       (list "17" "18" "19" "20")
	       (list "13" "14" "15" "16")
	       (list "9" "10" "11" "12"  )
	       (list "5" "6" "7" "8"))))

    (dolist (card number-cards-pairs array)
      (setf (aref array (incf count)) card))))
    



