(in-package :www.genworks.com)


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
    



