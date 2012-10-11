;;
;; Copyright 2012 Genworks International
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

(in-package :gwl)

;;
;; FLAG -- move this to glisp package and implement for known Lisp implementations. 
;;

#+allegro
(defun room-report (&optional gc?)
  "Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. 
 This is the main output from this object and gives a general overview of memory state. The especially
 noteworthy values are usually the :free-used-other and :old-used-other.

:&optional ((gc nil) \"Determines whether to do a full gc before probing the room data.\")
"
  (when gc? 
    (if (integerp gc?) (dotimes (n gc?) (excl:gc t)) (excl:gc t))) ;; future: (glisp:gc-full)
  (let ((self (make-object 'room-report))) 
    (let* ((data (the room-data))
	   (total (+ (* 8 (getf data :new-used-cons))
		     (* 8 (getf data :old-used-cons))
		     (getf data :new-used-other)
		     (getf data :old-used-other))))
      (let ((mb (/ (round-to-nearest total 1000000) 1000000)))
	(append data (list :total total
			   :MB-int mb
			   :MB (format nil "~a MB" mb)))))))
    
#+allegro
(define-object room-report ()

  :computed-slots 
  ((room-string (with-output-to-string (*standard-output*) (room)))

   (room-lines (with-input-from-string (in (the room-string))
		 (let (lines)
		   (do ((report-line (read-line in nil) (read-line in nil)))
		       ((null report-line) (nreverse lines))
		     (push report-line lines)))))

   (new-data (the (parse-line-data "New")))
   (old-data (the (parse-line-data "OTot")))

   ("Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, 
     :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. This is the main
     output from this object and gives a general overview of memory state. The especially
     noteworthy values are usually the :free-used-other and :old-used-other."
    room-data (append (the new-data) (the old-data))))

  :functions ((parse-line-data
	       (report-label)
	       (let ((prefix (cond ((string-equal report-label "New") "new")
				   ((string-equal report-label "OTot") "old"))))
		 (let ((report-line (find t (the room-lines)
				   :test #'(lambda(item report-line)
					     (declare (ignore item))
					     (and (search report-label report-line)
						  (not (search "-----" report-line)))))))
		   (let ((fields (remove "" (glisp:split-regexp "\\s" report-line) :test #'string-equal)))
		 
		     (let ((cons (third fields))
			   (other (fourth fields)))
		       (append
			(destructuring-bind (free used)
			    (glisp:split-regexp ":" cons)
			  (list (make-keyword (format nil "~a-free-cons" prefix)) (parse-integer free)
				(make-keyword (format nil "~a-used-cons" prefix)) (parse-integer used)))
			(destructuring-bind (free used)
			    (glisp:split-regexp ":" other)
			  (list (make-keyword (format nil "~a-free-other" prefix)) (parse-integer free)
				(make-keyword (format nil "~a-used-other" prefix)) (parse-integer used)))))))))))

