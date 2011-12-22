;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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


(in-package :gdl-lift-utils)

(defvar *gdl-test-definitions* nil)

(defparameter *lift-data-directory* 
    (merge-pathnames "tests/data/" glisp:*genworks-source-home*))

(defun register-test-definition (symbol)
  (pushnew symbol *gdl-test-definitions*))

(defun get-regression-data (symbol)
  (multiple-value-list (the-object (make-object symbol) :regression-test-data)))

(defun get-regression-data-from-object (object)
  (multiple-value-list (the-object object :regression-test-data)))

(defun test-object (object)
  (ensure-same (get-seed-data (the-object object type)) (get-regression-data-from-object object) :test 'equivalent-lists))

(defun seed-test-data (symbol)
  (let ((*print-length* nil))
    (with-open-file (out (merge-pathnames (make-pathname :name (string-downcase (format nil "~a" symbol))
							 :type "data") *lift-data-directory*)
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
      (print (get-regression-data symbol) out))))

(defun seed-all-test-data ()
  (dolist (symbol *gdl-test-definitions*)
    (seed-test-data symbol)))

(defun get-seed-data (symbol)
  (with-open-file (in (merge-pathnames (make-pathname :name (string-downcase (format nil "~a" symbol))
						       :type "data") *lift-data-directory*)
		   :direction :input
                   :if-does-not-exist nil)
    (when in #+allegro (scm::read-safely in nil nil) #-allegro (read in nil nil))))

(defun read-all-seed-data ()
  (dolist (symbol *gdl-test-definitions*)
    (format t "  ~s~%" symbol)
    (get-seed-data symbol))
  t)

(defun read-all-regression-data ()
  (dolist (symbol *gdl-test-definitions*)
    (format t "  ~s~%" symbol)
    (get-regression-data symbol))
  t)

(defun define-regression-tests ()
  (let ((lift:*test-evaluate-when-defined?* nil))
    (lift:deftestsuite gdl-lift-utils::gdl-regression-tests () ()
		       (:setup
			#+allegro
			(gc t)
			#+lispworks
			(gc-all)
			))
    (dolist (symbol (reverse *gdl-test-definitions*))
      (let ((test
	     (format nil 
		     "(lift:addtest (gdl-lift-utils::gdl-regression-tests) ~s (lift:ensure-same (gdl-lift-utils::get-seed-data '~s) (gdl-lift-utils::get-regression-data '~s) :test 'gdl-lift-utils::equivalent-lists))" symbol symbol symbol)))
	(eval (read-from-string test))))))

(defun add-regression-test (symbol)
  (let ((lift:*test-evaluate-when-defined?* nil)
        (test
         (format nil 
                 "(lift:addtest (gdl-lift-utils::gdl-regression-tests) ~s (lift:ensure-same (gdl-lift-utils::get-seed-data '~s) (gdl-lift-utils::get-regression-data '~s) :test 'gdl-lift-utils::equivalent-lists))" symbol symbol symbol)))
    (eval (read-from-string test))))

(defvar *regression-test-report* nil)
(defparameter *passed-report-size* 64)
(defvar *regression-tests-result* nil)
(defvar *regression-test-result* nil)

(defun run-regression-tests (&key (print-length 10))
  (let ((lift:*test-print-test-case-names* t)
	(lift:*lift-debug-output* *standard-output*)
	(lift:*test-break-on-errors?* nil)
	(lift:*test-break-on-failures?* nil)
	(*print-length* print-length))
    #+allegro
    (setf (sys:gsgc-parameter :open-old-area-fence) 0)
    (setq *regression-test-report*
      (with-output-to-string (out)
	(lift:describe-test-result (lift:run-tests :suite 'gdl-regression-tests
						   :report-pathname (merge-pathnames "gdl-regression-tests-result.txt"
										     (user-homedir-pathname))) out)))
    (setq *regression-tests-result* lift:*test-result*)
    (if (< (length *regression-test-report*) *passed-report-size*)
	*regression-test-report*
      "Some test failed check variable gdl-lift-utils::*regression-test-report* for details")))

(defun run-regression-tests-pass-fail (&key (print-length 10))
  (< (length (run-regression-tests :print-length print-length)) *passed-report-size*))

(defun run-regression-test (symbol &key (print-length 10))
  (let ((lift:*test-print-test-case-names* t)
	(lift:*lift-debug-output* *standard-output*)
	(lift:*test-break-on-errors?* nil)
	(lift:*test-break-on-failures?* nil)
	(*chain-beziers-for-display?* t)
	(*print-length* print-length)
	(lift:*lift-report-pathname* (merge-pathnames "gdl-regression-test-result.txt"
						      (user-homedir-pathname))))
    (setq *regression-test-result* (lift:run-test :suite 'gdl-regression-tests :test-case symbol))
    (with-output-to-string (out)
      (lift:describe-test-result *regression-test-result* out))))

(defun list-failures()
  (when lift:*test-result*
    (mapcar 'lift::test-method (lift:failures lift:*test-result*))))

(defun list-errors()
  (when lift:*test-result*
        (mapcar 'lift::test-method (lift:errors lift:*test-result*))))

(defun seed-failures()
  (when lift:*test-result*
    (mapcar #'(lambda (failure)
		(format t "Seeding ~a ...~%" (lift::test-method failure))
		(seed-test-data (intern (symbol-name (lift::test-method failure)) :gdl-lift-tests)))
	    (lift:failures lift:*test-result*)))
  nil)

;;
;; FLAG -- convert this to proper recursive comparison of nested lists
;;
(defun equivalent-lists (list1 list2)
  (cond ((and (consp list1) (consp list2))
	 (let ((l1 (flatten list1))
	       (l2 (flatten list2)))
	   (when (equal (length l1) (length l2))
	     (every #'(lambda (test)
			(equal test t))
		    (mapcar #'equivalent-atoms l1 l2)))))
	((and (atom list1) (atom list2))
	 (equivalent-atoms list1 list2))
	(t nil)))

(defun equivalent-atoms (a1 a2)
  (cond ((and (numberp a1) (numberp a2))
	 (cond ((and (near-zero? a1 0.1) (near-zero? a2 0.1)) t)
	       ((< (abs a1) 100) (near-to? (abs a1) (abs a2) 11.0)) 
	       (t (near-to? (abs a1) (abs a2) (* (abs a1) 0.1)))))
	((and (stringp a1) (stringp a2))
	 (string-equal a1 a2))
	((and (arrayp a1) (arrayp a2))
	 (equivalent-lists (coerce a1 'list) (coerce a2 'list)))
	((and (symbolp a1) (symbolp a2))
	 (equal (intern (string-upcase (string a1)) (symbol-package a1))
		(intern (string-upcase (string a2)) (symbol-package a2))))
	
	(t nil)))

(defun quick-test (symbol &key (times 100) (report-interval 10) (warmup-runs 0)
			       (output-stream *trace-output*))
  (stress-test symbol 
	       :times times :report-interval report-interval
	       :warmup-runs warmup-runs :output-stream output-stream))

(defun stress-test (symbol &key (times 1000) (report-interval 100) (warmup-runs 100)
				(output-stream *trace-output*))
  
  (dotimes (n warmup-runs)
    (the-object (make-object (intern symbol :gdl-lift-tests)) regression-test-data))
  
  (multiple-value-bind (mem1-prev mem2-prev) (#+allegro excl.osi:get-mem-info #-allegro values #-allegro 0 #-allegro 0)
    (let ((mem1-orig mem1-prev) (mem2-orig mem2-prev))
      (dotimes (n times)
	(when (and (not (zerop n)) (zerop (mod n report-interval)))
	  (format output-stream "Processing iteration ~a...~%" n)
	  (multiple-value-bind (mem1 mem2) (#+allegro excl.osi:get-mem-info #-allegro values #-allegro 0 #-allegro 0)
	    (when (or (/= mem1 mem1-prev)(/= mem2 mem2-prev))
	      (format output-stream "
Memory status changed from: ~a/~a to
                            ~a/~a~%" mem1-prev mem2-prev mem1 mem2))
	    (setq mem1-prev mem1 mem2-prev mem2)))
	(the-object (make-object (intern symbol :gdl-lift-tests)) regression-test-data) 
	#+allegro (gc)
        )
      
      
      #+allegro (gc t) #-allegro (gc-all)
      
      (format output-stream "Summary For test def ~a:~%~%" symbol) 
      
      (multiple-value-bind (mem1 mem2) (#+allegro excl.osi:get-mem-info #-allegro values #-allegro 0 #-allegro 0)
	(if (or (/= mem1 mem1-orig)
		(/= mem2 mem2-orig))
	    (format output-stream "
Cumulative memory change from ~a/~a to
                              ~a/~a for a diff of 
                              ~a/~a~%"
		    mem1-orig 
		    mem2-orig 
		    mem1 
		    mem2 
		    (- mem1 mem1-orig) 
		    (- mem2 mem2-orig))
	  (format output-stream "*** Memory status unchanged. ***~%~%"))))))

(defun stress-tests (&key (symbols *gdl-test-definitions*)
			  (times 100)
			  (report-interval 10)
			  (warmup-runs 10)
			  ;;(outfile (format nil "~~/~a.out" (gwl::iso-time (get-universal-time))))
			  )
  
  ;;(with-open-file (out outfile :direction :output :if-exists :supersede 
  ;;:if-does-not-exist :create))

  (let ((out *trace-output*))
    (dolist (symbol symbols)
      (format out "Running Stresstests for ~a...~%~%" symbol)
      (with-error-handling (:timeout 20000)
	(stress-test symbol 
		     :times times 
		     :report-interval report-interval
		     :warmup-runs warmup-runs
		     :output-stream out)))))
