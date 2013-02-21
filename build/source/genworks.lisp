;;
;; Copyright 2002-2011 Genworks International 
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


(defun copy-file (from to &key overwrite?)
  (fad:copy-file from to :overwrite overwrite?))

;;
;; FLAG -- replace with cl-fad version.
;;
(defun copy-directory (from-dir to-dir &rest args)
  (declare (ignore args))
  #+allegro (excl:copy-directory from-dir to-dir)
  #+lispworks (cl-copy-directory to-dir from-dir)
  #+clozure (ccl::recursive-copy-directory from-dir to-dir ))


(defun delete-directory-and-files (target &key force quiet (if-does-not-exist :error))
  #-allegro (declare (ignore force quiet))
  (cond ((probe-file target)
	 #+lispworks (system:run-shell-command (format nil "rm -rf ~a" target))
	 #+allegro (excl.osi:delete-directory-and-files 
		    target :force force :quiet quiet)
	 #-(or allegro (and unix lispworks))
	 (cl-fad:delete-directory-and-files target :if-does-not-exist if-does-not-exist))
	((null if-does-not-exist) nil)
	(t (ecase if-does-not-exist
	     (:ignore nil)
	     (:warn (warn "Target ~s does not exist.~%" target))
	     (:error (error "Target ~s does not exist.~%" target))))))


#+allegro
(ff: def-foreign-call (memory-status-dump "memory_status_dump") ())


(defun dump-memory (&key (output-name "mem")
		    (output-type "txt")
		    (output-path (merge-pathnames (make-pathname :name output-name
								 :type output-type)
						  (glisp:temporary-folder))))
  #+allegro
  (let ((status (memory-status-dump (namestring (translate-logical-pathname output-path)))))
    (unless (zerop status) (error "~&memory-status-dump failed with non-zero return code: ~a~%~%" status))
    output-path)
  #-allegro (declare (ignore output-path))
  #-allegro
  (error "Need implementation of dump-memory for currently running lisp."))

(defun implementation-identifier ()
  (asdf::implementation-identifier))

#-allegro(warn "~&Please implement generate-application for the currently running lisp.~%")
(defun make-gdl-app (&rest args)
  #+allegro 
  (let ((app-name (getf args :application-name))
        (target (getf args :destination-directory))
        (modules (append (list :asdf :compftype 
			       :aserve :phtml #+mswindows :winapi)
                         (getf args :modules)))
        (args (normalize-generate-application-args args)))
    (apply #'excl:generate-application app-name target modules args))
  #-allegro (declare (ignore args))
  #-allegro (error "~&generate-application is not implemented for the currently running lisp.~%"))


#+allegro 
(defun normalize-generate-application-args (args)
  (let ((class (getf args :application-class)))  
    (list :runtime (case class 
                     (:development :partners)
                     (otherwise :standard))
        
          :include-compiler (case class 
                              (:development t)
                              (otherwise nil))
        
          :include-devel-env (case class 
                               (:development t)
                               (otherwise nil))

          :icon-file (merge-pathnames "gdl/gwl/static/gwl/images/favicon.ico"
                                      glisp:*genworks-source-home*)
          
          :demo (getf args :demo-days)
	  :lisp-heap-size (getf args :lisp-heap-size)
	  :close-oldspace t
          :init-file-names (getf args :init-file-names)
          :pre-load-form (getf args :pre-load-form)
          :post-load-form (getf args :post-load-form)
          :restart-init-function (getf args :restart-init-function)
          :purify t
          :autoload-warning nil
          :runtime-bundle t
          :suppress-allegro-cl-banner t)))



(defun next-datestamp (prefix base)
  (let* ((base-length (length base))
	 (directory (directory prefix))
	 (files (mapcar #'(lambda(item)
			    (let ((name (pathname-name item))
				  (type (pathname-type item)))
			      (format nil "~a~a~a" name 
				      (if type "." "")
				      (or type "")))) directory))
	 (matches (sort (remove-if-not #'(lambda(file)
					   (and (> (length file) base-length)
						(string-equal (subseq file 0 base-length) base))) files)
			#'string-greaterp))
	 (datestamps (remove 
		      nil
		      (mapcar #'(lambda(match)
				  (let ((datestamp (subseq match (1+ base-length))))
				    (if (not (get-decoded-datestamp datestamp))
					(warn "Invalid datestamp ~a on file ~a in directory ~a.~%"
					      datestamp match prefix) datestamp))) matches)))
	 (latest (first datestamps)))
    (multiple-value-bind (latest-serial-number latest-date latest-month latest-year)
	(when latest (get-decoded-datestamp latest))
      (multiple-value-bind (seconds minutes hours date month year) (get-decoded-time)
	(declare (ignore seconds minutes hours))
	(let ((new-serial-number (if (and latest (= latest-date date)
					  (= latest-month month) (= latest-year year))
				     (1+ latest-serial-number) 0)))
	  (format nil "~a~2,'0d~2,'0d~2,'0d" year month date new-serial-number))))))



(defun get-decoded-datestamp (datestamp)
  (let (*read-eval*)
    (when (and (= (length datestamp) 10)
	       (integerp (read-from-string datestamp)))
      (let ((year (read-from-string (subseq datestamp 0 4)))
	    (month (read-from-string (subseq datestamp 4 6)))
	    (date (read-from-string (subseq datestamp 6 8)))
	    (serial-number (read-from-string (subseq datestamp 8 10))))
	(when (ignore-errors (encode-universal-time 0 0 0 date month year))
	  (values serial-number date month year))))))


