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

#+nil
(defmacro with-format ((format stream-or-file &rest args) &body body)
  "Void [Macro]. Used to establish an output format and a stream to which data is to be sent. This 
supports a full range of output options such as page dimensions, view transforms, view scales, etc.

:example
<pre>
 (gdl::with-format (pdf \"/tmp/box.pdf\" :view-transform (getf *standard-views* :trimetric)) 
    (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output))
</pre>"
  (let ((flag (gensym)))
    (let ((external-format (getf args :external-format))
	  (args (remove-plist-keys args (list :external-format))))
      `(let ((*%format%* (make-instance ',format ,@args)))
	 (let ((*stream* (if (or (stringp ,stream-or-file) (pathnamep ,stream-or-file))
			     (open ,stream-or-file :if-does-not-exist :create 
				   :if-exists :supersede :direction :output
				   :external-format ,(or external-format :default)

				   #+nil ,(or external-format
					      #+mswindows 
					      (excl:crlf-base-ef :1252) 
					      #-mswindows 
					      :default))
			   ,stream-or-file))
	       (,flag t))
	   (unwind-protect
	       (progn (multiple-value-prog1
			  ,(case format 
			     (pdf `(pdf:with-document() 
				     (pdf:with-page (:bounds (make-array 4 :initial-contents 
									 (list 0 0 (format-slot page-width) 
									       (format-slot page-length))))
				       (with-format-slots (page-width page-length background-color)
					 (setq background-color (coerce (lookup-color background-color 
										      :ground :background) 'list))
					 (pdf:with-saved-state 
					     (apply #'pdf:set-rgb-stroke (coerce background-color 'list))
					   (apply #'pdf:set-rgb-fill (coerce background-color 'list)) 
					   (pdf:move-to 0 0) 
					   (pdf:line-to page-width 0) 
					   (pdf:line-to page-width page-length) (pdf:line-to 0 page-length) 
					   (pdf:line-to 0 0) (pdf:fill-and-stroke)))
				       (with-format-slots (page-width page-length)
					 (pdf:translate (half page-width) (half page-length)))
				       ,@body)
				     (when *stream* (pdf:write-document *stream*))))
			     (pdf-multipage `(pdf:with-document() ,@body (when *stream* (pdf:write-document *stream*))))
			     ;;
			     ;; FLAG -- consider having default
			     ;; initialize-output and finalize-output
			     ;; methods instead of these special
			     ;; cases. Special case is only really
			     ;; needed when we need to wrap things like
			     ;; in PDF.
			     ;;
			     (dxf `(progn (write-string *dxf-header* *stream*)
					  ,@body
					  (write-string *dxf-footer* *stream*)))
			     (otherwise `(progn (write-env (initialize-output) )
						,@body
						(write-env (finalize-output)))))
			(setq ,flag nil)))
	     (when (and (or (stringp ,stream-or-file) (pathnamep ,stream-or-file)) (streamp *stream*))
	       (close *stream* :abort ,flag))) nil)))))



(defmacro with-format ((format stream-or-file &rest args) &body body)
  "Void [Macro]. Used to establish an output format and a stream to which data is to be sent. This 
supports a full range of output options such as page dimensions, view transforms, view scales, etc.

:example
<pre>
 (gdl::with-format (pdf \"/tmp/box.pdf\" :view-transform (getf *standard-views* :trimetric)) 
    (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output))
</pre>"

  (let ((flag (gensym)))
    (let (;;(external-format (getf args :external-format))
          (args (remove-plist-keys args (list :external-format))))
      `(let ((*%format%* (make-instance ',format ,@args))
	     (file? (or (stringp ,stream-or-file) (pathnamep ,stream-or-file))))
         (let ((*stream* (if file?
                             (open ,stream-or-file :if-does-not-exist :create 
                                   :if-exists :supersede :direction :output
				   :element-type '(unsigned-byte 8)
                                   ;;:external-format ,(or external-format :default)
				   )
                           ,stream-or-file))
               (,flag t))
	   
           (unwind-protect
               (progn (multiple-value-prog1
                          ,(case format 
                             (pdf `(let ((path (glisp:temporary-file)))
				     (pdf:with-document() 
				       (pdf:with-page (:bounds (make-array 4 :initial-contents 
									   (list 0 0 (format-slot page-width) 
										 (format-slot page-length))))
					 (with-format-slots (page-width page-length background-color)
					   (setq background-color (coerce (lookup-color background-color 
											:ground :background) 'list))
					   (pdf:with-saved-state 
					     (apply #'pdf:set-rgb-stroke (coerce background-color 'list))
					     (apply #'pdf:set-rgb-fill (coerce background-color 'list)) 
					     (pdf:move-to 0 0) 
					     (pdf:line-to page-width 0) 
					     (pdf:line-to page-width page-length) (pdf:line-to 0 page-length) 
					     (pdf:line-to 0 0) (pdf:fill-and-stroke)))
					 (with-format-slots (page-width page-length)
					   (pdf:translate (half page-width) (half page-length)))
					 ,@body)


				       (pdf:write-document (if file? ,stream-or-file path)))

				     (when (and *stream* (not file?))
				       (with-open-file (in path
							   :element-type '(unsigned-byte 8))
					 (do ((val (read-byte in nil nil) (read-byte in nil nil)))
					     ((null val))
					   (write-byte val *stream*))))
				     
				     (when (probe-file path) (delete-file path))))
                                     
			     ;;
			     ;; FLAG -- update multipage to match pdf case behavior
			     ;;
			     (pdf-multipage `(let ((path (glisp:temporary-file)))
					       (pdf:with-document() ,@body 
								 (pdf:write-document (if file? ,stream-or-file path)))
					       (when (and *stream* (not file?))
						 (with-open-file (in path :element-type '(unsigned-byte 8))
						   (do ((val (read-byte in nil nil) (read-byte in nil nil)))
						       ((null val))
						     (write-byte val *stream*))))
					       (when (probe-file path) (delete-file path))))

			     
			     #+nil
                             (pdf-multipage `(pdf:with-document() ,@body 
							       (when *stream* (pdf:write-document *stream*))))
                             ;;
                             ;; FLAG -- consider having default
                             ;; initialize-output and finalize-output
                             ;; methods instead of these special
                             ;; cases. Special case is only really
                             ;; needed when we need to wrap things like
                             ;; in PDF.
                             ;;
                             (dxf `(progn (write-string *dxf-header* *stream*)
                                          ,@body
                                          (write-string *dxf-footer* *stream*)))
                             (otherwise `(progn (write-env (initialize-output) )
                                                ,@body
                                                (write-env (finalize-output)))))
                        (setq ,flag nil)))
             (when (and (or (stringp ,stream-or-file) (pathnamep ,stream-or-file)) (streamp *stream*))
               (close *stream* :abort ,flag))) nil)))))
