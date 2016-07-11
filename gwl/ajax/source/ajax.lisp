;;
;; Copyright 2002-2011 Genworks International 
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

(defparameter *ajax-snap-restore?* nil)

(defparameter *self* nil)

(defun quick-save (self &key (snap-folder (glisp:snap-folder)))
  (when *ajax-snap-restore?*
    (let ((snap-file 
	   (merge-pathnames 
	    (make-pathname :name (format nil "~a" (the instance-id))
			   :type "snap") snap-folder)))
      (with-error-handling ()
	(the (write-snapshot :filename snap-file))))))


(defun restore-from-snap (iid)
  (when *ajax-snap-restore?*
    (let ((new-self
	   (let ((snap-file 
		  (merge-pathnames 
		   (make-pathname :name (format nil "~a" iid) 
				  :type "snap") (glisp:snap-folder))))
	     (when (probe-file snap-file)
	       (with-error-handling ()
		 (read-snapshot :filename snap-file
				:keys-to-ignore (list :time-last-touched 
						      :time-instantiated 
						      :expires-at)))))))
        
      (when new-self
	(setf (gethash (make-keyword (the-object new-self instance-id)) *instance-hash-table*)
	      (list new-self nil))
	(when (typep new-self 'session-control-mixin) (the-object new-self set-expires-at))
	(the-object new-self set-instantiation-time!)
	(the-object new-self set-time-last-touched!)
          
	(format t "~%~%*************** Session ~a Restarted! ***************~%~%" 
		(the-object new-self instance-id))
	  
	(the-object new-self custom-snap-restore!)
	  
	)
    
      ;;
      ;; FLAG for testing only. 
      ;; return error object instead of just throwing an error here. 
      ;;
      (unless new-self (error "Restoring of session ~a was not possible.~%" iid))

      new-self)))

(defparameter *new-debug?* nil)

(defun gdlAjax (req ent)

  (let* ((query-plist (gwl::assoc-list-to-plist (request-query req)))
         (plist (progn ;;(when *debug?* (print-variables query-plist))
		  (when *new-debug?* (print-variables query-plist))
		  (read-safe-string 
		   (base64-decode-safe (coerce (getf query-plist :|args|) 'string)))))
         (fields 
          (progn (when *new-debug?* (print-variables plist))
                 (let ((encoded-fields
                        (read-safe-string 
                         (base64-decode-safe (coerce (getf query-plist :|fields|) 
                                                     'string)))))
                   (mapcan 
                    #'(lambda(key value)
                        (list key 
                              (if (or (glisp:match-regexp "^radio-" (string-downcase (format nil "~a" key)))
                                      (glisp:match-regexp "-checkedp$" (string-downcase (format nil "~a" key))))
                                  value
				  (base64-decode-safe value))))
                    (plist-keys encoded-fields)
                    (plist-values encoded-fields)))))
         (raw-fields (getf query-plist :|raw-fields|))
         (iid (getf plist :|iid|))

         (self (first (gethash (make-keyword-sensitive iid) gwl:*instance-hash-table*)))

	 (self (or self (restore-from-snap iid)))

	 (plist-raw plist)
         (plist (decode-from-ajax plist self))
         (bashee (getf plist :|bashee|))
         (respondent (progn (when *debug?* (print-variables plist))
                            (or (getf plist :|respondent|) bashee)))
         (function (getf plist :|function|))
	 (js-vals (let ((jsvals (getf query-plist :|jsvals|)))
		    (when jsvals (read-from-string jsvals))))
         (arguments (getf plist :|arguments|)))
    


    (setq *self* self)
    
    (when *debug?*
      (let ((bashee-root (the-object bashee root-path))
            (respondent-root (the-object respondent root-path)))
        (print-variables plist-raw plist raw-fields bashee-root respondent-root fields function)
	))

    
    (let ((*clicked-x* (let ((string (getf query-plist :|x|)))
                         (when string (ignore-errors (parse-integer string)))))
          (*clicked-y* (let ((string (getf query-plist :|y|)))
                         (when string (ignore-errors (parse-integer string)))))

	  (js-to-eval-previi (mapcar #'(lambda(section)
					   (the-object section js-to-eval))
				       (the-object respondent html-sections))))
      
      (let ((f-e-p (make-object 'form-element-processor 
                                :bashee bashee 
                                :query-plist (merge-plist-duplicates fields))))
        
        (the-object f-e-p  validate-and-set!)
        (when *debug?* (setq *f-e-p* f-e-p)))
      
      (when js-vals (the-object bashee (set-js-vals! js-vals)))
      
      (when function 
        (with-error-handling (:timeout *ajax-timeout*) 
          (the-object bashee ((evaluate function) (:apply arguments)))))

      (when (and respondent (the-object respondent root) 
                 (typep (the-object respondent root) 'session-control-mixin))
        (the-object respondent root (set-expires-at)))
      
      (when (and respondent (the-object respondent root) 
                 (the-object respondent root (set-time-last-touched!))))
      
      (when (and respondent (the-object respondent root) 
                 (the-object respondent root (set-remote-host! req))))
      
      (quick-save self)

      (respond-with-new-html-sections req ent respondent
				      :js-to-eval-previi js-to-eval-previi))))


(defun publish-gdlajax ()
  (with-all-servers (server)
    (publish :path "/gdlAjax" :function 'gdlAjax :server server)))

(publish-gdlajax)

;;
;; FLAG -- remove this defunct version. 
;;
;; (publish :path "/gdlAjax" :function 'gdlAjax)


(defun wrap-cdata (string)
  (string-append "<![CDATA[" string "]]>"))


(defun respond-with-nothing (req ent)
  (with-http-response (req ent :content-type "text/xml")
    (with-http-body (req ent)
      (with-html-output(*html-stream* nil)
        ))))


(defun respond-with-new-html-sections (req ent respondent &key js-to-eval-previi)
  (the-object (make-object 'ajax-response :req req :ent ent :respondent respondent :js-to-eval-previi js-to-eval-previi)
	      respond!))

(defparameter *current-status* nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *current-status* *compile-dependency-tracking?*)
  (setq *compile-dependency-tracking?* nil))

(define-object ajax-response ()

  :input-slots (req ent respondent (js-to-eval-previi nil))

  :computed-slots 
  ((security-ok? (the respondent root do-security-check))
   (replace-lists (mapsend (the html-sections) :replace-list))
   (html-section-objects (the respondent html-sections))
   (mismatch? (< (length (the js-to-eval-previi)) (length (the replace-lists)))))

  :objects
  ((html-sections :type 'html-replacement-section
		  :sequence (:size (length (the html-section-objects)))
		  :js-to-eval-previous (nth (the-child index) (the js-to-eval-previi))
		  :section (nth (the-child index) (the html-section-objects))))
  
  :functions
  ((respond! 
    ()

    (when *debug?* (print-messages replace-lists))

    (when (the mismatch?)
      (warn "the js-to-eval-previous is not the same length as html-section-objects in ~s~%" 
	    (the respondent)))
    (with-http-response ((the req) (the ent) :content-type "text/xml")
      (with-http-body ((the req) (the ent))
	(with-html-output (*html-stream* nil)
	  (:document
	     (mapc #'(lambda(replace-pair js-to-eval-status)
		       (declare (ignore js-to-eval-status)) ;; this can play into the flag
		       (destructuring-bind (&key dom-id inner-html js-to-eval) replace-pair
			 (let ((js-to-eval? (and js-to-eval (not (string-equal js-to-eval "")))))
			   (when (or inner-html js-to-eval?)
			     (htm
			      (:html-section (:|replaceId| (str dom-id))
					     (:|newHTML| (str (if inner-html
								  (wrap-cdata (if (the security-ok?) inner-html
										  (with-cl-who-string ()
										    (:i "Security Error")))) "")))
					     (:|jsToEval| (str (if (and js-to-eval? (the security-ok?))
								   (wrap-cdata js-to-eval) "")))))))))
		   (the replace-lists) (the js-to-eval-previi)))))))))


(define-object html-replacement-section ()
  :input-slots (section js-to-eval-previous)
  :computed-slots ((status (the section (slot-status :inner-html)))
		   (js-status (the section (slot-status :js-to-eval)))
		   (stale? (or (eql (the status) :unbound)
			       (eql (the js-status) :unbound)))

		   (dom-id (the section dom-id))

		   (inner-html (when (eql (the status) :unbound)
				 (let ((timeout? nil))
				   (multiple-value-bind (value error backtrace)
				       (ignore-errors-with-backtrace (the section inner-html))
				     (cond ((typep error 'error)
					    (with-cl-who-string () 
					      (:i (esc (format nil "!! This section threw error: ~a !!" 
							       error)))
					      (:pre (esc backtrace))))
					   (timeout?
					    (with-cl-who-string ()
					      (:i (fmt "!! This section timed out after ~a seconds.
You can reload to get previous state" *ajax-timeout*))
					      (:pre (esc backtrace))))
					   (t value))))))

		   (js-to-eval (multiple-value-bind (value error)
				   (ignore-errors (the section js-to-eval))
				 (if (typep error 'error)
				     (with-cl-who-string ()
				       (:i (fmt "alert('This section threw error: ~a !!');" error)))
				     (if (equalp value :parse) "parseme" value))))


		   (replace-list (when (the stale?)
				   (list :dom-id (the dom-id)
					 :inner-html (the inner-html)
					 :js-to-eval (the js-to-eval))))))
		   
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *compile-dependency-tracking?* *current-status*))


#+nil
(defun remove-html-sections (sections string)
  (remove-substrings string (mapsend sections :main-div)))

#+nil
(defun remove-substrings (string substrings)
  (let ((result string))
    (dolist (substring substrings result)
      (setq result (replace-substring result substring "")))))




