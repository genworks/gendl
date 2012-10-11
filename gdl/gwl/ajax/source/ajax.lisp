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


(defun gdlAjax (req ent)

  (let* ((query-plist (gwl::assoc-list-to-plist (request-query req)))
         (plist (progn (when *debug?* (print-variables query-plist))
                       (read-safe-string 
                        (base64-decode-safe (coerce (getf query-plist :|args|) 'string)))))
         (fields 
          (progn (when *debug?* (print-variables plist))
                 (let ((encoded-fields
                        (read-safe-string 
                         (base64-decode-safe (coerce (getf query-plist :|fields|) 
                                                     'string)))))
                   (mapcan 
                    #'(lambda(key value)
                        (list key 
                              (if (or (glisp:match-regexp "^radio-" (format nil "~a" key))
                                      (glisp:match-regexp "-checkedp$" (format nil "~a" key)))
                                  value
				  (base64-decode-safe value)
				  ;;value

				  )))
                    (plist-keys encoded-fields)
                    (plist-values encoded-fields)))))
         (raw-fields (getf query-plist :|raw-fields|))
         (iid (getf plist :|iid|))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (plist (decode-from-ajax plist self))
         (bashee (getf plist :|bashee|))
         (respondent (progn (when *debug?* (print-variables plist))
                            (or (getf plist :|respondent|) bashee)))
         (function (getf plist :|function|))
         (arguments (getf plist :|arguments|)))
    

    
    (when *debug?*
      (let ((bashee-root (the-object bashee root-path))
            (respondent-root (the-object respondent root-path)))
        (print-variables raw-fields bashee-root respondent-root fields function)))

    
    (let ((*clicked-x* (let ((string (getf query-plist :|x|)))
                         (when string (ignore-errors (parse-integer string)))))
          (*clicked-y* (let ((string (getf query-plist :|y|)))
                         (when string (ignore-errors (parse-integer string))))))
      
      (let ((f-e-p (make-object 'form-element-processor 
                                :bashee bashee 
                                :query-plist (merge-plist-duplicates fields))))
        
        (the-object f-e-p  validate-and-set!)
        (when *debug?* (setq *f-e-p* f-e-p)))

      
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

      (respond-with-new-html-sections req ent respondent))))


(publish :path "/gdlAjax" :function 'gdlAjax)



(defun wrap-cdata (string)
  (string-append "<![CDATA[" string "]]>"))


(defun respond-with-nothing (req ent)
  (with-http-response (req ent :content-type "text/xml")
    (with-http-body (req ent)
      (with-html-output(*html-stream* nil)
        ))))


(defun respond-with-new-html-sections (req ent self ;;current-body-sans-sections
                                       )
  
  (let ((security-ok? (the root do-security-check)))
    (let (replace-list)
      (dolist (section (the html-sections) (reverse replace-list))
        (let ((status (the-object section (slot-status :inner-html)))
              (js-status (the-object section (slot-status :js-to-eval))))
        
          (when *debug?* (print-variables (the-object section root-path) status js-status))
        
          (when (or (eql status :unbound)
                    (eql js-status :unbound))
            (push (list (the-object section dom-id)
                        (let ((timeout? nil))
                          (multiple-value-bind (string error backtrace)
                              (ignore-errors-with-backtrace
                                (the-object section inner-html))
                            (cond ((typep error 'error)
                                   (with-output-to-string(ss)
                                     (with-html-output(ss)
                                       (:i (esc (format nil "!! This section threw error: ~a !!" 
                                                error)))
                                       (:pre (esc backtrace)))))
                                  (timeout?
                                   (with-output-to-string(ss)
                                     (with-html-output(ss)
                                       (:i (fmt "!! This section timed out after ~a seconds.
You can reload to get previous state" *ajax-timeout*))
                                       (:pre (esc backtrace)))))
                                  (t string))))
			(if t ;;(eql js-status :unbound)
			    (multiple-value-bind (string error)
				(ignore-errors (the-object section js-to-eval))
			      (if (typep error 'error)
				  (with-output-to-string(ss)
				    (with-html-output(ss)
				      (:i (fmt "alert('This section threw error: ~a !!');" error))))
				  (or string (the-object section js-always-to-eval) "")))

			    ""))
                  replace-list)))

	(when *debug?* (print-variables (the-object section js-always-to-eval)))
	)
    


      (when *debug?* (print-variables replace-list))
        
    
      (with-http-response (req ent :content-type "text/xml")
        (with-http-body (req ent)
          (with-html-output(*html-stream* nil)
            (:document
             (dolist (replace-pair replace-list)
               (htm
                (:html-section (:|replaceId| (str (first replace-pair)))
                               (:|newHTML| (str (wrap-cdata (if security-ok?
                                                                (second replace-pair)
                                                              "<i>Security Error</i>"))))
                               (:|jsToEval| (when (third replace-pair)
					      (str (wrap-cdata (if security-ok?
								   (third replace-pair)
								   "")))))))))))))))

(defun remove-html-sections (sections string)
  (remove-substrings string (mapsend sections :main-div)))


(defun remove-substrings (string substrings)
  (let ((result string))
    (dolist (substring substrings result)
      (setq result (replace-substring result substring "")))))
