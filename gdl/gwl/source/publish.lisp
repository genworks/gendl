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

(defparameter *bug* nil)

(with-all-servers (server)
  (publish-file :path "/favicon.ico"
                :server server
                :file (merge-pathnames "gdl/gwl/static/gwl/images/favicon.ico" glisp:*genworks-source-home*)))


(publish :path "/fetch-remote-input"
         :function
         #'(lambda(req ent)
             (let* ((query (request-query req))
                    (args (rest (assoc "args" query :test #'string-equal)))
                    (*ipaddr* (socket:ipaddr-to-dotted (socket:remote-host (request-socket req)))))
               (let ((args-list (base64-decode-list args)))
                 ;;
                 ;; FLAG -- consider a warning if package not found
                 ;;
                 (let ((*package* (or (find-package (getf args-list :package)) *package*)))
                   (let ((object (the-object (gethash (getf args-list :remote-id) *remote-objects-hash*)
                                             (follow-root-path (getf args-list :remote-root-path))))
                         (child (evaluate-object (first (getf args-list :child)) (rest (getf args-list :child))))
                         (message (getf args-list :message))
                         (gdl::*notify-cons* (decode-from-http (getf args-list :notify-cons)))
                         (part-name (getf args-list :part-name))
                         (args (getf args-list :args)))
                     (with-http-response (req ent)
                       (with-http-body (req ent)
                         (let ((value (if object (multiple-value-bind (value error)
                                                     (ignore-errors 
                                                       (apply (symbol-function (glisp:intern message :gdl-inputs))
                                                              object (glisp:intern part-name :gdl-acc) child args))
						   (if (typep error 'error) 
						       (let ((error-string 
							      (glisp:replace-regexp 
							       (format nil "~a" error) "\\n" " ")))
							 (cond ((or (search "could not handle"
									    error-string)
								    #+nil
								    (search "which is the root"
									    error-string)
								    (search "instances could handle"
									    error-string))
								'gdl-rule:%not-handled%)
							       (t (when *debug?* 
								    (format t "Throwing error on fetch-input server because gwl::*debug?* is set to non-nil~%")
								    (error error))
								  (list :error (format nil "~a" error)))))
                                                       value))
                                          (list :error :no-such-object (getf args-list :remote-id)))))
                           (let ((encoded-value (base64-encode-safe (format nil "~s" (encode-for-http value)))))
                             (html (format *html-stream* encoded-value))))))))))))


(publish :path "/unbind-slots"
         :function
         #'(lambda(req ent)
             (let* ((query (request-query req))
                    (args (rest (assoc "args" query :test #'string-equal)))
                    (*ipaddr* (socket:ipaddr-to-dotted (socket:remote-host (request-socket req)))))
               (let ((args-list (base64-decode-list args)))
                 (let ((object (let ((root (the-object (gethash (getf args-list :remote-id)
                                                                *remote-objects-hash*))))
                                 (when root (the-object root (follow-root-path
                                                              (getf args-list :remote-root-path))))))
                       (slot (getf args-list :slot)))

                   (when object (gdl::unbind-dependent-slots object slot))
                             
                   (with-http-response (req ent)
                     (with-http-body (req ent)
                       (let ((value nil))
                         (let ((encoded-value (base64-encode-safe (format nil "~s" (encode-for-http value)))))
                           (html (format *html-stream* encoded-value)))))))))))


(defun send-remote-message (req ent)
  (let* ((query (request-query req))
         (args (rest (assoc "args" query :test #'string-equal)))
         (*ipaddr* (socket:ipaddr-to-dotted (socket:remote-host (request-socket req)))))
    (let ((args-list (base64-decode-list args)))
      
      (when *debug?*
	(format t "~%In send-remote-message-object response func:~%")
	(print-variables args-list))

      (let ((*package* (or (find-package (getf args-list :package)) *package*)))
        (let ((object (when (gethash (getf args-list :remote-id) *remote-objects-hash*)
                        (the-object (gethash (getf args-list :remote-id) *remote-objects-hash*)
                                    (follow-root-path (getf args-list :remote-root-path)))))
              (message (getf args-list :message))
              (gdl::*notify-cons* (decode-from-http (getf args-list :notify-cons)))
              (args (getf args-list :args)))
          
          (with-http-response (req ent)
            (with-http-body (req ent)
              (let ((value (if object (multiple-value-bind (value error)
                                          (glisp:w-o-interrupts 
                                            (ignore-errors (if args
                                                               (the-object object ((evaluate message)
                                                                                   (:apply args)))
                                                             (the-object object (evaluate message)))))
					
                                        (if (typep error 'error)
                                            (progn
                                              (when *debug?* (error error))
                                              (list :error (format nil "~a" error)))
                                          value))
                             (list :error :no-such-object (getf args-list :remote-id)))))
		
                (let ((encoded-value (base64-encode-safe (format nil "~s" (encode-for-http value)))))
                  (html (format *html-stream* encoded-value)))))))))))


(publish :path "/send-remote-message"
         :function 'send-remote-message)

         

(publish :path "/send-remote-output"
         :function #'(lambda(req ent)
                       (let* ((query (request-query req))
                              (args (rest (assoc "args" query :test #'string-equal)))
                              (*ipaddr* (socket:ipaddr-to-dotted (socket:remote-host (request-socket req)))))
                         (let ((args-list (base64-decode-list args)))
                           (let ((*package* (or (find-package (getf args-list :package)) *package*)))
                             (let ((object (the-object (gethash (getf args-list :remote-id) *remote-objects-hash*)
                                                       (follow-root-path (getf args-list :remote-root-path))))
                                   (*%format%* (apply #'make-instance (getf (getf args-list :format) :type)
                                                      (decode-from-http (rest (rest (getf args-list :format))))))
                                   (message (getf args-list :message))
                                   (args (getf args-list :args)))
                               (with-http-response (req ent)
                                 (with-http-body (req ent)
                                   (let ((value
                                          (with-output-to-string(*stream*)
                                            (apply (glisp:intern message :gdl-output)
                                                   *%format%* object  t ;; flag pick up skin
                                                   args))))
                                     (let ((encoded-value
                                            (base64-encode-safe (format nil "~s" (encode-for-http value)))))
                                       (html (format *html-stream* encoded-value))))))))))))


;;
;; FLAG -- return proper error when requested object type does not exist
;; or make-object fails for any other reason.
;;
(publish :path "/make-remote-object"
         :function
         #'(lambda(req ent)
             (let* ((query (request-query req))
                    (ipaddr (socket:ipaddr-to-dotted (socket:remote-host (request-socket req))))
                    (*ipaddr* (socket:ipaddr-to-dotted (socket:remote-host (request-socket req))))
                    (args (rest (assoc "args" query :test #'string-equal))))

               (let ((args-list (base64-decode-list args)))
		 
		 (when *debug?*
		   (format t "~%In make-remote-object response func:~%")
		   (print-variables args-list))

                 (let ((*package* (or (find-package (getf args-list :package)) *package*))
                       (name (getf args-list :name))
                       (index (getf args-list :index))
                       (rest-args (remove-plist-keys args-list
                                                     (list :parent-form :current-id  :name :index
                                                           :type :package :host :port)))
                       (current-id (getf args-list :current-id))
                       (parent-form (getf args-list :parent-form)))

                   (setf (getf (rest parent-form) :host) ipaddr)

                   (when current-id
                     (let ((removed? (remhash current-id *remote-objects-hash*)))
                       (when removed?
                         (format t "~&~%Removed stale remote object with ID ~s.~%~%" current-id))))
                   (let ((object (make-object (read-safe-string (getf args-list :type))
                                                        
                                              :type (read-safe-string (getf args-list :type))))
                         (new-id (make-keyword (make-new-instance-id))))
                               
                     (the-object object (set-slot! :%name% name :warn-on-non-toplevel? nil))
                     (the-object object (set-slot! :remote-id new-id :remember? nil :warn-on-non-toplevel? nil))
                     (the-object object (set-slot! :%index% index :warn-on-non-toplevel? nil))
                     (setf (slot-value object 'gdl-acc::%parent%) 
                       (list (evaluate-object (first parent-form) (rest parent-form)) nil t))
                     (setf (gethash new-id *remote-objects-hash*) object)
                     (format t "~&~%Created new remote object with ID ~s and arglist:
~s~%~%"
                             new-id rest-args)
                     (with-http-response(req ent)
                       (with-http-body (req ent) (html (format *html-stream* "~s" new-id))))))))))
                       


(defmethod evaluate-object ((category (eql :remote-gdl-instance)) args)
  (let ((hash-key (list (getf args :id) (getf args :root-path))))
    (or (gethash hash-key gwl::*remote-proxies-hash* )
        (progn
          (format t "~%~%~s Not found in hash - creating fresh ~%~%" (getf args :id))
          (setf (gethash hash-key *remote-proxies-hash*)
            (gdl::make-object-internal 'remote-object 
                                       :%parent% (list nil nil t)
                                       :%index% (list (getf args :index) nil t)
                                       :remote-id (list (getf args :id) nil t)
                                       :host (list (or *ipaddr* (getf args :host)) nil t)
                                       :port (list (getf args :port) nil t)
                                       :remote-root-path (list (getf args :root-path) nil t)
                                       :remote-type (list (multiple-value-bind (result error)
                                                              (ignore-errors (read-from-string (getf args :type)))
                                                            (if (typep error 'error) :unknown result)) nil t)))))))
  


(defun clear-dgdl ()
  (clrhash *remote-proxies-hash*)
  (clrhash *remote-objects-hash*))
