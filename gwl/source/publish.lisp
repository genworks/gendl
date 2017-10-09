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

(defparameter *remote-hash-lock* (bt:make-lock "remote-hash-lock"))
(defparameter *remote-proxy-lock* (bt:make-lock "remote-proxy-lock"))
(defparameter *remote-evaluate-lock* (bt:make-lock "remote-evaluate-lock"))
(defparameter *remote-fetch-input-lock* (bt:make-lock "remote-fetch-input-lock"))

(defparameter *request-server-ipaddr* nil)

(defun decode-arglist-from-url (argstring)
  (base64-decode-list argstring))

(defun remote-function-handler (handler &key (encode t)) ;;&optional lock
  #'(lambda (req ent)
      (progn ;; bt:with-lock-held (lock)
        (let* ((socket (request-socket req))
               (*request-server-ipaddr* (socket:ipaddr-to-dotted (socket:local-host socket)))
               (*ipaddr* (socket:ipaddr-to-dotted (socket:remote-host socket)))
               (query (request-query req))
               (args (rest (assoc "args" query :test #'string-equal)))
               (args-list (decode-arglist-from-url args)))
          (glisp:with-heuristic-case-mode ()
            (with-http-response (req ent)
              (with-http-body (req ent)
                (let ((value (funcall handler args-list)))
                  (if encode
                      (let ((encoded-value (base64-encode-safe (format nil "~s" (encode-for-http value)))))
                        (html (format *html-stream* "~a" encoded-value)))
                      (html (format *html-stream* "~s" value)))))))))))

(defun publish-dgdl-funcs (server)
  
  (publish-file :path "/favicon.ico"
                :server server
                :file (format nil "~a"
			      (if glisp:*genworks-source-home*
				  (merge-pathnames "gwl/static/gwl/images/favicon.ico" glisp:*gendl-source-home*)
				  (merge-pathnames "static/gwl/images/favicon.ico" glisp:*gdl-home*))))



  (publish :path "/fetch-remote-input"
	   :server server
	   :function
           (remote-function-handler
            #'(lambda (args-list)
                ;;
                ;; FLAG -- consider a warning if package not found
                ;;
                (let ((*package* (or (find-package (getf args-list :package)) *package*)))
                  (let* ((object (the-object (gethash (getf args-list :remote-id) *remote-objects-hash*)
                                             (follow-root-path (getf args-list :remote-root-path))))

                         (part-name (getf args-list :part-name))

                         (index (getf args-list :index))

                         (child (if index
                                    (the-object object ((evaluate part-name) index))
                                    (the-object object (evaluate part-name))))

                         (message (getf args-list :message))
                         (gdl::*notify-cons* (decode-from-http (getf args-list :notify-cons)))

                         (args (getf args-list :args)))
                    (if object (multiple-value-bind (value error)
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
                        (list :error :no-such-object (getf args-list :remote-id))))))))


  (publish :path "/unbind-slots"
	   :server server
	   :function
           (remote-function-handler
            #'(lambda (args-list)
                (let ((object (let ((root (the-object (gethash (getf args-list :remote-id)
                                                               *remote-objects-hash*))))
                                (when root (the-object root (follow-root-path
                                                             (getf args-list :remote-root-path))))))
                      (slot (getf args-list :slot)))

                  (when object (gdl::unbind-dependent-slots object slot))

                  nil))))


  (publish :path "/send-remote-message"
	   :server server
	   :function (remote-function-handler 'send-remote-message))



  (publish :path "/send-remote-output"
	   :server server
	   :function
           (remote-function-handler
            #'(lambda (args-list)
                (let ((*package* (or (find-package (getf args-list :package)) *package*)))
                  (let ((object (the-object (gethash (getf args-list :remote-id) *remote-objects-hash*)
                                            (follow-root-path (getf args-list :remote-root-path))))
                        (*%format%* (apply #'make-instance (getf (getf args-list :format) :type)
                                           (decode-from-http (rest (rest (getf args-list :format))))))
                        (message (getf args-list :message))
                        (args (getf args-list :args)))
                    (with-output-to-string (*stream*)
                      (apply (glisp:intern message :gdl-output)
                             *%format%* object  t ;; flag pick up skin
                             args)))))))

  ;;
  ;; FLAG -- return proper error when requested object type does not exist
  ;; or make-object fails for any other reason.
  ;;
  (publish :path "/make-remote-object"
	   :server server
	   :function
           (remote-function-handler
            #'(lambda (args-list)
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

                  (setf parent-form
                        (multiple-value-bind (type plist) (destructure-object-from-http parent-form)
                          (setf (getf plist :host) *ipaddr*)
                          (construct-object-for-http type plist)))

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
                          (list (decode-from-http parent-form) nil t))
                    (setf (gethash new-id *remote-objects-hash*) object)
                    (format t "~&~%Created new remote object with ID ~s and arglist:
~s~%~%"
                            new-id rest-args)
                    new-id)))
            :encode nil)))

(pushnew 'publish-dgdl-funcs *publishers*)

(defun send-remote-message (args-list)
  (when *debug?*
    (format t "~%In send-remote-message-object response func:~%")
    (print-variables args-list *ipaddr* *request-server-ipaddr*))

  (let ((*package* (or (find-package (getf args-list :package)) *package*)))
    (let ((object (when (gethash (getf args-list :remote-id) *remote-objects-hash*)
                    (the-object (gethash (getf args-list :remote-id) *remote-objects-hash*)
                                (follow-root-path (getf args-list :remote-root-path)))))
          (message (getf args-list :message))
          (gdl::*notify-cons* (decode-from-http (getf args-list :notify-cons)))
          (args (getf args-list :args)))

      (if object (multiple-value-bind (value error)
                     (progn ;; bt:with-lock-held (*remote-evaluate-lock*)
                       (ignore-errors (if args
                                          (the-object object ((evaluate message)
                                                              (:apply args)))
                                          (the-object object (evaluate message)))))

                   (if (typep error 'error)
                       (progn
                         (when *debug?* (error error))
                         (list :error (format nil "~a" error)))
                       value))
          (list :error :no-such-object (getf args-list :remote-id))))))






(defmethod evaluate-object ((category (eql :remote-gdl-instance)) args)
  (let ((hash-key (list (getf args :id) (getf args :root-path))))
    (progn ;; bt:with-lock-held (*remote-proxy-lock*)
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
								  (if (typep error 'error) :unknown result)) nil t))))))))



(defun clear-dgdl ()
  (clrhash *remote-proxies-hash*)
  (clrhash *remote-objects-hash*))
