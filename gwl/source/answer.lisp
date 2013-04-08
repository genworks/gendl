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

(defparameter *f-e-p* nil)


(define-object session-control-mixin ()
  
  :documentation (:description "Mixin to the root object of the part which you wish to have session control over"
                  :author "Brian Sorg, Liberating Insight LLC (revised Dave Cooper, Genworks)")
  
  :input-slots
  (("Universal time after which the session should expire" 
    expires-at nil :settable)
   
   ("Length of time a session should last without activity in minutes"
    session-duration 20)

   ("Boolean. Determines whether expired sessions are replaced by recovery object. Default is nil."
    use-recovery-object? nil)
   
   ("Expiration time of the recovery object. After the recovery object has replaced the orginal 
instance at what time should the recovery instance expire?"
    recovery-expires-at (+ (get-universal-time) (* 60 (the session-duration))))
   
   ("Url to which a user will be redirected if requesting a session that has been cleared"
    recovery-url *recovery-url-default*)
   
   ("Type of original object, useful when viewing session report log"
    org-type (type-of self))
      
   (;; For future use
    %state-plist% '(0)))
  
  :functions
  (("This is the function called to check for and handle session control
:&key ((debug? nil) \"Boolean. Prints debug statement if needed\")" 
    clear-expired-session
    (&key debug?)
    (when debug?
     (format t "Validating instance ~a~% of class ~a~% Current Universal Time: ~a.~% Expiration Time: ~a.~% Clear now? ~a.~%"
                (the instance-id)
                (class-of self)
                (get-universal-time)
                (the expires-at)
                (the (clear-now?))))
    (when (the clear-now?)
      (the (session-clean-up))
      (glisp:w-o-interrupts (clear-instance (the instance-id))
        (when (and (the use-recovery-object?) (the recovery-expires-at))
          (let ((recovery (make-object 'session-recovery 
                                       :instance-id (the instance-id)
                                       :expires-at (the recovery-expires-at)
                                       :recovery-url (the recovery-url)
                                       :state-plist (the %state-plist%)
                                       :org-type (the org-type))))
            (setf (gethash (make-keyword (the instance-id)) *instance-hash-table*) 
                  ;;
                  ;; FLAG -- that last entry is actually supposed to be the skin class.
                  ;;
                  (list recovery (list 0) t)))))))
   ("Gets called right before the instance is going to get cleared. Is intended to be used to stop any instance states that may not be elequently handled by the garbage collector. ie database connections, multiprocessing locks, open streams etc."
    session-clean-up
    ())
      
   ("Boolean. Test to run to see if this session has expired and needs to be cleared now."
    clear-now?
    () (when (the expires-at) (> (get-universal-time) (the expires-at))))
   
   ("Method which will set the expires-at slot to the current time + the session-duration"
    set-expires-at
    (&optional (duration (the session-duration)))
    (the (set-slot! :expires-at (+ (get-universal-time) (* 60 duration)))))))



(define-object session-recovery (session-control-mixin)
  
  :input-slots
  (instance-id ))



(defun answer (req ent)
  (let ((query (request-query req)))
    
    (when *debug?* (format t "before multi-part processing, query is: ~s~%" query))
    
    (when (null query) (setq query (process-multipart-submission req)))
    
    (when gwl::*debug?* (print-variables query))
    
    
    (let* ((requestor (query-arg-to-root-path query "requestor"))
           (iid (make-keyword 
                 (rest (assoc "iid" query :test #'string-equal))))
           (hash-entry (gethash iid *instance-hash-table*))
           (root-object (first hash-entry)) (skin (third hash-entry))
           (recovery-object? (typep root-object 'session-recovery))
           (requestor (when (and root-object (not recovery-object?)) 
                        (the-object root-object 
                                    (follow-root-path requestor))))
           (bashee (when requestor (the-object requestor bashee)))
           (rest-plist (when (not recovery-object?)
                         (merge-plist-duplicates
                          (assoc-list-to-plist 
                           (remove-if 
                            #'(lambda(item) 
                                (member (first item) 
                                        (list "requestor" "iid") 
                                        :test #'string-equal)) query) 
                           :case-sensitive? t))))
           (possible-nils (when (not recovery-object?) 
                            (the-object requestor possible-nils)))
           (rest-plist (when requestor 
                         (append rest-plist 
                                 (mapcan #'(lambda(key) (list key nil)) 
                                         (set-difference possible-nils
                                                         (plist-keys rest-plist))))))
           (settables (when bashee (the-object bashee %settable-slots%)))
           (rest-keys (plist-keys rest-plist))
           (query-plist (mapcan #'(lambda(key) (list key (getf rest-plist key)))
                                (remove-if #'(lambda(key) (gethash (make-keyword-sensitive key) settables)) rest-keys))))
      
      
      
      (let ((respondent (the-object requestor respondent)))
        
        (the-object respondent root (set-remote-host! req))
      
        (unless (the-object respondent root do-security-check)
          (with-http-response (req ent)
            (setf (reply-header-slot-value req :cache-control) "no-cache")
            (setf (reply-header-slot-value req :pragma) "no-cache")
                     
            (with-http-body (req ent)
              (let ((*req* req) (*ent* ent))
                (the-object respondent root security-check-failed write-html-sheet))))
          (return-from answer nil)))
        
      
      (when gwl::*debug?* (print-variables query 
                                           requestor 
                                           iid 
                                           hash-entry 
                                           root-object 
                                           recovery-object?
                                           requestor 
                                           bashee 
                                           rest-plist 
                                           possible-nils
                                           settables 
                                           rest-keys 
                                           query-plist))
      
      (let ((fe-processor (make-object 'form-element-processor 
                                       :bashee bashee 
                                       :query-plist query-plist)))
        
        (cond (recovery-object? (with-http-response (req ent :response *response-moved-permanently*)
                                  (setf (reply-header-slot-value req :location)
                                    (defaulting (the-object root-object recovery-url) *failed-request-url*))
                                  (setf (reply-header-slot-value req :cache-control) "no-cache")
                                  (setf (reply-header-slot-value req :pragma) "no-cache")
                                  (with-http-body (req ent))))
              ((null requestor) (net.aserve::failed-request req))
              (t (let ((*query-plist* query-plist) (*field-plist* rest-plist)) 
                   (the-object requestor (before-set!)))
              
                 ;;(when *debug?* (print-variables *query-plist* rest-plist rest-keys))

                 (when *debug?* (setq *f-e-p* fe-processor))
                 
                 (dolist (key rest-keys)
                   (when (gethash (make-keyword-sensitive key) settables)
                     ;;(when *debug?* (print-variables key))
                     (the-object bashee (set-slot-if-needed! (make-keyword-sensitive key) (getf rest-plist key)))))
                 
                 (the-object fe-processor validate-and-set!)
                 
                 (when 
                     (and (fboundp 'process-graphics-fields)
                          (typep (the-object requestor) (read-from-string "base-html-graphics-sheet"))
                          (the-object requestor (evaluate :view-object)))
                   (setq query-plist (funcall (read-from-string "gwl::process-graphics-fields")
                                              query query-plist root-object requestor)))


                 (when (not (equalp (the-object bashee query-plist) query-plist))
                   (the-object bashee 
                               (set-slot! :query-plist query-plist 
                                          :remember? (not (member :query-plist 
                                                                  (the-object bashee transitory-slots))))))
      
                 (let ((result (let ((*req* req) (*ent* ent) (*skin* skin) 
                                     (*query* query)) (the-object requestor (after-set!)))))
                   
                   (let ((respondent (if (and (consp result) (eql (first result) :go-to)) (second result)
                                       (the-object requestor respondent))))
                     ;;
                     ;; Dashboard stuff
                     ;;
                     ;;
                     ;; FLAG - use actual application-root rather than simple root.
                     ;;
                     (when (typep root-object 'session-control-mixin) (the-object root-object (set-expires-at)))
                     (the-object respondent root (set-time-last-touched!))
                     
                     (the-object respondent root (set-slot! :last-visited-root-path 
                                                            (the-object respondent root-path)))
                     ;;
                     ;;
                     ;;
                   
                     (with-http-response (req ent :response *response-found*)
                       (setf (reply-header-slot-value req :cache-control) "no-cache")
                       (setf (reply-header-slot-value req :pragma) "no-cache")
                     
                       (setf (reply-header-slot-value req :location) (the-object respondent url))
                     
                       (let ((keys (plist-keys (the-object respondent header-plist)))
                             (values (plist-values (the-object respondent header-plist))))
                         (mapc #'(lambda(key val)
                                   (setf (reply-header-slot-value req key) val)) keys values))

                       (with-http-body (req ent)
                         (let ((*req* req) (*ent* ent) (*skin* skin))
                           (multiple-value-bind (check error)
                               (when (the-object respondent check-sanity?)
                                 (ignore-errors (the-object respondent check-sanity)))
                             (declare (ignore check))
                             (if error (the-object respondent (sanity-error error)))))))))))))))


(defun process-multipart-submission (req)
  (let (sep query 
        (directory 
         (glisp:temporary-folder)))
    (do ()(nil)
      (multiple-value-bind (kind name filename content-type)
          (parse-multipart-header (get-multipart-header req))
        (declare (ignorable content-type))
        (when filename
          (setq sep (max (or (position #\/ filename :from-end t) -1) (or (position #\\ filename :from-end t) -1)))
          (setq filename (subseq filename (1+ sep) (length filename))))
        
        
        (case kind
          (:eof (return query))
          (:data (push (cons name (get-all-multipart-data req)) query))
          (:file (let ((output-path (merge-pathnames filename directory)))
                   (push (cons name (format nil "~a" output-path)) query)
                   (with-open-file (out output-path :direction :output
                                    :if-exists :supersede :element-type '(unsigned-byte 8))
                     
                     (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
                       (do ()(nil)
                         (let ((count (get-multipart-sequence req buffer)))
                           (when (null count) (return))
                           (write-sequence buffer out :end count))))))))))))





(defun make-object-internal (req ent)
  (let ((query (request-query req)))
    (let ((part (rest (or (assoc "object" query :test #'string-equal)
                          (assoc "part" query :test #'string-equal)))))
      (gwl-make-part req ent part))))


(defun gwl-make-part (&rest args) (apply #'gwl-make-object args))

(defparameter *weak-objects* (glisp:make-weak-hash-table))


(defmethod restore-ui-object ((ui-server gwl-gdl-ui) object)
  
  (when (typep object 'base-html-sheet)
    (format t "~&Restoring GWL UI status for ~s...~%" object)
    ;;
    ;; FLAG -- handle skins
    ;;
    (let* ((instance-id (the-object object instance-id))
           (current (gethash (make-keyword-sensitive instance-id) *instance-hash-table*))
           (root-part-and-version (list object *dummy-version*)))
      (setf (gethash (first root-part-and-version) *weak-objects*) t)
      (when current
        (warn "Instance ID of restored object, ~a, is already in use. Generating new instance id..." instance-id)
        (setq instance-id (make-new-instance-id))
        (format t "~&New Instance ID is: ~a~%" 
                (progn (the-object object (set-slot! :instance-id instance-id))
                       (the-object object instance-id))))
      (the-object (first root-part-and-version) url)
      (setf (gethash (make-keyword-sensitive instance-id) *instance-hash-table*) root-part-and-version))))


(defun %gwl-make-object% (part &key make-object-args share? skin)
  (let* ((instance-id (if share? "share" (make-new-instance-id)))
         (current (gethash (make-keyword-sensitive instance-id) *instance-hash-table*))
         (skin (if skin (make-instance skin) t))
         (root-part-and-version 
          (if (or (not share?) (not current))
              (list (apply #'make-object (read-safe-string part)
                           :instance-id instance-id
                           make-object-args) *dummy-version*) current)))
    (setf (gethash (first root-part-and-version) *weak-objects*) t)
    (setq root-part-and-version (append root-part-and-version (list skin)))
    (when (or (not share?) (not current))
      (setf (gethash (make-keyword-sensitive instance-id) *instance-hash-table*) root-part-and-version))
    (first root-part-and-version)))
  

(defun gwl-make-object (req ent part &key make-object-args share? skin (instance-id (if share? "share" (make-new-instance-id))))
  "Void. Used within the context of the body of a :function argument to Allegroserve's 
publish function, makes an instance of the specified part and responds to the request 
with a redirect to a URI representing the instance.

:arguments (req \"Allegroserve request object, as used in the function of a publish\"
            ent \"Allegroserve entity object, as used in the function of a publish\"
            package-and-part \"String. Should name the colon- (or double-colon)-separated 
package-qualified object name\")

:&key ((make-object-args nil) \"Plist of keys and values. These are passed to the object upon instantiation.\"
       (share? nil) \"Boolean. If non-nil, the instance ID will be the constant string ``share'' rather than a real instance id.\")

:example <pre>
  (publish :path \"/calendar\"
           :function #'(lambda(req ent) (gwl-make-object req ent \"calendar:assembly\")))
  </pre>"
  (let ((query (request-query req)))
    (let ((part (or part (rest (assoc "part" query :test #'string-equal)))))
      (let* ((current (gethash (make-keyword-sensitive instance-id) *instance-hash-table*))
             (skin (if skin (make-instance skin) t))
             (root-part-and-version 
              (if (or (not share?) (not current))
                  (list (apply #'make-object (read-safe-string part) 
                               :instance-id instance-id :query-toplevel query make-object-args)
                        *dummy-version*)
                current)))
        (setf (gethash (first root-part-and-version) *weak-objects*) t)
        (setq root-part-and-version (append root-part-and-version (list skin)))
        (when (or (not share?) (not current)) 
          (setf (gethash (make-keyword-sensitive instance-id) *instance-hash-table*) root-part-and-version))
        
        (let ((object (first root-part-and-version)))
          (when (typep object 'session-control-mixin)
            (the-object object set-expires-at))
          (the-object object set-instantiation-time!)
          (the-object object set-time-last-touched!)
          (the-object object (set-remote-host! req :original? t)))
        
        (with-http-response (req ent :response *response-found*)
          (setf (reply-header-slot-value req :location)
            (format nil "~a" (the-object (first root-part-and-version) url)))
          (setf (reply-header-slot-value req :cache-control) "no-cache")
          (setf (reply-header-slot-value req :pragma) "no-cache")
          (with-http-body (req ent)))))))


(defun publish-shared (&key path object-type host (server *wserver*)
                       (key (make-keyword-sensitive path)))
    "Void. Used to publish a site which is to have a shared toplevel instance tree,  
and no URI rewriting (i.e. no \"/sessions/XXX/\" at the beginning of the path). So,
this site will appear to be a normal non-dynamic site even though the pages are
being generated dynamically. 

:&key ((path nil) \"String. The URI path to be published.\"
       (object-type nil) \"Symbol. The type of the toplevel object to be instantiated.\"
       (host nil) \"hostname for the URI to be published.\"
       (server *wserver*) \"Allegroserve server object. If you have additional servers other than the default
<tt>*wserver*</tt> (e.g. an SSL server) you may want to call this function for each server.\")

:example <pre>
  (publish-shared :path \"/\"
                  :object-type 'site:assembly
                  :host (list \"www.genworks.com\" \"ww2.genworks.com\" \"mccarthy.genworks.com\"))
  </pre>"

  (let ((object (make-object (if (stringp object-type)
                                 (read-safe-string object-type)
                                 object-type)
                             :instance-id key
                             :plain-url? t
                             :host host)))
    (setf (gethash key *instance-hash-table*) (list object))
    
    (publish :path path
             :server server
             :host host
             :function #'(lambda(req ent)
                         (let ((*req* req) (*ent* ent)
                               (object (first (gethash key *instance-hash-table*))))
                           (with-http-response (req ent)
                             (setf (reply-header-slot-value req :cache-control) "no-cache")
                             (setf (reply-header-slot-value req :pragma) "no-cache")
                             (with-http-body (req ent)
                               (the-object object write-html-sheet))))))))






(define-object form-element-processor ()
  :input-slots
  (bashee query-plist)

  
  :computed-slots
  (
   (form-element-objects (the bashee form-controls))
   
   (force-validation-for (the bashee force-validation-for))
   
   (form-element-keys (mapcar #'(lambda(object) (the-object object field-name))
                              (the form-element-objects)))
   
   (radios (mapcan 
            #'(lambda(key)
                (when (and (glisp:match-regexp "^radio-" (format nil "~a" key))
                           (string-equal (second (getf (the query-plist) key)) "true"))
                  (list (make-keyword-sensitive (glisp:replace-regexp
                                       (glisp:replace-regexp (format nil "~a" key) "^radio-" "")
                                       "-.*" ""))
                        (first (getf (the query-plist) key)))))
                   (plist-keys (the query-plist))))
   
   (query-plist-all 
    (append (the radios) 
	    (mapcan #'(lambda(key val)
			(unless (or (glisp:match-regexp "^radio-" (format nil "~a" key))
				    (glisp:match-regexp "-checkedp$" (format nil "~a" key)))
			  (list key val)))
		    (plist-keys (the query-plist))
		    (plist-values (the query-plist)))))
   
   (checked-booleans 
    (mapcan #'(lambda(key)
                (when (glisp:match-regexp "-checkedp$" (format nil "~a" key ))
                  (list 
                   (make-keyword-sensitive (glisp:replace-regexp (format nil "~a" key ) "-checkedp$" ""))
                   (let ((checked (getf (the query-plist) key)))
                     (not (string-equal checked "false"))))))
            (plist-keys (the query-plist))))


   (form-elements-to-bash%
     (let ((submitted-elements 
            (let* ((query-plist-keys (plist-keys (the query-plist-all)))

                   ;;
                   ;; Comment out this #+nil and uncomment the one below to switch to
                   ;; 1579p007 behavior (i.e. ability to have form-element from
                   ;; anywhere in the tree). This really should be the correct
                   ;; behavior. 
                   ;;
                   (keys (remove-if-not #'(lambda(key) 
                                            ;; FLAG -- check for value
                                            ;; form-control object for
                                            ;; each key
                                            ;;
                                            (typep 
                                             (ignore-errors 
                                              (the bashee root
                                                   (follow-root-path 
                                                    (base64-decode-list (string key))))) 'base-form-control))
                                        query-plist-keys))

                   ;;
                   ;;
                   #+nil
                   (keys (remove-if-not #'(lambda(key) 
                                            (find key query-plist-keys))
                                        (the form-element-keys))))
	      
              (mapcan #'(lambda(key) (list key 
                                           (if (member key (the checked-booleans))
                                               (getf (the checked-booleans) key)
                                             (getf (the query-plist-all) key))))
                      keys))))
       
       (let ((result
	      (append submitted-elements
		      (mapcan #'(lambda(object) (list (the-object object field-name)
						      (the-object object value)))
			      (remove-if #'(lambda(objct)
					     (member (the-object objct field-name)
						     (plist-keys submitted-elements)))
					 (ensure-list (the force-validation-for)))))))
	 result)))
   

   (form-elements-to-bash (the form-elements-to-bash%))

   #+nil
   (form-elements-to-bash (mapcan #'(lambda(key val)
                                      ;;
                                      ;; The comparison of old and new should work, to avoid unneeded
                                      ;; bashing when the old and new values are the same. 
                                      ;;
                                      (let (#+nil
                                            (new (getf
                                                  (the bashee root (follow-root-path
                                                                    (base64-decode-list (string key))) 
                                                       (validate-type val)) :typed-value))
                                            #+nil
                                            (old (the bashee root (follow-root-path
                                                                      (base64-decode-list (string key))) value)))
                                        
                                        ;;(unless (equalp new old) (list key val))
                                        
                                        (list key val)
                                        
                                        ))
                                  (plist-keys (the form-elements-to-bash%))
                                  (plist-values (the form-elements-to-bash%)))))
  
  :functions
  ((validate-and-set!
    ()

    (dotimes (n 2)
      (dolist (key (plist-keys (the form-elements-to-bash)))
        
        (let ((*out-of-bounds-sequence-reference-action* :silent))
          (let ((element (the bashee root (follow-root-path (base64-decode-list (string key))))))
            (when element
              (unless (eql (first (the-object element root-path)) :%delete-button%)
                (when (or (= n 1) (the bashee preset-all?) (the-object element preset?))
                  (the-object element (validate-and-set! (getf (the form-elements-to-bash) key))))))))))
    
    (dolist (key (plist-keys (the form-elements-to-bash)))
      (let ((*out-of-bounds-sequence-reference-action* :silent))
        (let ((element (the bashee root (follow-root-path (base64-decode-list (string key))))))
          (when element
            (when *debug?* (print-variables (the-object element root-path)))
            (when (eql (first (the-object element root-path)) :%delete-button%)
              (the-object element parent aggregate (delete! (the-object element parent index)))))))))))










