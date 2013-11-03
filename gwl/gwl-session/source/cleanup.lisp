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

(eval-when (compile load eval) (export 'clear-old-timers))

(define-object old-timers ()

  :input-slots
  ((idle-time-required 600))
  
  :computed-slots
  ((root-instances (let (result)
                     (maphash #'(lambda(key val)
                                  (declare (ignore key))
                                  (push (first val) result)) *instance-hash-table*)
                     result))
   
   (old-timers-ids (mapsend (remove-if-not #'(lambda(instance) 
                                               (> (- (get-universal-time) (the-object instance time-last-touched))))
                                           (the root-instances)) :instance-id)))
  
  :functions
  ((clear () (mapc #'(lambda(id) (clear-instance id)) (the old-timers-ids)))))


(defun clear-old-timers (&key (idle-time-required 600))
  "Void. This is a lighter-weight alternative to the session-object-mixin for timing out instances
in a web application. 

:&key ((idle-time-required 600) \"Time in seconds. The maximum age of a session for timeout.\")

"
  (the-object (make-object 'old-timers :idle-time-required idle-time-required) clear))




(defun clear-all-instances ()
  "Void. Clears all instances from GWL's master table of root-level instances.
The instance IDs are the numbers you see in published GWL URIs, and are available
as the \"instance-id\" message within each GWL object which inherit from base-html-sheet.

Clearing all the instances makes available for garbage collection all memory used by
the object hierarchies rooted at the instances, as well as all associated published URIs.

:example <pre>
  (clear-all-instance)
  </pre>"
  (maphash #'(lambda(key val)
               (declare (ignore val))
               (unless (member key *keys-to-preserve*)
                 (clear-instance key))) *instance-hash-table*))


(defparameter *instance-finalizers* nil 
  "CL Function of one argument. The argument is a keyword representing
a GWL Instance ID. This is an application-specific function (either a
symbol naming a function, or a lambda expression) which will be run
after an instance is cleared with the standard clear-instance
function. The default is nil which indicates that no finalizer
function will be run.")


(defun clear-instance (id)
  "Void. Clears the specified instance from GWL's master table of root-level instances.
The instance ID is the same number you see in published GWL URIs, and is available
as the \"instance-id\" message within all GWL objects which inherit from base-html-sheet.

Clearing the specified instance makes available for garbage collection all memory used by
the object hierarchy rooted at the instance, as well as all associated published URIs.


:arguments (id \"Integer or Keyword Symbol. The key whose entry you wish to clear from the *instance-hash-table*.\")

:example <pre>
  (clear-instance 639)
  </pre>"
  
  (when (not (keywordp id)) (setq id (make-keyword id)))
  (clear-instance-from-hash id)
  (unpublish-instance-urls id)

  (dolist (finalizer *instance-finalizers*)
    (funcall finalizer id)))


(defun unpublish-instance-urls (id &optional base-url)
  (when (not (keywordp id)) (setq id (make-keyword id)))
  
  (setq base-url (when base-url (subseq base-url 0 (- (length base-url) (length "index.html")))))
  
  (when *debug?* (print-variables  id base-url))

  (let ((urls (gethash id *url-hash-table*)) remaining-urls)
    (mapc #'(lambda(url) 
              
              (if (or (null base-url) (and (search base-url url)
                                           (not (search  "$$tatu" url))))
                  (progn
                    (when *debug?* (format t "found one~%"))
                    ;;
                    ;; FLAG figure out what to do for proxy locators
                    ;;
                    (unless (typep (first (wserver-locators *wserver*)) 'net.aserve::locator-proxy)
		      
		      ;;
		      ;;FLAG DEBUG
		      ;;
		      (when *debug?* (let ((url-to-unpub url)) (print-variables url-to-unpub)))

                      (net.aserve::unpublish-entity (first (wserver-locators *wserver*))
                                                    url nil nil)))
                (push url remaining-urls))) urls)
    (if remaining-urls (setf (gethash id *url-hash-table*) (nreverse remaining-urls))
      (remhash id *url-hash-table*))))
   

;;
;; !!!!
;; !!!! FLAG -- also remove all urls from *descriptive-url-hash* and *url-hash-table*
;; !!!!
;;
(defun clear-instance-from-hash (id)
  (when (not (keywordp id))
    (setq id (make-keyword id)))
  (remhash id *instance-hash-table*))
