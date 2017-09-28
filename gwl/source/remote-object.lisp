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

(defvar *agile-debug?* nil)

(defparameter *send-plist* nil)
(defparameter *make-object-plist* nil)
(defparameter *fetch-plist* nil)


;; FLAG -- not used here any more, remove once pyndl updated
(defun encode64-downcase (item)
  (let ((*print-case* :downcase))
    (base64-encode-list item)))
    
(defun encode-plist-for-url (encoded-plist)
  (let ((*print-case* :downcase))
    (base64-encode-list encoded-plist)))

(defun remote-execute (request host port plist &key (decode t))
  (let* ((encoded-plist (encode-plist-args plist))
         (argstring (encode-plist-for-url encoded-plist))
         (result (net.aserve.client:do-http-request 
                  (format nil "http://~a:~a/~a?args=~a" host port request argstring))))
    (read-safe-string (if decode (base64-decode-safe result) result))))


(define-object remote-object (vanilla-remote)
  :no-vanilla-mixin? t
  
  :input-slots (remote-type (input-parameters nil) host port
                
                (remote-root-path nil)
                
                (remote-id (read-safe-string
                            (let* ((current-id (the previous-id))
				   (plist (append (list :current-id current-id) (the remote-object-args))))

			      
			      (when *agile-debug?* (setq *make-object-plist* (append (remove-plist-keys plist (list :parent-form :name))
										     (list :name (string (getf plist :name)))
										     (list :parent-form (let ((parent-plist (rest (getf plist :parent-form))))
                                                                                                          (stringify-plist parent-plist))))))
                 
                              (let ((new-id
                                     (remote-execute "make-remote-object" (the host) (the port) plist :decode nil)))
                                (the (set-slot! :previous-id new-id :remember? nil :warn-on-non-toplevel? nil))
                                new-id))) :settable))

  
  :computed-slots
  ((remote-object-args (append (list :type (format nil "~a::~a" 
                                                   (string-downcase
						    (package-name (symbol-package (the remote-type)) ))
                                                   (string-downcase 
						    (symbol-name (the remote-type))))
                                     :package (encode-for-http *package*)
                                     ;;:host (the host)
                                     :name (the %name%)
                                     :index (the index)
                                     :parent-form (the parent-form)

				     )
                               (the input-parameters)))

   ;;(remote-object-args-json (cl-json: ) ;; FLAG -- fill in!

   (parent-form (encode-for-http (the parent)))
   
   (local-remote-id nil :settable)
   (previous-id nil :settable))
  
  
  
  :functions
  ((fetch-input
    (message part-name child &rest args)
    
    ;;
    ;; FLAG -- *notify-cons* is going to be broken now... have to unmarshal/marshal from hash table. 
    ;;         hold off on doing this until we switch to an Abstract Associative Map. 
    ;;
    (let ((plist (list :message (make-keyword message)
                       :part-name (make-keyword part-name)
                       :child (encode-for-http child)
                       :notify-cons (encode-for-http gdl::*notify-cons*)
                       :args args
                       :remote-id (the remote-id)
                       :remote-root-path (the remote-root-path)
                       :package *package*)))
      
      (when *agile-debug?* (setq *fetch-plist* (encode-plist-args plist)))
      
      (let ((result (remote-execute "fetch-remote-input" (the host) (the port) plist)))
	;;(print-variables result)
	
	
        (if (consp result)
	    (evaluate-object (first result) (rest result))
	    result))))
   
   
   (unbind-remote-slot 
    (slot)
    (let ((plist (list :slot slot 
                       :remote-id (the remote-id)
                       :remote-root-path (the remote-root-path))))
      (let ((result (remote-execute "unbind-slots" (the host) (the port) plist)))
        (decode-from-http result))))

   
   (send
    (message &rest args)
    (let ((plist (list :message (make-keyword message)
                       :notify-cons (encode-for-http gdl::*notify-cons*)
                       :args args
                       :remote-id (the remote-id)
                       :remote-root-path (the remote-root-path)
                       :package *package*)))

      (when *agile-debug?* (setq *send-plist* (stringify-plist (encode-plist-args plist))))
      
      (let ((result (remote-execute "send-remote-message" (the host) (the port) plist)))

        ;;
        ;; FLAG -- pass result through generic function to sanitize
        ;;
        
        (let ((result (decode-from-http result)))

	  
	  
          (cond ((and (consp result) (eql (first result) :error)
                      (eql (second result) :no-such-object))
                 (progn
                   (warn "~&Remote object returned error, creating a new one...~%")
                   (the (set-slot! :remote-id nil :warn-on-non-toplevel? nil))
                   (the (restore-slot-default! :remote-id))
                   (the (send (:apply (cons message args))))))
                ((and (consp result) (eql (first result) :error))
                 (format *error-output* "~&~%Remote object threw error:~%~%")
                 (error (format nil (second result))))
                (t result))))))

   
   
   (send-output 
    (message format &rest args)
    (let ((plist (list :message (make-keyword message)
                       :format format
                       :args args
                       :remote-id (the remote-id)
                       :remote-root-path (the remote-root-path)
                       :package *package*)))
          (let ((result (remote-execute "send-remote-output"  (the host) (the port) plist)))
            (write-string result *stream*))))))


(defmethod decode-from-http ((item t)) item)

(defmethod decode-from-http ((list list))
  (when list
    (multiple-value-bind (object-type initargs) (decode-object-from-http list)
      (cond (object-type (evaluate-object object-type initargs))
            (t (cons (decode-from-http (first list)) (decode-from-http (rest list))))))))

(defun encode-plist-args (plist)
  ;;
  ;; FLAG -- not sure if this is the perfect place to do this - have to downcase to work 
  ;; with mlisp slave. 
  ;;
  (let ((*print-case* :downcase))
    (when plist
      (cons (first plist)
	    (cons (encode-for-http (second plist)) (encode-plist-args (rest (rest plist))))))))

(defun stringify-plist (plist)
  (loop for (key val) on plist by #'cddr
    collect key
    collect (if (and val (symbolp val)) (format nil "~s" val) val)))

(defmethod encode-for-http ((item t)) item)

(defmethod encode-for-http ((item pathname))
  (format nil "~a" item))

(defmethod encode-for-http ((item package))
  (string-downcase (package-name item)))

(defmethod encode-for-http ((item list))
  (with-standard-io-syntax
    (when item
      (cons (encode-for-http (first item))
	    (encode-for-http (rest item))))))


(defmethod encode-for-http ((item base-format))
  (let ((slots-plist (mapcan #'(lambda(slot-definition)
                                 (let ((name (glisp:slot-definition-name slot-definition)))
                                   (list (make-keyword name)
                                         (encode-for-http (slot-value item name)))))
                             (glisp:class-slots (class-of item)))))
    (append (list :type (class-name (class-of item))) slots-plist)))


;;
;; FLAG -- replace item with self and get rid of the-object
;;
(defmethod encode-for-http ((item gdl-remote))
  (let ((id (or (the-object item root local-remote-id)
                (let ((new-id (make-keyword (make-new-instance-id))))
                  (setf (gethash new-id *remote-objects-hash*) (the-object item root))
                  (the-object item root (set-slot! :local-remote-id new-id 
						   :remember? nil 
						   :warn-on-non-toplevel? nil))
                  new-id))))
    (encode-object-for-http item id)))

(defmethod encode-for-http ((item gdl::gdl-basis))
  (let ((id (or (the-object item root remote-id)
                (let ((new-id (make-keyword (make-new-instance-id))))
                  (setf (gethash new-id *remote-objects-hash*) (the-object item root))
                  (the-object item root (set-slot! :remote-id new-id 
						   :remember? nil 
						   :warn-on-non-toplevel? nil))
                  new-id))))
    (encode-object-for-http item id)))

(defun gwl-host ()
  ;; FIXME: better way to get this ?
  (when (boundp '*wserver*)
    (let ((sock (wserver-socket *wserver*)))
      (when sock (glisp:local-host sock)))))

(defun encode-object-for-http (item id)
  (list :remote-gdl-instance
        :id id
        :index (the-object item index)
        :type (format nil "~s" (the-object item type))
        :root-path (the-object item root-path) 
        :host (or *request-server-ipaddr* :unknown)
        :port (server-port)))

(defun decode-object-from-http (list)
  (when (keywordp (first list))
    (values (first list) (rest list))))


(defmethod print-object ((object remote-object) stream)
  (format stream "#<remote GDL object of type ~s>, a.k.a. " (the-object object remote-type))
  (call-next-method))

