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

(defvar *preferred-remote-syntax* :lisp) ;; or :json

(defmethod encode-plist-for-url ((syntax (eql :lisp)) encoded-plist)
  (let ((*print-case* :downcase))
    (base64-encode-list encoded-plist)))


(defun upcase-plist (plist)
  (change-plist-case plist :up))

(defun downcase-plist (plist)
  (change-plist-case plist :down))

(defun change-plist-case (plist up-or-down)
  (let ((function (ecase up-or-down (:up #'string-upcase) (:down #'string-downcase))))
    (mapcan #'(lambda(key value)
		(list (make-keyword (funcall function key))
		      (cond ((keywordp value) (make-keyword
					       (funcall function value)))
			    ((symbolp value)
			     (cond ((null value) nil)
				   (t (funcall function
				       (string
					(intern (string value)
						(symbol-package value)))))))
			    ((consp value) (change-plist-case value up-or-down))
			    ((stringp value) value)
			    (t value))))
	    (plist-keys plist) (plist-values plist))))


(defmethod encode-plist-for-url ((syntax (eql :json)) plist)
  (let ((yason-string (with-output-to-string (s)
                        (yason:encode (yasonify plist) s))))
    
    (base64-encode-safe (format nil "~s" yason-string))))


#+nil
(defun yasonify (value)
  (cond ((or (null value) (eql value t) (numberp value) (stringp value)) value)
        ((vectorp value) (map 'vector #'yasonify value))
        ((atom value) (let ((*print-readably* t)) (format nil "~s" value)))
        ((and (keywordp (car value)) (evenp (length value)))
         (loop with hash = (make-hash-table :test 'equal)
           for (key val) on value by #'cddr
           do (setf (gethash (symbol-name key) hash) (yasonify val))
           finally (print-hash hash) (return hash)))
        (t (mapcar #'yasonify value))))


(defun yasonify (value)
  (cond ((or (null value) (eql value t) (numberp value) (stringp value)) value)
        ((vectorp value) (map 'vector #'yasonify value))
        ((atom value) (let ((*print-readably* t)) (format nil "~s" value)))
        ((and (keywordp (car value)) (evenp (length value)))
         (loop with hash = (make-hash-table :test 'equal)
           for (key val) on value by #'cddr
           do (setf (gethash (symbol-name key) hash) (yasonify val))
           finally (return hash)))
        (t (mapcar #'yasonify value))))



(defun register-remote-object (obj)
  (declare (ignore obj))
  #+ccl (ccl:terminate-when-unreachable obj)
  nil)

(defun do-remote-execute (request host port remote-syntax plist &key (decode t))
  (let* ((encoded-plist (encode-plist-args plist))
         (argstring (encode-plist-for-url remote-syntax encoded-plist))
         (result (net.aserve.client:do-http-request 
                     (let ((*print-case* :downcase))
                       (format nil "http://~a:~a/~a?args=~a&syntax=~s"
                               host port
                               request argstring remote-syntax)))))
    ;; Note that for now, return values do not obey remote-syntax, they are always in lisp....
    (if decode
        (decode-from-http (read-safe-string (base64-decode-safe result)))
        (read-safe-string result))))


(define-object remote-object (vanilla-remote)
  :no-vanilla-mixin? t
  
  :input-slots (remote-type (input-parameters nil)
                host port (remote-syntax *preferred-remote-syntax*)
                
                (remote-root-path nil)
                
                (remote-id (read-safe-string
                            (let* ((current-id (the previous-id))
				   (plist (append (list :current-id current-id) (the remote-object-args))))

			      
			      (when *agile-debug?* (setq *make-object-plist* (append (remove-plist-keys plist (list :parent-form :name))
										     (list :name (string (getf plist :name)))
										     (list :parent-form
                                                                                           (multiple-value-bind (type parent-plist)
                                                                                               (destructure-object-from-http (getf plist :parent-form))
                                                                                             (declare (ignore type))
                                                                                             (stringify-plist parent-plist))))))

                              (let ((new-id (the (remote-execute "make-remote-object" plist :decode nil))))
                                (the (set-slot! :previous-id new-id :remember? nil :warn-on-non-toplevel? nil))
                                (register-remote-object self)
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
  ((remote-execute
    (request plist &key (decode t))
    (do-remote-execute request (the host) (the port) (the remote-syntax) plist :decode decode))
   
   (fetch-input
    (message part-name child &rest args)
    
    ;;
    ;; FLAG -- *notify-cons* is going to be broken now... have to unmarshal/marshal from hash table. 
    ;;         hold off on doing this until we switch to an Abstract Associative Map. 
    ;;
    (let ((plist (list :message (make-keyword message)
                       :part-name (make-keyword part-name)
                       :index (the-object child index)
                       :notify-cons (encode-for-http gdl::*notify-cons*)
                       :args args
                       :remote-id (the remote-id)
                       :remote-root-path (the remote-root-path)
                       :package *package*)))
      
      (when *agile-debug?* (setq *fetch-plist* (encode-plist-args plist)))
      
      (the (remote-execute "fetch-remote-input" plist))))
   
   
   (unbind-remote-slot 
    (slot)
    (let ((plist (list :slot slot 
                       :remote-id (the remote-id)
                       :remote-root-path (the remote-root-path))))
      (the (remote-execute "unbind-slots" plist))))

   
   (send
    (message &rest args)
    (let ((plist (list :message (make-keyword message)
                       :notify-cons (encode-for-http gdl::*notify-cons*)
                       :args args
                       :remote-id (the remote-id)
                       :remote-root-path (the remote-root-path)
                       :package *package*)))

      (when *agile-debug?* (setq *send-plist* (stringify-plist (encode-plist-args plist))))
      
      (let ((result (the (remote-execute "send-remote-message" plist))))

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
              (t result)))))

   
   
   (send-output 
    (message format &rest args)
    (let ((plist (list :message (make-keyword message)
                       :format format
                       :args args
                       :remote-id (the remote-id)
                       :remote-root-path (the remote-root-path)
                       :package *package*)))
      (let ((result (the (remote-execute "send-remote-output" plist))))
            (write-string result *stream*))))))

;;
;; FLAG -- promote this to a generic glisp finalizer scheme
;;
(defparameter *remotes-to-purge* nil)
(defparameter *remotes-to-purge-lock* (bt:make-lock))


#+ccl
(defmethod ccl:terminate ((object remote-object))
  (let ((data (list (the-object object host)
                    (the-object object port)
                    (the-object object remote-syntax)
                    (the-object object remote-id))))
    (bt:with-lock-held (*remotes-to-purge-lock*)
      (push data *remotes-to-purge*))))

(defun terminate-remotes ()
  (loop
     (let ((remote (bt:with-lock-held (*remotes-to-purge-lock*)
                     (pop *remotes-to-purge*))))
       (unless remote (return))
       (destructuring-bind
             (host port syntax id) remote
         (let ((result
                (do-remote-execute "delete-remote-object" host port syntax
                                   (list :current-id (make-keyword id))
                                   :decode nil)))
           (unless (string-equal result "ok")
             (warn "Terminate of ~a returned ~a.~%" id result)))))))


(defun launch-terminator ()
  (bt:make-thread #'(lambda()
		      (do () (nil)
			(sleep 5)
			(terminate-remotes)))
		  :name "remote terminator, wakes every 5 seconds and cleans up stale remotes."))



(defun decode-from-http (item)
  (if (consp item)
      (multiple-value-bind (object-type initargs) (destructure-object-from-http item)
        (or (and object-type (evaluate-object object-type initargs))
            (mapcar #'decode-from-http item)))
      item))

(defun encode-plist-args (plist)
  ;;
  ;; FLAG -- not sure if this is the perfect place to do this - have to downcase to work 
  ;; with mlisp slave. 
  ;;
  (let ((*print-case* :downcase))
    #+allegro
    (when (eql excl:*current-case-mode* :case-sensitive-lower)
      (setq plist (upcase-plist plist)))
    (when plist
      (cons (first plist)
	    (cons (encode-for-http (second plist)) (encode-plist-args (rest (rest plist))))))))

(defun stringify-plist (plist)
  (loop for (key val) on plist by #'cddr
    collect key
    collect (cond ((or (null val) (eql val t) (numberp val)) val)
                  ((atom val) (format nil "~s" val))
                  ((keywordp (car val)) (stringify-plist val))
                  (t (mapcar #'stringify-plist val)))))

(defun unstringify-plist (plist)
  (loop for (key val) on plist by #'cddr
    collect key
    collect (cond ((or (null val) (eql val t)) val)
                  ((atom val) (read-safe-string val))
                  ((keywordp (car val)) (unstringify-plist val))
                  (t (mapcar #'unstringify-plist val)))))

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
    (encode-remote-gdl-instance item id)))

(defmethod encode-for-http ((item gdl::gdl-basis))
  (let ((id (or (the-object item root remote-id)
                (let ((new-id (make-keyword (make-new-instance-id))))
                  (setf (gethash new-id *remote-objects-hash*) (the-object item root))
                  (the-object item root (set-slot! :remote-id new-id 
						   :remember? nil 
						   :warn-on-non-toplevel? nil))
                  new-id))))
    (encode-remote-gdl-instance item id)))

(defun gwl-host ()
  ;; FIXME: better way to get this ?
  (when (boundp '*wserver*)
    (let ((sock (wserver-socket *wserver*)))
      (when sock (glisp:local-host sock)))))

(defun encode-remote-gdl-instance (item id)
  (construct-object-for-http :remote-gdl-instance
                             (list :id id
                                   :index (the-object item index)
                                   :type (format nil "~s" (the-object item type))
                                   :root-path (the-object item root-path) 
                                   :host (or *request-server-ipaddr* :unknown)
                                   :port (server-port))))

(defun construct-object-for-http (type plist)
  (list* :object-type type plist))

(defun destructure-object-from-http (list)
  (if (evenp (length list))
    (alexandria:when-let (object-type (getf list :object-type))
      (values object-type (remove-plist-key list :object-type)))
    ;; Support old object format for backward compatibility.
    (when (keywordp (first list))
      (values (first list) (rest list)))))


(defmethod print-object ((object remote-object) stream)
  (format stream "#<remote GDL object of type ~s>, a.k.a. " (the-object object remote-type))
  (call-next-method))

