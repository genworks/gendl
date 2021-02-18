;;;; package-net.uri.lisp

(in-package #:zacl)

(defclass net.uri:uri ()
  ((real-uri
    :reader real-uri
    :initarg :real-uri)
   (plist
    :accessor net.uri:uri-plist
    :initarg :plist))
  (:default-initargs
   :plist nil))

(defmethod real-uri ((uri quri:uri))
  uri)

(defmethod print-object ((uri net.uri:uri)  stream)
  (print-unreadable-object (uri stream :type t)
    (format stream "~S" (render-uri (real-uri uri)))))

(defun uri-designator (uri)
  (etypecase uri
    (string
     (uri uri))
    (net.uri:uri
     uri)))

(defun net.uri:parse-uri (uri-string)
  (make-instance 'net.uri:uri :real-uri (uri uri-string)))

(macrolet ((net-uri-accessors (&rest names)
             `(progn
                ,@(mapcan (lambda (name)
                            (let ((net-name (find-symbol (symbol-name name)
                                                         :net.uri)))
                              (unless net-name
                                (error "Unknown net.uri symbol ~A" name))
                              (list
                               `(defun ,net-name (uri)
                                  (,name (real-uri uri)))
                               `(defun (setf ,net-name) (new-value uri)
                                  (setf (,name (real-uri uri) ) new-value)))))
                          names))))
  (net-uri-accessors uri-path
                     uri-host
                     uri-port
                     uri-userinfo
                     uri-fragment
                     uri-query))

;;; uri-scheme isn't as simple as the other accessors

(defun net.uri:uri-scheme (uri)
  (let ((scheme (uri-scheme (real-uri uri))))
    (when scheme
      (values (find-symbol (string-upcase scheme) :keyword)))))

(defun (setf net.uri:uri-scheme) (new-value uri)
  (setf (uri-scheme (real-uri uri)) new-value))


(defun net.uri:copy-uri (uri
                         &key
                           (scheme (net.uri:uri-scheme uri))
                           (userinfo (net.uri:uri-userinfo uri))
                           (host (net.uri:uri-host uri))
                           (port (net.uri:uri-port uri))
                           (path (net.uri:uri-path uri))
                           (query (net.uri:uri-query uri))
                           (fragment (net.uri:uri-fragment uri)))
  (copy-uri (real-uri uri)
            :scheme scheme
            :userinfo userinfo
            :host host
            :port port
            :path path
            :query query
            :fragment fragment))

(defun net.uri:render-uri (uri &optional stream)
  (render-uri (real-uri uri) stream))

(defun net.uri:merge-uris (new-uri uri &optional place)
  (when place
    (error "PLACE option not yet implemented"))
  (merge-uris (real-uri (uri-designator new-uri))
              (real-uri (uri-designator uri))))

(defun net.uri::uri-string (uri)
  (render-uri (real-uri uri)))

(defun net.uri::uri-path-etc (uri)
  (let ((uri (real-uri uri)))
    (with-output-to-string (s)
      (format s "~A" (uri-path uri))
      (when (uri-query uri)
        (format s "?~A" (uri-query uri)))
      (when (uri-fragment uri)
        (format s "#~A" (uri-fragment uri))))))

(defun net.uri::.uri-parsed-path (uri)
  ;; XXX What is this slot for, anyway? Fake it for now.
  (net.uri:uri-path uri))
