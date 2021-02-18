;;;; tests.lisp

(defpackage #:zacl-tests
  (:use #:cl #:fiveam)
  (:import-from #:usocket
                #:socket-listen
                #:get-local-port
                #:socket-close)
  (:import-from #:net.aserve
                #:publish
                #:request-query
                #:with-http-response
                #:with-http-body
                #:*html-stream*)
  (:export #:run-tests))

(in-package #:zacl-tests)

(defun guesstimate-free-port ()
  (let* ((socket (socket-listen "localhost" 0))
         (port (get-local-port socket)))
    (socket-close socket)
    port))

(defvar *test-port* nil)
(defvar *test-host* "127.0.0.1")
(defvar *test-server* nil)

(defun make-test-url (path)
  (format nil "http://~A:~A~A" *test-host* *test-port* path))

(defun call-with-wserver (fun)
  (let* ((*test-port* (guesstimate-free-port))
         (*test-server* (net.aserve:start :port *test-port* :host *test-host*)))
    (unwind-protect
         (funcall fun *test-server*)
      (sleep 0.25)
      (net.aserve:shutdown :server *test-server*))))

(defmacro with-wserver ((server) &body body)
  `(call-with-wserver (lambda (,server)
                        ,@body)))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional size)
  `(simple-array octet (,size)))

(defun make-octet-vector (size)
  (make-array size :element-type 'octet))

(defun octet-vector (&rest octets)
  (let ((vector (make-octet-vector (length octets))))
    (replace vector octets)))

(defvar *crlf-vector* (octet-vector 13 10))

(defun assemble-octet-array (component-arrays)
  (let* ((size (reduce #'+ component-arrays :key #'length))
         (output-array (make-array size :element-type '(unsigned-byte 8)))
         (start 0))
    (dolist (array component-arrays output-array)
      (replace output-array array :start1 start)
      (incf start (length array)))))

(defun strings-to-octets (&rest strings)
  (flet ((octify (thing)
           (etypecase thing
             (octet (octet-vector thing))
             ((eql :crlf) *crlf-vector*)
             (octet-vector thing)
             (string
              (excl:string-to-octets thing)))))
    (assemble-octet-array (mapcar #'octify strings))))

(defun call-with-chunking-stream (fun)
  (excl:with-output-to-buffer (outer*)
    (let ((outer (flexi-streams:make-flexi-stream outer*)))
      (let ((inner (make-instance 'net.aserve::chunking-stream
                                  :output-handle outer
                                  :external-format :latin1)))
        (funcall fun inner)
        (close inner)))))

(defmacro with-chunking-stream ((stream) &body body)
  `(call-with-chunking-stream (lambda (,stream) ,@body)))

(defun decode-chunked-data (sequence)
  (excl:with-output-to-buffer (stream)
    (flexi-streams:with-input-from-sequence (outer sequence)
      (let ((inner (make-instance 'net.aserve::unchunking-stream
                                  :input-handle outer
                                  :external-format :latin1))
            (buffer (make-octet-vector 32)))
        (loop
          (let ((end (read-sequence buffer inner)))
            (when (zerop end)
              (return))
            (write-sequence buffer stream :end end)))))))


;;; Tests

(def-suite zacl-tests)

(in-suite zacl-tests)

(test buffer-output
  (is (equalp #(42)
             (excl:with-output-to-buffer (s)
               (write-byte 42 s)))))

(test make-a-process
  (let ((process (mp:make-process :name "bob")))
    (is (equalp "bob" (mp:process-name process)))
    (mp:process-kill process)))

(test uri-manipulation
  (let ((uri (net.uri:parse-uri "/"))
        (query "foo=bar")
        (host "www.example.com")
        (fragment "baz")
        (scheme :http)
        (full-uri "http://www.example.com/?foo=bar#baz"))
    (is (null (net.uri:uri-query uri)))
    (is (null (net.uri:uri-host uri)))
    (is (null (net.uri:uri-fragment uri)))
    (is (null (net.uri:uri-scheme uri )))
    (setf (net.uri:uri-query uri) query)
    (setf (net.uri:uri-host uri) host)
    (setf (net.uri:uri-fragment uri) fragment)
    (setf (net.uri:uri-scheme uri) scheme)
    (is (equalp (net.uri:uri-query uri) query))
    (is (equalp (net.uri:uri-host uri) host))
    (is (equalp (net.uri:uri-fragment uri) fragment))
    (is (equalp (net.uri:uri-scheme uri) scheme))
    (is (equalp (net.uri:render-uri uri) full-uri))))

(test utf8-enabled
  (is (= (length "€") 1)))

(test utf8-encoding
  (with-wserver (server)
    (let ((test-string "Hey €¥© Now"))
      (publish :server server
               :path "/utf8"
               :function #'(lambda (req ent)
                             (with-http-response (req ent :content-type "text/html; charset=utf-8")
                               (with-http-body (req ent :external-format :utf-8)
                                 (format *html-stream* test-string)))))
      (is (string= test-string (net.aserve.client:do-http-request
                                   (make-test-url "/utf8")
                                 :external-format :utf-8))))))

(defun request-value (req variable)
  (cdr (assoc variable (request-query req) :test 'string=)))

(test post-request
  (with-wserver (server)
    (let ((test-string "posted")
          (variable "test"))
      (publish :server server
               :path "/post-request"
               :function #'(lambda (req ent)
                             (with-http-response (req ent)
                               (with-http-body (req ent)
                                 (let ((value (request-value req variable)))
                                   (write-string value *html-stream*))))))
      (is (string= test-string (net.aserve.client:do-http-request
                                   (make-test-url "/post-request")
                                 :method :post
                                 :query (list (cons variable test-string))
                                 ))))))

(test get-request
  (with-wserver (server)
    (let ((test-string "getted")
          (variable "test"))
      (publish :server server
               :path "/get-request"
               :function #'(lambda (req ent)
                             (with-http-response (req ent)
                               (with-http-body (req ent)
                                 (let ((value (request-value req variable)))
                                   (write-string value *html-stream*))))))
      (is (string= test-string (net.aserve.client:do-http-request
                                   (make-test-url "/get-request")
                                 :method :get
                                 :query (list (cons variable test-string))))))))

;; Needed for SBCL's handling of stream-external-format
(defmethod zacl-cl:stream-external-format ((stream flexi-streams:vector-stream))
  nil)

(test unchunking-stream
  (let* ((string "foo bar baz")
         (decoded-length (length string))
         (encoded (strings-to-octets string))
         (encoded-length (length encoded))
         (chunked-data (strings-to-octets (format nil "~X" encoded-length)
                                          :crlf
                                          encoded :crlf
                                          ;; chunked eof
                                          "0" :crlf)))
    (flexi-streams:with-input-from-sequence (stream chunked-data)
      (let ((s (make-instance 'net.aserve::unchunking-stream
                              :external-format :latin1
                              :input-handle stream))
            (vector (make-octet-vector decoded-length)))
        (let ((end (read-sequence vector s)))
          (is (= end decoded-length))
          (is (equalp (strings-to-octets string) vector)))))))

(test chunking-round-trip
  (let* ((chunked-data (with-chunking-stream (s)
                         (write-string "Hello, " s)
                         (write-byte (char-code #\w) s)
                         (write-byte (char-code #\o) s)
                         (write-byte (char-code #\r) s)
                         (write-byte (char-code #\l) s)
                         (write-char #\d s)))
         (unchunked (decode-chunked-data chunked-data))
         (expected (strings-to-octets "Hello, world")))
    (is (equalp unchunked expected))))

(test prepend-stream
  (let* ((prefix "prefixed//")
         (text "Hello, world")
         (expected (strings-to-octets prefix text)))
    (let ((buffer (excl:with-output-to-buffer (stream)
                    (let ((s (make-instance 'net.aserve::prepend-stream
                                            :content prefix
                                            :output-handle stream)))
                      (write-string text s)))))
      (is (equalp buffer expected)))))

(test implementation-specific
  (is (fboundp 'zacl::stream-unix-fd))
  (is (fboundp 'zacl::fstat-size))
  (is (fboundp 'zacl::fstat-mtime))
  (is (fboundp 'zacl::file-kind))
  (is (fboundp 'zacl::socket-error-identifier))
  (is (fboundp 'zacl::socket-error-code)))
