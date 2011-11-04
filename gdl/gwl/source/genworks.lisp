(in-package :com.genworks.lisp)

(eval-when (compile load eval)
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:export #:*base64-encode-func*
             #:*base64-decode-func*
             #:class-slots
             #:slot-definition-name
             #:gc-full
	     #:initialize-multiprocessing
             #:load-html-parser
             #:match-regexp
             #:patches-dir
             #:process-run-function
             #:remote-host
             #:replace-regexp
             #:socket-bytes-written
	     #:split-regexp
             #:with-timeout-sym

             )))


(defparameter *base64-encode-func* 
  #'cl-base64:string-to-base64-string)

(defparameter *base64-decode-func* 
  #'cl-base64:base64-string-to-string)


(defun class-slots (class)
  #-(or allegro lispworks) (error "Need implementation for class-slots for currently running lisp.~%")
  (#+allegro mop:class-slots
   #+lispworks hcl:class-slots class))

(defun slot-definition-name (slot-definition)
  #-(or allegro lispworks) (error "Need implementation for slot-definition-name for currently running lisp.~%")
  (#+allegro mop:slot-definition-name
   #+lispworks hcl:slot-definition-name slot-definition))


(defun gc-full ()
  "Force a garbage collection."
  #+allegro  (excl:gc t)
  #+lispworks  (hcl:gc-all))

(defun initialize-multiprocessing ()
  #+lispworks (mp:initialize-multiprocessing)
  ;;
  ;; Don't have to do it in Allegro - see about other CLs when we get here. 
  ;;
  )

(defun load-html-parser ()
  #+allegro (require :phtml)
  #-allegro (quicklisp:quickload :cl-html-parse))


(defun match-regexp (string-or-regexp string-to-match
                     &key newlines-special case-fold return
                          (start 0) end shortest)
  (#+allegro excl:match-regexp 
   #-allegro acl-compat.excl:match-regexp string-or-regexp string-to-match 
   :newlines-special newlines-special 
   :case-fold case-fold
   :return return 
   :start start 
   :end end 
   :shortest shortest))

(defun patches-dir ()
  nil)

(defun process-run-function (name-or-options preset-function &rest args)
  (apply #+allegro #'mp:process-run-function
         #-allegro #'acl-compat.mp:process-run-function 
         name-or-options preset-function args))



(defun remote-host (socket)
  #+allegro (socket:remote-host socket)
  #-allegro (acl-compat.socket:remote-host socket))

(defun replace-regexp (string regexp to-string)
  (cl-ppcre:regex-replace-all regexp string to-string))

(defun socket-bytes-written (socket)
  #-allegro (declare (ignore socket))
  #+allegro (excl::socket-bytes-written socket)
  #-allegro (progn (warn "Returning 0 for socket-bytes-written -- 

please find implementation for the currently running lisp.~%")
                   0))


(defun split-regexp (regexp string)
  (cl-ppcre:split regexp string))

(defun with-timeout-sym ()
  "Returns the appropriate symbol for with-timeout, for substitution within macros."
  #+allegro 'sys:with-timeout
  #-allegro 'acl-compat.mp:with-timeout)


;;
;; FLAG -- below code needs to be merged into portableallegroserve. 
;;

;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;
;; $Id: main.cl,v 1.180 2007/04/17 22:05:04 layer Exp $

;; Description:
;;   aserve's main loop

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


;;
;; FLAG -- the hope is to get the changes below merged into official
;; aserve and portableaserve.
;;



#-allegro
(in-package :net.aserve)


#-allegro (eval-when (compile load eval) (glisp:begin-redefinitions-ok))

#-allegro
(defclass http-request (http-header-mixin)
  ;;
  ;; incoming request and information about the reply we send to it
  ;;
  (
   ;;
   ;; -- external slots --
   ;;  (accessors exported)
   
   (method  ;; keyword giving the command in this request :get .. etc.
    :initarg :method
    :accessor request-method)
   
   (uri  ;; uri object holding the current request with the scheme, host
         ;; and port filled in.
    :initarg :uri
    :accessor request-uri)

   (raw-uri  ;; uri object holding the actual uri from the command
    :initarg :raw-uri
    :accessor request-raw-uri)

   (decoded-uri-path
    :initarg :decoded-uri-path
    :accessor request-decoded-uri-path)
   
   (protocol ;; symbol naming the http protocol  (e.g. :http/1.0)
    :initarg :protocol
    :reader request-protocol)
   
   (protocol-string ;; string naming the protcol requested
    :initarg :protocol-string
    :reader request-protocol-string)
   
   (socket ;; the socket we're communicating through
    :initarg :socket
    :reader request-socket)
   
   (wserver ;; wserver object for web server this request came to
    :initarg :wserver
    :reader request-wserver)
   
   (raw-request  ;; the actual command line from the browser
    :initarg :raw-request
    :reader request-raw-request)
   
   (vhost  ;; the virtual host to which this request is directed
    :initarg :vhost
    :initform (wserver-default-vhost *wserver*)
    :accessor request-vhost)
   
   ;;
   ;; -- internal slots --
   ;;
   
   (query-alist 
    ;; list of conses (name . value) for the query part of the 
    ;; current uri.  This slot is filled in when information
    ;; is first requested by  the  request-query function
    :initform :empty
    :accessor request-query-alist)
   
   
   (headers ;; alist of headers *not* stored in slots
    ;* use header-slot-value to retrieve header values 
    ;  rather than looking here since not all headers are stored 
    ;  here
    :initform nil
    :accessor request-headers)

   (header-block  ;; *header-block-sresource* object
    :initform nil
    :accessor request-header-block)
    
   (request-body 
    ;; if we've read the request body then this 
    ;; is the string holding it.
    :initform nil
    :accessor request-request-body)

   ;; response
   (reply-code   ;; one of the *response-xx* objects
    :initform nil
    :accessor request-reply-code)
   
   ;;
   ;; DJC
   ;;
   (request-date
    :initform nil
    :accessor request-request-date)
   ;;
   ;; /DJC
   ;;
   
   (reply-date
    :initform (get-universal-time)  ; when we're responding
    :accessor request-reply-date)
   
   (reply-headers  ;; alist of headers to send out
    :initform nil
    :accessor request-reply-headers)
   
   (reply-content-type ;; mime type of the response
    :initform nil
    :accessor request-reply-content-type)
   
   (reply-stream   ;; stream to which to send response
    :initform nil
    :accessor request-reply-stream)
   
   (reply-content-length
    :initform nil  ;; nil means "i don't know"
    :accessor request-reply-content-length)
   
   (reply-strategy  ;; list of strategy objects
    :initform nil
    :accessor request-reply-strategy)
   
   (reply-plist    ;; general stuff in a property list form
    :initform nil
    :accessor request-reply-plist)

   (reply-protocol-sring
    ;; A web server announces the highest minor level of the 
    ;; major level of the protocol that was requested by the client.
    ;; Thus for now we're always http/1.1
    :initform "HTTP/1.1"
    :accessor request-reply-protocol-string)))

#-allegro
(defun process-connection (sock)
  ;; read an http request from the socket and process
  ;; it.
  ;; If the response indicates 'keep alive' then loop around for
  ;; another request.
  ;; When this function returns the given socket has been closed.
  ;;
  
  (unwind-protect
      (let (req error-obj (chars-seen (list nil)))

        ;; run the accept hook on the socket if there is one
        (let ((ahook (wserver-accept-hook *wserver*)))
          (if* ahook then (setq sock (funcall ahook sock))))
        
        ;; get first command
        (loop
           
          (with-timeout-local (*read-request-timeout* 
                               (debug-format :info "request timed out on read~%")
                               ; this is too common to log, it happens with
                               ; every keep alive socket when the user stops
                               ; clicking
                               ;;(log-timed-out-request-read sock)
                               (return-from process-connection nil))
            (multiple-value-setq (req error-obj)
              (ignore-errors (read-http-request sock chars-seen))))
          
          (if* (null req)
             then ; end of file, means do nothing
                  ; (logmess "eof when reading request")
                  ; end this connection by closing socket
                  (if* error-obj
                     then (brief-logmess 
                           (format nil "While reading http request~:_ from ~a:~:_ ~a" 
                                   (socket:ipaddr-to-dotted 
                                    (socket::remote-host sock))
                                   error-obj)))

                  ; notify the client if it's still listening
                  (if* (car chars-seen)
                     then (ignore-errors
                           (format sock "HTTP/1.0 400 Bad Request~a~a" 
                                   *crlf* *crlf*)
                           (force-output sock)))
                   
                  (return-from process-connection nil)
             else ;; got a request
                  ;;
                  ;; DJC
                  ;;
                  (setf (request-request-date req) (get-universal-time))
                  (setq *worker-request* req) 
                                  
                  (handle-request req)
                  (force-output-noblock (request-socket req))
                  
                  
                  (setf (request-reply-date req) (get-universal-time))
                  ;;
                  ;; /DJC
                  ;;
                  (log-request req)
                  
                  (setq *worker-request* nil)
                  (free-req-header-block req)
                  
                  (let ((sock (request-socket req)))
                    (if* (member :keep-alive
                                 (request-reply-strategy req)
                                 :test #'eq)
                       then ; continue to use it
                            (debug-format :info "request over, keep socket alive~%")
                            (force-output-noblock sock)
                            (setf (car chars-seen) nil)  ; for next use
                       else (return))))))
    ;; do it in two stages since each one could error and both have
    ;; to be attempted
    (ignore-errors (force-output-noblock sock))
    (ignore-errors (close sock :abort t))))

#-allegro
(defmethod request-query ((req http-request) &key (post t) (uri t)
                                                  (external-format 
                                                   *default-aserve-external-format*))
  ;; decode if necessary and return the alist holding the
  ;; args to this url.  In the alist items the value is the 
  ;; cdr of the alist item.
  ;;
  ;; If uri is true then we look for query information in the uri
  ;; (following a question mark)
  ;; If post is true and this is a post request then we look for
  ;; query information in the body of the query.
  ;; If both are true (and this is a post) then we look both places.
  ;;
  ;;
  (let ((alist (request-query-alist req))
        (signature (cons post uri)))

    
    (if* (not (eq alist :empty))
       then (let ((given-sig (getf (request-reply-plist req) 
                                   'request-query-sig)))
              (if* (equal given-sig signature)
                 then ; same args as before, cached value is legit
                      (return-from request-query alist))))
    
    (let (res)
      (if* uri
         then (let ((arg (uri-query (request-uri req))))
                (if* arg
                   then (setq res (form-urlencoded-to-query
                                   arg
                                   :external-format external-format)))))
      
      (if* post
         then (if* (and (eq (request-method req) :post)
                        
                        ;;
                        ;;DJC
                        ;;
                        (search "application/x-www-form-urlencoded"
                                (header-slot-value req :content-type))
                        
                        #+nil
                        (equal (header-slot-value req :content-type)
                               "application/x-www-form-urlencoded")
                        
                        ;;
                        ;; /DJC
                        ;;
                        )
                 then (setf res
                        (append res
                                (form-urlencoded-to-query
                                 (get-request-body req)
                                 :external-format external-format)))))
      (setf (getf (request-reply-plist req) 'request-query-sig)
        signature)
      (setf (request-query-alist req) res))))


;;
;; This is fixed in original-allegroserve - octets-to-string wrapped
;; around comment -- try to propogate into portableallegroserve.
;;
#-allegro
(defun proxy-request (req ent &key pcache-ent (respond t) 
                                   (level *browser-level*))
  ;; a request has come in with an http scheme given in uri
  ;; and a machine name which isn't ours.
  ;; 
  ;; the headers have been parsed.
  ;;
  ;; send out the request
  ;; get the response and if respond is true send back the response
  ;;
  (let* ((request-body (get-request-body req))
         (outbuf (get-header-block))
         (outend)
         (clibuf)
         (cliend)
         (sock)
         (uri (request-raw-uri req))
         (host (uri-host uri))
         (port (uri-port uri))
         (method (request-method req))
         (protocol :http/1.0)
         (state :pre-send)
         (keep-alive)
         (cached-connection)
         (phostport (and ent (entity-extra ent)))
         (otherheaders))

    (if* phostport
       then ; we're proxying to a proxy. yikes
            (setq host (car phostport)
                  port (cdr phostport)))
    
    (unwind-protect
        (tagbody
          
         retry-proxy

          (handler-bind ((error 
                          #'(lambda (cond)
                              (logmess
                               (format nil "error during proxy: ~a ~% with ~
cached connection = ~s~%" cond cached-connection))
                              (if* cached-connection
                                 then ; retry
                                      (logmess "retry proxy")
                                      (if* sock
                                         then (ignore-errors
                                               (close sock :abort t)))
                                      (go retry-proxy))
                                    
                              (if* pcache-ent
                                 then (kill-pcache-ent pcache-ent))
                                    
                                                   
                              (if* (not (member :notrap *debug-current*
                                                :test #'eq))
                                 then ; we want to auto-handle the error
                                      (if* (eq state :pre-send)
                                         then ; haven't sent anything
                                              ; so send failed response
                                              (ignore-errors
                                               (proxy-failure-response req ent)))
                                      (return-from proxy-request nil)))))
            


            (setq keep-alive nil ; assume not keep alive
                  cached-connection nil)
            
            ; create outgoing headers by copying
            (copy-headers (request-header-block req) outbuf
                          *header-client-array*)
    
            ;; now insert new headers
    
            ; content-length is inserted iff this is put or post method
            (if* (member method '(:put :post) :test #'eq)
               then (insert-header outbuf :content-length
                                   (format nil "~d"
                                           (if* request-body
                                              then (length request-body)
                                              else 0))))
    
            ; connection  we'll set to 'close' for now but at some point
            ; we'll connection caching so we'll want to do some keep-alive'ing
            ;  
            
            (insert-header outbuf :connection 
                           (if* *connection-caching*
                              then "Keep-Alive"
                              else "close"))
    

            ;(logmess "outbuf now")
            ;(dump-header-block outbuf *initial-terminal-io*)
            
            ; send host header 
            (let ((host (or (request-header-host req)
                            (header-buffer-header-value
                             (request-header-block req) :host)
                            (if* port
                               then (format nil "~a:~d"
                                            host port)
                               else host))))
              (insert-header outbuf :host host))
                             
            
            ; if the proxier decides to check authorization before
            ; doing a proxy then the authorization header will appear
            ; in the request-headers list and we want to prevent the
            ; authorization header from being sent twice in this case:
            ; [spr33532]
            (dolist (header (request-headers req))
              (if* (not (eq (car header) :authorization))
                 then (insert-non-standard-header outbuf (car header) (cdr header))))
            
            (setq outend (add-trailing-crlf outbuf 1))

            (if-debug-action :xmit
                             (format *debug-stream* "proxy converted headers toward server~%")
                             (dotimes (i outend)
                               (write-char (code-char (aref outbuf i)) *debug-stream*))
                             (format *debug-stream* "---- end---~%")
                             (force-output *debug-stream*))
  
  
  
                   
                   
                   
            ; time to make a call to the server
            (handler-case
                (multiple-value-setq (sock cached-connection)
                  (get-possibly-cached-connection
                   host (or port 80)))
              (error (cond)
                (declare (ignore cond))
                (if* respond
                   then (with-http-response (req ent :response
                                                 *response-not-found*)
                          (with-http-body (req ent)
                            (html
                             (:html
                              (:head (:title "404 - Not Found"))
                              (:body
                               (:h1 "404 - Not Found")
                               "The proxy failed to connect to machine "
                               (:b (:princ-safe host))
                               " on port "
                               (:b (:princ-safe (or port 80)))))))))
                (return-from proxy-request)))

            (if* *watch-for-open-sockets*
               then (schedule-finalization 
                     sock 
                     #'check-for-open-socket-before-gc))
            

            ;; there are bogus ip redirectors out there that want to
            ;; see the whole request in the packet. (e.g www.cbs.com)
            ;; so we build as much as we can and then blast that out
            
            ; this is written in this non-pretty way for speed
            
            
            (let ((firstbuf (get-header-block))
                  (ind 0)
                  (cmdstrings
                   '((:get . #.(make-array 3
                                           :element-type '(unsigned-byte 8)
                                           :initial-contents
                                           (list
                                            (char-int #\G)
                                            (char-int #\E)
                                            (char-int #\T))))
                     (:post . #.(make-array 4
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents
                                 (list
                                  (char-int #\P)
                                  (char-int #\O)
                                  (char-int #\S)
                                  (char-int #\T))))
                                       
                     ))
                  (prot-strings
                   '((:http/1.0 . #.(make-array 8
                                                :element-type '(unsigned-byte 8)
                                                :initial-contents
                                                (list
                                                 (char-int #\H)
                                                 (char-int #\T)
                                                 (char-int #\T)
                                                 (char-int #\P)
                                                 (char-int #\/)
                                                 (char-int #\1)
                                                 (char-int #\.)
                                                 (char-int #\0)
                                                 )))
                     (:http/1.1 . #.(make-array 8
                                     :element-type '(unsigned-byte 8)
                                     :initial-contents
                                     (list
                                      (char-int #\H)
                                      (char-int #\T)
                                      (char-int #\T)
                                      (char-int #\P)
                                      (char-int #\/)
                                      (char-int #\1)
                                      (char-int #\.)
                                      (char-int #\1)
                                      )))))
                     
                  )
              (let ((cmd (cdr (assoc method cmdstrings :test #'eq))))
                
                ; write method
                (if* cmd
                   then (dotimes (i (length cmd))
                          (setf (ausb8 firstbuf i)  (ausb8 cmd i)))
                        (incf ind (length cmd))
                   else ; unusual method, turn method into a string
                        (let ((str (string-upcase (string method))))
                          (dotimes (i (length str))
                            (setf (ausb8 firstbuf i) 
                              (char-int (schar str i))))
                          (incf ind (length str))))
                
                (setf (ausb8 firstbuf ind) #.(char-int #\space))
                (incf ind)
                
                
                ; now the uri
                (let ((str (if* phostport
                              then ; proxying so send http://...
                                   (net.uri:render-uri (request-raw-uri req) 
                                                       nil)
                              else (net.aserve.client::uri-path-etc uri))))
                  (dotimes (i (length str))
                    ; should do string-to-octets...
                    (setf (ausb8 firstbuf ind) 
                      (char-int (schar str i)))
                    (incf ind)))
                
                (setf (ausb8 firstbuf ind) #.(char-int #\space))
                (incf ind)
                
                ; now the protocol
                    
                (let ((cmd (cdr (assoc protocol prot-strings :test #'eq))))
                  (if* (null cmd)
                     then (error "can't proxy protocol ~s" protocol))
                  (dotimes (i (length cmd))
                    (setf (ausb8 firstbuf ind)  (ausb8 cmd i))
                    (incf ind)))
                    
                (setf (ausb8 firstbuf ind) #.(char-int #\return))
                (incf ind)
                (setf (ausb8 firstbuf ind) #.(char-int #\linefeed))
                (incf ind)
                    
                    
                ; now add as much of the headers as we can 
                (do ((i 0 (1+ i))
                     (tocopy (min (- (length firstbuf) ind) outend)))
                    ((>= i tocopy)
                     
                     ; 
                     (if-debug-action 
                      :xmit
                      (format *debug-stream* "about to send~%")
                      (dotimes (i ind)
                        (write-char (code-char (ausb8 firstbuf i))
                                    *debug-stream*))
                      (format *debug-stream* "<endof xmission>~%"))
                     (write-sequence firstbuf sock :end ind)
                     (if* (< i outend)
                        then ; still more from original buffer left
                             (write-sequence outbuf sock
                                             :start i
                                             :end outend))
                     )
                      
                  (setf (ausb8 firstbuf ind) (ausb8 outbuf i))
                  (incf ind))
                    
                (free-header-block firstbuf)))

            
            
            
            ; now the body if any
            (if* request-body
               then (write-sequence request-body sock))
    
            (force-output sock)
          
            ; a shutdown would make sense here but it seems to confuse
            ; the aol servers
            ;(socket:shutdown sock :direction :output)

            (let (protocol response comment header-start given-content-length
                  body-buffers body-length)
              (loop
                ; loop until we don't get a 100 continue
                ;
                ; now read the response and the following headers
                (setq outend (read-headers-into-buffer sock outbuf))

                (if* (null outend)
                   then ; response coming back was truncated
                        (error "truncated proxy response"))
                      
  
                (multiple-value-setq (protocol response comment header-start)
                  (parse-response-buffer outbuf))

                (if* (null protocol)
                   then ; bogus response
                        (return-from proxy-request
                          (proxy-failure-response req ent)))
              
                (if* (not (eql response 100)) then (return)))
    

              (setf (request-reply-code req) 
                (code-to-response response)) ; for the logging
            
              (setq otherheaders
                (parse-header-block outbuf header-start outend))
            
              ; Get the body of the message if any.
              ; there is never a response  to a :head request although the header
              ;  fields may imply there is.
              ; These response codes don't have a message body:
              ; 1xx, 204, 304
              ; All other responses include a message body which may be of zero size
              ;
    
              (if* (setq given-content-length
                     (header-buffer-header-value outbuf :content-length))
                 then (setq given-content-length
                        (net.aserve.client::quick-convert-to-integer 
                         given-content-length)))


              
            
              (if* (not (or (eq (request-method req) :head)
                            (<= 100 response 199) 
                            (eq response 204)
                            (eq response 304)))
                 then ; got to read the body
                      (multiple-value-setq (body-buffers body-length)
                        (read-into-block-buffers sock 
                                                 given-content-length))
                      
                      (if* (and given-content-length
                                (not (eql body-length given-content-length)))
                         then (warn "content-length ~s but body length ~d"
                                    given-content-length body-length)
                              (setq given-content-length body-length)))
              
              
              (setf (request-reply-content-length req) 
                (or body-length given-content-length 0))

              (setq keep-alive
                (equalp (header-buffer-header-value outbuf :connection)
                        "keep-alive"))
              
              (if* keep-alive
                 then (add-to-connection-cache sock
                                               host
                                               (or port 80))
                 else (close sock))
              
              (setq sock nil)

            
              ; convert the header we received from the server into one
              ; to send to the client
              (setq clibuf (get-sresource *header-block-sresource*))
    
          
              (copy-headers outbuf clibuf *header-server-array*)
    
              ; add content-length if known
              (if* given-content-length
                 then (insert-header clibuf :content-length 
                                     (format nil "~s" given-content-length)))
    
              ; should add a 'via' line
    
              ; transfer-encoding - 
              ; we won't chunk back since we know the content length
              
              (dolist (header otherheaders)
                (insert-non-standard-header clibuf (car header) (cdr header)))

              (setq cliend (add-trailing-crlf clibuf 2))

          
              (if-debug-action 
               :xmit
               (format *debug-stream* "~%~%proxy converted headers toward client~%")
               (dotimes (i cliend)
                 (write-char (code-char (aref clibuf i)) 
                             *debug-stream*))
               (format *debug-stream* "---- end---~%")
               (force-output *debug-stream*))

              ; do the response
              (setq state :post-send)
              
              (if* respond
                 then (ignore-errors
                       (let ((rsock (request-socket req)))
                
                         (format rsock "HTTP/1.1 ~d ~a~a" response (and comment (octets-to-string comment)) *crlf*)
      
                         (write-sequence clibuf rsock :end cliend)
                         (if* body-length 
                            then (write-body-buffers rsock body-buffers 
                                                     body-length))
                         (force-output rsock))))
                
              (if* (and pcache-ent 
                        (eq (request-method req) :get))
                 then ; we are caching
                      (let ((tmp-clibuf clibuf)
                            (tmp-body-buffers body-buffers))
                        (setf clibuf nil
                              body-buffers nil)
                        (cache-response req pcache-ent
                                        response comment tmp-clibuf 
                                        tmp-body-buffers body-length level)
                        ; these buffers have been saved in the cache
                        ; so nil them out so they aren't freed
                        ))
                
              (dolist (block body-buffers) (free-header-block block))
              )))
    
      ;; cleanup forms
      (if* sock 
         then (ignore-errors (force-output sock))
              (ignore-errors (close sock :abort t)))
      
      (free-header-block outbuf)
      (free-header-block clibuf))))





;;
;; FLAG -- below definition might not be needed with new portableaserve for LW6:
;;
  
;;#+lispworks
#+nil
(in-package :acl-compat-mp)

;;#+lispworks
#+nil
(defun invoke-with-timeout (timeout bodyfn timeoutfn)
  (block timeout
    (let* ((process mp:*current-process*)
           (unsheduled? nil)
           (timer (mp:make-timer
                   (lambda ()
                     (mp:process-interrupt process
                                           #'(lambda ()
                                               (unless unsheduled?
                                                 (return-from timeout
                                                   (funcall timeoutfn)))))))))
      (mp:schedule-timer-relative timer timeout)
      (unwind-protect (funcall bodyfn)
        (without-interrupts
         (mp:unschedule-timer timer)
         (setf unsheduled? t))))))

#-allegro (eval-when (compile load eval) (glisp:end-redefinitions-ok))
