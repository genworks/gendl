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


(defvar *log-buffer* nil)

(defvar *log-report-buffer* nil)

(defvar *log-file* nil)

(defun start-log-maker (&key (interval 30)  (resolve-dns? t) (server net.aserve:*wserver*))
  
  (setq *log-file* (format nil "/home/dcooper8/kitchen/logs-~a-~a.lisp" 
                           (gwl::iso-time (get-universal-time))
                           (slot-value (slot-value server 'net.aserve::socket) 'socket::local-port)))
  (glisp:process-run-function
      "log-maker"
    #'(lambda()
        (do ()(nil) (clear-log-buffer :resolve-dns? resolve-dns?) (sleep interval)))))

(defun read-log-entries ()
  (when (and *log-file* (probe-file *log-file*))
    (clear-log-buffer :resolve-dns? t)
    (with-open-file (in *log-file*)
      (let (result)
	(do ((entry (read in nil) (read in nil)))
	    ((null entry) (nreverse result))
	  (push entry result))))))
	  

(defun clear-log-buffer (&key resolve-dns?)
  (let (temp-buffer)
    (glisp:w-o-interrupts
       (setq temp-buffer *log-buffer*)
       (setq *log-buffer* nil))
    (ensure-directories-exist (make-pathname :directory (pathname-directory *log-file*)))
    
    (with-open-file (out *log-file* :direction :output
                     :if-exists :append :if-does-not-exist :create)
      (dolist (log temp-buffer)
        (let* ((ipaddr (getf log :ip-address))
               (name (when resolve-dns? (or (socket:ipaddr-to-hostname ipaddr) (format nil "Unknown (~a)" ipaddr)))))
          (let ((processed-log (append (list :start-iso-time (when (getf log :start-time) (iso-time (getf log :start-time)))
                                             :end-iso-time (when (getf log :end-time) (iso-time (getf log :end-time)))
                                             :domain name) log)))
            (print processed-log out)))))))


(defun iso-time (universal-time)
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time universal-time)
    (format nil "~a-~2,,,'0@a-~2,,,'0@aT~2,,,'0@a:~2,,,'0@a:~2,,,'0@a"
            year month date hours minutes seconds)))


(defmethod net.aserve::log-request :after ((req http-request))

  (dolist (function *aserve-log-request-after-functions*)
    (funcall function req)))

(defparameter *aserve-log-request-after-functions* nil)

#+nil
(defmethod monkey-log ((req http-request))
  
  (when *log-file*
    (let* ((ipaddr (socket:remote-host (request-socket req)))
	   (end-time   (net.aserve::request-reply-date req))
	   (start-time (net.aserve::request-request-date req))
	   (code   (let ((object (net.aserve::request-reply-code req)))
		     (if object (net.aserve::response-number object) 999)))
	   (length  (or (net.aserve::request-reply-content-length req)
			(glisp:socket-bytes-written (request-socket req))))
	   (referrer (net.aserve:header-slot-value req :referer))
	   (user-agent (net.aserve::header-slot-value req :user-agent)))

      (let* ((uri (net.aserve::request-uri req))
	     (log-plist (list :ip-address (socket:ipaddr-to-dotted ipaddr)
			      :start-time start-time
			      :end-time end-time
			      :code code
			      :length length
			      :query (request-query req)
			      :uri-path (net.aserve::uri-path uri)
			      :uri-host (net.aserve::uri-host uri)
			      :uri-port (net.aserve::uri-port uri)
			      :uri-scheme (net.aserve::uri-scheme uri)
			      :method (net.aserve::request-method req)
			      :referrer referrer
			      :user-agent user-agent)))

	(when (not (or (search "cgi-bin" (getf log-plist :uri-path))
		       (search "ubb" (getf log-plist :uri-path))))

	  (glisp:w-o-interrupts
	    (if *log-buffer*
		(nconc *log-buffer* (list log-plist))
		(setq *log-buffer* (list log-plist)))))))))
  


