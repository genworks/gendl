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

(defun start-session-reaper (&key (minutes 20) (debug? t) (listeners 20) extra-functions restart-server?)
    (declare (ignore listeners))
    (format t "~2%Lauching Expired Session Reaper to awaken and run every ~a minute~:p~2%" minutes)
    (glisp:process-run-function
     "GWL Session Reaper" 
     #'(lambda() 
	 (do ()(nil) (sleep (* minutes 60))
	   (when debug? (format t "~&Reaper waking up...~%"))
	   (when *reap-expired-sessions?* 
	     (maphash #'(lambda(key val) (declare (ignore key))
			       (when (typep (first val) 'session-control-mixin)
				 (the-object (first val) (clear-expired-session :debug? debug?))))
		      *instance-hash-table*))
	   (when restart-server?
	     (glisp:w-o-interrupts
	       (let ((port (server-port)))
		 (when (and port (>= port 1000))
		   (net.aserve:shutdown) (net.aserve:start :port port :listeners listeners)))))
	   (glisp:w-o-interrupts
	     (mapc #'funcall (ensure-list extra-functions)) (glisp:gc-full))))))


(defun publish-make-and-answer (server)

  (publish :path "/make"
           :server server
           :content-type "text/html"
           :function 'make-object-internal)

  (publish :path "/answer"
           :server server
           :content-type "text/html"
           :function 'answer))

(pushnew 'publish-make-and-answer *publishers*)

(defun publish-fixed-prefix (fixed-prefix)
  (with-all-servers (server)
    (publish :path (string-append "/" fixed-prefix "/make")
             :server server
             :content-type "text/html"
             :function 'make-object-internal)

    (publish :path (string-append "/" fixed-prefix "/answer")
             :server server
             :content-type "text/html"
             :function 'answer)))
