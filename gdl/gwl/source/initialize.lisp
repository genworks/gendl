;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(defun publish-images ()
  (with-all-servers (server)
    (publish-directory
     :prefix "/images/gwl/"
     :server server
     :destination (cond (glisp:*genworks-source-home*
			 (format nil "~a" (merge-pathnames "gdl/gwl/static/gwl/images/" 
							   glisp:*genworks-source-home*)))
			(t (format nil "~a" (merge-pathnames "static/gwl/images/" glisp:*gdl-home*)))))))
		      
(defun publish-statics ()
  (with-all-servers (server)
    (publish-directory
     :prefix "/static/"
     :server server
     :destination (cond (glisp:*genworks-source-home*
			 (format nil "~a" (merge-pathnames "gdl/gwl/static/" 
							   glisp:*genworks-source-home*)))
			(t (format nil "~a" (merge-pathnames "static/" glisp:*gdl-home*)))))))

(defun publish-style ()
  (with-all-servers (server)
    (publish-directory
     :prefix "/style/"
     :server server
     :destination (cond (glisp:*genworks-source-home*
			 (format nil "~a" (merge-pathnames "gdl/gwl/static/style/" 
							   glisp:*genworks-source-home*)))
			(t (format nil "~a" (merge-pathnames "static/style/" glisp:*gdl-home*)))))))

(defvar *aserve-listeners* 25)

(defun client-test (port)
  (multiple-value-bind (result error)
      (ignore-errors  
	(net.aserve.client:do-http-request   
           (format nil "http://localhost:~a" port)))
    (declare (ignore result))
    (when (typep error 'error)
      port)))

(defun start-gwl (&key (port 9000) (listeners *aserve-listeners*) (external-format :utf8-base))
  (net.aserve:shutdown)
  (let ((port port))
    (do ((error (client-test port) (client-test port)))
        (error (format t "~&~%Starting AllegroServe on ~a...~%~%" port)
         (net.aserve:start :port port :listeners listeners #-mswindows :external-format #-mswindows external-format)
         port)
      (incf port))))


(defun initialize-gwl (&key edition) (declare (ignore edition)) 
       ;;(glisp:initialize-multiprocessing)
       (publish-images) 
       (publish-statics) 
       (publish-style)
       (start-gwl))


(push #'initialize-gwl *gdl-init-functions*)

