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

(defvar *static-home* nil)

(defun ensure-static-relative-pathname (relative)
  (let ((pathname (merge-pathnames relative *static-home*)))
    (or (probe-file pathname)
	(error "Required static subdirectory ~a does not appear to exist.~%" pathname))))

(defun publish-images ()
  (with-all-servers (server)
    (publish-directory
     :prefix "/images/gwl/"
     :server server
     :destination (namestring (ensure-static-relative-pathname "gwl/images/")))))
		      
(defun publish-statics ()
  (with-all-servers (server)
    (publish-directory
     :prefix "/static/"
     :server server
     :destination (namestring (ensure-static-relative-pathname "")))))

(defun publish-style ()
  (with-all-servers (server)
    (publish-directory
     :prefix "/style/"
     :server server
     :destination (namestring (ensure-static-relative-pathname "gwl/style/")))))


(defvar *aserve-listeners* 25)
(defvar *aserve-port* 9000)

(defun client-test (port)
  (multiple-value-bind (result error)
      (ignore-errors  
	(glisp:with-timeout (2 (error "AllegroServe port probe timed out on port ~a. 
Perhaps a zombie process is holding port ~a?~%" port port))
	  (net.aserve.client:do-http-request   
	      (format nil "http://localhost:~a" port))))
    (declare (ignore result))
    (when (typep error 'error)
      port)))


(defun start-gwl (&key (port *aserve-port*) (listeners *aserve-listeners*) 
		  (external-format :utf8-base))
  (net.aserve:shutdown)
  (let ((port port))
    (do ((error (client-test port) (client-test port)))
        (error (format t "~&Starting AllegroServe on ~a...~%" port)
         (net.aserve:start :port port :listeners listeners #-mswindows :external-format #-mswindows external-format)
         port)
      (incf port))))

(defvar *settings* 
  (list (list '*static-home* *static-home* 
	      #'(lambda()
		  (or (and glisp:*gendl-source-home*
			   (probe-file (merge-pathnames "gwl/static/" 
							glisp:*gendl-source-home*)))
		      (and glisp:*gdl-home*
			   (probe-file (merge-pathnames "static/"
							glisp:*gdl-home*)))

		      (and glisp:*gdl-program-home*
			   (probe-file (merge-pathnames "static/"
							glisp:*gdl-program-home*)))
		      
		      (warn "~%Static home not found in source directory or parent of program directory.~%"))))))


;;
;; FLAG -- move publishes into global param with the basic publishing data and 
;;         call general-purpose publish function on this data. 
;;    also detect changes in the publishing to include in return value.
;;
(defun initialize ()
  ;;
  ;; FLAG -- investigate if this initialize-multiprocessing business
  ;; is still needed, currently LW-only.
  ;;
  (glisp:initialize-multiprocessing)

  (let (anything-changed?)
    (setq anything-changed? (glisp:set-settings *settings*))
    (publish-images) (publish-statics) (publish-style) (start-gwl) 
    anything-changed?))

