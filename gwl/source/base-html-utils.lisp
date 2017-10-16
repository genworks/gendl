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

;; FLAG -- remove this and see what breaks.
;;
(defparameter *try* nil)


(defvar *developing?* nil "Boolean. Used in some parts like application-mixin
and node-mixin, to decide whether to display developer links. Can also be used
by user application code.")

(defun query-arg-to-root-path (query arg &optional value)
  (let ((value (or value (rest (assoc arg query :test #'string-equal)))))
    (when value
      (setq value (glisp:replace-regexp  value "\\+" " "))
      (let (skip? 
            (components (read-from-string 
                         (string-append "(" value ")")))
            (result-list nil))
        (dotimes (position (length components) (nreverse result-list))
          (cond (skip? (setq skip? nil))
                ((consp (nth (1+ position) components))
                 (progn
                   (push (list (make-keyword (nth position components))
                               (first (nth (1+ position) components))) result-list)
                   (setq skip? t)))
                (t (push (make-keyword (nth position components)) result-list))))))))

(defun root-path-to-query-arg (root-path)
  (format nil "~{~(~a~)~^+~}"
          (mapcar #'(lambda(component)
                      (if (atom component) 
                          component 
                        (format nil "~a(~a)" (first component) (second component))))
                  root-path)))


;;
;; FLAG -- this is not thread-safe - you can get the same instance ID
;; if called simultaneously by two threads.
;;
;; FLAG -- we also have to protect against the (unlikely but possible)
;; case that the same number comes back twice within a second from the
;; random generator. Only way is to store the resutls and check
;; against previous results.
;;
;;
(defun make-new-instance-id (&key (max-value *max-id-value*))
  (let ((*print-base* 16) (universal-time (get-universal-time))
        (random (random max-value *iid-random-state*)))
    (let ((string (format nil "~a~a" random universal-time)))
      (if (and (glisp:featurep :allegro) 
	       (eql (symbol-value (read-from-string "excl:*current-case-mode*"))
		    :case-insensitive-upper))
	  (string-upcase string) string))))
         


(defun assoc-list-to-plist (assoc-list &key case-sensitive?)
  (mapcan #'(lambda(cons)
              (list (if case-sensitive? (make-keyword-sensitive (first cons))
			(make-keyword-sensitive (first cons)))
                    (rest cons)))
          assoc-list))


(defun merge-plist-duplicates (plist)
  (let ((keys (remove-duplicates (plist-keys plist))))
    (if (= (length keys) (half (length plist))) plist
      (mapcan #'(lambda(key)
                  (list key (let ((matches (find-plist-matches plist key)))
                              (if (consp (rest matches)) matches (first matches))))) keys))))

(defun find-plist-matches (plist key)
  (when plist 
    (if (eql key (first plist))
        (cons (second plist) (find-plist-matches (rest (rest plist)) key))
      (find-plist-matches (rest (rest plist)) key))))
                  




            
            

(defun html-escape (string)
  (glisp:replace-regexp
   (glisp:replace-regexp
    (glisp:replace-regexp
     (glisp:replace-regexp
      (glisp:replace-regexp (with-output-to-string (ss)
                              (cl-who:with-html-output (ss)
                                (cl-who:esc string))) " " "-") "\\(" "_") "\\)" "_") "\\^" "hat") "\\*" "-star-"))





(defun publish-gwl-app (path string-or-symbol &key make-object-args (server *wserver*))
  "Void. Publishes an application, optionally with some initial arguments to be passed in as input-slots.

:arguments (path \"String. The URL pathname component to be published.\"
            string-or-symbol \"String or symbol. The object type to insantiate.\")

:&key (make-object-args \"Plist. Extra arguments to pass to make-object.\")
"

  (publish :path path
	   :server server
           :function #'(lambda(req ent)
                         (gwl-make-object req ent 
                                          (format nil (if (stringp string-or-symbol) "~a" "~s")
                                                  string-or-symbol)
                                          :make-object-args make-object-args))))


(defun publish-string-content (url string &rest publish-args &key (server *wserver*) &allow-other-keys)
  "String (representing a url path). Publishes given url to respond with text content as 
specified by given string.


:arguments (url \"String. The url to be published.\"
            string \"String. The content to be emitted when the url is requested.\")

:&rest (publish-args \"plist.  Arguments to be passed on to publish function, e.g. :content-type.\")
  

"
  
  (apply #'publish  
         :path url
	 :server server
         :function #'(lambda (req ent)
                       (with-http-response (req ent)
                         (setf (reply-header-slot-value req :cache-control) "no-cache")
                         (with-http-body (req ent)
                           (let ((stream (request-reply-stream req)))
                             (write-string string stream)))))
         publish-args)
  url)
