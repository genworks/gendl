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

;;#+allegro
;;(setf (net.html.parser::tag-no-end :button) nil)

(defun crawl (&key
		part
		url-path ;; to give explicit url instead of part type. 
		(host "localhost") 
                (port 9000) 
                (output-root (make-pathname 
                              :directory (list :absolute "tmp" "sites"
					       (cond (part (format nil "~(~a~)" (read-from-string part)))
						     (url-path (format nil "~(~a~)"
								       (first
									(remove "" (glisp:split-regexp "/" url-path )
										:test #'string-equal))))
						     (t (error "Neither :part nor :url was specified to the crawl function.~%"))))))
                (visited-urls (make-hash-table :test #'equalp))
                make-part-args)
  
  "Void. Instantiates and ``Crawls'' a given object instance and creates static HTML pages reflecting 
the instance tree. This means it will recursively follow all the links for the object. By default 
the files are written into \"/tmp/sites/\".

:arguments (part \"String. Names a package-qualified part which should mix in <tt>base-html-sheet</tt>.\")
:&key ((host \"localhost\") \"String. Host on which the server is running.\"
       (port 9000) \"Integer. Port on which the server is running.\"
       (output-root \"/tmp/sites/[non-package-qualified-part-name]/\") \"String or pathname. Directory where filfes will be written\"
       make-part-args \"Plist. Other make-instance arguments to use to initialize the object.\")

:example 
<pre>
    (gwl:crawl \"yadd:assembly\")
</pre>"

  (let ((developing? *developing?*))
    (setq *developing?* nil)
  
    (let ((url (or url-path
		   (format nil "/make?part=~a~{~a~}" 
			   part
			   (mapcar #'(lambda(key val)
                                       (format nil "&~a=~a" key val))
				   (plist-keys make-part-args) (plist-values make-part-args))))))
      (crawl-url url host port output-root visited-urls))
  
    (setq *developing?* developing?)))


(defun crawl-anchor (anchor host port output-root visited-urls)
  (let ((uri (net.uri:parse-uri (getf (rest (first anchor)) :href))))
    (let ((scheme (net.uri:uri-scheme uri))
	  (uri-host (net.uri:uri-host uri))
	  (uri-port (net.uri:uri-port uri))
	  (path (net.uri:uri-path uri)))
      (unless (or (null path) ;; empty path - likely hash mark anchor. 
		  scheme uri-host uri-port ;; this is external - don't crawl.
		  (gethash path visited-urls)) ;; already crawled - don't crawl
	(crawl-url path host port output-root visited-urls)))))

    
(defun crawl-url (url host port output-root visited-urls)
  (setf (gethash url visited-urls) t)
  (multiple-value-bind (html code headers uri)
      (net.aserve.client:do-http-request (format nil "http://~a:~a~a" host port url))
      (declare (ignore code headers))
      (let* ((lhtmls (net.html.parser:parse-html
		      html
		      :callbacks (list (cons :a #'(lambda(anchor)
						    (crawl-anchor anchor host port
								  output-root visited-urls))))))
	     (lhtml1 (first lhtmls))
	     (lhtml2 (second lhtmls))
	     (lhtml (if (eql (first lhtml1) :!doctype) lhtml2 lhtml1))
             (uri-path (net.uri:uri-path uri))
             (output-path 
              (let* ((components (let ((components (remove "" (glisp:split-regexp "/" uri-path) :test #'string-equal)))
				   (if (find "sessions" components :test #'string-equal) (rest (rest components)) components)))
                     (directory (butlast components)))
		(destructuring-bind (name &optional type) (glisp:split-regexp "\\." (lastcar components))
                  (make-pathname :directory (append (pathname-directory output-root) directory) :name name :type type)))))

        (let ((lhtml (relativize-lhtml lhtml uri-path output-path host port visited-urls)))
          (ensure-directories-exist (directory-namestring output-path))
          (with-open-file (out output-path :direction :output :if-exists :supersede :if-does-not-exist :create)
            (html-print lhtml out))))))

(defun relativize-lhtml (lhtml url output-path host port visited-urls)
  (when (and lhtml url (not (string-equal url "#"))
	     (not (eql lhtml :unknown)) (not (eql url :unknown)))
    (mapcar #'(lambda(element)
		(let ((link-tag (when (and (consp element)(oddp (length element)))
				  (cond ((getf (rest element) :src) :src)
					((getf (rest element) :href) :href)))))
				      
                  (cond ((atom element) element)
			((and (eql (first element) :a)
			      (let* ((uri (let ((url-string (getf (rest element) :href)))
					    (unless url-string (error ":a tag without :href found in ~s" lhtml))
					    (net.uri:parse-uri url-string)))
				     (uri-host (net.uri:uri-host uri))
				     (uri-scheme (net.uri:uri-scheme uri))
				     (uri-port (net.uri:uri-port uri))
				     (uri-path (net.uri:uri-path uri))
				     (components (remove "" (glisp:split-regexp "/" uri-path) :test #'string-equal))
				     (components (if (find "sessions" components :test #'string-equal)
						     (rest (rest components)) components)))

				(and (consp components) (null uri-host) (null uri-scheme) (null uri-port)
				     (destructuring-bind (name &optional type) (glisp:split-regexp "\\." (lastcar components))
				       (declare (ignore name))
				       (member type '("htm" "html") :test #'string-equal)))))

			 
			 (let ((element (copy-list element)))
                           (setf (getf (rest element) :href) (relativize-url (getf (rest element) :href) url))

                           element))

			((eql (first element) :a) element)

			((and (or (and link-tag (member (first element) '(:img :link)))
				  (and (eql (first element) :input)
				       (string-equal (format nil "~(~a~)"
							     (getf (rest element) :type)) "image")))
                       
			      (let* ((uri (let ((url-string (getf (rest element) link-tag))) ;; "(\\.\\./)+" "/\\&"
					    (unless url-string (error "~a tag without :href found in ~s" link-tag lhtml))
					    (net.uri:parse-uri url-string)))
				     (uri-host (net.uri:uri-host uri))
				     (uri-scheme (net.uri:uri-scheme uri))
				     (uri-port (net.uri:uri-port uri)))
				(and (null uri-host) (null uri-scheme) (null uri-port))))

			 (let* ((element (copy-list element))
				(link-path (setf (getf (rest element) link-tag)
						 (relativize-url (getf (rest element) link-tag) url))))

			   (let ((link-output (merge-pathnames link-path output-path))
				 (link-url (format nil "http://~a:~a/~a" host port
						   (glisp:replace-regexp  link-path "\\.\\./" ""))))


			     (unless (gethash link-url visited-urls)
			       (ensure-directories-exist link-output)
			       (with-open-file (out link-output :direction :output
						    :element-type '(unsigned-byte 8)
						    :if-exists :supersede :if-does-not-exist :create)
				 (write-sequence (net.aserve.client:do-http-request link-url :format :binary) out))
			       (setf (gethash link-url visited-urls) t)))
			 
			   element))
			
			((or (member (first element) '(:img :link))
			     (and (eql (first element) :input)
				  (string-equal (format nil "~(~a~)"
							(getf (rest element) :type)) "image"))) element)
		      
			(t (relativize-lhtml element url output-path host port visited-urls)))))
            lhtml)))


(defun relativize-url (url base)

  (if (string-equal (subseq url 0 2) "..") url
      (let ((url-list (split url #\/))
            (base-list (split base #\/)))
	(let ((index 0) done?)
	  (mapc #'(lambda(url-component base-component)
                    (if (and (not done?)
                             (string-equal url-component base-component))
			(incf index) 
			(setq done? t)))
		url-list base-list)
        
	  (when (and (eql index (length url-list))
                     (eql (length url-list) (length base-list))
                     (string-equal (lastcar url-list) (lastcar base-list)))
            (decf index))
        
	  (setq url-list (subseq url-list index)
		base-list (subseq base-list index))
        
	  (let ((parent-levels (if (null base-list) 0
				   (- (length base-list)
                                      (if (search "." (lastcar base-list)) 1 0)))))
            (format nil "~{~a~}~{~a~^/~}" 
                    (make-list parent-levels :initial-element "../")
                    url-list))))))


#+nil
(defun relativize-image-source (url base)
  (let ((length (- (length (split base #\/)) 3)))
    (cond ((> length 0)
           (format nil "~{~a~^/~}~a"
                   (make-list length :initial-element "..")
                   url))
          ((and (zerop length) (eql (aref url 0) #\/))
           (subseq url 1))
          (t url))))


    

(defun copy-url (url dest)
  (with-open-file (out dest :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence (net.aserve.client:do-http-request url :format :binary) out)))

