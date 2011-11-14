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


(setf (net.html.parser::tag-no-end :button) nil)


(defun crawl (part 
              &key (host "localhost") 
                   (port 9000) 
                   (output-root (make-pathname 
                                 :directory (list :absolute "tmp" "sites"
                                                  (format nil "~(~a~)" (read-from-string part)))))
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
  
    (let ((url (format nil "/make?part=~a~{~a~}" 
                       part
                       (mapcar #'(lambda(key val)
                                   (format nil "&~a=~a" key val))
                               (plist-keys make-part-args) (plist-values make-part-args)))))
      (crawl-url url host port output-root visited-urls))
  
    (setq *developing?* developing?)))




(defun crawl-anchor (anchor host port output-root visited-urls)
  (let ((new-url (getf (rest (first anchor)) :href)))
    ;;
    ;; FLAG -- check here to ensure this is an internal URL - if not, don't crawl it.
    ;;
    (when (not (or (gethash new-url visited-urls)
                   ;;
                   ;; FLAG -- do other tests to make sure new-url is internal to our site.
                   ;;
                   (< (length new-url) (1+ (length "sessions")))
                   (not (string-equal (subseq new-url 1 (1+ (length "sessions"))) "sessions"))
                   (search "mailto:" new-url)
                   (not (search ".htm" new-url))
                   ))
      (crawl-url new-url host port output-root visited-urls))))
  

(defun crawl-url (url host port output-root visited-urls)
  (when (not (gethash url visited-urls))
    (setf (gethash url visited-urls) t)
    (multiple-value-bind (html code headers uri)
        (net.aserve.client:do-http-request (format nil "http://~a:~a~a" host port url))
      (declare (ignore code headers))
      (let* ((lhtml      
              (first (net.html.parser:parse-html 
                      html
                      :callbacks 
                      (list (cons :a
                                  #'(lambda(anchor)
                                      (crawl-anchor anchor host port output-root visited-urls)))))))
             (url-string (net.uri:uri-path uri))
             (output-path 
              (let* ((components (rest (rest (split url-string #\/))))
                     (directory (butlast components))
                     (file  (lastcar components))
                     (name-type (split file #\.))
                     (name (first name-type))
                     (type (second name-type)))
                
                (make-pathname :directory (append (pathname-directory output-root) directory)
                               :name name
                               :type type))))
        
        
        (let ((lhtml (relativize-lhtml lhtml url-string output-path host)))
          
          (cl:ensure-directories-exist (directory-namestring output-path))
          (with-open-file (out output-path :direction :output :if-exists :supersede :if-does-not-exist :create)
            (html-print lhtml out)))))))

(defun relativize-lhtml (lhtml url output-path host)
  (when lhtml
    (mapcar #'(lambda(element)
                (cond ((atom element) element)
                      ((and (eql (first element) :a)
                            (not (let ((url (getf (rest element) :href)))
                                   (or (string-equal (subseq url (- (length url) 3)) "pdf")
                                       (string-equal (subseq url (- (length url) 3)) "igs")
                                       (string-equal (subseq url (- (length url) 4)) "iges")
                                       (string-equal (subseq url (- (length url) 4)) "step")))))
                       (when (null (getf (rest element) :href))
                         (error ":a tag without :href found in ~s" lhtml))
                       (let ((element (copy-list element)))
                         (setf (getf (rest element) :href)
                           (relativize-url (getf (rest element) :href) url))
                         element))
                      ((or (eql (first element) :img)
                           (eql (first element) :link)
                           (eql (first element) :embed)
                           (eql (first element) :a)
                           (and (eql (first element) :input)
                                (string-equal (format nil "~(~a~)" (getf (rest element) :type)) "image")))
                       
                       (let ((link-tag (cond ((getf (rest element) :src) :src)
                                             ((getf (rest element) :href) :href)
                                             (t (error ":img or :link or :embed or image type :input tag without :src or :href found in ~s" lhtml)))))
                         (let ((link-url 
                                (format nil "http://~a:9000~a" host (getf (rest element) link-tag))
                                ))
                           (let ((element (copy-list element)))
                             (setf (getf (rest element) link-tag)
                               (relativize-image-source (getf (rest element) link-tag) url))
                             (let ((image-output (merge-pathnames 
                                                  (getf (rest element) link-tag)
                                                  (make-pathname :directory (pathname-directory output-path)))))
                               (ensure-directories-exist (make-pathname :directory (pathname-directory image-output)))
                               (with-open-file (out image-output :direction :output
                                                :if-exists :supersede :if-does-not-exist :create)
                                 
                                 (write-sequence
                                  (net.aserve.client:do-http-request link-url :format :binary) out)))
                             element))))
                      (t (relativize-lhtml element url output-path host)))) 
            lhtml)))
(defun relativize-image-source (url base)
  (let ((length (- (length (split base #\/)) 3)))
    (cond ((> length 0)
           (format nil "~{~a~^/~}~a"
                   (make-list length :initial-element "..")
                   url))
          ((and (zerop length) (eql (aref url 0) #\/))
           (subseq url 1))
          (t url))))
                                    
(defun relativize-url (url base)
  (if (or (not (eql (aref url 0) #\/))
          (< (length url) 8)
          (string-equal (subseq url 0 8) "http://"))
      url
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
        
        (let ((parent-levels (if (null base-list)
                                 0
                               (- (length base-list)
                                  (if (search "." (lastcar base-list)) 1 0)))))
          (format nil "~{~a~}~{~a~^/~}" 
                  (make-list parent-levels :initial-element "../")
                  url-list))))))
    

(defun copy-url (url dest)
  (with-open-file (out dest :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence (net.aserve.client:do-http-request url :format :binary) out)))

