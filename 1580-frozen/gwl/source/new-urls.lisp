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

(defun compute-url (root-path)
  (if (null root-path)
      "index.html"
    (format nil "~{~(~a~)/~}index.html"
            (mapcar #'(lambda(component)
                        (if (listp component)
                            (format nil "~a/~a"
                                    (first component) (second component))
                          component))
                    (reverse root-path)))))
  
(defun compute-root-path (root-path-components)
  (let ((result-list nil))
    (do ((component (pop root-path-components)
                    (pop root-path-components)))
        ((null component) (nreverse result-list))
      (cond ((string-equal component "index.html"))
            ((numberp (read-from-string component))
             (let ((part (pop root-path-components)))
               (push (list (make-keyword part)
                           (read-from-string component)) result-list)))
            (t (push (make-keyword component) result-list))))))



(defun present-part (req ent url &key instance-id header-plist fixed-prefix)
  (declare (ignore header-plist))
  (when fixed-prefix (setq url (subseq url (1+ (length fixed-prefix)))))
  
  (let ((cookies (when *process-cookies?* (get-cookie-values req)))
        (components (split url #\/)))
    (let* ((hash-entry (gethash (make-keyword (or instance-id (second components))) *instance-hash-table*))
           (root-object (first hash-entry)) (skin (third hash-entry))
           (root-path (multiple-value-bind (value found?) (gethash url *descriptive-url-hash*)
                        (if found? value
                          (compute-root-path (reverse (if instance-id components (rest (rest components))))))))
           (respondent (when root-object (the-object root-object (follow-root-path root-path)))))
      
      ;;
      ;; FLAG -- use actual application-root rather than global root here
      ;;
      (when (and respondent (the-object respondent root) 
                 (typep (the-object respondent root) 'session-control-mixin))
        (the-object respondent root (set-expires-at)))
      (when (and respondent (the-object respondent root) 
                 (the-object respondent root (set-time-last-touched!))))
      (when (and respondent (the-object respondent root) 
                 (the-object respondent root (set-slot! :last-visited-root-path root-path))))
      (when (and respondent (the-object respondent root) 
                 (the-object respondent root (set-remote-host! req))))
      
      (when (and *process-cookies?* respondent) (the-object respondent (set-slot! :cookies-received cookies)))
      
      (when *process-cookies?* (the-object respondent process-cookies!))
      
      (let ((header-plist (the-object respondent header-plist))
            (security-ok? (the-object respondent root do-security-check)))
        
        (cond ((and respondent security-ok?)
               (with-http-response (req ent :response (cond ((getf header-plist :location) 
                                                             *response-moved-permanently*)
                                                            (t *response-ok*)))
            
                 (setf (reply-header-slot-value req :cache-control) "no-cache")
                 (setf (reply-header-slot-value req :pragma) "no-cache")
                 (mapc #'(lambda(key val)
                           (setf (reply-header-slot-value req key) val)) 
                       (plist-keys header-plist) (plist-values header-plist))

                 (when *process-cookies?*
                   (mapc #'(lambda(plist)
                             (apply #'set-cookie-header req plist))
                         (the-object respondent cookies-to-send)))
            
                 (with-http-body (req ent)
                   (when (null (getf header-plist :location))
                     (let ((*req* req) (*ent* ent) (*skin* skin))
          
                       (the-object respondent (:before-present!))
                       (let ((*stream* *html-stream*))
                         (the-object respondent (:write-html-sheet)))
                       (the-object respondent (:after-present!)))))))
              (respondent
               (with-http-response (req ent)
                 (setf (reply-header-slot-value req :cache-control) "no-cache")
                 (setf (reply-header-slot-value req :pragma) "no-cache")
                 (with-http-body (req ent)
                   (the-object respondent root security-check-failed write-html-sheet))))
              (t (net.aserve::failed-request req)))))))
