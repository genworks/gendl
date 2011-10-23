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

(in-package :tree)

(defun gdltreetoggle (req ent)

  (let ((query (request-query req)))
    (let* ((iid (rest (assoc "iid" query :test #'string-equal)))
           (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
           (plist (gwl::assoc-list-to-plist query :case-sensitive? t)))
      (let ((respondent-root-path (getf plist :respondentRootPath))
            (root-path (getf plist :rootPath)))
        (setq respondent-root-path (base64-decode-list respondent-root-path)
              root-path (base64-decode-list root-path))
        (let ((object  (the (follow-root-path root-path)))
              (respondent (the (follow-root-path respondent-root-path))))
          (the-object object toggle-state!)
          (respond-with-new-html-sections req ent respondent))))))

(publish :path "/gdltreetoggle" :function 'gdltreetoggle)


(defun respond-with-new-html-sections (req ent self)
  (let (replace-list)
    (dolist (section (ensure-list (the html-sections)) (reverse replace-list))
      (let ((status (the-object section (slot-status :main-view))))
        (when (eql status :unbound)
          (when *debug?* (format t "pushing html for ~a...~%" (the-object section dom-id)))
          (push (list (the-object section dom-id)
                      (the-object section main-view)) replace-list))))
    
    (with-http-response (req ent :content-type "text/xml")
      (with-http-body (req ent)
        (with-html-output(*html-stream* nil)
          (:document
           (dolist (replace-pair replace-list)
             (htm
              (:html-section (:|replaceId| (str (first replace-pair)))
                             (:|newHTML| (str (wrap-cdata (second replace-pair)))))))))))))


(defun wrap-cdata (string)
  (string-append "<![CDATA[" string "]]>"))

