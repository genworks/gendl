;;
;; Copyright 2013 Genworks International
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

(in-package :ta2)


(defun reset-self (self)
  (if (and (typep self 'base-html-sheet)
           (not (typep self 'assembly))
           (not (the parent))
           (typep (the $$ta2) 'assembly))
      (the $$ta2)
    self))


#+nil
(defun reset-self (self)
  self)
      

(defun ta2operate (req ent)

  (let* ((query (request-query req))
         (iid (rest (assoc "iid" query :test #'string-equal)))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (self (reset-self self))
         (plist (gwl::assoc-list-to-plist query))
         (object (the root-object (follow-root-path 
                                   (read-safe-string 
                                    (base64-decode-safe (getf plist :|rootPath|)))))))
    
    (the (perform-action! object))
    
    (respond-with-new-html-sections req ent self)))


(publish :path "/ta2operate" :function 'ta2operate)                                


(defun ta2treetoggle (req ent)

  (let ((query (request-query req)))
    (let* (
           (iid (rest (assoc "iid" query :test #'string-equal)))
           (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
           (self (reset-self self))
           (plist (gwl::assoc-list-to-plist query)))
      (let ((root-path (getf plist :|rootPath|)))
        (setq root-path (base64-decode-safe root-path))
        
        (when *debug?* (print-variables self root-path))
        
        (let ((object  (the (follow-root-path (read-safe-string root-path)))))
          (the-object object toggle-state!)
          (respond-with-new-html-sections req ent self))))))

              
(publish :path "/ta2treetoggle" :function 'ta2treetoggle)                                  




(defun ta2evaluate (req ent)
    
  (let* ((query (request-query req))
         (iid (rest (assoc "iid" query :test #'string-equal)))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (self (reset-self self))
         (plist (gwl::assoc-list-to-plist query))
         (message (make-keyword (base64-decode-safe (getf plist :|message|)))))

    (with-http-response (req ent :content-type "text/xml")
      (with-http-body (req ent)
        (with-html-output(*html-stream* nil )
          (:document (:html-section (:|newHTML| (str (wrap-cdata (with-output-to-string(*html-stream*)
                                                                   (with-format (html-format *html-stream*)
                                                                     (write-the inspector (message-value (make-keyword message))))))))
                                    (:|replaceId| (str (format nil "inspector_~a" message))))))))))


(publish :path "/ta2evaluate" :function 'ta2evaluate)

(defun ta2setcontrol (req ent)
  
    
  (let* ((query (request-query req))
         (iid (rest (assoc "iid" query :test #'string-equal)))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (self (reset-self self))
         (plist (gwl::assoc-list-to-plist query))
         (slot (make-keyword (getf plist :|control|)))
         (value (getf plist :|value|)))
    

    (if (eql (read-safe-string value) 'gdl::restore-default)
        (the click-mode (restore-slot-default! slot))
      (the click-mode (set-slot-if-needed! slot value)))
    
    (respond-with-new-html-sections req ent self)))

(publish :path "/ta2setcontrol" :function 'ta2setcontrol)



(defun gdlsetslots (req ent)
    
  (let* ((query (let ((query (request-query req))) query))
         (iid (rest (assoc "iid" query :test #'string-equal)))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (self (reset-self self))
         (plist (gwl::assoc-list-to-plist query))
         (root-path (base64-decode-list (getf plist :|rootPath|)))
         (object (the root-object (follow-root-path root-path)))
         (plist (remove-plist-key (remove-plist-key plist :|iid|) :|rootPath|)))

    
    (let ((keys (read-safe-string (getf plist :|keys|)))
          (values (base64-decode-list (getf plist :|values|))))
      
      (mapc #'(lambda(key value)
                (the-object object (set-slot-if-needed! key value :infer-types? nil))) keys values))

    
    ;;(funcall (the tatu-update-function))
    
    ;;(the viewport (toggle-view-toggle!))
    
    
    (respond-with-new-html-sections req ent self)))


(publish :path "/gdlsetslots" :function 'gdlsetslots)


(defun gdlsetcontrol (req ent)
    
  (let* ((query (request-query req))
         (iid (rest (assoc "iid" query :test #'string-equal)))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (self (reset-self self))
         (plist (gwl::assoc-list-to-plist query))
         (root-path (read-safe-string (getf plist :|rootPath|)))
         (object (the (follow-root-path root-path)))
         (slot (read-safe-string (getf plist :|control|)))
         (value (getf plist :|value|)))
    
    
    (if (eql (read-safe-string value) 'gdl::restore-default)
        (the-object object (restore-slot-defaults! (ensure-list slot)))
      (the-object object (set-slot-if-needed! slot value)))

    (when (typep object 'web-drawing) (setq object (the-object object parent)))
    
    (when (typep object 'base-html-graphics-sheet) (the-object object toggle-view-toggle!))
    
    
    (respond-with-new-html-sections req ent self)))


(publish :path "/gdlsetcontrol" :function 'gdlsetcontrol)



(defun ta2setexpandmode (req ent)
    
  (let* ((query (request-query req))
         (iid (rest (assoc "iid" query :test #'string-equal)))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (self (reset-self self))
         (plist (gwl::assoc-list-to-plist query))
         (mode (make-keyword (getf plist :|mode|))))

    (the tree (set-slot! :expand-mode mode))

    (respond-with-new-html-sections req ent self)))


(publish :path "/ta2setexpandmode" :function 'ta2setexpandmode)



(defun ta2clickbutton (req ent)
  
  (let* ((query (let ((query (request-query req))) query))
         (iid (rest (assoc "iid" query :test #'string-equal)))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (self (reset-self self))
         (plist (gwl::assoc-list-to-plist query))
         (mode (getf plist :|mode|)))

    
    (setq self (the click-mode))
    (setq mode (make-keyword mode))
    
    
    (case mode
      
      (:break
       #+nil
       (let ((no-trap? (member :notrap net.aserve::*debug-current*)))
         (unless no-trap? 
           (net.aserve::debug-on :notrap)
           (format t "~&Disabling AllegroServe error trapping (i.e. enabling debugging) 
  -- you can re-enable it with (net.aserve::debug-off :notrap)~%")
           (net.aserve::debug-on :notrap)))
       
       
       (the (set-slot! :click-mode mode)))

      (:clear! (funcall (the clear-viewport-function)))
              
      (:update! (let ((standalone? (not (eql (the tatu-root root) (the tatu-root root-object root)))))
                  (if standalone?
                      (progn
                        (funcall (the object-update-function))
                        (funcall (the tatu-update-function))
                        (funcall (the toggle-view-toggle-function)))
                    (let ((root (the root))
                          (tatu-root-path (the tatu-root root-path)))
                      (funcall (the object-update-function))
                      (the-object root (follow-root-path tatu-root-path) viewport toggle-view-toggle!)))))
                
      (:update-full! (let ((standalone? (not (eql (the tatu-root root) (the tatu-root root-object root)))))
                       (if standalone?
                           (progn
                             (funcall (the object-update-full-function))
                             (funcall (the tatu-update-function))
                             (funcall (the toggle-view-toggle-function)))
                         (let ((root (the root))
                               (tatu-root-path (the tatu-root root-path)))
                           (funcall (the object-update-full-function))
                           (the-object root (follow-root-path tatu-root-path) viewport toggle-view-toggle!)))))
      
      (:up-root! (funcall (the up-root-function)))

      (:reset-root! (funcall (the reset-root-function)))
      
      
      (:undo (funcall (the pop-operation-function)))
      
      (:debug (funcall (the debug-function)))
      
      (otherwise (the (set-slot! :click-mode mode))))
    
    
    
    (respond-with-new-html-sections req ent (the parent))))


(publish :path "/ta2clickbutton" :function 'ta2clickbutton)



(defun digitize-point (req ent)
    
  (let* ((query (request-query req))
         (iid (rest (assoc "iid" query :test #'string-equal)))
         (self (first (gethash (make-keyword iid) gwl:*instance-hash-table*)))
         (self (reset-self self))
         (plist (gwl::assoc-list-to-plist query))
         (self (the (follow-root-path (read-safe-string (getf plist :|rootPath|))))))
    
    ;;
    ;; FLAG -- this is internal stuff, should be handled by a simple function on the self object.
    ;;

    (let ((local-point (make-point (- (parse-integer (getf plist :|x|)) 
				      (half (the view-object width)))
				   (let ((y (- (the view-object length) (parse-integer (getf plist :|y|)))))
                                                            (- y (half (the view-object length)))))))
      (let ((digitized-point 
             (add-vectors (the view-object user-center)
                          (scalar*vector (/ (the view-object user-scale))
                                         local-point))))
        (ecase (the digitation-mode)
          
          (:measure-distance 
           (let ((new-point (the view-object main-view (model-point (make-point (get-x local-point)
                                                                                (get-y local-point)
                                                                                0))))
                 (old-point (the digitized-point)))

             (if (null old-point)
                 (progn
                   (the (set-slot! :digitized-point new-point))
                   (the (restore-slot-default! :digitized-distance)))
               (progn
                 (the (set-slot! :digitized-distance (list :total (3d-distance old-point new-point)
                                                           :x (- (get-x new-point) (get-x old-point))
                                                           :y (- (get-y new-point) (get-y old-point))
                                                           :z (- (get-z new-point) (get-z old-point)))))
                 (the (restore-slot-default! :digitized-point))))))
          
          
          (:report-point 
           (the (report-point (get-x local-point) (get-y local-point))))
          
          (:zoom-and-center
           (the view-object (set-slot! :user-center (make-point (get-x digitized-point)
                                                                (get-y digitized-point))))
           (the view-object (set-slot! :user-scale 
                                       (* (the view-object user-scale) (the zoom-factor))))))))
    
    
    (respond-with-new-html-sections req ent (the root))))


(publish :path "/digitizePoint" :function 'digitize-point)                                 



(defun wrap-cdata (string)
  (string-append "<![CDATA[" string "]]>"))

(defun respond-with-new-html-sections (req ent self)
  (let (replace-list)
    (dolist (section (the html-sections) (reverse replace-list))
      (let ((status (the-object section (slot-status :inner-html))))
        (when (eql status :unbound)
          (when *debug?* (format t "pushing html for ~a...~%" (the-object section dom-id)))
          (push (list (the-object section dom-id)
                      (the-object section inner-html)) replace-list))))
    
    
    
    (with-http-response (req ent :content-type "text/xml")
      (with-http-body (req ent)
        (with-html-output(*html-stream* nil)
          (:document
           (dolist (replace-pair replace-list)
             (htm
              (:html-section (:|replaceId| (str (first replace-pair)))
                             (:|newHTML| (str (wrap-cdata (second replace-pair)))))))))))))


