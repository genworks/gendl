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

(defun process-graphics-fields (query query-plist root-object requestor)
  
  (setq query-plist (mapcan #'(lambda(key value)
                                (list (if (or (string-equal key "image.x")
                                              (string-equal key "image.y"))
                                          (make-keyword (string-upcase key)) key) value))
                            (plist-keys query-plist)
                            (plist-values query-plist)))
                                
  (let ((tree-toggle-path
         (let ((item (find ":tree-toggle+" (mapcar #'string-downcase (mapcar #'first query)) :test #'search)))
           (when item 
             (list (query-arg-to-root-path 
                    nil nil
                    (subseq  item
                             (length ":tree-toggle+")
                             (- (length item) 2))) t))))
        
        (view-object (let ((candidate (the-object requestor :view-object)))
                       (when (not (typep candidate 'null-part)) candidate)))
        (restore? (getf query-plist :restore-view))
        (image.x (read-safe-string (getf query-plist :IMAGE.X)))
        (image.y (read-safe-string (getf query-plist :IMAGE.Y))))
    
    (the-object requestor toggle-view-toggle!)
    
    (cond 
     ((second tree-toggle-path)
      (let ((tree-toggle-node (the-object root-object (:follow-root-path (first tree-toggle-path)))))
        (the-object tree-toggle-node
                    (:set-slot! :tree-state (ecase (the-object tree-toggle-node :tree-state)
                                              (:open :closed) (:closed :open))))))
       
       
     (restore? (the-object view-object (restore-slot-defaults! (list :user-center :user-scale))))

     ((and image.x image.y)
        
      (the-object requestor (dig-point :x image.x :y image.y))))
    
    (remove-plist-key (remove-plist-key query-plist :IMAGE.X) :IMAGE.Y)))
