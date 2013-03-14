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

(in-package :gdl)

(defun instantiate-part-on-demand (self self-type attr-sym)
  (let ((current-object (previous-value self attr-sym)))
    (if current-object
        (prog1 (change-class current-object self-type) 
          (unless (eql attr-sym 'gdl-acc::root-object-object)
            (the-object current-object (update! :replace-bashed-values? nil))))
      (make-object-internal self-type 
                            :%name% (list attr-sym nil t) 
                            :%parent% (list self nil t) 
                            :%root% (gdl-acc::%root% self)))))



(defun chase-up-trickle-down (slot-symbol self args)
  (let ((parent (the-object self parent)))
    (if (null parent) (not-handled self (make-keyword slot-symbol))
      (let ((result
             (let (*error-on-not-handled?*)
               (funcall (symbol-function (glisp:intern slot-symbol :gdl-inputs)) parent (the %name%) self))))
        (if (eql result 'gdl-rule:%not-handled%)
            (let ((result
                   (let (*error-on-not-handled?*)
                     (cond ((member (make-keyword slot-symbol) (the-object parent %trickle-down-slots%))
                            (apply (symbol-function slot-symbol) parent args))
                           (t (apply (symbol-function (glisp:intern slot-symbol :gdl-trickle-downs)) parent args))))))
              (if (eql result 'gdl-rule:%not-handled%) (not-handled self (make-keyword slot-symbol)) result))
          result)))))


(defun trickle-down-basis (self slot args)
  (let (*error-on-not-handled?* (parent (the parent)))
    (cond ((null parent) (not-handled self (make-keyword slot)))
          ((member (make-keyword slot) (the-object parent %trickle-down-slots%))
           (apply (symbol-function (glisp:intern slot :gdl-slots)) parent args))
          (t (apply (symbol-function (glisp:intern slot :gdl-trickle-downs)) parent args)))))
