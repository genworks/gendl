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



(in-package :gdl)


(defclass gdl-class (standard-class)
  ((messages :accessor messages :initform nil)
   (gdl-documentation :accessor gdl-documentation :initform nil)
   (required-input-slots :accessor required-input-slots :initform nil)
   (optional-input-slots :accessor optional-input-slots :initform nil)
   (defaulted-input-slots :accessor defaulted-input-slots :initform nil)
   (computed-slots :accessor computed-slots :initform nil)
   (query-slots :accessor query-slots :initform nil)
   (settable-computed-slots :accessor settable-computed-slots :initform nil)
   (uncached-computed-slots :accessor uncached-computed-slots :initform nil)
   (settable-optional-input-slots :accessor settable-optional-input-slots :initform nil)
   (settable-defaulted-input-slots :accessor settable-defaulted-input-slots :initform nil)
   (functions :accessor functions :initform nil)
   (methods :accessor methods :initform nil)
   (cached-functions :accessor cached-functions :initform nil)
   (cached-methods :accessor cached-methods :initform nil)
   (objects :accessor objects :initform nil)
   (quantified-objects :accessor quantified-objects :initform nil)
   (hidden-objects :accessor hidden-objects :initform nil)
   (quantified-hidden-objects :accessor quantified-hidden-objects :initform nil)
   (trickle-down-slots :accessor trickle-down-slots :initform nil)
   (settable-slots :accessor settable-slots :initform nil)
   (message-documentation :accessor message-documentation :initform nil)
   
   (message-source :accessor message-source :initform nil)
   (message-source-copy :accessor message-source-copy :initform nil)
   (message-source-changed :accessor message-source-changed :initform nil)
   
   (format-functions :accessor format-functions :initform nil)
   
   (child-inputs :accessor child-inputs :initform nil)
   
   ))


(defclass gdl-format-class (standard-class)
  ((gdl-documentation :accessor gdl-documentation :initform nil)
   (format-functions :accessor format-functions :initform nil)
   (output-functions :accessor output-functions :initform (make-hash-table))
   (view-documentation :accessor view-documentation :initform (make-hash-table))))


(defclass gdl-skin-class (standard-class) ())

(defmethod glisp:validate-superclass  ((class gdl-class) (superclass standard-class)) t)
(defmethod glisp:validate-superclass  ((class gdl-format-class) (superclass standard-class)) t)
(defmethod glisp:validate-superclass  ((class gdl-skin-class) (superclass standard-class)) t)
