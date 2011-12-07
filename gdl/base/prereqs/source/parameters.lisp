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

(eval-when (compile load eval)
  (when (not (boundp '*production-build?*)) (defvar *production-build?* nil)))

(defparameter *compiling-changes* nil)

(defparameter *load-source-code-database?* (not *production-build?*)
  "Boolean. Determines whether pre-compiled source code 
information will be loaded from compiled files. Defaults to T.")

(defparameter *curve-chords* 50 "Integer. The number of chords to use per Bezier curve when rendering curves as a sequence of straight chords (e.g. in VRML).")

(defparameter *load-documentation-database?* t ;;(not *production-build?*)
  "Boolean. Determines whether pre-compiled documentation strings
information will be loaded from compiled files. Defaults to T.")

(defparameter *compile-source-code-database?* (not *production-build?*)
  "Boolean. Determines whether source code information
information will be compiled into compiled files. Defaults to T.")

(defparameter *compile-documentation-database?* t ;;(not *production-build?*)
  "Boolean. Determines whether documentation strings 
information will be compiled into compiled files. Defaults to T.")

(defparameter *retain-object-identity-on-type-change?* t)

(defparameter *dummy-version* (list 0))

(defparameter *notify-cons* nil)

(defparameter *root-checking-enabled?* t
  "Boolean. Determines whether dependency-tracking carries over between objects which
do not share a common root. Default is T which means dependency-tracking does <b>not</b>
carry over (the checking prevents it).")


(defparameter *till-now* nil)

(defvar *compile-circular-reference-detection?* nil
  "Boolean. This is a compile-time switch. Determines whether GDL detects circular references
in messages.  Defaults to NIL.")

(defvar *run-with-circular-reference-detection?* nil
  "Boolean. This is a runtime switch. Determines whether GDL detects circular references
in messages.  Defaults to NIL.")

(defvar *compile-dependency-tracking?* t
  "Boolean. This is a compile-time switch. Determines whether GDL keeps track of object and message 
dependencies at runtime, thereby enabling the modification of messages and subsequent proper 
demand-driven recomputation of other messages in the object hierarchy. This switch must
be set at the beginning of a GDL session before comiling all code; switching it in the middle 
of a session (especially from NIL to T) will have unpredictable effects and very likely will 
result in incorrect operation. Defaults to T.")

(defvar *run-with-dependency-tracking?* t
  "Boolean. This is a runtime switch. Determines whether GDL keeps track of object and message 
dependencies at runtime, thereby enabling the modification of messages and subsequent proper 
demand-driven recomputation of other messages in the object hierarchy. This switch must
be set at the beginning of a GDL session; switching it in the middle of a session (especially
from NIL to T) will have unpredictable effects and very likely will result in incorrect
operation. Defaults to T.")


(defvar *undeclared-parameters-enabled?* nil ;;t
  "Boolean. This is a compile-time switch. Determines whether GDL will handle inputs passed
to child parts through :parameters plists, where the input is not declared in any other 
part as either an input-slot or computed-slot. If you leave this as NIL when compiling
your application you may see an improvement in runtime performance of up to 10% as 
compared to applications compiled with it set to T. Defaults to NIL.")


(defparameter *debug?* nil)

(defparameter *debug-object* nil)

(defparameter *report-gdl-redefinitions?* t)

(defparameter *allowed-part-documentation-keywords* 
    (list :author :purpose :description :examples :example))

(defparameter *message-categories*
    (list :required-input-slots :optional-input-slots :settable-optional-input-slots
          :defaulted-input-slots :settable-defaulted-input-slots :computed-slots :settable-computed-slots 
          :uncached-computed-slots
          :objects :quantified-objects :hidden-objects :quantified-hidden-objects :functions
          :query-slots))

(defparameter *stream* nil)

(defparameter *colors-default* (list :foreground :black :background :white)
  "Plist. Should contain keys of at least <tt>:foreground</tt> and <tt>:background</tt>, whose
values are a color indicator for the default foreground and background of graphics viewports. The
 default is <tt>:black</tt> for foreground, and <tt>:white</tt> for background.")


(defparameter *%format%* nil)

(defvar args-arg (gensym))
(defvar parent-arg (gensym))
(defvar part-arg (gensym))
(defvar val-arg (gensym))
(defvar self-arg (gensym))


(defparameter *error-on-not-handled?* t)

(glisp:define-constant +reserved-words+
    (mapcar #'(lambda(symbol)
                (glisp:intern symbol :gdl-acc))
            '(aggregate 
              children 
              ;;documentation 
              first 
              first? 
              follow-root-path 
              hidden-children 
              index 
              last 
              last? 
              leaf? 
              message-list 
              mixins 
              next 
              parent
              parent-tree 
              previous 
              restore-slot-default! 
              root 
              root-path 
              root? 
              set-slot! 
              set-slots! 
              slot-documentation 
              type 
              update!
              color-decimal
              edge-center
              face-center
              face-normal-vector
              local-center
              obliqueness
              vertex)))

(defparameter *error-on-reserved-words?* t)


(defparameter *compile-for-dgdl?* nil "Boolean. Determines whether
  global methods are defined during compilation to allow calling any
  message on a gdl remote-object. This functionality is not available
  in the core GDL kernel and depends on additional web/networking
  modules. Defaults to nil.")

(defparameter *gs-path* nil)
