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

(in-package :gwl-user)

(define-object test (gwl::session-control-mixin base-html-sheet)
  
  :computed-slots
  ((expires-at (+ (get-universal-time) 10))
   (recovery-url "/test.gdl")
   (recovery-expires-at (+ (the expires-at) (* 60 2))))) ;; 2 minutes after the recovery instance created

;;
;; FLAG -- move into main package.lisp for GWL.
;;
(eval-when (compile load eval) (export 'session-control-mixin))

(define-view (html-format test) ()
  
  :output-functions
  ((main-sheet
    ()
    (html
     (:html
      (:body
       (when gwl:*developing?* (the (write-development-links)))
       (:p (the write-self-link))
       (with-html-form () ((:input :type :submit)))))))))
    

(publish :path "/test.gdl"
         :function #'(lambda (req ent)
                       (gwl-make-object req ent "gwl-user::test")))
