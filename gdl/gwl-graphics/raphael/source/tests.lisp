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

(in-package :raphael)

#+nil
(define-object test-sheet (base-ajax-sheet)
  
  :computed-slots ((use-raphael? t)
                   (html-sections (list (the viewport))))
  
  :objects
  ((viewport :type 'test-vp
             :respondent self)))


#+nil
(define-lens (html-format test-sheet)()
  :output-functions
  ((main-sheet-body
    ()
    (with-html-output (*stream*)
      (write-the viewport main-div)))))

#+nil
(define-object test-vp (base-ajax-graphics-sheet)

  :computed-slots ((js-to-eval (the raphael-string)))
  
  :objects ((surf :type 'surf::test-b-spline-surface)
            
            (view-object :type 'web-drawing
                         :page-width 500
                         :page-length 500
                         :projection-vector (getf *standard-views* (the projection-direction value))
                         :objects (list (the surf)))
            
            (projection-direction :type 'menu-form-control
                                  :size 1
                                  :default :trimetric
                                  :choice-list (plist-keys *standard-views*)
                                  :onclick (the (gdl-ajax-call :form-controls (list (the-child)))))

            
            (restore-button :type 'button-form-control
                            :label "Restore"
                            :onclick (the (gdl-ajax-call :function-key :restore-zoom!))))
  
  :functions ((restore-zoom!
               ()
               (the view-object (restore-slot-defaults! (list :user-scale :user-center))))))


#+nil
(define-lens (html-format test-vp)()
  :output-functions
  ((main-view
    ()
    (with-html-output (*stream*)
      (write-the raphael-canvas)
      (str (the restore-button form-control-string))
      (str (the projection-direction html-string))
      ))))
           
#+nil
(defun publish-object (object &key (path (format nil "/~a" object)))
  (publish :path path
           :function #'(lambda(req ent)
                         (gwl-make-object req ent (format nil "~s" object)))))

;;(publish-object 'test-sheet)
