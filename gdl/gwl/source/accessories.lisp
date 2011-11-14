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

(define-object update (base-html-sheet)

  :input-slots
  (return-to))

(define-lens (html-format update)()
  :output-functions
  ((main-sheet
    ()
    (let ((root (the :root))
          (return-to-root-path (the :return-to :root-path)))
      
      (the return-to (:update!))
      ;; FLAG -- handle condition where this part no longer exists
      ;; after update.
      ;;
      (the-object root (:follow-root-path return-to-root-path) 
                  (:write-html-sheet))))))


(define-object update-full (base-html-sheet)
  :input-slots
  (return-to))

(define-lens (html-format update-full)()
  :output-functions
  ((main-sheet
    ()
    (let ((root (the :root))
          (return-to-root-path (the :return-to :root-path)))
      
      (the-object root (:update!))
      ;; FLAG -- handle condition where this part no longer exists
      ;; after update.
      ;;
      (the-object root (:follow-root-path return-to-root-path) 
                  (:write-html-sheet))))))


(define-object break-on (base-html-sheet)

  :input-slots
  (break-on)

  :functions
  ((write-html-sheet
    nil
    (set-self (the break-on))
    (the break-on write-html-sheet))))





