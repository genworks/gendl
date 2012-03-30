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

(defparameter *icon-hash* nil)


(define-object tree (sheet-section tree-node-mixin)
  :input-slots
  (("Function of one argument. This function takes a node in the tree as an argument, and should return
  a plist with keys :function and :arguments, which is a function in the bashee which will be called
  with the given arguments when the given node in the tree is clicked."
    onclick-function nil)
   
   (respondent (the parent))
   
   (tree-style  
    (format nil "


table.Tree {border-style: solid; 
cursor: default;
table-layout: fixed;
border-color: ~0@*~a; 
border-width: 0px;}

table.Tree td.LeftBottomBorder {height: ~2@*~apx;
line-height: ~2@*~apx;
border-style: solid; 
border-color: ~0@*~a; 
border-width: 0px;
border-left-width: ~4@*~apx; 
border-bottom-width: ~4@*~apx;}

table.Tree td.LeftBorder {height: ~2@*~apx;
line-height: ~2@*~apx;
border-style: solid; 
border-color: ~0@*~a;
border-width: 0px;
border-left-width: ~4@*~apx;}

table.Tree td.EmptyLeftBorder {height: 2px;
line-height: 2px;
border-style: solid; 
border-color: ~0@*~a;
border-width: 0px;
border-left-width: ~4@*~apx;}

table.Tree td.BottomBorder {height: ~2@*~apx;
line-height: ~2@*~apx;
border-style: solid; 
border-color: ~0@*~a;
border-width: 0px;
border-bottom-width: ~4@*~apx;}


table.Tree td.Button {background-color: ~1@*~a; 
text-align: center; 
vertical-align: middle;
margin: 1px;
border-color: white; 
border-top-width: ~4@*~apx; 
border-bottom-width: ~4@*~apx;
height: ~3@*~apx ;
line-height: ~3@*~apx;}


table.Tree td.Content {
cursor: default;
height: ~3@*~apx;
line-height: ~3@*~apx;
vertical-align: middle;}


table.Tree td.Empty {
height: ~2@*~apx;
line-height: ~2@*~apx ;}



table.Tree td.EmptyRow {
border-width: 0px;
height: 2px;
line-height: 2px ;}


table.Tree td.HeaderCell {border-width: 0px;
width: ~5@*~apx;
line-height: 1px;
height: 1px;}

table.Tree td.HeaderCellSpace {border-width: 0px;
width: ~6@*~apx;
line-height: 1px;
height: 1px;



}

" 
            (the tree-color)(the button-color) (the half-height) (the row-height) 
            (the tree-width)(the cell-width) (half (the cell-width))))
   
   (row-height 20)
   
   (tree-width 1)
   
   ("String " tree-color "red")
   
   ("" button-color "blue")
   
   (cell-width 6)
   
   (half-height (half (the row-height)))
   
   (tree-dom-id (the dom-id)))
  
  
  :computed-slots ((style-view (with-cl-who-string ()
                                 (write-the style-view))))
  
  :trickle-down-slots (respondent))



(define-lens (html-format tree)()
  :output-functions
  ((style-view 
    ()
    (with-cl-who ()
      ((:div :id "tree-css") ((:style :type "text/css" :media "screen") 
                              (str (the tree-style))))))
   
   (inner-html
    ()
    (html-stream *stream*
                 ((:div :id (the tree-dom-id)) (write-the tree))))))
   



(define-object tree-node-mixin (skeleton-ui-element)
  
  :input-slots
  (respondent
   
   (onclick-function nil)
   
   (target-object self)
   
   (%tree-root% self :defaulting)
   
   (depth (if (eql self (the %tree-root%)) 0 (1+ (the parent depth))))
   
   (remaining-depth (if (or (null (the child-nodes)) (the closed?)) 0
                      (+ 1 (apply #'max (mapsend (the child-nodes) :remaining-depth)))))
   
   (total-depth (if (eql self (the %tree-root%)) (the remaining-depth) (the parent total-depth)))
   
   (last-node? (and (not (eql self (the %tree-root%))) (eql self (lastcar (the parent child-nodes))))))

  :trickle-down-slots (%tree-root%)
  
  :computed-slots
  (
   ;;
   ;; FLAG -- to skeleton-ui-mixin and out of base-html-sheet.
   ;;
   (base64-encoded-root-path (base64-encode-safe (format nil "~s" (the root-path))))
   (dom-id (the base64-encoded-root-path))
   (inner-html (with-output-to-string (*html-stream*)
                (with-format (html-format *html-stream*)
                  (write-the inner-html))))
   ;;
   ;; endFLAG
   ;;
   
   (closed? (not (eql self (the %tree-root%))) :settable)
   (open? (not (the closed?)))
   

   (child-nodes (list-elements (the nodes))))

  
  
  :hidden-objects ((nodes :type 'tree-node-mixin
                          :sequence (:size (length (the target-object children)))
                          :pass-down (respondent onclick-function)
                          :target-object (nth (the-child index) (the target-object children))))
  
  :functions
  ((toggle-state! () (the (set-slot! :closed? (not (the closed?)))))))


(define-lens (html-format tree-node-mixin)()
  :output-functions
  (
   ;;
   ;; required by skeleton-ui-mixin
   ;;
   (inner-html () (write-the tree))

   
   (tree
    () (let ((*icon-hash* (make-hash-table)))
         (html-stream *stream*
          ((:table :class "Tree" :border 0 :cellpadding 0 :cellspacing 0) 
           :newline 
        ;; this first row sets up the column widths. table style is defined as fixed-width to enforce these
           (:tr ((:td :class "HeaderCell") "&nbsp;")
                ((:td :class "HeaderCell") "&nbsp;")
                (dotimes (i (twice (the total-depth))) (html ((:td :class "HeaderCell") "&nbsp;")))
                ((:td :class "HeaderCellSpace") "&nbsp;")
                ((:td :style "line-height: 1px; height: 1px; border-width: 0px;")"&nbsp;"))
           (write-the tree-row)))))
   
   (tree-row
    () (html-stream *stream* :newline
             ((:tr :style "height: 10px;")
              (let ((depth (the depth)))
                (dotimes (n depth)
                  (cond 
                   ((and (= n (1- depth)) (the last-node?))
                    (setf (gethash n *icon-hash*) :l)
                    (html ((:td :class "Empty") "&nbsp;") ((:td :class "LeftBottomBorder") "&nbsp;")))
                   ((= n (1- depth))
                    (setf (gethash n *icon-hash*) :t)
                    (html ((:td :class "Empty")"&nbsp;") ((:td :class "LeftBottomBorder") "&nbsp;")))
                   ((member (gethash n *icon-hash*) (list :i :t))
                    (setf (gethash n *icon-hash*) :i)
                    (html ((:td :class "Empty")"&nbsp;") 
                          ((:td :class "LeftBorder") "&nbsp;")))
                   (t (html ((:td :colspan 2 :class "Empty") 
                             :br))))))
           
               
               
              (if (and (typep self 'tree-node-mixin) (the child-nodes))
                  (html
                   :newline
                   ((:td :class "Button" :rowspan 2 :colspan 2 
                         :style "cursor: pointer;" 
                         :onclick (the (gdl-ajax-call :function-key :toggle-state!)))
                    (:b (:princ (if (the closed?) "+" "-")))))

                 
                (html ((:td :colspan 2 :class "BottomBorder") "&nbsp;")))

               
               
              ((:td :class "Empty":rowspan 2 )  "&nbsp;" ) 
              ((:td :rowspan 2 :class "Content" :colspan (+ 1 (twice (- (the total-depth) (the depth)))))
               ((:span :style "cursor: pointer;"
                       
                       :if* (the onclick-function)
                       :onclick (funcall (the onclick-function) (the target-object))
                       
                       ;;:onclick (the (gdl-ajax-call 
                       ;;(:apply (funcall (the onclick-function) (the target-object)))))
                       
                       #+nil
                       (format nil "return gdlAjax('args=~a');" 
                               (the (encode-ajax-args (:apply (funcall (the onclick-function) 
                                                                       (the target-object)))))))
                (html (:princ (the target-object strings-for-display))))))
             (let ((depth (the depth)))
               (html (:tr 
                      (dotimes (n depth)
                        (cond 
                         ((and (= n (1- depth)) (the last-node?))
                          (setf (gethash n *icon-hash*) :l)
                          (html ((:td :class "Empty")"&nbsp;") ((:td :class "Empty")"&nbsp;")))
                         ((= n (1- depth))
                          (setf (gethash n *icon-hash*) :t)
                          (html ((:td :class "Empty")"&nbsp;") ((:td :class "LeftBorder") "&nbsp;")))
                         ((member (gethash n *icon-hash*) (list :i :t))
                          (setf (gethash n *icon-hash*) :i)
                          (html ((:td :class  "Empty")"&nbsp;") 
                                ((:td :class "LeftBorder") "&nbsp;")))
                         (t (html ((:td :colspan 2 :class "Empty") :br)))))
                      (unless (and (typep self 'tree-node-mixin) (the child-nodes))
                        (html ((:td :colspan 2 :class  "Empty") "&nbsp;"))))))
             
             (:tr
              (let ((depth (the depth)))
                (dotimes (n depth)
                  (cond 
                   ((and (= n (1- depth)) (the last-node?))
                    (setf (gethash n *icon-hash*) :l)
                    (html ((:td :class "EmptyRow") "&nbsp;") ((:td :class "EmptyRow") "&nbsp;")))
                   ((= n (1- depth))
                    (setf (gethash n *icon-hash*) :t)
                    (html ((:td :class "EmptyRow")"&nbsp;") ((:td :class "EmptyLeftBorder") "&nbsp;")))
                   ((member (gethash n *icon-hash*) (list :i :t))
                    (setf (gethash n *icon-hash*) :i)
                    (html ((:td :class "EmptyRow")"&nbsp;") 
                          ((:td :class "EmptyLeftBorder") "&nbsp;")))
                   (t (html ((:td :colspan 2 :class "EmptyRow") :br))))))
               
              
              (if (and (typep self 'tree-node-mixin) (the child-nodes))
                  (html
                   :newline
                   ((:td :class "EmptyRow") "&nbsp;") 
                   ((:td :class (if (the closed?) "EmptyRow" "EmptyLeftBorder")) "&nbsp;"))
                (html ((:td :colspan 2 :class "EmptyRow") "&nbsp;"))))

             ;;
             ;; FLAG -- when would self ever not be of type 'tree-node-mixin ??
             ;;
             (when (and (typep self 'tree-node-mixin) (the open?))
               (mapc #'(lambda(node) (write-the-object node (tree-row))) (the child-nodes)))))))


