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

(in-package :gwl)

(defparameter *icon-hash* nil)

(define-object node-mixin (layout-mixin)

  :documentation
  (:description "Generates a default GWL user interface with a model-inputs area,
user-navigable tree with child applications, graphics view with controls, and rule display. 

Child objects should be of type <tt>node-mixin</tt> or <tt>application-mixin</tt>. Child hidden-objects
may be of any type.

The <tt>ui-display-list-objects</tt> is appended up automatically from those of the children.")

  :input-slots
  (("Integer. Determines how many descendant levels to show in the tree initially. Default is 1."
    default-tree-depth 1)
   ("GDL object list. Appends additional objects to the automatically-appended <tt>ui-display-list-objects</tt> 
from the children."
    node-ui-display-list-objects nil))

  :computed-slots
  ((child-nodes (remove-if-not #'(lambda (kid) (typep kid 'layout-mixin))
                               (the :children)))
   ("List of GDL object roots. The leaves of these objects will be 
displayed in the graphics. Defaults to the appended result of children's 
<tt>ui-display-list-objects</tt>."
    ui-display-list-objects (apply #'append (the :node-ui-display-list-objects)
                                   (mapcar #'(lambda (kid)
                                               (when (<= (the-object kid depth) *max-node-depth*)
                                                 (ensure-list (the-object kid :ui-display-list-objects))))
                                           (the :child-nodes))))
   (node-ui-display-list-leaves nil)
   
   
   (ui-display-list-leaves (apply #'append (the :node-ui-display-list-leaves)
                                   (mapcar #'(lambda (kid)
                                               (when (<= (the-object kid depth) *max-node-depth*)
                                                 (ensure-list (the-object kid :ui-display-list-leaves))))
                                           (the :child-nodes))))
   (tree-state (if (< (the depth) (the :default-tree-depth))
                   :open
                 :closed) :settable))

  :trickle-down-slots (tree-root))

(define-lens (html-format node-mixin) ()

  :output-functions
  ((tree
    (&key (current-node self))
    (if (eql self (the :tree-root))
        (let ((*icon-hash* (make-hash-table)))
          (html ((:table :border 0 :cellpadding 0 :cellspacing 0) 
                 :newline
                 (write-the (:tree-row :current-node current-node)))))
      (write-the :tree-root (:tree :current-node current-node))))))
                

(define-object application-mixin (layout-mixin)

  :documentation
  (:description "This mixin generates a default GWL user interface, similar to <tt>node-mixin</tt>, but you should use
<tt>application-mixin</tt> if this is a leaf-level application (i.e. has no children of type <tt>node-mixin</tt>
or <tt>application-mixin</tt>"))


(define-skin infinite ())


(define-lens (html-format layout-mixin)()

  :skin infinite

  
  :output-functions
  ((main-sheet
    ()
    (with-html-output (*html-stream* nil :prologue t :indent t)
      (:html (:head (:title (str (the :page-title)))
                    
                    (:link :rel "stylesheet" :href "/static/css/main.css" :type "text/css" :media "screen")
                    "<!--[if IE]><link rel=\"stylesheet\" href=\"/static/css/ie.css\" type=\"text/css\" media=\"screen\" /><![endif]-->"
                    (:script :type "text/javascript" :src "/static/js/niftycube.js"))

             ((:body :class "demo")

              
              
              (when *developing?* (write-the (:development-links)))
              
              
              ((:div :class "content")
              
               (:center
                (with-html-form (:multipart? (the multipart-form?))
                  

                  ((:div :class "column_left")
                   (:h3 (str (the :inputs-title)))
                   (write-the (:model-inputs)))
                  
                  
                  ((:div :class "column_center")
                   (:h2 (str (the strings-for-display)))
                   (write-the viewport))
                  
                  
                  ((:div :class "footer")
                   (the (:write-standard-footer))))))))))
   
   

   (viewport
    ()
    (ecase (the image-format) 
      (:links (the (write-geometry-links)))
      ((:png :jpeg) (the write-geometry)) 
      (:vrml (the write-embedded-vrml-world))))
   
   

   (model-inputs ())
   
   (tree
    ()
    (let ((tree-root (the :tree-root))) 
      (when (and tree-root (typep tree-root 'node-mixin)) 
        (write-the :tree-root (:tree :current-node self)))))

   (tree-row
    (&key current-node)
    (with-html-output (*html-stream* nil :indent t)
      (htm :newline
           (:tr (let ((depth (the :depth)))
                  (dotimes (n depth)
                    (cond ((and (= n (1- depth)) (the :last-node?)) 
                           (setf (gethash n *icon-hash*) :l)
                           (htm ((:td :width 20 ) ((:img :src "/images/gwl/l-t.gif")))))
                          ((= n (1- depth)) 
                           (setf (gethash n *icon-hash*) :t)
                           (htm ((:td :width 20 ) ((:img :src "/images/gwl/t-t.gif")))))
                          ((member (gethash n *icon-hash*) (list :i :t))
                           (setf (gethash n *icon-hash*) :i)
                           (htm ((:td :width 20 ) ((:img :src "/images/gwl/i-t.gif")))))
                          (t (htm ((:td :width 20 ) :br))))))
                (if (and (typep self 'node-mixin) (the :child-nodes))
                    (htm ((:td :width 9 ) 
                          ((:input :type :image
                                   :src (ecase (the :tree-state)
                                          (:open "/images/gwl/minus.gif")
                                          (:closed "/images/gwl/plus.gif"))
                                   :name (format nil ":tree-toggle+~a" 
                                                 (the :url-encoded-root-path))))))
                  (htm ((:td :width 5 :align :center)
                        ((:img :src "/images/gwl/red-dot.gif" 
                               :width 8 )))))
                ((:td :nowrap :nowrap 
                      :colspan  (1+ (- (the :total-depth) (the :depth))))
                 (if (eql self current-node)
                     (htm (:b (str (the :strings-for-display))))
                   (the (:write-self-link)))))
           (when (and (typep self 'node-mixin) (eql (the :tree-state) :open))
             (mapc #'(lambda(node)
                       (write-the-object node (:tree-row :current-node current-node)))
                   (the :child-nodes))))))

   (violated-rules
    ()
    (when (the :violated-rules)
      (with-html-output (*html-stream* nil :indent t)
        (:table 
         (dolist (rule (the :violated-rules))
           (htm (:tr (:td (the-object 
                           rule 
                           (:write-self-link 
                            :target "Rule-Display"
                            :display-color :red
                            :display-string (format nil "~a~a~a"
                                                    (the-object rule :rule-title)
                                                    (if (the-object rule :rule-result) ": " "")
                                                    (or (the-object rule :rule-result) ""))))))))))))
   (other-rules
    ()
    (when (the :other-rules)
      (with-html-output (*html-stream* nil :indent t)
        (:table 
         (dolist 
             (rule 
                 (the :other-rules))
           (htm (:tr (:td (the-object 
                           rule 
                           (:write-self-link 
                            :target "Rule-Display"
                            :display-color :blue
                            :display-string (format nil "~a~a~a" (the-object rule :rule-title)
                                                    (if (the-object rule :rule-result) ": " "")
                                                    (or (the-object rule :rule-result) ""))))))))))))))


(define-lens (html-format layout-mixin)()
   
  :output-functions
  (
   (viewport
    ()
    (ecase (the image-format) 
      (:links (the (write-geometry-links)))
      ((:png :jpeg) (the write-geometry)) 
      (:vrml (the write-embedded-vrml-world))))
   
   (main-sheet
    ()
    (if (getf (the view-object query-plist) :resize-pdf)
        (the view-object (write-html-sheet))
      (html
       (format *html-stream* "<!doctype HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">~%")      
       (:html (:head (:title (:princ (the :page-title))))
              ((:body :if* (the :body-bgcolor) :bgcolor (lookup-color (the :body-bgcolor) :format :hex))
               (when (the show-title?) (html (:center (:h2 (:princ (the :page-title))))))
               (when *developing?* 
                 (the (:write-development-links)))
               (:center
                (with-html-form (:multipart? (the multipart-form?))
                  (:p (:table 
                       (:tr ((:td :valign :top :if* (the :inputs-bgcolor) 
                                  :bgcolor (lookup-color (the :inputs-bgcolor) :format :hex)
                                  :rowspan (if (the display-tree?) 1 2)) 
                             (:center (:h3 (:princ (the :inputs-title)))) (write-the (:model-inputs)))
                            
                            ((:td :rowspan 2 :valign :top) (write-the viewport))
                            
                            (when (the display-rules?)
                              (html
                               ((:td :valign :top :if* (the :violated-rules-bgcolor) 
                                     :bgcolor (lookup-color (the :violated-rules-bgcolor) :format :hex))
                                (:center (:h3 (:princ (the :violated-rules-title))))
                                (write-the (:violated-rules))))))
                       (:tr (when (the display-tree?)
                              (html ((:td :valign :top :if* (the :tree-bgcolor) 
                                          :bgcolor (lookup-color (the :tree-bgcolor) :format :hex))
                                     (:center (:h3 (:princ (the :tree-title)))) (write-the (:tree)))))
                            (when (the display-rules?)
                              (html ((:td :align :left :valign :top :if* (the :other-rules-bgcolor) 
                                          :bgcolor (lookup-color (the :other-rules-bgcolor) :format :hex))
                                     (:center (:h3 (:princ (the :other-rules-title)))) 
                                     (write-the (:other-rules))))))))))
               :hr (the (:write-standard-footer)))))))

   (model-inputs ())
   
   (tree
    ()
    (let ((tree-root (the :tree-root))) 
      (when (and tree-root (typep tree-root 'node-mixin)) 
        (write-the :tree-root (:tree :current-node self)))))

   ;;
   ;; FLAG -- try to replace this tree with the generic one in
   ;; tree.lisp and remove duplicate *icon-hash* defparameter.
   ;;
   (tree-row
    (&key current-node)
    (html :newline
          (:tr (let ((depth (the :depth)))
                 (dotimes (n depth)
                   (cond ((and (= n (1- depth)) (the :last-node?)) 
                          (setf (gethash n *icon-hash*) :l)
                          (html ((:td :width 20 ) ((:img :src "/images/gwl/l-t.gif")))))
                         ((= n (1- depth)) 
                          (setf (gethash n *icon-hash*) :t)
                          (html ((:td :width 20 ) ((:img :src "/images/gwl/t-t.gif")))))
                         ((member (gethash n *icon-hash*) (list :i :t))
                          (setf (gethash n *icon-hash*) :i)
                          (html ((:td :width 20 ) ((:img :src "/images/gwl/i-t.gif")))))
                         (t (html ((:td :width 20 ) :br))))))
               (if (and (typep self 'node-mixin) (the :child-nodes))
                   (html ((:td :width 9 ) 
                          ((:input :type :image
                                   :src (ecase (the :tree-state)
                                          (:open "/images/gwl/minus.gif")
                                          (:closed "/images/gwl/plus.gif"))
                                   :name (format nil ":tree-toggle+~a" 
                                                 (the :url-encoded-root-path))))))
                 (html ((:td :width 5 :align :center)
                        ((:img :src "/images/gwl/red-dot.gif" 
                          :width 8 )))))
               ((:td :nowrap :nowrap 
                     :colspan  (1+ (- (the :total-depth) (the :depth))))
                (if (eql self current-node)
                    (html (:b (:princ (the :strings-for-display))))
                  (the (:write-self-link)))))
          (when (and (typep self 'node-mixin) (eql (the :tree-state) :open))
            (mapc #'(lambda(node)
                      (write-the-object node (:tree-row :current-node current-node)))
                  (the :child-nodes)))))

   (violated-rules
    ()
    (when (the :violated-rules)
      (html 
       (:table 
        (dolist (rule (the :violated-rules))
          (html (:tr (:td (the-object 
                           rule 
                           (:write-self-link 
                            :target "Rule-Display"
                            :display-color :red
                            :display-string (format nil "~a~a~a"
                                                    (the-object rule :rule-title)
                                                    (if (the-object rule :rule-result) ": " "")
                                                    (or (the-object rule :rule-result) ""))))))))))))
   (other-rules
    ()
    (when (the :other-rules)
      (html 
       (:table 
        (dolist 
            (rule 
                (the :other-rules))
          (html (:tr (:td (the-object 
                           rule 
                           (:write-self-link 
                            :target "Rule-Display"
                            :display-color :blue
                            :display-string (format nil "~a~a~a" (the-object rule :rule-title)
                                                    (if (the-object rule :rule-result) ": " "")
                                                    (or (the-object rule :rule-result) ""))))))))))))))


(define-object gwl-rule-object (base-html-graphics-sheet base-rule-object)

  :documentation
  (:description "Used to display a rule as a GWL web page. 
Mixes together <tt>base-html-sheet</tt> and <tt>base-rule-object</tt>."))




(define-lens (html-format gwl-rule-object)()
  
  :output-functions
  ((main-sheet 
    ()
    (html (:html (:head (:title (:princ (the :rule-title))))
                 (:body 
                  (when gwl:*developing?* (write-the (development-links)))
                  (:center (:h2 (:princ (the :rule-title))))
                  ((:table :border 0)
                   (:tr ((:td :valign :top) (write-the (:main-area)))
                        (:td (write-the (:right-user-area))))
                   (:tr (:td (write-the (:bottom-user-area)))))))))
   (main-area
    ()
    (html (:table (:tr ((:td :colspan 2) (the (write-self-link :display-string "Redemand Sheet"))))
                  (:tr ((:td :bgcolor :yellow)
                        "Title") (:td (:princ (the :rule-title))))
                  (:tr ((:td :bgcolor :yellow)
                        "Description") (:td (:princ (the :rule-description))))
                  (:tr ((:td :bgcolor :yellow)
                        "Result") (:td (:princ (the :rule-result))))
                  (:tr ((:td :bgcolor :yellow)
                        "Violated?") (:td (if (the :violated?)
                                              (html (:b ((:font :color :red) "Yes")))
                                            (html (:b ((:font :color :green) "No")))))))))
   

   
   (right-user-area
    ())
   
   (bottom-user-area
    ())))
    


                   

