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

(define-object layout-mixin (base-html-graphics-sheet)

  :documentation
  (:description "This is mixed into both <tt>node-mixin</tt> and <tt>application-mixin</tt>. It contains the common
messages for nodes in a GWL application tree. For any <tt>node-mixin</tt> or <tt>application-mixin</tt>, you may override the default (empty)
<tt>model-inputs</tt> output-function of the corresponding html-format view to make specific model-inputs for that node.")

  :input-slots
  (("String. The title to display on the page and in the tree. Defaults to <tt>(the strings-for-display)</tt>."
    page-title (the :strings-for-display))
   
   ("Boolean. Indicates whether to display the title at the top of the page. Defaults to T."
    show-title? t)
   
   ("String. Title for the model-inputs section. Defaults to \"Model Inputs\"."
    inputs-title "Model Inputs")
   ("String. Title for the Tree section. Defaults to \"Assembly Tree\" if the tree-root is only a 
subclass of <tt>application-mixin</tt>, and \"Assembly Tree\" if the tree-root is an actual node with 
child applications."
    tree-title (if (typep (the :tree-root) 'node-mixin)
                   "Assembly Tree"
                 "Standalone App"))
   ("String. Title for the violated-rules section. Defaults to \"Violated Rules\"."
    violated-rules-title "Violated Rules")
   ("String. Title for the other-rules section. Defaults to \"Other Rules\"."
    other-rules-title "Other Rules")
   ("Keyword symbol. Color keyword from <tt>*color-table*</tt> for the body background. Defaults to <tt>:blue-sky</tt>."
    body-bgcolor :blue-sky)
   ("Keyword symbol. Color keyword from <tt>*color-table*</tt> for the model-inputs area background. Defaults to <tt>:aquamarine</tt>."
    inputs-bgcolor :aquamarine)
   ("Keyword symbol. Color keyword from <tt>*color-table*</tt> for the violated-rules area background. Defaults to <tt>:aquamarine</tt>."
    violated-rules-bgcolor :aquamarine)
   ("Keyword symbol. Color keyword from <tt>*color-table*</tt> for the other-rules area  background. Defaults to <tt>:aquamarine</tt>."
    other-rules-bgcolor :aquamarine)
   ("Keyword symbol. Color keyword from <tt>*color-table*</tt> for the tree area background. Defaults to <tt>:aquamarine</tt>."
    tree-bgcolor :aquamarine)
   ("Integer. Height (top to bottom on screen) in pixels of the graphics area. Defaults to 500."
    graphics-height 500 :defaulting)
   ("Integer. Height (left to right on screen) in pixels of the graphics area. Defaults to 500."
    graphics-width 500 :defaulting)
   ("List of GDL objects. This should be overridden with a list of objects of your choice. The leaves of these objects will 
be scaled to fit and displayed in the graphics area. Defaults to NIL."
    ui-display-list-objects nil)
   
   ("List of GDL objects. This should be overridden with a list of objects of your choice. These objects (not their leaves,
but these actual nodes) will be scaled to fit and displayed in the graphics area. Defaults to NIL."
    ui-display-list-leaves nil)
   
   (depth (if (eql self (the :tree-root)) 0 (1+ (the :parent :depth))))
   (remaining-depth (if (or (null (the :child-nodes))
                            (eql (the :tree-state) :closed))
                        0
                      (1+ (apply #'max
                                 (mapsend (the :child-nodes) :remaining-depth)))))
   (total-depth (if (eql self (the :tree-root))
                    (the :remaining-depth)
                  (the :parent :total-depth)))
   (last-node? (and (not (eql self (the :tree-root)))
                    (eql self (lastcar (the :parent :child-nodes)))))
   ("List of GDL objects of type <tt>base-rule-object</tt> or (preferably) <tt>gwl-base-rule-object</tt>. 
Links to these will be displayed in the other-rules section. Default to the collection of all objects of type
<tt>base-rule-object</tt> from this node in the tree down to the leaves, whose <tt>violated?</tt> message
evaluates to NIL."
    other-rules (maptree self #'identity
                         #'(lambda (item)
                             (and (typep item 'base-rule-object)
                                  (not (the-object item suppress-display?))
                                  (not (the-object item :violated?))))
                         #'(lambda(item) (or (and (typep item 'layout-mixin)
                                                  (> (the-object item depth) *max-node-depth*))
                                             (typep item 'base-rule-object)))))
   
   ("List of GDL objects of type <tt>base-rule-object</tt> or (preferably) <tt>gwl-base-rule-object</tt>. 
Links to these will be displayed in the other-rules section. Default to the collection of all objects of type
<tt>base-rule-object</tt> from this node in the tree down to the leaves, whose <tt>violated?</tt> message
evaluates to non-NIL."
    violated-rules (maptree self #'identity
                            #'(lambda (item)
                                (and (typep item 'base-rule-object)
                                     (not (the-object item suppress-display?))
                                     (the-object item :violated?)))
                            #'(lambda(item) (or (and (typep item 'layout-mixin)
                                                     (> (the-object item depth) *max-node-depth*))
                                                (typep item 'base-rule-object)))))
   
   ("Boolean. Indicates whether the Rules panel should be displayed. Defaults to T."
    display-rules? t :defaulting)
   
   ("Boolean. Indicates whether the Tree area should be displayed. Defaults to T."
    display-tree? t :defaulting)
   
   ("Boolean. Determines whether the standard-saved-slots are automatically used by default for the
saved-slots. This is a trickle-down slot so its value will be passed to descendent objects automatically. 
The default value is NIL."
    use-standard-saved-slots? nil :defaulting)
   
   
   ("List of keyword symbols. Determines which formats are available in the Preferences. Defaults to :png, :jpeg, and :vrml."
    available-image-formats (list :png :jpeg :vrml :links))
   
   
   (image-format :png)
   
   (length 0) (width 0) (height 0)
   
   
   ("Boolean. Determines whether the embedded form will support multipart MIME parts. Defaults to NIL."
    multipart-form? nil))
  
  :trickle-down-slots
  (tree-root display-rules? display-tree? use-standard-saved-slots? graphics-height graphics-width)


  :computed-slots
  ((child-nodes nil)
   
   ("List of keyword symbols or lists. 

The first of this list should be the unique name for this tree node for the purposes of saving slots. 
The rest of this list is made up of either keyword symbols or lists. A keyword symbol indicates the 
name of a slot to be saved in the current object. These slot names should correspond to <tt>:settable</tt> 
slots of this object. A list indicates slots to be saved in a child object, specified as 
follows: the first of the list is the name of the child part, and the rest is made up of keywords naming
the slots in the child part to be saved. These should correspond to <tt>:settable</tt> 
slots in the child object.

 The default value is the <tt>standard-saved-slots</tt> if the <tt>use-standard-saved-slots?</tt> is non-NIL, NIL otherwise."
    saved-slots (when (the use-standard-saved-slots?) (the standard-saved-slots)))
   
   ("List of keyword symbols. The first of this list is the <tt>name-for-display</tt> of this object. The rest of the list
are all the keyword symbols representing the settable computed-slots and input-slots which have a default value. Required
input-slots (i.e. input-slots without a default value) are not included in this list. If you wish to include required
inputs with the saved-slots, you should explicitly append them to this list when specifying the <tt>saved-slots</tt>."
    standard-saved-slots (let ((slots (append (the (message-list :category :settable-computed-slots 
                                                                 :message-type :local))
                                              (the (message-list :category :settable-optional-input-slots 
                                                                 :message-type :local))
                                              (the (message-list :category :settable-defaulted-input-slots 
                                                                 :message-type :local)))))
                           (when slots (cons (the name-for-display) slots)))))
  
  :hidden-objects
  ((view-object :type 'web-drawing
                :object-roots (ensure-list (the :ui-display-list-objects))
                :objects (ensure-list (the :ui-display-list-leaves))
                :page-width (the :graphics-width)
                :page-length (the :graphics-height)
                :projection-vector (getf *standard-views* (the :view))))

  :functions
  ((write-html-sheet
    nil
    (with-format (html-format t) (write-the (:main-sheet))))
   
   (restore! ())
   
   
   ("Void. Reads the slots data from <tt>filename</tt>, restores the corresponding slots in this
object and matching descendant objects, and calls the <tt>restore!</tt> function on each object.
 
:&key ((filename \"/tmp/&lt;object type&gt;\") \"String or pathname. Name of file to be read.\")"
    read-saved-slots
    (&key (filename (format nil "/tmp/~(~a~).gdl" (the type))))
    (with-open-file (in filename)
      (let ((*package* (find-package (second (read in)))) result)
        (let ((result (do ((node-list (read in nil nil) (read in nil nil)))
                          ((null node-list) (nreverse result)) 
                        (push node-list result))))
          (the (restore-saved-slots result))))) (values))
   
   ("Void. Writes the unique application name names and values of all saved-slots in this and all 
descendants which are of type node-mixin or application-mixin.
 
:&key ((filename-or-stream \"/tmp/&lt;object type&gt;\") \"String, pathname, or stream. Name or stream for file to be written\")"
    write-saved-slots 
    (&key (filename-or-stream (format nil "/tmp/~(~a~).gdl" (the type))) (toplevel? t))
    (let ((*print-right-margin* 100) (*print-case* :capitalize))
      (if (streamp filename-or-stream)
          (the (write-saved-slots-to-stream filename-or-stream :toplevel? toplevel?))
        (with-open-file (out filename-or-stream :direction :output :if-exists :supersede :if-does-not-exist :create)
          (the (write-saved-slots-to-stream out :toplevel? toplevel?)))) (values)))
   
   (restore-saved-slots 
    (slots)
    (let* ((app-id (first (the saved-slots)))
           (value-plist (rest (find app-id slots :key #'first))))
      (mapc #'(lambda(slot)
                (if (atom slot)
                    (let ((value (eval (getf value-plist slot))))
                      (the (set-slot! slot value)))
                  (let ((self (the (evaluate (first slot))))
                        (values (getf value-plist (first slot))))
                    (mapc #'(lambda(sub-slot)
                              (let ((value (getf values sub-slot)))
                                (the (set-slot! sub-slot value))))
                          (rest slot)))))
            (rest (the saved-slots))) 
      (the restore!)
      (mapc #'(lambda(child) (the-object child (restore-saved-slots slots)))
            (remove-if-not #'(lambda(child) (typep child 'layout-mixin)) (the children)))))

   
   (write-saved-slots-to-stream
    (stream &key toplevel?)
    (when toplevel? (print `(in-package ,(make-keyword (package-name *package*))) stream))
    (when (the saved-slots) (print (cons (first (the saved-slots))
                                         (mapcan #'(lambda(key) 
                                                     (if (atom key)
                                                         (list key (readable-expression (the (evaluate key))))
                                                       (list (first key)
                                                             (mapcan #'(lambda(sub-key)
                                                                         (list sub-key
                                                                               (readable-expression 
                                                                                (the (evaluate (first key))
                                                                                  (evaluate sub-key)))))
                                                                     (rest key)))))
                                                 (rest (the saved-slots))))
                                   stream))
    (mapc #'(lambda(child) (the-object child (write-saved-slots :filename-or-stream stream :toplevel? nil)))
          (remove-if-not #'(lambda(child) (typep child 'layout-mixin)) (the children))))))
