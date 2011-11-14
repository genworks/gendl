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

(defun vanilla-finalization (obj)
  (format t "Finalizing ~s...~%" obj))

(define-object vanilla-mixin* ()
  
  :no-vanilla-mixin? t
  
  :documentation (:description "Vanilla-Mixin is automatically inherited by every object
created in GDL. It provides basic messages which are common to all GDL objects defined
with the define-object macro, unless <tt>:no-vanilla-mixin t</tt> is specifqqied at the toplevel
of the define-object form.")
  
  :input-slots
  (
   
   (remote-id nil)
   
   (quantify-box (or (the parent) self))
   
   (transitory-slots nil)
   (%name% :toplevel)
   (%parent% nil)
   (%aggregate% nil)
   (%index% nil)
   
   ("String or List of Strings. Determines how the name of objects of this type will be printed in most places. 
This defaults to the name-for-display (generally the part's name as specified in its
parent), followed by an index number if the part is an element of a sequence."
    strings-for-display (format nil "~a~a~a" (the :name-for-display)
                                (if (the :index) " " "")
                                (or (the :index) "")))
   (parent-tree nil)
   
   ("List of GDL Instances. Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL."
    visible-children nil)
   
   
   
   
   
   ("GDL Instance. The root-level node in this object's ``tree'' (instance hierarchy)."
    root (if (the :parent) (the :parent :root) self))
   
   ("Boolean. Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil." 
    hidden? nil))


  :computed-slots
  (
   (%corners% nil)
   (%vertex-array% nil)
   
   (null-part? (typep self 'null-part))
   
   ("Symbol. The GDL Type of this object."
    type (class-name (class-of self)))
   
   ("GDL Instance. The parent of this object, or NIL if this is the root object."
    parent (the :%parent%))
   
   ("List of GDL Objects. A Collection of the leaf nodes of the given object."
    leaves (maptree self #'identity #'(lambda(object)(the-object object leaf?))))

   
   (viewable-slots nil)
   
   (query-plist nil :settable)
   (%parameters% (when (the parent) 
                   (gdl-inputs::%parameters% (the parent) 
                                             (the %name%) 
                                             self)))
   
   (%trickle-down-slots% (gdl-rule::%trickle-down-slots% self))
   (%settable-slots% (gdl-rule::%settable-slots% self))
   (%object-keywords% (mapcar #'make-keyword (gdl-rule::%object-keywords% self)))
   (%hidden-object-keywords% (mapcar #'make-keyword (gdl-rule::%hidden-object-keywords% self)))
   
   ("Keyword symbol. The part's simple name, derived from its object specification in the parent or from
  the type name if this is the root instance."
    name-for-display (make-keyword (the %name%))) ;;(gdl-acc::%name% self)

   
   (%name-for-display% (make-keyword (the %name%)))
   
   ("List of Symbols. Lists the direct superclasses of the type of this object."
    direct-mixins (the (mixins :local? t)))
   
   ("List of Symbols. Lists all the superclasses of the type of this object."
    all-mixins (the (mixins :local? nil)))
   
   
   
   ("GDL Instance. In an element of a sequence, this is the container object which holds all elements."
    aggregate (the :%aggregate%))
   
   ("Integer. Sequential index number for elements of a sequence, NIL for singular objects."
    index (the :%index%))
   
   ("List of GDL Instances. All objects from the :objects specification, including elements of sequences
 as flat lists."
    children (remove-if #'(lambda(obj) 
                            (and (typep obj 'vanilla-mixin) (the-object obj hidden?)))
                        (mapcan #'(lambda (part-keyword)
                                    (let ((part (the (evaluate part-keyword))))
                                      (when (not (the-object part null-part?))
                                        (if (typep part 'quantification)
                                            (copy-list (list-elements (the-object part)))
                                          (list part)))))
                                (the :%object-keywords%))))
   
   
   ("List of GDL Instances. All objects from the :objects specification, including elements of sequences
 as flat lists. Any children which throw errors come back as a plist with error information"
    safe-children (remove-if #'(lambda(obj) 
                                 (and (typep obj 'vanilla-mixin) (the-object obj hidden?)))
                             (mapcan #'(lambda (part-keyword)
                                         (multiple-value-bind (part error)
                                             (ignore-errors (the (evaluate part-keyword)))
                                           (cond ((typep error 'error)
                                                  (list (list :object-key part-keyword :error error)))
                                                 (;;(typep part 'null-object)
                                                  (the-object part null-part?)
                                                  nil)
                                                 ((typep part 'quantification)
                                                  (copy-list (the-object part safe-list-elements)))
                                                 (t (list part)))))
                                     (the :%object-keywords%))))
   
   
   ("Boolean. T if this object has no children, NIL otherwise."
    leaf? (null (the :children)))
   
   
   ("List of GDL Instances. All objects from the :hidden-objects specification, including elements of sequences
 as flat lists."
    hidden-children (mapcan #'(lambda (part-keyword)
                                (let ((part (the (evaluate part-keyword))))
                                  (if (typep part 'quantification)
                                      (copy-list (list-elements (the-object part)))
                                    (list part))))
                            (remove-if #'(lambda(key) (member (make-keyword key) (the %internal-hidden-object-keywords%)))
                                       (the :%hidden-object-keywords%))))
   
   
   ("List of GDL Instances. All objects from the :hidden-objects specification, including elements of sequences
 as flat lists. Any children which throw errors come back as a plist with error information"
    safe-hidden-children (remove-if #'(lambda(obj) 
                                 (and (typep obj 'vanilla-mixin) (the-object obj hidden?)))
                             (mapcan #'(lambda (part-keyword)
                                         (multiple-value-bind (part error)
                                             (ignore-errors (the (evaluate part-keyword)))
                                           (cond ((typep error 'error)
                                                  (list (list :object-key part-keyword :error error)))
                                                 (;;(typep part 'null-object)
                                                  (the-object part null-part?)
                                                  nil)
                                                 ((typep part 'quantification)
                                                  (copy-list (the-object part safe-list-elements)))
                                                 (t (list part)))))
                                     (the :%hidden-object-keywords%))))
   
   
   (%internal-hidden-object-keywords% nil)
   
   
 
   
   
   ("Boolean. T iff this part has NIL as its parent and therefore is the root node."
    root? (null (the :parent)))
   
   ("List of Symbols or of Pairs of Symbol and Integer. Indicates the path through
the instance hierarchy from the root to this object. Can be used in conjunction with
the <tt>follow-root-path</tt> GDL function to return the actual instance."
    root-path (when (the :parent)
                (cons (if (the :index)
                          (cons (the :%name-for-display%) (ensure-list (the :index)))
                        (the :%name-for-display%))
                      (the :parent :root-path))))
   
   ("List of Symbols or of Pairs of Symbol and Integer. Indicates the path through
the instance hierarchy from the local root to this object. Can be used in conjunction with
the <tt>follow-root-path</tt> GDL function to return the actual instance."
    root-path-local (unless (eql self (the root))
                      (cons (if (the :index)
                                (cons (the :%name-for-display%) (ensure-list (the :index)))
                              (the :%name-for-display%))
                            (the :parent :root-path-local))))
   
   
   (root-path-string (replace-substring
                      (replace-substring
                       (replace-substring (format nil "~a" (the root-path)) " " "_") "(" "_") ")" "_"))

   
   ("Boolean. For elements of sequences, T iff there is no previous element."
    first? (when (the index) (= (the :index) 0)))
   
   ("Boolean. For elements of sequences, T iff there is no next element."
    last? (when (the index) (= (the :index) (1- (the :aggregate :number-of-elements)))))
   
   ("GDL Instance. For elements of sequences, returns the next part in the sequence."
    next (when (the index) (the :aggregate (:get-member (1+ (the :index))))))
   
   ("GDL Instance. For elements of sequences, returns the previous part in the sequence."
    previous (when (the index) (the :aggregate (:get-member (1- (the :index))))))
   
   ;;(finalization #'vanilla-finalization)
   
   (display-controls nil)
   
   (color-hex (let ((color-symbol (getf (the display-controls) :color)))
                (when color-symbol (if (eql (aref (format nil "~a" color-symbol) 0) #\#)
                                       color-symbol
                                     (gethash color-symbol *color-table*)))))
   )

  :functions
  (
      
   (ignore-messages-from ())
   
   ("Void. Restores all settable-slots in this instance to their default values."
    restore-all-defaults!
    ()
    
    (maphash #'(lambda(key val) 
                 (declare (ignore val))
                 (the (restore-slot-default! key)))
             (the %settable-slots%)))
   
   ("Void. Restores all settable-slots in this instance, and recursively in all descendant instances,
to their default values."
    restore-tree!
    (&key (quick? t))
    (if quick?
        (progn
          (setf (gdl-acc::%version-tree% self) nil)
          (the update!))
      (let ((settables (let (result)
                         (maphash #'(lambda(key val) (declare (ignore val)) (push key result)) (the %settable-slots%))
                         (nreverse result))))
      
        (the (restore-slot-defaults! settables))
      
        (maptree  self 
                  (lambda(node) (when (typep node 'gdl::gdl-basis) (the-object node restore-tree!)))
                  #'always
                  #'never
                  (lambda(node) (append (the-object node safe-children)
                                        (the-object node safe-hidden-children))))
        )))
   
   
   
   
   ("GDL Instance. Using this instance as the root, follow the reference chain
represented by the given path.
:arguments (path \"List of Symbols or Pairs of Symbol and Integer\")"
    follow-root-path
    (path)
    ;;
    ;; FLAG -- get rid of this eval!!!
    ;;
    (eval `(the-object ,self ,@(reverse path))))

   
   ("List of Symbols. Returns the names of the immediate superclasses of this object.

:&key ((local? t) \"Boolean. Indicates whether to give only direct mixins 
or all mixins from the entire inheritance hierarchy.\")"
    mixins (&key (local? t)) 
    (mapcar #'class-name (find-all-superclasses (class-of self) local?)))
      
   
   ("NIL. Restores the value of the given slot to its default, thus ``undoing'' any forcibly set value
 in the slot. Any dependent slots in the tree will respond accordingly when they are next demanded. 
 Note that the slot must be specified as a keyword symbol (i.e. prepended with a colon (``:'')), 
 otherwise it will be evaluated as a variable according to normal Lisp functional evaluation rules.
 :arguments (slot \"Keyword Symbol\")"
    restore-slot-default!
    (attribute &key (force? *force-restore-slot-default?*))
    (when (or force? (eql (the (slot-status attribute)) :set))
      (let (*leaf-resets*)
        (let ((slot (glisp:intern (symbol-name attribute) :gdl-acc)))
          (unless (eq (first (ensure-list (slot-value self slot))) 'gdl-rule::%unbound%)
            (unbind-dependent-slots self slot) 
            (setf (slot-value self slot) 
              (if *remember-previous-slot-values?*
                  (list 'gdl-rule::%unbound% nil nil (first (slot-value self slot)))
                'gdl-rule::%unbound%))))
        (let ((root (the :root)) 
              
              (root-path (remove :root-object-object (the root-path)))
              ;;(root-path (the root-path))
              
              )
          ;;
          ;; FLAG -- this pushnew should never be necessary...
          ;;
          (pushnew (list root-path)
                   (gdl-acc::%version-tree% root) :test #'equalp :key #'(lambda(item) (list (first item))))
          (setf (rest (assoc root-path (gdl-acc::%version-tree% root) :test #'equalp)) 
            (remove-plist-key  (rest (assoc root-path
                                            (gdl-acc::%version-tree% root) :test #'equalp)) attribute)))
        (when *eager-setting-enabled?*
          (dolist (reset *leaf-resets*)
            (the-object (first reset) (evaluate (second reset))))))))
   
   
   (restore-attribute-default! (attribute) (the (restore-slot-default! attribute)))

   ("nil. Restores the value of the given slots to their defaults, thus ``undoing'' any forcibly set values
 in the slots. Any dependent slots in the tree will respond accordingly when they are next demanded. 
 Note that the slots must be specified as keyword symbols (i.e. prepended with colons (``:'')), 
 otherwise they will be evaluated as variables according to normal Lisp functional evaluation rules.

:arguments (slots \"List of Keyword Symbols\")
 
:&key ((force? *force-restore-slot-default?*) \"Boolean. Indicates whether the slot values should be unbound, regardless of whether it had actually been bashed previously.\")

"

    restore-slot-defaults!
    (attribute-list &key (force? *force-restore-slot-default?*))
    (mapc #'(lambda (attribute) (the (:restore-slot-default! attribute :force? force?)))
          attribute-list))
   
   (restore-attribute-defaults! (attribute-list) 
                                (the (restore-slot-defaults! attribute-list)))


   (set-slot-if-needed!
    (slot value &key (ensure-lists? *ensure-lists-when-bashing?*) (infer-types? t))
    (when *debug?* (print-variables slot value infer-types?))
    (let ((current (the (evaluate slot)))
          (remember? (not (member slot (the transitory-slots)))))
      
      (when (typep current 'pathname) (setq current (format nil "~a" current)))
      
      (if infer-types?
          (etypecase current
            (string (let ((new value))
                      (when *debug?* (print-variables current value new))
                      (unless (string= new current) 
                        (the (set-slot! slot new :remember? remember?)))))
            (keyword (let ((new (make-keyword value)))
                       (when *debug?* (print-variables current value new))
                       (unless (eql new current) 
                         (the (set-slot! slot new :remember? remember?)))))
            (list (let ((new 
                         (cond ((and (null current)
                                     (eql (read-safe-string value) t)) t)
                               (ensure-lists? (ensure-list value))
                               (t value))))
                    (when *debug?* (print-variables current value new))
                    (unless (equalp new current) (the (set-slot! slot new)))))
            (number (let ((new (or (let ((result (ignore-errors 
                                                  (read-safe-string value))))
                                     (when (numberp result) result)) 0)))
                      (when *debug?* (print-variables current value new))
                      (unless (= new current) 
                        (the (set-slot! slot new :remember? remember?)))))
            (symbol (let ((new (ignore-errors (read-safe-string value))))
                      (when *debug?* (print-variables current value new))
                      (unless (eql new current) 
                        (the (set-slot! slot new :remember? remember?))))))
        (let ((equality-function (if (and (stringp value) (stringp current))
                                     #'string= #'equalp)))
          (unless (funcall equality-function value current)
            (the (set-slot! slot value :remember? remember?)))))))

   

   ("NIL. Forcibly sets the value of the given slot to the given value. The slot must be defined
as <tt>:settable</tt> for this to work properly. Any dependent slots in the tree will 
respond accordingly when they are next demanded. Note that the slot must be specified as a keyword 
symbol (i.e. prepended with a colon (``:'')), otherwise it will be evaluated as a variable according 
to normal Lisp functional evaluation rules.
 :arguments (slot \"Keyword Symbol\"
             value \"Lisp Object. (e.g. Number, String, List, etc.)\")

 :&key ((remember? t) \"Boolean. Determines whether to save in current version-tree.\")"
    set-slot!
    (attribute value &key (remember? t))
    (unless (eql attribute :%primary?%)
      (when (not *run-with-dependency-tracking?*)
        (error "Dependency Tracking must be enabled in order to forcibly
set slot values. 

Please setq the variable `*run-with-dependency-tracking?*' to T,
make a fresh root-level object, and start again."))
      (let (*leaf-resets*)
        (let ((slot (glisp:intern (symbol-name attribute) :gdl-acc)))
          (unbind-dependent-slots self slot)
          (setf (slot-value self slot) 
            (list value nil t)))
        (when remember?
          (let ((root (the :root)) (root-path 
                                    (remove :root-object-object (the root-path))
                                     ;;(the root-path)
                                     ))
            
            (pushnew (list root-path)
                     (gdl-acc::%version-tree% root)
                     :test #'equalp
                     :key #'(lambda(item) (list (first item))))

          
            (setf (gdl-acc::%version-tree% root)
                  (sort (gdl-acc::%version-tree% root)
                        #'(lambda(item1 item2)
                            (< (length (first item1)) (length (first item2))))))
          
            (when (the primary?)
              (setf (getf (rest (assoc root-path (gdl-acc::%version-tree% root) :test #'equalp)) :%primary?%)
                t))

            (let ((non-root (remove nil (gdl-acc::%version-tree% root) :key #'first))
                  (root-list (find nil (gdl-acc::%version-tree% root) :key #'first)))
              (let ((primaries (remove-if-not #'(lambda(item)
                                                  (getf (rest item) :%primary?%))
                                              non-root))
                    (non-primaries (remove-if #'(lambda(item)
                                                  (getf (rest item) :%primary?%))
                                              non-root)))
              (setf (gdl-acc::%version-tree% root)
                    (cons (or root-list (list nil))
                          (append primaries non-primaries)))))
            
            (setf (getf (rest (assoc root-path (gdl-acc::%version-tree% root) :test #'equalp)) attribute)
                  value)))
      
        (when *eager-setting-enabled?*
          (dolist (reset *leaf-resets*)
            (the-object (first reset) (evaluate (second reset))))))))

   
   (modify-attribute!
    (attribute value &key (remember? t))
    (the (:set-slot! attribute value :remember? remember?)))

   
   ("NIL. Forcibly sets the value of the given slots to the given values. The slots must be defined
as <tt>:settable</tt> for this to work properly. Any dependent slots in the tree will 
respond accordingly when they are next demanded. Note that the slots must be specified as a keyword 
symbols (i.e. prepended with a colon (``:'')), otherwise they will be evaluated as variables according 
to normal Lisp functional evaluation rules.
 :arguments (slots-and-values \"Plist. Contains alternating slots and values to which they are to be set.\")"
    set-slots!
    (keys-and-values &key (remember? t))
    (mapc #'(lambda (key value)
              (the (:set-slot! key value :remember? remember?)))
          (plist-keys keys-and-values) (plist-values keys-and-values)))
   
   (modify-attributes!
    (keys-and-values &key (remember? t))
    (the (:set-slots! keys-and-values :remember? remember?)))

   
   ("Plist. Returns the <tt>:documentation</tt> plist which has been specified the 
specific part type of this instance."
    documentation () (gdl-documentation (class-of self)))
   
   (part-documentation () (the documentation))

   
   ("Body of GDL code, in list form. 

:arguments (slot \"Keyword Symbol. Names the slot for which documentation is being requested.\")"
    slot-source
    (message)
    (gdl-rule::%message-source% self message))

   
   ("Plist of Symbols and Strings. Returns the part types and slot documentation which has been
  specified for the given slot, from most specific to least specific in the CLOS inheritance order.
Note that the slot must be specified as a keyword symbol (i.e. prepended with a colon (``:'')), 
otherwise it will be evaluated as a variable according to normal Lisp functional evaluation rules.
:arguments (slot \"Keyword Symbol. Names the slot for which documentation is being requested.\")"
    slot-documentation
    (message)
    (gdl-rule::%message-remarks% self message))
   
   ("String. This is synonymous with <tt>slot-documentation</tt>"
    message-documentation (message) (the (slot-documentation message)))
   
   (message-remarks (message) (the (slot-documentation message)))

   (reference-chain
    (reference-chain)
    (the (:follow-root-path
          (mapcar #'(lambda (component) (list component 0)) reference-chain))))

   ("List of Keyword Symbols. Returns the messages (slots, objects, and functions) of this
object, according to the filtering criteria as specified by the arguments.
:&key ((category :all) \"Keyword. Either <tt>:all</tt> or the individual category of 
                         messages to be returned. This can be one of:
                           <tt><ul>
                           <li>:computed-slots</li>
                           <li>:settable-computed-slots</li>
                           <li>:required-input-slots</li>
                           <li>:optional-input-slots</li>
                           <li>:defaulted-input-slots</li>
                           <li>:query-slots</li>
                           <li>:functions</li>
                           <li>:objects</li>
                           <li>:quantified-objects</li>
                           <li>:hidden-objects</li>
                           <li>:quantified-hidden-objects</li></ul></tt>\"
             (message-type :global) \"Keyword Symbol, :local or :global. Indicates whether to return messages
only from the local specific part type, or from all superclasses (mixins) as well.\"
             (return-category? nil) \"Boolean. Indicates whether or not the category of each message should be
returned before each message in the returned list.\"
             (base-part-type nil) \"Symbol naming a GDL Part Type. Indicates a ``base'' part from which no messages
should be returned, nor should messages be returned from superclasses (mixins) of this base part. If NIL (the default),
messages are considered from all superclasses.\"
             (sort-order :unsorted) \"Keyword Symbol. One of: <tt>:unsorted</tt>, <tt>:by-category</tt>, or 
<tt>:by-name</tt>.\"
             (filter :normal) \"Function Object of two arguments or <tt>:normal</tt>. If a function object,
applies this function to each returned category and message keyword, and filters out all pairs for which
the function returns NIL. If <tt>:normal</tt> (the default), then no filtering is done.\")"
    message-list
    (&key (category :all) (message-type :global) return-category? base-part-type
          (sort-order :unsorted) (filter :normal))
    (let ((unfiltered
           (gdl-rule::%message-list% self category message-type base-part-type)))
      (let* ((whittled (remove-duplicates unfiltered :key #'first :from-end t))
             (chiseled (case category
                         (:all whittled)
                         (otherwise (let ((category (normalize-category-name category)))
                                      (remove-if-not #'(lambda(pair) (eql (second pair) category)) whittled)))))
             (filtered (if (functionp filter)
                           (remove-if-not #'(lambda(message-and-category)
                                              (apply filter (reverse message-and-category)))
                                          unfiltered) chiseled)))
        (let ((sorted
               (ecase sort-order
                 (:unsorted filtered)
                 (:by-name (sort filtered #'string-lessp :key #'first))
                 (:by-category (sort filtered #'string-lessp :key #'second)))))
          (if return-category? (flatten-pairs sorted) (mapcar #'first sorted))))))

   
   ("Void. Uncaches all cached data in slots and objects througout the instance 
tree from this node, forcing all code to run again the next time values are 
demanded. This is useful for updating an existing model or part of an existing 
model after making changes and recompiling/reloading the code of the underlying 
definitions.  Any set (modified) slot values will, however, be preserved 
by the update."
    update!
    (&key (replace-bashed-values? t))
    (let ((all-messages (the (:message-list)))
          (methods (the (:message-list :category :functions)))
          (immune-messages (list :$$ta2 :$$ta2-object 
                                 :$$tatu :$$tatu-object))
	  ;;
	  ;; FLAG -- get rid of special-casing and ignore-errors for root-object-object.
	  ;;
          (root-object-object-vt 
           (when (ignore-errors (the root-object-object))
             (mapcar #'(lambda(path) 
                         (when (consp (rest path))
                           (cons (append (first path) (list :root-object-object))
                                 (rest path))))
                     (gdl-acc::%version-tree% (ignore-errors (the root-object-object)))))))
      (let ((cached-messages 
             (set-difference all-messages (append methods immune-messages))))
        
        (dolist (key cached-messages)
          (let ((key (glisp:intern (symbol-name key) :gdl-acc)))
            (unbind-dependent-slots self key :updating? t))))
      (let* ((root-path (remove :root-object-object (the root-path))) 
             ;;(root-path (the root-path))
             (root-path-length (length root-path))
             (version-tree
              (when replace-bashed-values?
                (append
                (remove-if-not #'(lambda(node) 
                                   (let ((local-root-path (first node)))
                                     (and (> (length local-root-path) root-path-length)
                                          (equalp (subseq local-root-path
                                                          (- (length local-root-path) 
                                                             root-path-length))
                                                  root-path))))
                               (gdl-acc::%version-tree% (the root)))
                root-object-object-vt))))

        (when *debug?* (print-variables version-tree))
        (dolist (version-node version-tree (values))
          (let ((root (the root))
                (root-path (first version-node)) (value-plist (rest version-node)))
            (mapc #'(lambda(key value)
                      
                      (when (not (eql value 'gdl-acc::%default%))
                        ;;
                        ;; FLAG - make this into a more specific error check for 
                        ;;        nonexistence of the object (e.g. not handled error).
                        ;;
                        (with-error-handling (:timeout nil)
                          (the-object root (:follow-root-path root-path) 
                                      (:set-slot! key value)))))
                  (plist-keys value-plist) (plist-values value-plist)))))))

   

   ;;
   ;; Causing mismatched emacs parens.
   ;;
   (active-inputs
    ()
    (let ((inputs (append (the (message-list :category :optional-input-slots))
                          (the (message-list :category :required-input-slots))
                          (the (message-list :category :defaulted-input-slots)))) result)
      (mapc #'(lambda (slot)
                (let ((contents (funcall (read-from-string (format nil "gdl-acc::~a" slot)) self)))
                  (when (and (not (member slot '(:%name% :%parent% :%aggregate% :%index% :$$tatu-object)))
                             (consp contents) (third contents))
                    (push slot result) (push (readable-expression (first contents) self) result)))) inputs)
      (nreverse result)))
   

   
   ("Void. Writes a file containing the toplevel inputs and modified settable-slots starting from the root of the 
current instance. Typically this file can be read back into the system using the <tt>read-snapshot</tt> function.

:&key ((filename \"/tmp/snap.gdl\") \"String or pathname. The target file to be written.\")"
    write-snapshot
    (&key (filename "/tmp/snap.gdl"))
    (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (print `(in-package ,(make-keyword (package-name *package*))) out)
      (mapcar #'(lambda(node)
                  (let ((root-path (or (first node) (the type))) (value-plist (rest node)))
                    (let ((keys (plist-keys value-plist)) (values (plist-values value-plist)))
                      (let ((snap (cons root-path 
                                        (append (when (atom root-path)
                                                  (let (result)
                                                    (let* ((toplevel-inputs 
                                                            (remove-plist-key (the root active-inputs)
                                                                              :remote-id))
                                                           (toplevel-input-keys (plist-keys toplevel-inputs))
                                                           (toplevel-input-values (plist-values toplevel-inputs)))
                                                      (mapc #'(lambda(key value) 
                                                                (when (not (member key keys)) 
                                                                  (push key result) (push value result)))
                                                            toplevel-input-keys toplevel-input-values))
                                                    (nreverse result)))
                                                (mapcan #'(lambda(key val)
                                                            (let ((expression (readable-expression val self)))
                                                              (unless (eql expression :%unreadable%)
                                                                (list key expression))))
                                                        keys values))))) (print snap out)))))


              (or (gdl-acc::%version-tree% (the root)) (list nil))))
    (values))
   
   

   
   
   ("Keyword symbol. Describes the current status of the requested slot:
<ol>
<li><tt>:unbound</tt>: it has not yet been demanded (this could mean either
    it has never been demanded, or something it depends on has been
    modified since the last time it was demanded and eager setting is not enabled).</li>
<li><tt>:evaluated</tt>: it has been demanded and it is currently bound to the default
value based on the code.</li>
<li><tt>:set</tt>: (for :settable slots only, which includes all required
:input-slots) it has been modified and is currently bound to
the value to which it was explicitly set.</li>
<li><tt>:toplevel</tt>: (for root-level object only) its value was passed 
into the root-level object as a toplevel input at the time of object 
instantiation.</li></ol>"
    slot-status 
    (slot)
    (let ((slot-value (funcall (glisp:intern (symbol-name slot) :gdl-acc) self)))
      (cond ((eql (first (ensure-list slot-value)) 'gdl-rule::%unbound%) :unbound)
            ((and (consp slot-value) (null (third slot-value))) :evaluated)
            ((let ((toplevel-value (getf (gdl-acc::%toplevel-inputs% self) slot)))
               (and toplevel-value (equalp toplevel-value (first slot-value)))) :toplevel)
            (t :set))))
   
   (show-dependency-tree
    (slot)
    (let* ((slot-value (funcall (glisp:intern (symbol-name slot) :gdl-acc) self))
           (notify-cons (when (consp slot-value) (second slot-value))))
      (mapcar #'(lambda(notify-cons-pair)
                  (let ((object (first notify-cons-pair))
                        (message-key (make-keyword (second notify-cons-pair))))
                    (list (the-object object root-path)
                          message-key
                          (the-object object (show-dependency-tree message-key)))))
              notify-cons)))

   ;;
   ;; The following are defined here to avoid compiler warnings
   ;; because they are referenced as special-cases in core functions
   ;; and methods.
   ;;
   
   (unbind-remote-slot 
    (slot)
    (declare (ignore slot))
    (warn "this is only defined for remote-object.~%"))
   
   ;;
   ;; FLAG -- put this back in and get rid of defaulting's below, have
   ;; to fix ta2 and tasty's handling of root-paths to account for
   ;; root-object-object returning nil.
   ;;
   #+nil
   (root-object-object 
    ()
    nil)
   
   
   (primary? 
    ()
    nil)
   
   (fetch-input 
    ()
    nil)
   
   ))
   



(define-object vanilla-mixin (vanilla-mixin* gdl-basis)
  :no-vanilla-mixin? t)


(defmethod readable-expression ((object vanilla-mixin) &optional self)
  (if (same-tree? object self)
      `(the :root (follow-root-path ',(the-object object root-path)))
    (error "~s appears to be a GDL object from a different object hierarchy from the current
object ~s. GDL currently does not support writing out and reading back in GDL objects from
a separate object hierarchy." object self)))

(defclass base-gdl-ui ()())

(defvar *ui-server* (make-instance 'base-gdl-ui))



(defmethod restore-ui-object ((ui-server base-gdl-ui) object) (declare (ignore object)))

(defparameter *unbound-slots* nil)


(defun unbind-dependent-slots (object slot &key updating?)
  (let ((*unbound-slots* (glisp:make-sans-value-equalp-hash-table)))
    (%unbind-dependent-slots% object slot :updating? updating?)))


(defun %unbind-dependent-slots% (object slot &key updating?)
  
  (let ((current (gethash (list object slot) *unbound-slots*)))
    (unless current
      (setf (gethash (list object slot) *unbound-slots*) t)
      (if (slot-exists-p object slot)
          (let ((slot-value (slot-value object slot)))
            (when (not (eq (first (ensure-list slot-value)) 'gdl-rule::%unbound%))
              (when (and *eager-setting-enabled?* (null (second slot-value)))
                (push (list object slot) *leaf-resets*))
              
              
              (let ((notify-ht (second (slot-value object slot))))
                (when notify-ht
                  (maphash #'(lambda(node messages)
                               (mapc #'(lambda(message) 
                                         (%unbind-dependent-slots%  node message :updating? updating?)) messages))
                           notify-ht)))
              
              (setf (second (slot-value object slot)) nil)
              (when (not (third slot-value))
                (setf (slot-value object slot) 
                  (if (or updating? (not *remember-previous-slot-values?*))
                      'gdl-rule::%unbound%
                    (list 'gdl-rule::%unbound% nil nil (first (slot-value object slot))))))))
        (when nil (typep object 'gdl-remote)
          (the-object object (unbind-remote-slot slot)))))))
    

(define-format base-format ()
  :slots ((foreground-color nil) (background-color nil))
  
  :functions
  ((initialize-output ())
   
   (finalize-output ())
   
   (:newline-out
    ()
    (format *stream* "~%"))
   
   (:a
    (&rest strings)
    (mapcar #'(lambda(string) (format *stream* "~a" string)) strings))))


(define-lens (base-format vanilla-mixin)()
  :output-functions
  ((cad-output-tree
    ()
    (if (the leaf?)
        (write-the cad-output)
      (mapc #'(lambda(child) (write-the-object child cad-output-tree)) (the children))))
   
   (cad-output ())))


(defun flatten-pairs (pair-list)
  (let (result)
    (dolist (pair pair-list (nreverse result))
      (push (first pair) result)
      (push (second pair) result))))









