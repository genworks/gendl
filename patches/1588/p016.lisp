(in-package :gdl)

(defparameter *1588p016-doc* 
    "Improve sort ordering for input forms from snap files 
and stored in version-tree root data structure")


(defun double-length-sort (&optional (list test-list))
  (let ((first (safe-sort list #'(lambda(item1 item2)
				   (let ((first1 (first (first item1)))
					 (first2 (first (first item2))))
				     (< (if (consp first1) (length first1) 0)
					(if (consp first2) (length first2) 0)))))))
    (stable-sort first #'(lambda(x  y) (< (length (first x)) (length (first y)))))))


(#+allegro 
 excl:without-redefinition-warnings
 #-allegro progn


(define-object-amendment vanilla-mixin* ()

  :functions
  (("Void. Uncaches all cached data in slots and objects throughout the instance 
tree from this node, forcing all code to run again the next time values are 
demanded. This is useful for updating an existing model or part of an existing 
model after making changes and recompiling/reloading the code of the underlying 
definitions.  Any set (modified) slot values will, however, be preserved 
by the update."
    update!
    (&key (replace-bashed-values? t) immune-message-keys)
    (let ((all-messages (the (:message-list)))
	  (methods (the (:message-list :category :functions)))
	  (immune-messages (append (list :$$ta2 :$$ta2-object 
					 :$$tatu :$$tatu-object)
				   (ensure-list immune-message-keys)))
	  (true-root (if (and (the root parent) (the root parent root?)
			      (defaulting (the root parent root-object-object))
			      (eql (the root parent root-object-object) self))
			 (the root parent) (the root))))
      (let ((cached-messages 
	     (set-difference all-messages (append methods immune-messages))))
        
	(dolist (key cached-messages)
	  (let ((key (glisp:intern (symbol-name key) :gdl-acc)))
	    (unbind-dependent-slots self key :updating? t))))
      (let* ( ;;(root-path (remove :root-object-object (the root-path))) 
	     (root-path (the root-path))
	     (root-path-length (length root-path))
	     (version-tree
	      (when replace-bashed-values?
		(remove-if-not #'(lambda(node) 
				   (let ((local-root-path (first node)))
				     (and (> (length local-root-path) root-path-length)
					  (equalp (subseq local-root-path
							  (- (length local-root-path) 
							     root-path-length))
						  root-path))))
			       (gdl-acc::%version-tree% true-root)))))

	(dolist (version-node version-tree (values))
	  (let ((root true-root)
		(root-path (first version-node)) (value-plist (rest version-node)))
	    (mapc #'(lambda(key value)
                      
		      (when (not (eql value 'gdl-acc::%default%))
			;;
			;; FLAG - make this into a more specific error check for 
			;;        nonexistence of the object (e.g. not handled error).
			;;
			 
			(with-error-handling (:timeout nil)
			  (the-object root (:follow-root-path root-path) 
				      (:set-slot! key value :re-sort? nil)))))
		  (plist-keys value-plist) (plist-values value-plist)))))))


   ("NIL. Forcibly sets the value of the given slot to the given value. The slot must be defined
as <tt>:settable</tt> for this to work properly. Any dependent slots in the tree will 
respond accordingly when they are next demanded. Note that the slot must be specified as a keyword 
symbol (i.e. prepended with a colon (``:'')), otherwise it will be evaluated as a variable according 
to normal Lisp functional evaluation rules.

<p>
Note also that this must not be called (either directly or indirectly)
from within the body of a Gendl computed-slot. The caching and
dependency tracking mechanism in Gendl will not work properly if this
is called from the body of a computed-slot, and furthermore a runtime
error will be generated.
</p>
 :arguments (slot \"Keyword Symbol\"
             value \"Lisp Object. (e.g. Number, String, List, etc.)\")

 :&key ((remember? t) \"Boolean. Determines whether to save in current version-tree.\"
        (warn-on-non-toplevel? t) \"Boolean. Determines whether to warn if this is called from the body of a cached slot.\" )"
    set-slot!
    (attribute value &key (remember? t) (warn-on-non-toplevel? t) (override-non-settable? *override-non-settables?*)
	       (re-sort? t) (keys-to-ignore (list :%primary?%)) (keys-to-forget (list :query-plist :cookies-received :view-toggle)))

    (when (member attribute keys-to-forget) (setq remember? nil))
    
    (unless (member attribute keys-to-ignore)
      (unless (or (gethash attribute (the %settable-slots%))
		  (member attribute (the (message-list :category :required-input-slots))))
	(let ((message (format nil "The slot ~s is not settable in ~a of type ~a." 
			       attribute
			       (cons 'the (reverse (the root-path))) 
			       (the type))))
	  (if override-non-settable? 
	      (warn (format nil "~a Setting it anyway... 
To get a continuable error instead, use :override-non-settable? t or 
globally (setq *override-non-settables?* t).

Note that this behavior may change to error by default in a future GDL release.

" 
			    message))
	      (cerror  "Set the slot anyway." message))))
    
      (when (and warn-on-non-toplevel?
		 *notify-cons*)
	(warn "It is not recommended to call set-slot! from within dependency-tracking context, e.g. from the body of a computed-slot. 

THIS CAN LEAD TO INCONSISTENT DEPENDENCY-TRACKING!!!

Set-slot was called on 

  ~s 

with value 

  ~s

from 

  ~s.~%"
	      (cons 'the (append (reverse (the root-path))
				 (list (make-keyword attribute))))
	      value
	      (cons 'the (append (reverse (the-object (first *notify-cons*) root-path))
				 (list (make-keyword (second *notify-cons*)))))))
      (progn ;; bt:with-lock-held (*binding-lock*) FLAG - bring in later in bootstrapping. 
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
	      (let ((root (let ((maybe-root (the :root)))
			    (if (the-object maybe-root root?)
				maybe-root
				(the-object maybe-root parent))))
		    (root-path (the root-path)
		      ;;(remove :root-object-object (the root-path))
		      ;;(the root-path)
		      ))

		(cond ((and (gdl-acc::%version-tree% root)
			    (not (find root-path (gdl-acc::%version-tree% root)
				       :test #'equalp :key #'first)))
		       (nconc (gdl-acc::%version-tree% root)
			      (list (list root-path))))
		      ((null (gdl-acc::%version-tree% root))
		       (setf (gdl-acc::%version-tree% root) 
			     (list (list root-path)))))

		(when (the primary?)
		  (setf (getf (rest (assoc root-path (gdl-acc::%version-tree% root) :test #'equalp)) :%primary?%)
			t))
	      
		(when re-sort?
		  
		  (setf (gdl-acc::%version-tree% root)
			(double-length-sort (gdl-acc::%version-tree% root)))
		
		  (let ((non-root (remove nil (gdl-acc::%version-tree% root) :key #'first))
			(root-list (find nil (gdl-acc::%version-tree% root) :key #'first)))
		    (let ((primaries (remove-if-not #'(lambda(item)
							(or (getf (rest item) :%primary?%)
							    (getf (rest item) :element-index-list)))
						    non-root))
			  (non-primaries (remove-if #'(lambda(item)
							(or (getf (rest item) :%primary?%)
							    (getf (rest item) :element-index-list)))
						    non-root)))
		      (setf (gdl-acc::%version-tree% root)
			    (cons (or root-list (list nil))
				  (append primaries non-primaries))))))
            
		(setf (getf (rest (assoc root-path (gdl-acc::%version-tree% root) :test #'equalp)) attribute)
		      value)))
      
	    (when *eager-setting-enabled?*
	      (dolist (reset *leaf-resets*)
		(the-object (first reset) (evaluate (second reset)))))))))))))





(defun read-snapshot (&key (filename "/tmp/snap.gdl") object keep-bashed-values? make-object-args 
			   keys-to-ignore
			   (keys-to-ignore-default (list :query-plist :view-toggle :cookies-received)))
    
  "GDL Instance. Reads the snapshot file indicated by filename. If no optional keyword <tt>object</tt>
argument is given, a new GDL instance based on the data in the snapshot file is returned. If an
<tt>object</tt> is given, the object should be compatible in type to that specified in the 
snapshot file, and this existing object will be modified to contain the set slot values and
toplevel inputs as specified in the snapshot file.

:&key ((filename \"/tmp/snap.gdl\") \"String or pathname. File to be read.\"
       (keep-bashed-values? nil) \"Boolean. Indicates whether to keep the currently bashed values in object before reading snap values into it.\"
       (object nil) \"GDL object. Existing object to be modified with restored values.\")"
  
  (let ((keys-to-ignore (append keys-to-ignore keys-to-ignore-default)))
    
  
    (with-open-file (in filename)
      (let ((package-form (read in)))
	(when (or (null package-form) (not (find-package (second package-form))))
	  (error "Invalid package specification at beginning of ~a.~%" filename))
	(let* ((*package* (find-package (second package-form))) (root-form (read in)))
	  (when (or (null root-form) 
		    (not (eql (class-of (find-class (first root-form))) (find-class 'gdl-class))))
	    (error "Invalid object type specifier as first element of second form in ~a.~%" root-form))

        
	  (let* ((object (cond ((and object keep-bashed-values?) object)
			       (object (the-object object restore-tree!) object)
			       (t (progn
				    (apply #'make-object (first root-form) make-object-args))))))

	    
	    (let ((self object) (value-plist (rest root-form)))

	      (mapc #'(lambda(key expression) 
			(unless (member key keys-to-ignore)
			  (when self (the-object self (set-slot! key (eval expression) :re-sort? nil)))))
		    (plist-keys value-plist) (plist-values value-plist)))
          
	    (let (forms)
	      (do ((form (read in nil nil) (read in nil nil)))
		  ((null form) forms)
		(push form forms))
	    
	      (setq forms (nreverse forms))
	    
	      (let* ((forms (double-length-sort forms))
		     
		     (primaries (remove-if-not #'(lambda(item) (or (getf (rest item) :%primary?%)
								   (getf (rest item) :element-index-list))) forms))
		     (non-primaries (remove-if #'(lambda(item) (or (getf (rest item) :%primary?%)
								   (getf (rest item) :element-index-list))) forms))
		     (forms (append primaries non-primaries)))
              
		(dolist (form forms)
            
		  (let ((root-path (first form)) (value-plist (rest form)))
		    (let ((self 
			   (with-error-handling (:timeout nil)
			     (the-object object (follow-root-path root-path)))))
		      (when self
			(mapc #'(lambda(key expression) 
				  (unless (member key keys-to-ignore)
				    (the-object self (set-slot! key (eval `(let ((self ,self)) ,expression)) :re-sort? nil))))
			      (plist-keys value-plist) (plist-values value-plist))))))))
	    object))))))





