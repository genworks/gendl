(in-package :gdl)

(defparameter *1588p006-doc* 
" Keep the items in internal %version-tree% sorted based on root-path length.")


(define-object-amendment vanilla-mixin* ()

  :functions
  (("NIL. Restores the value of the given slot to its default, thus ``undoing'' any forcibly set value
 in the slot. Any dependent slots in the tree will respond accordingly when they are next demanded. 
 Note that the slot must be specified as a keyword symbol (i.e. prepended with a colon (``:'')), 
 otherwise it will be evaluated as a variable according to normal Lisp functional evaluation rules.
 :arguments (slot \"Keyword Symbol\")
 :key (force? \"Boolean. Specify as t if you want to force non-settable slots to recompute (e.g. 
reading from databases or external files). Defaults to nil.\")"
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
        (let ((root (let ((maybe-root (the :root)))
		      (if (the-object maybe-root root?)
			  maybe-root
			  (the-object maybe-root parent))))
              
              ;;(root-path (remove :root-object-object (the root-path)))
              (root-path (the root-path))
              
              )
          ;;
          ;; FLAG -- this pushnew should never be necessary...
          ;;
          (pushnew (list root-path)
                   (gdl-acc::%version-tree% root) :test #'equalp :key #'(lambda(item) (list (first item))))
          (setf (rest (assoc root-path (gdl-acc::%version-tree% root) :test #'equalp)) 
            (remove-plist-key  (rest (assoc root-path
                                            (gdl-acc::%version-tree% root) :test #'equalp)) attribute))

	  (setf (gdl-acc::%version-tree% root)
		(sort (gdl-acc::%version-tree% root)
		      #'(lambda(item1 item2)
			  (< (length (first item1)) (length (first item2)))))))

        (when *eager-setting-enabled?*
          (dolist (reset *leaf-resets*)
            (the-object (first reset) (evaluate (second reset))))))))



   ("Void. Writes a file containing the toplevel inputs and modified settable-slots starting from the root of the 
 current instance. Typically this file can be read back into the system using the <tt>read-snapshot</tt> function.

 :&key ((filename \"/tmp/snap.gdl\") \"String or pathname. The target file to be written.\"
       (root-paths-to-ignore nil) \"List of root-paths or nil. Any objects with matching root-path will be ignored for the snapshot write.\"
       )"
    write-snapshot
    (&key (filename "/tmp/snap.gdl") (root-paths-to-ignore nil))

    
    (let (*print-level*
	  (root-object-object? (and (the parent) (the parent root?)
				    (defaulting (the parent root-object-object))
				    (eql (the parent root-object-object) self))))

      (let ((root (if root-object-object? (the root parent) (the root))))

	(setf (gdl-acc::%version-tree% root)
	      (sort (gdl-acc::%version-tree% root)
		    #'(lambda(item1 item2)
			(< (length (first item1)) (length (first item2))))))

	(with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (print `(in-package ,(make-keyword (package-name *package*))) out)
	  (mapcar #'(lambda(node)
		      (let ((root-path (first node)) (value-plist (rest node)))
			(unless (or (member root-path root-paths-to-ignore :test #'equalp)
				    (and root-object-object? 
					 (not (eql (lastcar root-path) :root-object-object))))
			  (when root-object-object? 
			    (setq root-path (if (consp (rest root-path))
						(butlast root-path)
						(the type))))
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
							      keys values))))) (print snap out))))))
		
		  (or (gdl-acc::%version-tree% root) (list nil))))))
    filename)))
