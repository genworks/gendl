(in-package :gdl)

(defparameter *1588p012-doc* 
" Add check for listp for root-path.")


(#+allegro 
 excl:without-redefinition-warnings
 #-allegro progn

 (define-object-amendment vanilla-mixin* ()

   :functions
   (("Void. Writes a file containing the toplevel inputs and modified settable-slots starting from the root of the 
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

	 ;;
	 ;; Should always be sorted now. 
	 ;;
	 #+nil
	 (setf (gdl-acc::%version-tree% root)
	       (sort (gdl-acc::%version-tree% root)
		     #'(lambda(item1 item2)
			 (< (length (first item1)) (length (first item2))))))
	 
	 (let* ((root-object-entry (when root-object-object?
				     (find :root-object-object (gdl-acc::%version-tree% root) 
					   :key #'(lambda(item) (first (first item))))))
		(version-tree (if root-object-entry
				  (cons root-object-entry
					(remove :root-object-object (gdl-acc::%version-tree% root) 
						:key #'(lambda(item) (first (first item)))))
				(gdl-acc::%version-tree% root))))
	 
	   (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
	     (print `(in-package ,(make-keyword (package-name *package*))) out)
	     (mapcar #'(lambda(node)
			 (let ((root-path (first node)) (value-plist (rest node)))
			 
			   (unless (or (member root-path root-paths-to-ignore :test #'equalp)
				       (and root-object-object? 
					    (not (eql (lastcar root-path) :root-object-object))))

			     (when (or (and root-object-object? (null (rest root-path)))
				       (and (not root-object-object?) (null root-path)))
			       (setq root-path (the type)))

			     (when (and root-object-object? (consp root-path) (consp (rest root-path)))
			       (setq root-path (butlast root-path)))
			   
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
		
		     (or version-tree (list nil)))))))
     filename))))
