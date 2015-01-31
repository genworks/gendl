(in-package :gdl)

(defparameter *1588p004-doc* 
  "1. Fix restore-tree to restore to true root, and protect root-object-object
 (the object of tasty or ta2) from being blown away and replaced with new object. 

2. Make write-snapshot remove the root-object-object from root-paths if called on 
   root-object-object below toplevel parent.

3. Update the update! method to retrieve the version information from
   the true root, in the case we are working on a root-object-object
   below the toplevel parent.
")


(define-object-amendment vanilla-mixin* ()

  :functions
  (("Void. Restores all settable-slots in this instance, and
recursively in all descendant instances, to their default values."
    restore-tree!
    (&key (quick? t)
	  (keep-function #'always)
	  (prune-function #'never)
	  (child-function (lambda(node) (the-object node safe-children))))

    (let ((node (if (and (the parent) (the parent root?)
			 (defaulting (the parent root-object-object))
			 (eql (the parent root-object-object) self))
		    (the parent) self)))
      (if quick?
	  (progn
	    (setf (gdl-acc::%version-tree% node) nil)
	    (the-object node (update! :immune-message-keys (list :root-object-object
								 :root-object-type)))
	    (unless (eql self node) (the-object node root-object-object update!)))
	  (maptree  self 
		    (lambda(node) (when (typep node 'gdl::gdl-basis) (the-object node restore-node!)))
		    keep-function
		    prune-function
		    child-function))))


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

		(or (gdl-acc::%version-tree% (if root-object-object? (the root parent) (the root))) 
		    (list nil)))))
    filename)

   ("Void. Uncaches all cached data in slots and objects throughout the instance 
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
        (when *debug?* (print-variables version-tree))
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
                                      (:set-slot! key value)))))
                  (plist-keys value-plist) (plist-values value-plist)))))))))




(format t "~&~%Changes in 1588p004:~%~%~a~%~%" *1588p004-doc*)
