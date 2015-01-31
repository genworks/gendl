(in-package :gdl)

(defparameter *1588p002-doc* 
  "On Update!, pulls version-tree for true root down to
  root-object-object, if the root-object-object is a child of the true
  root and has its root set to itself. This is the case for the object
  being inspected in ta2 and tasty, for example. This makes (the
  update!) work as expected for the root-object-object of tasty or
  ta2, even though the actual version-tree is stored at the
  parent (i.e. at the true root).

 NOTE: This patch is superseded by p004.")


(define-object-amendment vanilla-mixin* ()
  :functions
  (("Void. Uncaches all cached data in slots and objects throughout the instance 
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
           (when (and (ignore-errors (the root-object-object))
		      (not (eql (the root-object-object) self)))
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

	     (version-tree (if (and (the parent)
				    (ignore-errors (the parent root-object-object))
				    (eql (the parent root-object-object) self))
			       (remove-if-not #'(lambda(path)
						  (eql (lastcar (first path)) 
						       :root-object-object))
					      (gdl-acc::%version-tree% (the parent)))
			       (gdl-acc::%version-tree% (the root))))

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
                               version-tree)
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
                  (plist-keys value-plist) (plist-values value-plist)))))))))


(format t "~&~%Changes in 1588p002:~%~%~a~%~%" *1588p002-doc*)

