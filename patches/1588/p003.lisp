(in-package :gdl)

(defparameter *1588p003-doc* 
  "Fix restore-slot-default to blank out version information at the true root 
in the case that we are a root-object-object child of true parent.")

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
                                            (gdl-acc::%version-tree% root) :test #'equalp)) attribute)))
        (when *eager-setting-enabled?*
          (dolist (reset *leaf-resets*)
            (the-object (first reset) (evaluate (second reset))))))))))


(format t "~&~%Changes in 1588p003:~%~%~a~%~%" *1588p003-doc*)
