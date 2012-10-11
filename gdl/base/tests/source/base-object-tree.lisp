(in-package :gdl-user)


(define-object base-object-tree ()

  :input-slots ((depth 0) (max-depth 6) (branch-factor 5))


  :computed-slots ((depth*index (* (or (the index) 0) (the depth)))

		   (leaf-results (mapsend (the leaves) :depth*index)))


  :functions ((write-results 
	       (&key (output-path (merge-pathnames "leaf-results.dat" (glisp:temporary-folder))))
	       (with-format (base-format output-path)
		 (write-env (:a (the leaf-results))))
	       output-path))

  :objects ((kids :type 'base-object-tree
		  :sequence (:size (if (<= (the depth) (the max-depth))
				       (the branch-factor) 0))
		  :depth (1+ (the depth))
		  :pass-down (max-depth branch-factor))))



(define-object base-object-tree-noseq ()

  :input-slots ((depth 0) (max-depth 6) (branch-factor 5))


  :computed-slots ((depth*index (* (or (the index) 0) (the depth)))

		   (leaf-results (mapsend (the leaves) :depth*index)))


  :functions ((write-results 
	       (&key (output-path (merge-pathnames "leaf-results.dat" (glisp:temporary-folder))))
	       (with-format (base-format output-path)
		 (write-env (:a (the leaf-results))))
	       output-path))

  :objects ((kid-1 :type (if (<= (the depth) (the max-depth)) 'base-object-tree-noseq 'null-part)
		   :depth (1+ (the depth))
		   :pass-down (max-depth branch-factor))
	    (kid-2 :type (if (<= (the depth) (the max-depth)) 'base-object-tree-noseq 'null-part)
		   :depth (1+ (the depth))
		   :pass-down (max-depth branch-factor))
	    (kid-3 :type (if (<= (the depth) (the max-depth)) 'base-object-tree-noseq 'null-part)
		   :depth (1+ (the depth))
		   :pass-down (max-depth branch-factor))
	    (kid-4 :type (if (<= (the depth) (the max-depth)) 'base-object-tree-noseq 'null-part)
		   :depth (1+ (the depth))
		   :pass-down (max-depth branch-factor))
	    (kid-5 :type (if (<= (the depth) (the max-depth)) 'base-object-tree-noseq 'null-part)
		   :depth (1+ (the depth))
		   :pass-down (max-depth branch-factor))))

	    

		  
(defparameter *obj* nil)
(defparameter *arr* nil)

;;
;; 97656 total nodes.
;;				   
(defun bot-run ()
  (let (initial-memory delta  leaf-count)
    
    (setq initial-memory (getf (gwl:room-report t) :MB-int))

    (setq *arr* (make-obj-array))

    (setq delta (- (getf (gwl:room-report t) :MB-int) initial-memory))

    (format t "After 97656 standalone objects in array: ~a MB~%" delta)
    (setq *arr* nil)
    1 2 3 4 5
    (setq delta (- (getf (gwl:room-report 3) :MB-int) initial-memory))
    (format t "After clearing *arr*: ~a MB~%" delta)
    

    (setq *obj* (make-object 'base-object-tree))
    
    ;;(maptree *obj* #'(lambda(obj) (declare (ignore obj)) (incf object-count)))
    ;;(format t "~a total nodes.~%" object-count)
    (format t "Demanding leaves with dep tracking...~%")
    (setq leaf-count (length (the-object *obj* leaves-uncached)))
    (setq delta (- (getf (gwl:room-report t) :MB-int) initial-memory))
    (format t "After ~a leaves with dep tracking: ~a MB~%" leaf-count delta)
    (setq *obj* nil)
    1 2 3 4 5
    (setq delta (- (getf (gwl:room-report 3) :MB-int) initial-memory))
    (format t "After clearing *obj*: ~a MB~%" delta)
    
    
    (let ((*run-with-dependency-tracking?* nil))
      (setq *obj* (make-object 'base-object-tree))
      (setq initial-memory (getf (gwl:room-report t) :MB-int))
      ;;(maptree *obj* #'(lambda(obj) (declare (ignore obj)) (incf object-count)))
      ;;(format t "~a total nodes.~%" object-count)
      (format t "Demanding leaves without dep tracking...~%")
      (setq leaf-count (length (the-object *obj* leaves-uncached)))
      (setq delta (- (getf (gwl:room-report t) :MB-int) initial-memory))
      (format t "After ~a leaves without dep tracking: ~a MB~%" leaf-count delta)
      (setq *obj* nil)
      (setq delta (- (getf (gwl:room-report 3) :MB-int) initial-memory))
      (format t "After clearing *obj*: ~a MB~%" delta))))


(defun make-obj-array (&key (size 97656))
  (let ((array (make-array size)))
    (dotimes (n (length array))
      (setf (aref array n) (make-object 'base-object-tree)))
    array))