;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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


(in-package :tasty)



(define-object viewport (base-ajax-graphics-sheet)
  
  :input-slots
  (tatu-root root-object 
	     
	     (vector-graphics-onclick? (not (eql (the digitation-mode) :select-object)))
	     
	     (color nil)
	     
	     (line-thickness-selected nil)
	     
	     onclick-function
	     
	     (inspected-object nil)
	     
	     (display-list-object-roots 
	      (let (result)
		;;(the recompute?)
		(maphash #'(lambda(root-path value)
			     (declare (ignore value))
			     (push (if (listp root-path)
				       (the root-object (follow-root-path root-path))
				       root-path) result))
			 (the view-object-roots-root-paths-hash)) (nreverse result)))
	     
	     
	     (display-list-objects 
	      (let (result)
		(the recompute?)
		(maphash #'(lambda(root-path value)
			     (declare (ignore value))
			     (push (the root-object (follow-root-path root-path)) result))
			 (the view-objects-root-paths-hash)) (nreverse result)))
	     
	     (image-format-default :raphael)
	     
	     )

  
  
  :computed-slots
  (;;(dom-id "ta2viewport")
   
   (use-bsplines? (the use-bsplines-checkbox value))
   
   #+nil
   (js-to-eval (when (eql (the image-format) :raphael)
                 (the raphael-string)))

   
   (recompute? nil :settable)
   
   (respondent (the tatu-root))
   
   

   ;;
   ;; FLAG -- consider changing back to a list so we can preserve ordering.
   ;;
   (view-object-roots-root-paths-hash (make-hash-table :test #'equalp 
                                                       #+allegro :values #+allegro nil) :settable)

   

   
   (view-objects-root-paths-hash (make-hash-table :test #'equalp 
                                                  #+allegro :values #+allegro nil) :settable)
   

   

   (display-controls-root-path-hash (make-hash-table :test #'equalp) :settable)
   
   (display-controls-hash (let ((ht (make-hash-table)))
                            (maphash #'(lambda(root-path plist)
                                         (setf (gethash (the root-object (follow-root-path root-path)) ht) plist))
                                     (the display-controls-root-path-hash)) ht))
   
   
   (operations-list nil :settable)
   
   
   )
  
  
  
  :hidden-objects
  ((clear-button :type 'button-form-control 
                 :label "Clear!"
                 :onclick (the (gdl-ajax-call :function-key :clear!)))
   
   (use-bsplines-checkbox :type 'checkbox-form-control
                          :default *use-bsplines-for-vrml?*
                          :prompt "Use Bsplines for VRML?")
   
   )

  
  :functions
  (
   ;;
   ;; FLAG -- consider changing back to list here and on add-node!, to
   ;; preserve ordering.
   ;;
   (add-leaves!
    (object) 
    
    (the (add-display-controls! object))
    (let ((hash (the view-object-roots-root-paths-hash))
	  (root-path (the-object object root-path)))
      
      (the (set-slot! :view-object-roots-root-paths-hash
		      (progn (unless (second (multiple-value-list (gethash root-path hash)))
			       (setf (gethash root-path hash) t)) hash))))
    
    (the (set-slot! :operations-list 
		    (append (the operations-list)
			    (list (list :operation :add-leaves :object object))))))
   
   
   (enter-debugger!
    ()
    
    (when (typep (the image-url) 'error)
      (let ((object (getf (lastcar (the operations-list)) :object)))
        (set-self object)
        (let ((*package* (symbol-package (the-object object root type))))
          
          (let ((no-trap? (member :notrap net.aserve::*debug-current*)))
            (unless no-trap? 
              (net.aserve::debug-on :notrap)
              (format t "~&Disabling AllegroServe error trapping (i.e. enabling debugging) 
  -- you can re-enable it with (net.aserve::debug-off :notrap)~%")
              (net.aserve::debug-on :notrap)))
          
          (break (the image-url))))))
   
   
   (pop-operation!
    ()
    (let ((item (lastcar (the operations-list))))
      (let ((operation (getf item :operation))
            (object (getf item :object)))
        (ecase operation
          (:add-leaves (the (delete-leaves! object)))
          (:add-leaves* (dolist (leaf (the-object object leaves))
                          (the (delete-leaves! leaf))))
          (:add-node (the (delete-node! object))))))
    (the (set-slot! :operations-list (butlast (the operations-list)))))

   
   
   (add-leaves*!
    (object)
    (the (add-display-controls! object))
    (dolist (leaf (the-object object leaves)) (the (add-node! leaf)))
    (the (set-slot! :operations-list 
                    (append (the operations-list)
                            (list (list :operation :add-leaves* :object object))))))
   
   
   (leaves-displayed? 
    (object)
    (gethash (the-object (first (the-object object leaves)) root-path)
	     (the view-objects-root-paths-hash)))

   (toggle-leaves*!
    (object)
    (if (the (leaves-displayed? object))
	(the (add-leaves*! object))
	(the (delete-leaves! object))))
   
   (delete-node!
    (object)
    (let ((display-controls-hash (the display-controls-root-path-hash))
          (root-path (the-object object root-path)))

      (the (clear-from-hash! :view-objects-root-paths-hash root-path))
      
      (the (set-slot! :display-controls-root-path-hash
                      (progn
                        (maphash #'(lambda(key val)
                                     (declare (ignore val))
                                     (when (and (<= (length root-path) (length key))
                                                (equalp (reverse root-path) (subseq (reverse key) 0 
                                                                                    (length root-path))))
                                       (remhash key display-controls-hash))) display-controls-hash) 
                        display-controls-hash)))))
   
   (delete-leaves!
    (object)
    (let ((display-controls-hash (the display-controls-root-path-hash))
          (root-path (the-object object root-path)))

      (dolist (hash-slot-key (list :view-object-roots-root-paths-hash
                                   :view-objects-root-paths-hash))
        (the (clear-from-hash! hash-slot-key root-path)))
      
      (the (set-slot! :display-controls-root-path-hash
                      (progn
                        (maphash #'(lambda(key val)
                                     (declare (ignore val))
                                     (when (and (<= (length root-path) (length key))
                                                (equalp (reverse root-path) (subseq (reverse key) 0 
                                                                                    (length root-path))))
                                       (remhash key display-controls-hash))) display-controls-hash) 
                        display-controls-hash)))))

   (clear-from-hash!
    (slot-key root-path)
    (let ((hash (the (evaluate slot-key))))
      (the (set-slot! slot-key
                      (progn
                        (maphash #'(lambda(key val)
                                     (declare (ignore val))
                                     (when (or (and (<= (length root-path) (length key))
                                                    (equalp (reverse root-path) 
                                                            (subseq (reverse key) 0 (length root-path)))))
                                       (remhash key hash))) hash) hash)))))
   
   (draw-leaves! 
    (object)     
    (the clear!) (the (add-leaves! object)))
   
   (add-node!
    (object)     
    (the (add-display-controls! object))
    (let ((hash (the view-objects-root-paths-hash))
          (root-path (the-object object root-path)))
      (the (set-slot! :view-objects-root-paths-hash
                      (progn (setf (gethash root-path hash) t) hash))))
    (the (set-slot! :operations-list 
                    (append (the operations-list)
                            (list (list :operation :add-node :object object))))))

   (draw-node! 
    (object)     
    (the clear!) (the (add-node! object)))
   
   (add-display-controls!
    (object)
    (let ((hash (the display-controls-root-path-hash))
          (root-path (the-object object root-path)))
      (the (set-slot! :display-controls-root-path-hash
                      (progn (let ((color (the color))
                                   (line-thickness (the line-thickness-selected)))
                               
                               (if (or color line-thickness)
                                   (setf (gethash root-path hash) 
					 (list :line-thickness line-thickness
					       :color (the color)))
				   (when (gethash root-path hash) (remhash root-path hash))))
                             hash)))))
   
   (clear! () 
           (the (clear-display-controls!)) (the (clear-view-object-roots!)) (the (clear-view-objects!)))
   
   (clear-display-controls!
    () (let ((hash (the display-controls-root-path-hash)))
         (the (set-slot! :display-controls-root-path-hash (progn (clrhash hash) hash)))))
   
   (clear-view-object-roots!
    () (let ((hash (the view-object-roots-root-paths-hash)))
         (the (set-slot! :view-object-roots-root-paths-hash (progn (clrhash hash) hash)))))
   
   (clear-view-objects!
    () (let ((hash (the view-objects-root-paths-hash)))
         (the (set-slot! :view-objects-root-paths-hash (progn (clrhash hash) hash)))))
   
   
   
   (set-object-to-inspect!
    (object)
    (the inspector (set-object! object)))
   
   ;;
   ;; FLAG! -- SvdE @ 14-08-09 -- new functionality to be added for tasty beta release:
   ;; refresh viewport by redrawing all nodes from
   ;; :view-objects-root-paths-hash with default display-controls. Button in menu.lisp is present and linked.
   ;;
   (set-display-defaults!
    ()
    )
   
   ))


(define-lens (html-format viewport)()
  :output-functions
  (
   (inner-html
    ()

    (with-cl-who ()
      (let ((*display-controls* (the display-controls-hash)))
        (let ((image-format (if (the 2d-boxes?) (the image-format) :png)))

          (ecase image-format
            (:links (the (write-geometry-links)))
            (:raphael (write-the vector-graphics))
            ((:png :jpeg) (write-the raster-graphics))
            (:vrml (the (write-embedded-vrml-world :include-view-controls? nil)))
            (:x3d (the (write-embedded-x3d-world :include-view-controls? nil)))
            (:x3dom (the (write-embedded-x3dom-world :include-view-controls? nil))))))))))








