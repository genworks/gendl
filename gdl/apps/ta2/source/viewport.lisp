(in-package :ta2)

(define-object viewport (gwl:base-html-graphics-sheet)
  :input-slots
  (tatu-root root-object color-selected line-thickness-selected  inspected-object)
  
  :computed-slots
  ((dom-id "ta2viewport")
   
   (recompute? nil :settable)
   
   (respondent (the tatu-root))
   
   (available-image-formats (list :png :jpeg :vrml :links))
   
   
   ;;
   ;; FLAG -- consider changing back to a list so we can preserve ordering.
   ;;
   (view-object-roots-root-paths-hash (make-hash-table :test #'equalp 
                                                       #+allegro :values #+allegro nil) :settable)
   (view-object-roots (let (result)
                        (the recompute?)
                        (maphash #'(lambda(root-path value)
                                     (declare (ignore value))
                                     (push (the root-object (follow-root-path root-path)) result))
                                 (the view-object-roots-root-paths-hash)) (nreverse result)))

   
   (view-objects-root-paths-hash (make-hash-table :test #'equalp 
                                                  #+allegro :values #+allegro nil) :settable)
   
   (view-objects (let (result)
                   (the recompute?)
                   (maphash #'(lambda(root-path value)
                                (declare (ignore value))
                                (push (the root-object (follow-root-path root-path)) result))
                            (the view-objects-root-paths-hash)) (nreverse result)))

   
   (display-controls-root-path-hash (make-hash-table :test #'equalp) :settable)
   
   (display-controls-hash (let ((ht (make-hash-table)))
                            (maphash #'(lambda(root-path plist)
                                         (setf (gethash (the root-object (follow-root-path root-path)) ht) plist))
                                     (the display-controls-root-path-hash)) ht))
   
   
   (operations-list nil :settable)
   
   )
  
  :hidden-objects
  ((view-object :type 'web-drawing
                ;;:dom-id (the dom-id)  FLAG -- this is self-computed now, I think. 
                :page-length (the length)
                :page-width (the width)
                :projection-vector (getf *standard-views* (the view))
                :objects (the view-objects)
                ;;:touched-geometry (the touched-geometry)
                :object-roots (the view-object-roots)
                :background-color :black
                :field-of-view-default (the field-of-view-default)
                :view (progn (when *debug?* (print-messages view)) (the view))

                ))
  
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
                                       
                                       (progn (format t "Removing ~s from ~s~%" root-path hash))
                                       
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
                      (progn (let ((color-decimal (gethash (the color-selected) *color-table-decimal*))
                                   (line-thickness (the line-thickness-selected)))
                               (if (or color-decimal line-thickness)
                                   (setf (gethash root-path hash) (list :color-decimal color-decimal :line-thickness line-thickness
                                                                        :color (the color-selected)))
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
         (the (set-slot! :view-objects-root-paths-hash (progn (clrhash hash) hash)))))))


(define-lens (html-format viewport)()
  :output-functions
  ((geometry-error
    ()
    (print-messages operations-list) 
    (html-stream 
     *stream*
     ((:table :border 1 :cellspacing 0 :cellpadding 0 :bgcolor :white)
      (:tr
       ((:td :width (the :view-object :width) :height (the :view-object :length))
             
        ((:span :style "color: red; font-style: italic") "Graphics Threw Error: " (:princ-safe (the image-url)))
             
        :br
             
        (let ((item (lastcar (the operations-list))))
          (let ((operation (getf item :operation))
                (object (getf item :object)))
            (html
             (:p :br "Operation: " (:prin1-safe operation)
                 :br "Object: "    (:prin1-safe object)
                 :br "Root-path: " (:prin1-safe (the-object object root-path)))
             (:p "Choices:"
                 (:ul
                  (:li ((:input :type :submit :value "Undo" :name :undo
                                :onclick (format nil "return ta2clickbutton('~a', '~a');" (the instance-id) :undo))))
                  (:li ((:input :type :submit :value "Debug" :name :undo
                                :onclick (format nil "return ta2clickbutton('~a', '~a');" (the instance-id) :debug))))))))))))))
   
   (main-view
    () 
    (with-html-form (:name "viewport-form" :on-submit "return false;")
      (let ((*display-controls* (the display-controls-hash)))     
        (ecase (the image-format)
          (:links (the (write-geometry-links)))
          ((:png :jpeg) (the (write-geometry :include-view-controls? t :use-ajax? t)))
          (:vrml (the (write-embedded-vrml-world :include-view-controls? t)))))))))


             
