(in-package :ta2)


(defparameter *trap-errors?* t)

(define-object object-tree (base-html-sheet)
  :input-slots
  (root-object tatu-root display-controls-hash click-mode (initial-depth 2))
  
  :computed-slots ((expand-mode :children :settable)
                   
                   (dom-id "ta2tree")
                    

                   
                   )
  
  :hidden-objects
  ((tree-root :type 'tree-node
              :tree-toplevel self
              :pass-down (tatu-root display-controls-hash click-mode expand-mode initial-depth)
              :node (the root-object)))
  
  :functions
  ((reset-recompute! () 
                     (the tree-root reset-recompute!))))
                                 

(define-lens (html-format object-tree)()
  :output-functions
  ((main-view
    ()
    (with-html-form (:name "tree-form" :on-submit "return false;")
      ((:table  
        :width "100%"
        :height "100%"
        :bgcolor (gethash :grey *color-table*)
        :cellpadding "0" :cellspacing "0" :border "0"
                   
        )
           
       (:tr ((:td :valign :top )
             
             (if (the root-object)
                 (write-the tree-root main-view)
               (html (:b (:big "No Current Instance"))))))
           
           
       (:tr ((:td :valign :top)
             (:table (:tr ((:td :if* (eql (the expand-mode) :leaves) :bgcolor :yellow)
                           (if (eql (the expand-mode) :leaves) (html (:b "L"))
                             (html ((:input :type :submit  :value "L"
                                            :onclick (format nil "return ta2setexpandmode('~a', ':leaves');" (the instance-id))
                                            :title "Set Tree Expand Mode to \"leaves\".")))))
                          ((:td :if* (eql (the expand-mode) :children) :bgcolor :yellow)
                           (if (eql (the expand-mode) :children) (html (:b "C"))
                             (html ((:input :type :submit  :value "C"
                                            :onclick (format nil "return ta2setexpandmode('~a', ':children');" (the instance-id))
                                            :title "Set Tree Expand Mode to \"children\".")))))
                          ((:td :if* (eql (the expand-mode) :remember) :bgcolor :yellow)
                           (if (eql (the expand-mode) :remember) (html (:b "R"))
                             (html ((:input :type :submit  :value "R"
                                            :onclick (format nil "return ta2setexpandmode('~a', ':remember');" (the instance-id))
                                            :title "Set Tree Expand Mode to \"remember\".")))))
                          ((:td :if* (eql (the expand-mode) :auto-close) :bgcolor :yellow)
                           (if (eql (the expand-mode) :auto-close) (html (:b "A"))
                             (html ((:input :type :submit  :value "A"
                                            :onclick (format nil "return ta2setexpandmode('~a', ':auto-close');" (the instance-id))
                                            :title "Set Tree Expand Mode to \"auto-close\".")))))
                          )))))))))

;;
;; FLAG -- why does this need base-html-sheet?
;;
(define-object tree-node (base-html-sheet)
  :input-slots
  (node tatu-root tree-toplevel (depth 0) (total-depth (the remaining-depth))
   initial-depth display-controls-hash click-mode expand-mode (visible-child? nil))

  :computed-slots
  (
   (recompute? nil :settable)
   (respondent (the tatu-root))
   (closed? (>= (the depth) (the initial-depth)) :settable)
   (open? (not (the closed?)))
   (remaining-depth (if (or (zerop (the nodes number-of-elements)) (the closed?)) 0
                      (1+ (apply #'max (mapsend (list-elements (the :nodes)) :remaining-depth)))))
   
   (tatu-color (or (getf (the local-display-controls) :color)
                   (the action-object color-hex)))
   
   
   (tatu-line-thickness 
    (or (getf (the local-display-controls) :line-thickness) 
        (or (ignore-errors (the node line-thickness)) 1)))
   
   (local-display-controls 
    (or (gethash (the node) (the display-controls-hash))
        (when (typep (the parent) 'tree-node) (the parent local-display-controls))))
   
   
   (kids-error (when (typep (the safe-children) 'error) (the safe-children)))
   
   
   (safe-children (append (if *trap-errors?* 
                              (the node safe-children)
                            (the node children))
                          (the node visible-children)))
   
   
   (safe-child-first (length (the node safe-children)))
   
   )
   
   
  ;;
  ;; FLAG -- add back support for visible-children.
  ;;
  :hidden-objects
  ((nodes :type 'tree-node
          :sequence (:size (progn (the recompute?) (length (the safe-children))))
          :pass-down (tatu-root tree-toplevel total-depth display-controls-hash click-mode expand-mode initial-depth)
          :visible-child? (>= (the-child index) (the safe-child-first))
          :depth (1+ (the depth))
          :node (let ((node (nth (the-child index) (the safe-children))))
                  (if (listp node)
                      (make-object 'ta2-child-error
                                   :error (getf node :error)
                                   :object-key (getf node :object-key))
                    node)))
   
   (action-object :type 'action-object
                  :pass-down (node tatu-root tatu-color click-mode kids-error)))
  
  :functions
  ((reset-recompute!
    ()
    (the (set-slot! :recompute? (not (the recompute?))))
    (mapc #'(lambda(child) (the-object child reset-recompute!)) (list-elements (the nodes))))
   
   (open-all!
    ()
    (the (set-slot! :closed? nil))
    (mapc #'(lambda(node) (the-object node open-all!)) (list-elements (the nodes))))
   
   
   (close-all!
    ()
    (the (set-slot! :closed? t))
    (mapc #'(lambda(node) (the-object node close-all!)) (list-elements (the nodes))))
   
   (expand-parents!
    ()
    (when (and (the parent)
               (typep (the parent) 'tree-node))
      (the parent (set-slot! :closed? nil))
      (the parent expand-parents!)))
   
   
   (toggle-state!
    ()
    (the (set-slot! :closed? (the open?)))
    (the propogate-toggle!))
                 
   
   (propogate-toggle! 
    ()
    
    (when (eql (the expand-mode) :auto-close) (the tree-toplevel tree-root close-all!))
    
    (ecase (the expand-mode)
      (:children (mapc #'(lambda(node) (when (null (the-object node closed?))
                                         (the-object node (set-slot! :closed? t))))
                       (list-elements (the nodes))))
      (:leaves (mapc #'(lambda(node) (the-object node open-all!)) (list-elements (the nodes))))
      ((:remember :auto-close)))
    
    (when (eql (the expand-mode) :auto-close) (the expand-parents!))
    
    )))


(defparameter *icon-hash* nil)

(define-lens (html-format tree-node)()
  :output-functions
  ((main-view 
    ()
    (let ((*icon-hash* (make-hash-table)))
      (html ((:table :border "0" 
                     :cellpadding "0" :cellspacing "0") (write-the (row-view))))))
   
   (row-view 
    ()
    (html (:tr
           (let ((depth (the :depth)))
             
             (dotimes (n depth)
               (cond ((and (= n (1- depth)) (the :last?))
                      (setf (gethash n *icon-hash*) :l)
                      (html ((:td  :height 20 :width 20) ((:img :src "/images/gwl/l-t.gif")))))
                     ((= n (1- depth))
                      (setf (gethash n *icon-hash*) :t)
                      (html ((:td  :height 20 :width 20) ((:img :src "/images/gwl/t-t.gif")))))
                     ((member (gethash n *icon-hash*) (list :i :t))
                      (setf (gethash n *icon-hash*) :i)
                      (html ((:td  :height 20 :width 20) ((:img :src "/images/gwl/i-t.gif")))))
                     (t (html ((:td  :height 20 :width 20) :br))))))
           
           (if (not (zerop (the nodes number-of-elements)))
               (html ((:td :height 20 :width 20)
                      ((:span :style "cursor: pointer;" 
                              :onclick (format nil "return ta2treetoggle('~a', '~a');" 
                                               (the instance-id) 
                                               (base64-encode-safe (format nil "~s" (the root-path-local)))))
                       ((:img :src (cond ((the :closed?) "/images/gwl/plus.gif")
                                         ((the :open?) "/images/gwl/minus.gif"))
                              :border 0
                              :title (cond ((the :closed?) "Expand") ((the :open?) "Contract")))))))
             (html ((:td :height 20 :width 20)
                    ((:span :style "cursor: pointer;" 
                            :onclick (format nil "return ta2treetoggle('~a', '~a');" (the instance-id) 
                                             (base64-encode-safe (format nil "~s" (the root-path-local)))))
                     (if (the kids-error)
                         (html
                          ((:img :src "/images/gwl/nonplus.png"
                                 :border 0
                                 :title (format nil "Cannot Expand: ~a" (the kids-error)))))
                       (html ((:img :src "/images/gwl/leafbutn.gif"
                                    :border 0))))))))
           

           ((:td :colspan (1+ (- (the :total-depth) (the :depth))) :nowrap :nowrap)
             
            ((:span :style "cursor: pointer;" 
                    :onclick (unless (eql (the click-mode) :ui)
                               (format nil "return ta2operate('~a', '~a');"
                                       (the instance-id)
                                       (base64-encode-safe (format nil "~s" (the action-object node root-path))))))
             (:princ (with-output-to-string(ss)
                       (let* ((simple-link? (and (eql (the click-mode) :ui)
                                                 (typep (the action-object node) 'base-html-sheet)))
                              (text (if simple-link?
                                        (with-output-to-string(sss)
                                          (html-stream 
                                           sss
                                           ((:a :href (the action-object node url))
                                            (:princ (the action-object strings-for-display)))))
                                      (the action-object strings-for-display))))
                         (html-stream ss ((:font :color (or (the tatu-color) :black))
                                          (if (>= (the tatu-line-thickness) 2)
                                              (html (:b (if simple-link?
                                                            (html (:princ text))
                                                          (html (:princ-safe text)))))
                                            (let ((visible-child? (the visible-child?)))
                                              (when visible-child? (html "["))
                                              (if simple-link?
                                                  (html (:princ text))
                                                (html (:princ-safe text)))
                                              (when visible-child? (html "]"))))))))))))

          (when (the :open?)
            (mapc #'(lambda (node) (write-the-object node (row-view)))
                  (list-elements (the :nodes))))))))
           

(define-object ta2-child-error ()
  :input-slots (error object-key)
  
  :computed-slots ((strings-for-display (format nil "~a threw error: ~a" (the object-key) (the error)))))


