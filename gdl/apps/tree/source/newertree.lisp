;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(in-package :tree)

(defparameter *trap-errors?* t)

(define-object newertree (sheet-section)
  
  :input-slots
  (("Function of one argument. This function takes a node in the tree as an argument, and should return
  a plist with keys :function and :arguments, which is a function in the bashee which will be called
  with the given arguments when the given node in the tree is clicked."
    onclick-function nil)
   (respondent (the parent))
   (tree-dom-id (the dom-id))
   (target-object self)
   
   (display-controls-hash nil)
   
   (%tree-root% self)

   (expand-mode :remember :settable)
   
   (show-onmouseover-buttons? t)
   
   )

    
  :computed-slots 
  (
   ;;
   ;; FLAG -- to skeleton-ui-mixin and out of base-html-sheet.
   ;;
   (base64-encoded-root-path (base64-encode-safe (format nil "~s" (the root-path))))
   (dom-id (the base64-encoded-root-path))
   ;;
   ;; END FLAG
   ;;
   
   (tatu-color (or (when (getf (the local-display-controls) :color)
                     (string-append "#" (getf (the local-display-controls) :color)))
                   (multiple-value-bind (color error)
                       (ignore-errors (the target-object color-hex))
                     (if (typep error 'error) (lookup-color :red :format :hex) color))))
                   
   
   
   (tatu-line-thickness (or (getf (the local-display-controls) :line-thickness)
                            (or (ignore-errors (the target-object line-thickness)) 1)))

   
   (local-display-controls (gethash (the target-object) (the display-controls-hash)))

   (kids-error (when (typep (the safe-children) 'error) (the safe-children)))
   
   (safe-children (append (if *trap-errors?* 
                              (the target-object safe-children)
                              (the target-object children))
                          (the target-object visible-children)))

   ;;
   ;; DJC - try to correct error reported 2011-03-30 by SvdE
   ;;
   (safe-strings-for-display 
    (let ((string (multiple-value-bind (strings error)
                      (ignore-errors (the target-object 
                                       strings-for-display))
                    (cond ((typep error 'error)
                           (format nil "! ~a ! from strings-for-display" error))
                          ((the color-error?)
                           (format nil "~a ! ~a ! from color-hex" strings 
                                   (the color-or-error)))
                          (t strings)))))
      (if (stringp string) string 
        (progn
          (warn "safe-strings-for-display returned non-string: ~s. 

Converting to a string to avoid error."
                string)
          (format nil "~s" string)))))

   ;;
   ;; DJC - try to correct error reported 2011-03-30 by SvdE
   ;;
   #+nil
   (safe-strings-for-display (multiple-value-bind (strings error)
                                  (ignore-errors (the target-object strings-for-display))
                                (cond ((typep error 'error)
                                       (format nil "! ~a ! from strings-for-display" error))
                                      ((the color-error?)
                                       (format nil "~a ! ~a ! from color-hex" strings (the color-or-error)))
                                      (t strings))))

   (color-or-error (multiple-value-bind (color error)
                       (ignore-errors (the target-object color-hex))
                     (if (typep error 'error) error color)))
   
   (color-error? (typep (the color-or-error) 'error))
   
   (color-hex (if (the color-error?) (lookup-color :red :format :hex) (the color-or-error)))
   
   (safe-child-first (length (the target-object safe-children)))
   
   (closed? (not (eql self (the %tree-root%))) :settable) 
   (open? (not (the closed?)))
   
   (child-nodes (list-elements (the nodes)))
   
   (inner-html
    (with-cl-who-string ()
      ((:ul :id "tree-root" :class "tasty-tree") 
       (str (the tree-node-view)))))
   
   
   (tree-node-view
    (with-cl-who-string ()
  
      ((:li)
       ;;(:li :id "tree-node" :class "")
       
       (when (the child-nodes)
         (if (the closed?)
             (progn (htm ((:span 
                           :class "directory" 
                           :title "Click to expand ..."
                           :onclick (the (gdl-ajax-call :function-key :toggle-state!))))))
             (progn (htm ((:span 
                           :class "expanded" 
                           :onclick (the (gdl-ajax-call :function-key :toggle-state!))))))
             ))  
       
       ;; JB-090810 when the node does not have a child node, it is a leaf - so display
       ;; a leaf graphic and this is coupled via CSS
       (unless (the child-nodes)
         (htm ((:span :class "leaf")))
         )
       
       ;; this is the name of the object including the gdlAjax call wrapped in a span
       ;; JB-090813 in order to let the actions hover when the mouse is over the
       ;; object name, and remove the actions imgs when the mouse is leaving the
       ;; object name (or the images) I need to wrap this in an extra span
       ;; to correct visual stuff this gets a class .container
       ;; this code depends heavily on the presence of jQuery as the show() and hide()
       ;; functions, including the selectors are jQuery functions       

       
       ((:span :class "container"
               ;; JB-090813 yep this is jQuery functionality
               :onmouseover "$(this).children('.actions').css('display','inline')"
               :onmouseout "$(this).children('.actions').hide()")
        
        ((:span :class "tree-node"
                :style (when (or (the tatu-color)
                                 (the tatu-line-thickness))
                         (format nil "~a~a" 
                                 (if (the tatu-color) (format nil " color: ~a; " (the tatu-color)) "")
                                 (let ((font-weight
                                        (cond 
                                         ((or (null (the tatu-line-thickness))
                                              (= (the tatu-line-thickness) 1) )400)
                                         ((minusp (the tatu-line-thickness)) 100)
                                         ((< (the tatu-line-thickness) 1) (* 400 (the tatu-line-thickness)))
                                         ((<= (the tatu-line-thickness) 3) (* 300 (the tatu-line-thickness)))
                                         (t 900))))
                                   (if font-weight (format nil " font-weight: ~a; " font-weight) ""))))
                                                                 
                :onclick (when (the onclick-function)
                           (funcall  (the onclick-function) (the target-object))))
         
         (esc (the safe-strings-for-display)))
       
        ;; here the intree command images are placed inside a span
        
        (when (the show-onmouseover-buttons?)
          
          (htm
           ((:span  :class "actions" 
                    :style "display:none;")
        
        
        
            ((:span :class "draw"
                    :onclick (the (gdl-ajax-call :function-key :set-and-call-click-mode! 
                                                 :arguments (list :add-leaves)))
                    :title "Draw this object (AL)"))
        
            ((:span :class "inspect"
                    :onclick (the (gdl-ajax-call :function-key :set-and-call-click-mode! 
                                                 :arguments (list :inspect)))
                    :title "Inspect this object"))
        
            ((:span :class "highlight"
                    :onclick (the (gdl-ajax-call :function-key :set-and-call-click-mode! 
                                                 :arguments (list :highlight)))
                    :title "Highlight this object")))))
       
       )
       
      ;; if there are siblings, make them here (inside the li)      
      (when (and (typep self 'newertree) 
                 (the open?)
                 (the child-nodes))
        (htm
         (:ul
          (mapc #'(lambda (tree-node) 
                    (str (the-object tree-node tree-node-view)))
                (the child-nodes)))))))))
 
  :functions
  (
   (toggle-state! ()
                  (the (set-slot! :closed? (not (the closed?))))
                  (the propogate-toggle!))
   
   (propogate-toggle! 
    ()
    
    (ecase (the expand-mode)

      (:auto-close (the %tree-root% (close-all! :except self))
         (the expand-parents!))
         
      
      (:children (mapc #'(lambda(node) (when (null (the-object node closed?))
                                         (the-object node (set-slot! :closed? t))))
                       (the child-nodes)))
      
      (:leaves (mapc #'(lambda(node) (the-object node open-all!)) 
                     (the child-nodes)))
      
      (:remember ;; do nothing
         ))
    
    )
   
   (open-all!
    ()
    (the (set-slot! :closed? nil))
    (mapc #'(lambda(node) (the-object node open-all!)) 
          (the child-nodes)))
   
   
   (close-all!
    (&key except)
    (unless (eql self except) (the (set-slot! :closed? t)))
    (mapc #'(lambda(node) (the-object node (close-all! :except except)))
          (the child-nodes)))
   
   (expand-parents!
    ()
    (when (and (the parent)
               (typep (the parent) 'newertree))
      (the parent (set-slot! :closed? nil))
      (the parent expand-parents!)))
   
   (set-and-call-click-mode!
    (mode)
    (ecase mode
      (:inspect (the inspector (set-object! (the target-object))))
      (:add-leaves (the viewport (add-leaves! (the target-object))))
      (:highlight        (if (the target-object highlighted?)
                             (progn
                               (the target-object (set-slot! :highlighted? nil))
                               (the viewport (add-leaves! (the target-object))))
                           (progn
                             (the target-object (set-slot! :highlighted? t))
                             (the viewport (add-leaves! (the target-object)))
                             ))))))
     
  
  :hidden-objects ((nodes :type 'newertree
                          :sequence (:size (length (the safe-children)))
                          :pass-down (%tree-root% respondent onclick-function expand-mode display-controls-hash
                                                  show-onmouseover-buttons?
                                                  )
                          :visible-child? (>= (the-child index) (the safe-child-first))
                          :depth (1+ (the depth))
                          :target-object (let ((node (nth (the-child index) (the safe-children))))
                                           (if (listp node)
                                               (make-object 'tasty-child-error
                                                            :error (getf node :error)
                                                            :object-key (getf node :object-key))
                                               node)))

                   #+nil
                   (nodes :type 'newertree
                          :sequence (:size (length (the target-object children)))
                          :pass-down (respondent onclick-function expand-mode)
                          :target-object (nth (the-child index) (the target-object children)))))

(define-object tasty-child-error ()
  :input-slots (error object-key)
  
  :computed-slots ((strings-for-display (format nil "~a threw error: ~a" (the object-key) (the error)))))
