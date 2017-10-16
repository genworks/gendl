;;
;; Copyright 2002-2011 Genworks International 
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

(in-package :gwl)


(defmacro with-all-servers ((server) &rest body)
  (let ((server-var (gensym)))
    `(dolist (,server-var (list *wserver* *ssl-server*))
       (when ,server-var
         (let ((,server ,server-var)) ,@body)))))

(define-object form-mixin ())


(define-lens (html-format form-mixin)()
  :output-functions
  ((:form 
    (slot-names &key (size-default 5)
                (sizes (make-list (length slot-names) :initial-element size-default))
                (prompts (mapcar #'(lambda(slot-name) (symbol-name slot-name)) slot-names))
                (cellpadding 0)
                (heading-color :yellow))
    (html 
     (:p
      ((:table :cellpadding cellpadding)
       (:tr ((:th :colspan 2)  "Dimensions"))
       (mapc #'(lambda(slot-name prompt size)
                 (html (:tr ((:td :bgcolor heading-color) (:princ (string-capitalize prompt)))
                            (:td ((:input :type :text :name (symbol-name slot-name) :size size 
                                          :value (or (the (evaluate slot-name)) ""))))))) slot-names prompts sizes)))
     (:p ((:input :type :submit :name :subbmit :value " OK ")))))))
    



(defmacro html-form (slot-names 
                     &key (size-default 5)
                          (sizes (make-list (length slot-names) :initial-element size-default))
                          (prompts (mapcar #'(lambda(slot-name) (symbol-name slot-name)) slot-names))
                          (cellpadding 0)
                          (heading-color :yellow))
  `(html 
    (:p
     ((:table :cellpadding ,cellpadding)
      (:tr ((:th :colspan 2)  "Dimensions"))
      ,@(mapcar #'(lambda(slot-name prompt size)
                    `(:tr ((:td :bgcolor ,heading-color) (:princ ,(string-capitalize prompt)))
                          (:td ((:input :type :text :name ,(symbol-name slot-name) :size ,size 
                                        :value (or (the ,slot-name) "")))))) slot-names prompts sizes)))
    (:p ((:input :type :submit :name :subbmit :value " OK ")))))



(defmacro with-html-form ((&key name id  multipart? enctype target requestor on-submit suppress-border? local-anchor cl-who?) 
                          &body body)
  
  "Enclose a body of code with a form.

FLAG -- fill in.


"
  
  (let ((%enctype% (gensym))
        (fixed-prefix (gensym)))
    `(let ((,%enctype% (cond (,enctype ,enctype) (,multipart? "multipart/form-data")))
           (,fixed-prefix (let ((prefix (the fixed-url-prefix)))
			    (when prefix (string-append "/" prefix)))))
       (,@(if cl-who? '(with-html-output (*stream* nil :indent t)) 
            '(html 
              ;;html-stream *stream*
              
              ))
          ((:form :method :|post| 
                  :id ,id
                  :action (string-append (or ,fixed-prefix "")
                                 (format nil (if ,local-anchor (format nil "/answer#~a" ,local-anchor) "/answer")))
                  
                  :name ,(or name `(the root-path-string))
                  ,@(if cl-who? `(:enctype ,%enctype%) `(:if* ,%enctype% :enctype ,%enctype%))
                  ,@(if cl-who? `(:target ,target) `(:if* ,target :target ,target))
                  ,@(if cl-who? `(:on-submit ,on-submit) `(:if* ,on-submit :onsubmit ,on-submit))
                  ,@(if cl-who? `(:style ,(when suppress-border? "padding: 0; border: 0; margin: 0"))
                      `(:if* ,suppress-border? :style "padding: 0; border: 0; margin: 0")))
           
           ((:input :type :hidden :name :|requestor| :value ,(if (null requestor) `(the url-encoded-root-path)
                                                             `(the-object ,requestor url-encoded-root-path))))
           
           ((:input :type :hidden :name :|iid| :value (the instance-id))) ,@body)))))


#+nil
(defmacro with-html-form ((&key name multipart? enctype target requestor on-submit suppress-border? local-anchor cl-who?) 
                          &body body)
  
  "Enclose a body of code with a form.

FLAG -- fill in.


"

  
  
  (let ((%enctype% (gensym))
        ;;(fixed-prefix (gensym))
	)
    `(let ((,%enctype% (cond (,enctype ,enctype) (,multipart? "multipart/form-data")))
           ;;(,fixed-prefix (the fixed-url-prefix))
	   )

       (,@(if cl-who? '(with-html-output (*stream* nil :indent t)) 
            '(html 
              ;;html-stream *stream*
              
              ))
          ((:form :method :|post| 
                  
                  :action (string-append "" ;;(or ,fixed-prefix "")
                                 (format nil (if ,local-anchor (format nil "/answer#~a" ,local-anchor) "/answer")))
                  
                  :name ,(or name `(the root-path-string))
                  ,@(if cl-who? `(:enctype ,%enctype%) `(:if* ,%enctype% :enctype ,%enctype%))
                  ,@(if cl-who? `(:target ,target) `(:if* ,target :target ,target))
                  ,@(if cl-who? `(:on-submit ,on-submit) `(:if* ,on-submit :onsubmit ,on-submit))
                  ,@(if cl-who? `(:style ,(when suppress-border? "padding: 0; border: 0; margin: 0"))
                      `(:if* ,suppress-border? :style "padding: 0; border: 0; margin: 0")))
           
           ((:input :type :hidden :name :|requestor| :value ,(if (null requestor) `(the url-encoded-root-path)
                                                             `(the-object ,requestor url-encoded-root-path))))
           
           ((:input :type :hidden :name :|iid| :value (the instance-id)))

	   ,@body)))))

        
  
  


