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

;; FLAG -- remove this and see what breaks.
;;
(defparameter *try* nil)


(defvar *developing?* nil "Boolean. Used in some parts like application-mixin
and node-mixin, to decide whether to display developer links. Can also be used
by user application code.")

(defun query-arg-to-root-path (query arg &optional value)
  (let ((value (or value (rest (assoc arg query :test #'string-equal)))))
    (when value
      (setq value (glisp:replace-regexp  value "\\+" " "))
      (let (skip? 
            (components (read-from-string 
                         (string-append "(" value ")")))
            (result-list nil))
        (dotimes (position (length components) (nreverse result-list))
          (cond (skip? (setq skip? nil))
                ((consp (nth (1+ position) components))
                 (progn
                   (push (list (make-keyword (nth position components))
                               (first (nth (1+ position) components))) result-list)
                   (setq skip? t)))
                (t (push (make-keyword (nth position components)) result-list))))))))

(defun root-path-to-query-arg (root-path)
  (format nil "~{~(~a~)~^+~}"
          (mapcar #'(lambda(component)
                      (if (atom component) 
                          component 
                        (format nil "~a(~a)" (first component) (second component))))
                  root-path)))


;;
;; FLAG -- this is not thread-safe - you can get the same instance ID
;; if called simultaneously by two threads.
;;
;; FLAG -- we also have to protect against the (unlikely but possible)
;; case that the same number comes back twice within a second from the
;; random generator. Only way is to store the resutls and check
;; against previous results.
;;
;;
(defun make-new-instance-id (&key (max-value *max-id-value*))
  (let ((*print-base* 16) (universal-time (get-universal-time))
        (random (random max-value *iid-random-state*)))
    (let ((string (format nil "~a~a" random universal-time)))
      (if (and (glisp:featurep :allegro) 
	       (eql (symbol-value (read-from-string "excl:*current-case-mode*"))
		    :case-insensitive-upper))
	  (string-upcase string) string))))
         


(defun assoc-list-to-plist (assoc-list &key case-sensitive?)
  (mapcan #'(lambda(cons)
              (list (if case-sensitive? (make-keyword-sensitive (first cons))
			(make-keyword-sensitive (first cons)))
                    (rest cons)))
          assoc-list))


(defun merge-plist-duplicates (plist)
  (let ((keys (remove-duplicates (plist-keys plist))))
    (if (= (length keys) (half (length plist))) plist
      (mapcan #'(lambda(key)
                  (list key (let ((matches (find-plist-matches plist key)))
                              (if (consp (rest matches)) matches (first matches))))) keys))))

(defun find-plist-matches (plist key)
  (when plist 
    (if (eql key (first plist))
        (cons (second plist) (find-plist-matches (rest (rest plist)) key))
      (find-plist-matches (rest (rest plist)) key))))
                  


(define-format html-format (base-format)

  :functions
  ((:file-fields 
    (&key file-function-name file-function-args)
    (when file-function-name
      (let ((package (package-name (symbol-package file-function-name)))
            (name    (symbol-name file-function-name)))
        (html ((:input :type "hidden"
                       :name "file-function-name"
                       :value name))
              ((:input :type "hidden"
                       :name "file-function-package"
                       :value package)))))
    (when file-function-args
      (html ((:input :type "hidden" 
                     :name "file-function-args" 
                     :value (format nil "~{~a~^+~}" file-function-args))))))))



(define-lens (html-format base-html-sheet)()
  
  :output-functions
  ((local-development-links
    ()
    (with-html-output (*html-stream*)
      
      (:p (unless (null (the parent)) (html (write-the (local-update-link))  " | "))
          (write-the (local-update-full-link)) " | " 
          (write-the (local-break-link)) 
          ;;(write-the (tatu-link)) " | "
          ;;(when (find-package :ta2) (write-the (ta2-link)))
          
          )))
   
   (development-links
    ()
    (with-html-output (*html-stream*)
      
      (:p (unless (null (the parent)) (html (write-the (update-link))  " | "))
          (write-the (update-full-link)) " | " 
          (write-the (break-link)) " | "
          ;;(write-the (tatu-link)) " | "
          (when (find-package :ta2) (write-the (ta2-link))))))
   
   (update-link
    (&key (display-string "Update!"))
    (html (write-the :$$update (:self-link :display-string display-string))))
   
   (local-update-link
    (&key (display-string "Update!"))
    (html (write-the :$$update (:self-link :display-string display-string))))
   
   (update-full-link
    (&key (display-string "Full Update!"))
    (html (write-the :$$update-full (:self-link :display-string display-string))))
   
   (local-update-full-link
    (&key (display-string "Full Update!"))
    (html (write-the :$$update-full (:self-link :display-string display-string))))
   
   
   (break-link
    (&key (display-string "Break"))
    (html (write-the :$$break (:self-link :display-string display-string))))
   
   (local-break-link
    (&key (display-string "Break"))
    (html (write-the :$$break (:self-link :display-string display-string))))
   
   (tatu-link
    (&key (display-string "TaTu"))
    (write-the :$$tatu-object (:self-link :display-string display-string)))
   
   (ta2-link
    (&key (display-string "Ta2"))
    (write-the :$$ta2-object (:self-link :display-string display-string)))

   (self-link
    (&key (display-string (the strings-for-display))
          (display-color nil)
          (target nil)
          (title nil)
          class id
          on-click
          on-mouse-over on-mouse-out
          local-anchor)
    (html ((:a :href (if local-anchor (format nil "~a#~a" (the url) local-anchor) (the :url))
               :if* target :target target 
               :if* on-mouse-over :onmouseover on-mouse-over 
               :if* on-mouse-out :onmouseout on-mouse-out
               :if* title :title title
               :if* class :class class
               :if* on-click :onclick on-click
               :if* id :id id)
           (if display-color
               (html ((:font :color display-color) (:princ display-string)))
             (html (:princ display-string))))))
   
   (main-sheet
    ()
    (html (:html (:head (:title "Here is GWL!!"))
                 (:body 
                  (:center 
                   (:h1 "GWL")
                   (:p
                    (:princ "No particular HTML format method has been defined for:")
                    ((:table :border 1)
                     (:newline)
                     (:tr ((:td :bgcolor "yellow") "Instance") (:td (:princ-safe self)))
                     (:newline)
                     (:tr ((:td :bgcolor "yellow") "Type") (:td (:princ-safe (the :type))))
                     (:newline))))
                  (:p 
                   (if (the :children)
                       (html
                        (:princ "However, its children are as follows:")
                        (:ul)
                        (dolist (child (the :children))
                          (html
                           (:newline)
                           (:li (the-object child :write-self-link)))))
                     (html "This part has no children.")))))))))


(define-object color-palette ()
  :input-slots
  ((components (list "FF" "CC" "99" "66" "33" "00"))))



(define-lens (html-format color-palette)()
  :output-functions
  ((full-grid 
    (&key (cell-size 17) (layout :horizontal) target link-objects)
    (let ((colors (the components))
          (link-objects (coerce link-objects 'vector)) (count -1))
      (ecase layout 
        (:horizontal
         (html ((:table :border "0" :cellspacing "0" :cellpadding "0")
                (:tr (dolist (red colors)
                       (html (:td ((:table :border "0" :cellspacing "0" :cellpadding "0")
                                   (dolist (green colors)
                                     (html (:tr (dolist (blue colors)
                                                  (html ((:td :bgcolor (format nil "#~a~a~a" red green blue) ;;:height cell-size :width cell-size
                                                              :style (format nil "height: ~apx; width: ~apx;" cell-size cell-size))
                                                         (when (not (zerop (length link-objects)))
                                                           (the-object (svref link-objects (incf count))
                                                                       (write-self-link 
                                                                        :target target
                                                                        :title (format nil "#~a~a~a" red green blue)
                                                                        :on-mouse-over (format nil "window.status='Set Color mode to RGB Hex value: ~a.'; return true;" 
                                                                                               (format nil "#~a~a~a" red green blue))
                                                                        :on-mouse-out  "window.status=''; return true;"

                                                                        :display-string 
                                                                        (with-output-to-string(ss)
                                                                          (html-stream ss ((:font :size (div cell-size 15) 
                                                                                                  :color  (format nil "#~a~a~a" red green blue)) 
                                                                                           (:small "m")))))))))))))))))))))
        (:vertical 
         (html 
          ((:table :border 0 :cellspacing 1 :cellpadding 0)
           (:tr 
            ((:td :valign :top)
             ((:table :border 0 :cellspacing 1 :cellpadding 0)
              (dolist (red (subseq colors 0 (ceiling (half (length colors)))))
                (html 
                 (:tr 
                  (:td ((:table :border 0 :cellspacing 1 :cellpadding 0)
                        (dolist (green colors)
                          (html (:tr (dolist (blue colors)
                                       (html ((:td :bgcolor (format nil "#~a~a~a" red green blue) :height cell-size :width cell-size)
                                              (when (not (zerop (length link-objects)))
                                                (the-object (svref link-objects (incf count))
                                                            (write-self-link 
                                                             :target target
                                                             :on-mouse-over (format nil "window.status='Set Color mode to RGB Hex value: ~a.'; return true;" 
                                                                                    (format nil "#~a~a~a" red green blue))
                                                             :on-mouse-out  "window.status=''; return true;"

                                                             :display-string 
                                                             (with-output-to-string(ss)
                                                               (html-stream ss ((:font :size (div cell-size 15) 
                                                                                       :color  (format nil "#~a~a~a" red green blue)) 
                                                                                (:small (:small "m")))))))))))))))))))))
            ((:td :valign :top)
             ((:table :border 0 :cellspacing 1 :cellpadding 0)
              (dolist (red (subseq colors (ceiling (half (length colors)))))
                (html 
                 (:tr 
                  (:td ((:table :border 0 :cellspacing 1 :cellpadding 0)
                        (dolist (green colors)
                          (html (:tr (dolist (blue colors)
                                       (html ((:td :bgcolor (format nil "#~a~a~a" red green blue) :height cell-size :width cell-size)
                                              (when (not (zerop (length link-objects)))
                                                (the-object (svref link-objects (incf count))
                                                            (write-self-link 
                                                             :target target
                                                             :on-mouse-over (format nil "window.status='Set Color mode to RGB Hex value: ~a.'; return true;" 
                                                                                    (format nil "#~a~a~a" red green blue))
                                                             :on-mouse-out  "window.status=''; return true;"

                                                             :display-string 
                                                             (with-output-to-string(ss)
                                                               (html-stream ss ((:font :size (div cell-size 15) 
                                                                                       :color  (format nil "#~a~a~a" red green blue)) 
                                                                                (:small (:small "m"))))))))))))))))))))))))))))))
            
            

(defun html-escape (string)
  (glisp:replace-regexp
   (glisp:replace-regexp
    (glisp:replace-regexp
     (glisp:replace-regexp
      (glisp:replace-regexp (with-output-to-string (ss)
                              (cl-who:with-html-output (ss)
                                (cl-who:esc string))) " " "-") "\\(" "_") "\\)" "_") "\\^" "hat") "\\*" "-star-"))





(defun publish-gwl-app (path string-or-symbol &key make-object-args)
  "Void. Publishes an application, optionally with some initial arguments to be passed in as input-slots.

:arguments (path \"String. The URL pathname component to be published.\"
            string-or-symbol \"String or symbol. The object type to insantiate.\")

:&key (make-object-args \"Plist. Extra arguments to pass to make-object.\")
"

  (publish :path path
           :function #'(lambda(req ent)
                         (gwl-make-object req ent 
                                          (format nil (if (stringp string-or-symbol) "~a" "~s")
                                                  string-or-symbol)
                                          :make-object-args make-object-args))))


(defun publish-string-content (url string &rest publish-args)
  "String (representing a url path). Publishes given url to respond with text content as 
specified by given string.


:arguments (url \"String. The url to be published.\"
            string \"String. The content to be emitted when the url is requested.\")

:&rest (publish-args \"plist.  Arguments to be passed on to publish function, e.g. :content-type.\")
  

"
  
  (apply #'publish  
         :path url
         :function #'(lambda (req ent)
                       (with-http-response (req ent)
                         (setf (reply-header-slot-value req :cache-control) "no-cache")
                         (with-http-body (req ent)
                           (let ((stream (request-reply-stream req)))
                             (write-string string stream)))))
         publish-args)
  url)
