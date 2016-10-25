;;
;; Copyright 2016 Genworks International 
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
