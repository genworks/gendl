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

(in-package :gwl)

(defmacro with-expanded-html-output ((var &optional stream
                                            &key prologue
                                                 ((:indent cl-who::*indent*) cl-who::*indent*))
                                           &body body)
  (let ((new-body
         (mapcar #'(lambda(form)
                     (cond ((and (listp form)
                                 (listp (first form))
                                 (member (first (first form)) '(:input :select :textarea :button)))
                            (cons (append (first form)
                                          `(:disabled 
                                            (if (the disabled?) t nil)
					    ;;
					    ;; The new HTML5 stuff:
					    ;;
					    :placeholder (the placeholder)


                                            :readonly (if (the readonly?) t nil)
                                            :ismap (if (the ismap?) t nil)
                                            :size  (the size)
                                            :maxlength (the maxlength)
                                            :src (the src)
                                            :alt (the alt)
                                            :usemap (the usemap)
                                            :tabindex (the tabindex)
                                            :accesskey (the accesskey)
                                            :onfocus (the onfocus)
                                            :onblur (the onblur)
                                            :onselect (the onselect)
                                            :onchange (the onchange)
                                            :ondblclick (the ondblclick)
                                            :onclick (the onclick)
                                            :onmousedown (the onmousedown)
                                            :onmouseup (the onmouseup)
                                            :onmouseover (the onmouseover)
                                            :onmousemove (the onmousemove)
                                            :onmouseout (the onmouseout)
                                            :onkeypress (the onkeypress)
                                            :onkeydown (the onkeydown)
                                            :onkeyup (the onkeyup)
                                            :accept (the accept)
                                            :lang (the lang)
                                            :title (the title)
                                            :style (the style)
                                            :align (the align)))
                                  (rest form)))
                           (t form))) body)))
    `(with-html-output (,var ,stream :prologue ,prologue :indent :indent)
       ,@new-body)))
  
