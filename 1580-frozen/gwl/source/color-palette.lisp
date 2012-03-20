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

(in-package :gwl-user)


(publish :path "/color-map"
         :function #'(lambda(req ent)
                       (gwl-make-part req ent "gwl-user::color-map")))


(publish :path "/color-palette"
         :function #'(lambda(req ent)
                       (gwl-make-part req ent "gwl-user::color-map")))

(define-object color-map (base-html-sheet)

  :documentation
  (:description "Shows a list of the default colors. This is published as the URI \"/color-map\" of the running GWL webserver.")

  :functions
  ((write-html-sheet
    nil
    (html (:html (:title "Andrew's Color Map"))
          (:body (:center (:h2 "Andrew's Color Map"))
                 ((:table :border 1) (:tr (:th "Color") (:th "Hex") (:th "Decimal"))
                                     (maphash #'(lambda (key hex)
                                                  (html (:tr ((:td :bgcolor hex) (:prin1 key))
                                                             (:td (:princ "\"" hex "\""))
                                                             (:td
                                                              (format *html-stream* "~{~a~^, ~}"
                                                                      (gethash key *color-table-decimal*))))))
                                              *color-table*)))))))
