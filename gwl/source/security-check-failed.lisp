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


(define-object security-check-failed (base-html-sheet))


(define-lens (html-format security-check-failed)()
  :output-functions
  ((main-sheet
    ()
    (with-cl-who ()
      (:html (:head (:title "Security Check Failed"))
             (:body (:center (:h2 "Security Check Failed"))
                    (:p  "This is most likely because you are coming from a different IP address than the one originally assigned to this session.")))))))
