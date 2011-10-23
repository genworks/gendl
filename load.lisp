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

(in-package :common-lisp-user)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" 
                              (user-homedir-pathname))))
  (if (probe-file setup) (load setup)
    (progn 
      (load (merge-pathnames "quicklisp.lisp" *load-truename*))
      (funcall (read-from-string "quicklisp-quickstart:install")))))

;;
;; FLAG -- Temporary hacks around currently broken Quicklisp .asd files and sytems:
;;

#+allegro 
(load (merge-pathnames "quicklisp-local/portableaserve-20110730-cvs/aserve/aserve.asd"
                       *load-truename*))

(let ((fasl (compile-file 
             (merge-pathnames 
              "quicklisp-local/cl-typesetting-20110219-svn/hyphenation-fp.lisp"
                                           *load-truename*))))
  (load fasl)
  (delete-file fasl))


;;
;; FLAG -- figure out how to get all these loaded automatically 
;; or placed/linked in a standard location. 
;;

(load (merge-pathnames "gdl/base/gdl-base.asd" *load-truename*))
(load (merge-pathnames "gdl/cl-lite/gdl-cl-lite.asd" *load-truename*))
(load (merge-pathnames "gdl/geom-base/gdl-geom-base.asd" *load-truename*))
(load (merge-pathnames "gdl/gwl/gdl-gwl.asd" *load-truename*))
(load (merge-pathnames "gdl/gwl-graphics/gdl-gwl-graphics.asd" *load-truename*))
(load (merge-pathnames "gdl/apps/tree/gdl-tree.asd" *load-truename*))
(load (merge-pathnames "gdl/apps/ta2/gdl-ta2.asd" *load-truename*))
(load (merge-pathnames "gdl/apps/tasty/gdl-tasty.asd" *load-truename*))
(load (merge-pathnames "demos/gdl-demos.asd" *load-truename*))
(load (merge-pathnames "build/gdl-build.asd" *load-truename*))
(load (merge-pathnames "gdl/apps/yadd/gdl-yadd.asd" *load-truename*))
(load (merge-pathnames "gdl-all.asd" *load-truename*))

(ql:quickload :gdl-all)

;;
;; FLAG -- following two and other specific feature additions should be done by .asd system
;; "perform" statements (when I figure out how those work).
;;

(pushnew :genworks-base *features*)
(pushnew :genworks-gdl-base *features*)
(pushnew :genworks-cl-lite *features*)



;;
;; Skeleton "vanilla" geometry kernel - nonfunctional without SMLib module. 
;;
(load (merge-pathnames "gdl/geom-nurbs/surf/gdl-surf.asd" *load-truename*))
