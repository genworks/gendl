;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gwl-graphics :description
 "The Gendl™ GWL embedded graphics support" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130710" :depends-on (:geom-base :gwl)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "gwl/source/application-mixin")
  (:file "gwl/source/base-ajax-graphics-sheet")
  (:file "gwl/source/base-html-graphics-sheet")
  (:file "gwl/source/layout-mixin")
  (:file "gwl/source/process-graphics-fields")
  (:file "gwl/source/web-drawing") (:file "gwl/source/x3d-try")
  (:file "raphael/source/package") (:file "raphael/source/formats")
  (:file "raphael/source/lenses") (:file "source/parameters")
  (:file "zzinit/source/initialize") (:file "zzinit/source/zzinit")))
