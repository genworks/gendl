;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:bus :description
 "The Gendl™ Wireframe School Bus Demo" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130710" :depends-on (:gwl-graphics)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/body") (:file "source/chassis")
  (:file "source/interior") (:file "source/rule-ackermann")
  (:file "source/axle") (:file "source/fleet")
  (:file "source/frame-rail") (:file "source/frame")
  (:file "source/html-writer-assembly")
  (:file "source/html-writer-body")
  (:file "source/html-writer-chassis")
  (:file "source/html-writer-interior")
  (:file "source/html-writer-rule-ackermann")
  (:file "source/inter-seat-clearance-check")
  (:file "source/inter-seat-spacing") (:file "source/knuckle")
  (:file "source/parameters") (:file "source/publish")
  (:file "source/rear-axle") (:file "source/seat")
  (:file "source/seating-section") (:file "source/seating-side")
  (:file "source/wheel")))
