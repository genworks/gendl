
(asdf:defsystem #:gdl-demos
  :description
  "Auto-generated asdf defsys from Genworks GDL cl-lite."
  :author
  "Genworks and Dave Cooper unless otherwise indicated"
  :license
  "AGPL unless otherwise indicated"
  :serial
  t
  :version
  "2012030300"
  :depends-on
  (:gdl-gwl-graphics)
  :components
  ((:file "bus/source/package") (:file "bus/source/assembly")
   (:file "bus/source/body") (:file "bus/source/chassis")
   (:file "bus/source/interior") (:file "bus/source/rule-ackermann")
   (:file "bus/source/axle") (:file "bus/source/fleet")
   (:file "bus/source/frame-rail") (:file "bus/source/frame")
   (:file "bus/source/html-writer-assembly")
   (:file "bus/source/html-writer-body")
   (:file "bus/source/html-writer-chassis")
   (:file "bus/source/html-writer-interior")
   (:file "bus/source/html-writer-rule-ackermann")
   (:file "bus/source/inter-seat-clearance-check")
   (:file "bus/source/inter-seat-spacing")
   (:file "bus/source/knuckle") (:file "bus/source/parameters")
   (:file "bus/source/publish") (:file "bus/source/rear-axle")
   (:file "bus/source/seat") (:file "bus/source/seating-section")
   (:file "bus/source/seating-side") (:file "bus/source/wheel")
   (:file "glassbox/source/assembly")
   (:file "glassbox/source/eco-tree") (:file "ledger/source/package")
   (:file "ledger/source/assembly") (:file "ledger/source/html")
   (:file "robot/source/package") (:file "robot/source/assembly")
   (:file "site/source/package") (:file "site/source/assembly")
   (:file "site/source/contact") (:file "site/source/demos")
   (:file "site/source/glossary") (:file "site/source/index")
   (:file "site/source/landing") (:file "site/source/licensing")
   (:file "site/source/maintenance") (:file "site/source/make")
   (:file "site/source/mixins") (:file "site/source/new")
   (:file "site/source/opportunities") (:file "site/source/options")
   (:file "site/source/people") (:file "site/source/pricing")
   (:file "site/source/products") (:file "site/source/publish")
   (:file "site/source/security") (:file "wire-world/source/package")
   (:file "wire-world/source/assembly")))