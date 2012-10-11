
(asdf:defsystem #:gdl-regression
  :description
  "Auto-generated asdf defsys from Genworks GDL cl-lite."
  :author
  "Genworks and Dave Cooper unless otherwise indicated"
  :license
  "AGPL unless otherwise indicated"
  :serial
  t
  :version
  "2012101100"
  :depends-on
  (:lift :gdl-surf :gdl-tasty)
  :components
  ((:file "utils/source/package") (:file "utils/source/genworks")
   (:file "utils/source/functions") (:file "utils/source/parameters")
   (:file "assembly-output/source/package")
   (:file "assembly-output/source/bracket-test")
   (:file "assembly-output/source/bracket")
   (:file "geom-base/source/box-matrix") (:file "gwl/source/hey-now")
   (:file "gwl/source/large-data") (:file "gwl/source/package")
   (:file "gwl/source/svg-error-handling") (:file "source/package")
   (:file "source/parameters") (:file "source/approximated-curve")
   (:file "source/arc-curve") (:file "source/b-spline-curve")
   (:file "source/b-spline-surface") (:file "source/blended-solid")
   (:file "source/boolean") (:file "source/box-solid")
   (:file "source/boxed-curve") (:file "source/boxed-surface")
   (:file "source/brep-brep-solve") (:file "source/brep-intersect")
   (:file "source/compatible-curves")
   (:file "source/compatible-surfaces")
   (:file "source/composed-curve") (:file "source/composed-curves")
   (:file "source/cone-solid") (:file "source/cylinder-solid")
   (:file "source/decomposed-curves") (:file "source/dropped-curve")
   (:file "source/dual-blend-surface")
   (:file "source/edge-blend-surface")
   (:file "source/elliptical-curve")
   (:file "source/extended-surface") (:file "source/extruded-solid")
   (:file "source/fitted-conic") (:file "source/fitted-curve")
   (:file "source/fitted-surface") (:file "source/gdlxml")
   (:file "source/general-dual-blend-surface")
   (:file "source/general-note") (:file "source/general-sweep")
   (:file "source/global-filleted-polyline-curves")
   (:file "source/iges-reader") (:file "source/iges-writer")
   (:file "source/intersected-solid") (:file "source/iso-curve")
   (:file "source/joined-surfaces") (:file "source/linear-curve")
   (:file "source/lofted-surface") (:file "source/merged-solid")
   (:file "source/native-reader") (:file "source/orientations")
   (:file "source/pegasus-regioned-solid")
   (:file "source/planar-contour-surface")
   (:file "source/planar-offset-curve")
   (:file "source/planar-section-curve")
   (:file "source/planar-section-curves")
   (:file "source/planar-surface") (:file "source/poly-brep")
   (:file "source/projected-curve")
   (:file "source/rectangular-surface")
   (:file "source/regioned-solid")
   (:file "source/reparameterized-curve")
   (:file "source/revolved-surface")
   (:file "source/revolved-surfaces") (:file "source/ruled-surface")
   (:file "source/separated-solid") (:file "source/sewn-solid")
   (:file "source/shelled-solid") (:file "source/silhouette-curves")
   (:file "source/spherical-surface") (:file "source/split-surface")
   (:file "source/step-reader") (:file "source/step-writer")
   (:file "source/stitched-solid") (:file "source/subtracted-solid")
   (:file "source/surface-knot-reduction")
   (:file "source/swept-solid") (:file "source/test-harness")
   (:file "source/transformed-curve")
   (:file "source/transformed-solid")
   (:file "source/transformed-surface")
   (:file "source/trimmed-curve") (:file "source/trimmed-surface")
   (:file "source/united-solid")))