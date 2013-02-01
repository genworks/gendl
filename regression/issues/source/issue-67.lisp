(in-package :gdl-user)


(define-object issue-67 (base-object)
  :objects
  ((box :type 'box-solid
	:width 2 :length 2 :height 2)

   (clean-surfaces :type 'surface
		   :sequence (:size 6)
		   :built-from (the box (faces (the-child index)) basis-surface))

   (stitched :type 'stitched-solid :faces-in (list-elements (the clean-surfaces)))

   (stitched-alternative :type 'stitched-solid :faces-in (list-elements (the box :faces) (the-element basis-surface)))

   (stitched-manifold :type 'manifold-solid :brep (the stitched))))
