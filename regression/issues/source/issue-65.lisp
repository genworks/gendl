(in-package :gdl-user)

(define-object manifold-sample (base-object)
  :objects
  ((box :type 'box-solid
    :width 2 :length 2 :height 2)
   (merged :type 'merged-solid :other-brep (the box :face-breps :list-elements))
   (merged2 :type 'merged-solid :other-brep (the box :face-breps :list-elements) :make-manifold? t)))