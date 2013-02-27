(in-package :gdl-user)

(define-object tasty-center-test (base-object)
  :objects
  ((child1 :type 'box
       :center (make-point 1 0 0) :width 1 :length 1 :height 1)
   (child2 :type 'box :width 1 :length 1 :height 1)))
