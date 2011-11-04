(in-package :gdl-user)
(define-object robot-drawing (base-drawing)
  :objects
  ((main-view :type 'base-view
              :projection-vector (getf *standard-views* :trimetric)
              :object-roots (list (the robot)))
   (robot :type 'robot::assembly
          :hidden? t)))
