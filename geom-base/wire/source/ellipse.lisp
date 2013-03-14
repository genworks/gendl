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

(in-package :geom-base)

(define-object ellipse (arcoid-mixin base-object)
  
  :documentation (:description "A curve which is the locus of all points in the plane 
the sum of whose distances from two fixed points (the foci) is a given positive constant.
This is a simplified 3D ellipse which will snap to the nearest quarter if you make it a 
partial ellipse. For a full ellipse, do not specify start-angle or end-angle."
                  
                  :examples "<pre>
  
  (in-package :gdl-user)

  (define-object ellipse-sample (ellipse)
    :computed-slots
    ((minor-axis-length 10)
     (major-axis-length (* (the minor-axis-length) +phi+))
     (start-angle 0)
     (end-angle pi)))

  (generate-sample-drawing :objects (make-object 'ellipse-sample))
  
  </pre>")

  
  :input-slots
  ("Number. Length of (generally) the longer ellipse axis"
   major-axis-length
   
   "Number. Length of (generally) the shorter ellipse axis"
   minor-axis-length
   
   ("Angle in Radians. Start angle of the ellipse. Defaults to 0 for full ellipse."
    start-angle 0) 
   
   ("Angle in Radians. End angle of the ellipse. Defaults to 2pi for full ellipse."
    end-angle 2pi))
  
  :computed-slots
  (
   (%renderer-info% (list :vrml? t :view-default :top))
   
   (%curves-to-draw% (the :control-point-lists))
   
   (curves (the :control-point-lists))
   
   (control-point-lists (if (and (null (the start-angle))
                                 (null (the end-angle)))
                            (the :control-point-lists-all)
                          (remove nil
                                  (list (when (and (< (the start-angle-normalized) pi/2)
                                                   (>= (the end-angle-normalized) pi/2))
                                          (the (:control-point-list :rear :right)))
                                        (when (and (<= (the start-angle-normalized) pi/2)
                                                   (>= (the end-angle-normalized) pi))
                                          (the (:control-point-list :left :rear)))
                                        (when (and (<= (the start-angle-normalized) pi)
                                                   (>= (the end-angle-normalized) (/ (* pi 3) 2)))
                                          (the (:control-point-list :front :left)))
                                        (when (and (<= (the start-angle-normalized) (/ (* pi 3) 2))
                                                   (>= (the end-angle-normalized) 2pi))
                                          (the (:control-point-list :right :front)))))))
   
   (control-point-lists-all (list (the (:control-point-list :rear  :right))
                                  (the (:control-point-list :left  :rear))
                                  (the (:control-point-list :front :left))
                                  (the (:control-point-list :right :front))))
   
   (length (the minor-axis-length))
   (width (the major-axis-length))
   (height 0)
   
   )
  
  :functions
  ((control-point-list
    (face-1 face-2)
    (let ((minor-axis-length (the :minor-axis-length))
          (major-axis-length (the :major-axis-length)))
      (let ((face-1-radial-length (half (ecase face-1 ((:rear :front) minor-axis-length)
                                               ((:right :left) major-axis-length))))
            (face-2-radial-length (half (ecase face-2 ((:rear :front) minor-axis-length)
                                               ((:right :left) major-axis-length)))))
        (let ((face-1-kappa-factor (*  face-1-radial-length +kappa+))
              (face-2-kappa-factor (* face-2-radial-length +kappa+)))
          (let ((center (the :center)))
            (list (translate center face-1 face-1-radial-length)
                  (translate center face-1 face-1-radial-length face-2 face-2-kappa-factor)
                  (translate center face-2 face-2-radial-length face-1 face-1-kappa-factor)
                  (translate center face-2 face-2-radial-length)))))))))



(defun sub-arc (radius start-angle end-angle true-center
                right-vector rear-vector front-vector top-vector)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((center +nominal-origin+)
         (start-to-end-angle (- end-angle start-angle))
         (theta-1 (half start-to-end-angle))
         (cos-theta-1 (cos theta-1))
         (x-1 (* (/ (- 4 cos-theta-1) 3) radius))
         (y-1 (* (/ (* (- 1 cos-theta-1) (- cos-theta-1 3)) (* 3 (sin theta-1))) radius))
         (mid-point (translate-along-vector center right-vector x-1))
         (construction-p-1 (translate-along-vector mid-point rear-vector y-1))
         (construction-p-2 (translate-along-vector mid-point front-vector y-1))
         (start (translate-along-vector center (rotate-vector right-vector start-angle top-vector) radius))
         (end   (translate-along-vector center (rotate-vector right-vector end-angle top-vector) radius))
         (new-angle (+ start-angle theta-1))
         (p-1 (rotate-point construction-p-1 center top-vector :angle new-angle))
         (p-2 (rotate-point construction-p-2 center top-vector :angle new-angle)))
    (list (add-vectors start true-center)
          (add-vectors p-1 true-center)
          (add-vectors p-2 true-center)
          (add-vectors end true-center))))


;; FLAG -- remove this and handle in base-view output of generic
;; curves (if possible). Or, do only the fill part here.
;;
;;(define-view (pdf ellipse)()
;;  :output-functions
;;  ((cad-output
;;    ()
;;    ;;
;;    ;; FLAG -- computed curves in a pre-chained manner to avoid having to chain them.
;;    ;;
;;    (if (and (null (the start-angle))
;;           (null (the end-angle)))
;;      (let ((curves (chain-curves (the curves) (first (first (the curves))) :closed? t)))
;;        (pdf:with-saved-state
;;            (write-the line-thickness-setting)
;;          (write-the rgb-stroke-setting)
;;          (when (the fill-color-decimal) (apply #'pdf:set-rgb-fill (the fill-color-decimal)))
;;          (let ((start (world-to-drawing (first (first curves)))))
;;            (pdf:move-to (get-x start) (get-y start))
;;            (mapc #'(lambda(curve)
;;                      (destructuring-bind (p1 p2 p3 p4) (world-to-drawing curve)
;;                        (declare (ignore p1))
;;                        (pdf:bezier-to (get-x p2) (get-y p2)
;;                                       (get-x p3) (get-y p3)
;;                                       (get-x p4) (get-y p4)))) curves))
;;          (if (the fill-color-decimal) (pdf:fill-and-stroke) (pdf:stroke))))
;;      (call-next-method)))))
;;

