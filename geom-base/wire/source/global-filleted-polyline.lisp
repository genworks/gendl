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

(define-object global-filleted-polyline-mixin (global-polyline-mixin)
  
  :documentation (:description "Generates a polyline with the corners filleted according to default radius or the radius-list.")
  
  :input-slots
  (vertex-list
   
   ("Boolean. Controls whether the filleted-polyline should automatically be closed."
    closed? (coincident-point? (first (the :vertex-list))
                               (lastcar (the :vertex-list))))
   ("List of Numbers. Specifies the radius for each vertex (``corner'') of the filleted-polyline."
    radius-list nil)
   
   ("Number. Specifies a radius to use for all vertices. Radius-list will take precedence over this."
    default-radius (lastcar (the :radius-list))))
  
  :computed-slots
  (("List of pairs of 3D points. Each pair represents the start and end of each straight segment of the
filleted-polyline."
    straights (let ((count -1) (length (length (the :lines))))
                (mapcar #'(lambda(line)
                            (incf count)
                            ;;
                            ;; FLAG -- does not handle straight section on closed polyline
                            ;;         from end to start
                            ;;
                            (list (cond ((and (= count 0) (the :closed?))
                                         (second (the :fillets :last :tangents)))
                                        ((= count 0) (first line))
                                        ((typep (the (:fillets (1- count))) 'null-part)
                                         (first line))
                                        (t (second (the (:fillets (1- count)) :tangents))))
                                  (cond ((and (= count (1- length)) (the :closed?))
                                         (first (the :fillets :last :tangents)))
                                        ((= count (1- length)) (second line))
                                        ((typep (the (fillets count)) 'null-part)
                                         (second line))
                                        (t (first (the (:fillets count) :tangents))))))
                        (the :lines))))

   
   (%lines-to-draw% (the :straights))
   
   (%%lines-to-draw%% (the :straights))
   
   (%corners% nil)
   
               
   ;;
   ;; FLAG --replace with function call returning fillet-curves.
   ;;
   (%curves-to-draw% (apply #'append (list-elements (the :fillets) (the-element :%curves-to-draw%))))

   (%arcs% (list-elements (the fillets)))
   
   (%renderer-info% (list :vrml? t :view-default :top))


   (path-info (let ((first? t))
		(mapcan #'(lambda(straight curve-set)
			    (append (if first? (progn (setq first? nil)
						      (list :move (first straight) :line (second straight)))
					(list :line (second straight)))

				    (apply #'append
					   (mapcar #'(lambda(curve) (cons :curve (rest (reverse curve))))
						   (reverse (the-object curve-set %curves-to-draw%))))))
			(the straights) (list-elements (the fillets)))))
				


   
   (fillet-types (mapcar #'(lambda(test)
                             (if (the-object test valid?) 'fillet 'null-part))
                         (list-elements (the fillet-tests)))))
  
  
  ;;
  ;; FLAG --replace with function call returning fillet-curves.
  ;;
  :hidden-objects
  ((fillet-tests :type 'fillet-tester
                 :sequence (:size (the fillets number-of-elements))
                 :direction-vectors (list (reverse-vector (direction-vector (nth (the-child :index) (the :lines))))
                                          (if (and (the-child :last?) (the :closed?))
                                              (direction-vector (first (the :lines)))
                                            (direction-vector (nth (1+ (the-child :index)) (the :lines)))))
                 :valid? (not (or (apply #'same-direction-vectors? (the-child direction-vectors))
                                  (same-direction-vectors? (first (the-child direction-vectors))
                                                           (reverse-vector (second (the-child direction-vectors)))))))

   )
   

  :hidden-objects
  (("Sequence of fillets. Each fillet is essentially an arc representing the curved elbow
of the filleted-polyline."
    fillets :type (:sequence (the fillet-types))
            :sequence (:size (if (the :closed?) 
                                 (1- (length (the :vertex-list)))
                               (- (length (the :vertex-list)) 2)))
            :radius (or (nth (the-child :index)(the :radius-list))
                        (the :default-radius)
                        (error "You must specify a radius for global-filleted-polyline - root-path is ~s~%"
                               (the root-path)))

            :local-vertex (nth (1+ (the-child :index)) (the :vertex-list))
             
            :direction-vectors (the (fillet-tests (the-child index)) direction-vectors))
   
   (lins :type 'line
          :sequence (:size (length (the straights)))
          :start (first (nth (the-child index) (the straights)))
          :end (second (nth (the-child index) (the straights))))
   
   )
  
  :functions
  (
   
   (interpolated-points
    (&optional (curve-chords *curve-chords*))
    (let (last-bezier-point)
      (append
       (mapcan #'(lambda(straight fillet)
                   (append (list (first straight))
                           (let ((beziers (chain-curves (list-elements (the-object fillet beziers)) (second straight))))
                             (mapcan #'(lambda(bezier)
                                         (let ((points (the-object bezier (equi-spaced-points curve-chords))))
                                           (setq last-bezier-point (lastcar points))
                                           (butlast points))) beziers))))
               (the straights) (list-elements (the fillets)))
       
       (list last-bezier-point)

       (if (the closed?)
           (list (first (first (the straights))))
         (list (lastcar (lastcar (the straights))))))))))


(define-object global-filleted-polyline (global-filleted-polyline-mixin)
  
    :documentation (:description "A sequence of points connected by straight line segments, whose
corners are filleted according to specified radii. Please see global-filleted-polyline-mixin
for documentation on the messages."
                  
                  :examples "<pre>

  (in-package :gdl-user)

  (define-object global-filleted-polyline-sample (global-filleted-polyline)
    :computed-slots
    ((default-radius 5)
      (vertex-list (list (make-point 0 0 0)
                        (make-point 10 10 0)
                        (make-point 30 10 0)
                        (make-point 40 0 0)
                        (make-point 30 -10 0)
                        (make-point 10 -10 0)
                        (make-point 0 0 0)))))

  (generate-sample-drawing :objects (make-object 'global-filleted-polyline-sample))

  </pre>"))



(define-object fillet-tester (base-object)
  
  :input-slots (direction-vectors valid?))



;;
;;FLAG -- try to compute curves in pre-chained manner so this is not necessary...
;;
(defun chain-curves (curves start &key closed? (rank 3))
  (when curves
    (let* ((beziers? (typep (first curves) 'bezier-curve))
           (first-getter (if beziers? #'(lambda(obj)(first (the-object obj control-points))) #'first))
           (lastcar-getter (if beziers? #'(lambda(obj)(lastcar (the-object obj control-points))) #'lastcar))
           (reverser (if beziers? #'(lambda(obj) (the-object obj reverse)) #'reverse)))
      (let ((start-match (position start curves :test (lambda(p1 p2) (coincident-point? p1 p2 :rank rank)) :key first-getter))
            (end-match (position start curves :test (lambda(p1 p2) (coincident-point? p1 p2 :rank rank)) :key lastcar-getter)))
        (cond ((and (not closed?) start-match end-match)
               (error "start matches both start and end of curve in chain-curves"))
              ((and (null start-match) (null end-match))
               (error "curves did not chain in chain-curves"))
              (start-match
               (cons (nth start-match curves) 
                     (chain-curves (append (subseq curves 0 start-match)
                                           (subseq curves (1+ start-match)))
                                   (funcall lastcar-getter (nth start-match curves)) :rank rank)))
              (end-match (cons (funcall reverser (nth end-match curves)) 
                               (chain-curves (append (subseq curves 0 end-match)
                                                     (subseq curves (1+ end-match)))
                                             (funcall first-getter (nth end-match curves)) :rank rank))))))))





