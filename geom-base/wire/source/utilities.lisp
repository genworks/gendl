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

(defun bounding-box-from-list (objects &key (local-objects))
  (let ((object-boxes (remove nil (mapcar #'(lambda(object) (when (typep object 'base-object)
                                                              (the-object object bounding-box))) objects)))
        (local-object-boxes (remove nil (mapcar #'(lambda(object) (when (typep object 'base-object)
                                                                    (the-object object local-box))) 
                                                local-objects))))
    (let (xmin ymin zmin xmax ymax zmax)
      (mapcar #'(lambda(box)
                  (let ((min (first box)) (max (second box)))
                    (let ((x-min (get-x min)) (y-min (get-y min)) (z-min (get-z min))
                          (x-max (get-x max)) (y-max (get-y max)) (z-max (get-z max)))
                      (when (or (null xmin) (< x-min xmin)) (setq xmin x-min))
                      (when (or (null ymin) (< y-min ymin)) (setq ymin y-min))
                      (when (or (null zmin) (< z-min zmin)) (setq zmin z-min))
                      (when (or (null xmax) (> x-max xmax)) (setq xmax x-max))
                      (when (or (null ymax) (> y-max ymax)) (setq ymax y-max))
                      (when (or (null zmax) (> z-max zmax)) (setq zmax z-max))))) 
              (append local-object-boxes object-boxes))
      (if xmin
          (list (make-point xmin ymin zmin) (make-point xmax ymax zmax))
        (list (make-point 0 0 0) (make-point 0 0 0))))))




(defun bounding-box-from-points (points)
  (let (xmin ymin zmin xmax ymax zmax)
    (mapcar #'(lambda(point)
                (let ((x (get-x point)) (y (get-y point)) (z (get-z point)))
                  (when (or (null xmin) (< x xmin)) (setq xmin x))
                  (when (or (null ymin) (< y ymin)) (setq ymin y))
                  (when (or (null zmin) (< z zmin)) (setq zmin z))
                  (when (or (null xmax) (> x xmax)) (setq xmax x))
                  (when (or (null ymax) (> y ymax)) (setq ymax y))
                  (when (or (null zmax) (> z zmax)) (setq zmax z)))) points)
    (list (make-point xmin ymin zmin) (make-point xmax ymax zmax))))
