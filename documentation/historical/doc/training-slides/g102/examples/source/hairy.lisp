(in-package :training-g102)

(define-object hairy-calc ()
  
  :input-slots (num) 
  
  :computed-slots
  ((a (twice (the num)))
   (b (+ (the a) 1000))
   (c (if (> (the b) 5000) 
	  (+ (the b) (* (the b) 0.05))
	(the b)))
   (d (/ (the c) 24))))


(define-object silly-object () :computed-slots ((a 50) (b (expt (the a) 2))))

(define-object silly-object-2 () :input-slots (a) :computed-slots ((b (expt (the a) 2))))

(define-object hello-there () 
  :input-slots (name fancy?) 
  :computed-slots ((greeting (format nil "Hello ~a~a~a" (if (the fancy?) "there " "") (the name)
				     (if (the fancy?) ", How Are You?" "")))))



(define-object silly-adder ()
  :input-slots (a b)
  :computed-slots ((sum (+ (the a) (the b)))))


(defun compute-sum (a b)
  (let ((self (make-object 'silly-adder :a a :b b)))
    (the sum)))

(defun silly-adder (a b)
  (+ a b))



(define-object city (box) 
  :input-slots
  ((length 10)
   (width 10)
   (height 0.25)
   (max-allowed-daily-water-usage 5000)
   (building-1-data '(:type bank :height 30 :width 70 :length 50))
   (building-2-data '(:type hotel :height 100 :width 40 :length 30)))
  
  :computed-slots
  ((total-daily-water-usage 
    (+ (the building-1 daily-water-usage) 
       (the building-2 daily-water-usage)))
   (too-much-water-usage? 
    (> (the total-daily-water-usage) (the max-allowed-daily-water-usage))))
   
  :objects
  ((building-1 :type (getf (the building-1-data) :type) 
	       :height (getf (the building-1-data) :height)
	       :width (getf (the building-1-data) :width)
	       :length (getf (the building-1-data) :length))
   (building-2 :type (getf (the building-2-data) :type) 
	       :height (getf (the building-2-data) :height)
	       :width (getf (the building-2-data) :width)
	       :length (getf (the building-2-data) :length))))


(define-object city-2 (box) 
  :input-slots
  ((length 10)
   (width 10)
   (height 0.25)
   (max-allowed-daily-water-usage 5000)
   (building-1-data '(:type bank :height 30 :width 70 :length 50))
   (building-2-data '(:type hotel :height 100 :width 40 :length 30)))
  
  :computed-slots
  ((total-daily-water-usage (+ (the building-1 daily-water-usage) (the building-2 daily-water-usage)))
   (too-much-water-usage? (> (the total-daily-water-usage) (the max-allowed-daily-water-usage))))
   
  :objects
  ((:building-1 :type (getf (the building-1-data) :type) 
		:parameters (rest (rest (the building-1-data))))
   (:building-2 :type (getf (the building-2-data) :type) 
		:parameters (rest (rest (the building-2-data))))))


(define-object city-3 (box) 
  
  :input-slots
  ((length 10) 
   (width 10) 
   (height 0.25)
   (max-allowed-daily-water-usage 5000)
   (building-data '((:type bank :height 30 :width 70 :length 50)
		    (:type hotel :height 100 :width 40 :length 30))))
   :computed-slots
   ((total-daily-water-usage
     (apply #'+ (list-elements (the buildings) (the-element daily-water-usage))))
    
    (too-much-water-usage?
     (> (the total-daily-water-usage) (the max-allowed-daily-water-usage))))
   
   :objects
   ((:buildings :type (:sequence (mapcar #'(lambda (plist) (getf plist :type)) 
					 (the building-data))) 
		:sequence (:size (length (the building-data))) 
		:parameters (rest (rest (nth (the-child index) (the building-data)))))))



(defparameter *city-data*
    `(:buildings
      ((:type bank :height 30 :width 70 :length 50 :center ,(make-point 50 50 15))
       (:type hotel :height 100 :width 40 :length 30 :center ,(make-point -50 -50 50)))))


(define-object city-4 (box) 
  :input-slots
  ((length 200)
   (width 200) 
   (height 10)
   (max-allowed-daily-water-usage 5000)
   (building-data (getf *city-data* :buildings)))
  
  :computed-slots
  ((display-controls (list :color :blue :transparency 0.5)))
  
  :objects
  ((:buildings :type (:sequence (mapcar #'(lambda (plist) (getf plist :type)) (the building-data))) 
	       :sequence (:size (length (the building-data)))
	       :center (getf (nth (the-child index) (the building-data)) :center)
	       :parameters (rest (rest (nth (the-child index) (the building-data)))))))




(defpart point-demo (null-geometric-object) 
  :attributes
  (:point-1 (make-point 1 0 1) :point-2 (make-point 1 0 0) 
   :direction-vector (subtract-vectors (the point-2) (the point-1))))


(defpart translation-demo (null-geometric-object) 
  :attributes
  (:point-1 (make-point 1 0 1) :point-2 (make-point 1 0 0) :direction-vector
   (subtract-vectors (the point-2) (the point-1)) :midpoint
   (translate-along-vector (the point-1) (the direction-vector)
			   (half (3d-distance (the point-1) (the point-2))))))



(defpart self-aligning-box (box) 
  :inputs (:top-vector :front-vector) 
  :attributes
  (:orientation (alignment :top (the top-vector) :front (the front-vector))))


(define-object building (box))

(define-object bank (building)
  :computed-slots
  ((daily-water-usage 2000)))

(define-object hotel (building)
  :computed-slots
  ((daily-water-usage 4000)))
