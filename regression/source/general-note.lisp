(in-package :gdl-lift-tests)

(define-object general-note-test (base-object)

  :objects
  (
   (center-sphere :type 'sphere 
		  :radius .1)
		  
   (box :type 'box :length 10 :width 10 :height 10
	:display-controls (list :transparency 0.7 :color :orange))

   (note :type 'general-note
	 :display-controls (list :color (list .32 .54 .26)
				 :specular-color (list 0.46 0.46 0.46)
				 :ambient-intensity 0.0933)
	 :strings "Hey Now")

   

   (kid :type 'general-note-kid
	:center (translate (the center) :rear 10))

   ))


(define-object general-note-kid (base-object)

  :objects
  ((center-sphere :type 'sphere 
		  :radius .1)
		  
   (box :type 'box :length 10 :width 10 :height 10
	:display-controls (list :transparency 0.7 :color :orange))

   (note :type 'general-note
	 :display-controls (list :color (list .32 .54 .26)
				 :specular-color (list 0.46 0.46 0.46)
				 :ambient-intensity 0.0933)
	 :strings "Hey Now")


   ))


    