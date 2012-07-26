(in-package :gdl-lift-tests)

(define-object general-note-test (base-object)

  :objects
  (
   (center-sphere :type 'sphere 
		  :radius .1)

   (note :type 'general-note
	 :display-controls (list :color (list .32 .54 .26)
				 :specular-color (list 0.46 0.46 0.46)
				 :ambient-intensity 0.0933
				 :billboard t)
	 :strings "Hey Now")


   (kid :type 'general-note-kid
	:center (translate (the center) :rear 10))

   ))


(define-object general-note-kid (base-object)

  :objects
  ((center-sphere :type 'sphere 
		  :radius .1)
		  
   (note :type 'general-note
	 :display-controls (list :color (list .32 .54 .26)
				 :billboard t
				 :specular-color (list 0.46 0.46 0.46)
				 :ambient-intensity 0.0933)
	 :strings "Hey Now kid")


   ))


    