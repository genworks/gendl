(in-package :training-g105)

(define-object session-01 (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Introduction to Distributed GDL")
   
   (slide-data `((:title "Goals for Distributed GDL" :bullet-points
			 ((:description "Enable standard GDL message-passing communication between pairs of GDL runtime hosts")
			  (:description "Provide a basis for convenient parallel processing using multiple hosts/cores")
			  (:description "Reduce garbage collector load by decomposing 1 large memory heap into <i>n</i> smaller ones")))

		 (:title "Topics Covered in G105" :bullet-points
		  ((:description "The remote-object primitive, overview") 
		   (:description "Remote-object syntax")
		   (:description "Current dGDL Best Practices")))

		 
		 (:title "The <tt>remote-object</tt> Primitive")
		 
		 (:title "<tt>remote-object</tt> syntax")
		 
		 (:title "Current dGDL Best Practices")))))
