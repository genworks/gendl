(in-package :www.genworks.com)

(define-object people (base-site-sheet)

  :computed-slots
  ((title "Genworks International - People")
   (link-title  "People")
   
   (right-section-inner-html
    (with-cl-who-string ()
      (:H2 "Our people") 
      ((:DIV :ID "welcome"))
      (:H3 "Chiciudean Teodor Gelu  ")
      ((:DIV :ID "profile")
       ((:DIV :ID "peop")
	(:H4 "Engineering Consultant, composite materials and KBE ")
	(:P
	 "Teodor founded KE-Works S.R.L. Romania in 2009. At that time, he was a PhD Researcher 
at Delft University of Technology in the department of Systems Engineering and Aircraft Design. 
In 2011 he joined Genworks International as a consultant for Documentation, Geometry Primitives 
Development and Support.")
	(:P
	 "Teodor holds a Dipl-Ing. &quot;M.Sc.&quot; degree in Aircraft Propulsion System, from Politehnica 
University of Bucharest, Faculty of Aerospace Engineering and a Doctor degree in Chemical Engineering &quot;Composite Materials&quot; 
from Politehnica University Bucharest, Faculty of Applied Chemistry and Materials Science. ")))
      (:H3 "Alina Alexandra Tache ")
      ((:DIV :ID "profile")
       ((:DIV :ID "peop")
	(:H4 "Engineering Consultant, composite materials and NDI")
	(:P
	 "Alina has a background in Chemical Engineering. She holds a M.Sc. degree from Politehnica University of 
Bucharest, Faculty of Applied Chemistry and Materials Science. She is involved with research in improving the  
polymer matrix fiber interface, Bio-composite materials design, prototype manufacturing and testing.")
	(:P
	 "She possesses expertise in Nondestructive Inspection (NDI) and it is a certified&nbsp; level I 
practitioner in ARN-TN Insignificant Radiologic Risk Activities - Nuclear Techniques by the Romanian 
National Commission for Nuclear Activities Control (CNCAN). ")))
      (:H3 "Carcu Andrei Eugen ")
      ((:DIV :ID "profile")
       ((:DIV :ID "peop") (:H4 "IT Consultant, visualization and networking ")
	(:P
	 "Andrei  has a background in Telecommunications and Multimedia Technology. He holds a Dipl-Ing. &quot;M.Sc.&quot; 
degree from Technical University of Cluj Napoca, Faculty of Telecommunications and Technology of Information. 
He is involved in product configuration management (hardware and software &quot;C, C#, C++&quot;), 
database &quot;SQL&quot; and server deployment support.")
	(:P
	 "Andrei has several years of experience as a support engineer at CEMA AG and Alcatel-Lucent 
and holds a Microsoft Active Directory and Cisco certification.")))
      (:H3 "Varga Diana Iulia ")
      ((:DIV :ID "profile")
       ((:DIV :ID "peop") (:H4 "Consultant, marketing and customer relations")
	(:P
	 "Diana has a background in Management in which she holds a M.Sc. degree from University Babes-Bolyai, Cluj-Napoca. 
She is involved in finance budgeting, customer relations, and licensing.")))))))

