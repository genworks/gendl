(in-package :www.genworks.com)

(define-object people (base-site-sheet)

  :computed-slots
  ((title "Genworks International - People")
   (link-title  "People")
   
   (right-section-inner-html
    (with-cl-who-string ()
      (:h2 "The People who make Genworks possible") 
      
      (:h3 "J.R. \"Bob\" Dobbs")
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "President")
	(:p
	 "\"Bob\" founded Genworks in 1997 with the intention to make Generative technology and 
Knowledge Based Engineering available to more than just the toplevel niche markets.")))

      (:h3 "Dave Cooper")
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Head of Product Development")
	(:p
	 "Dave started as Head of Product Development with the first commercial release of 
Genworks GDL (GenDL) in August, 2002.  Prior to that, he performed contract KBE programming with 
The ICAD System&reg; at Ford Motor Company, General Motors, Raytheon Aircraft, Embraer, and elsewhere. 
Prior to that Dave was an employee at Ford Motor Company for several years, involved with ICAD application
development and systems deployment.")
	(:p "Dave holds B.S and M.S degrees in Computer Science and Engineering, 
	   both from the University of Michigan in Ann Arbor, MI.")
	(:p "Dave is also the founder of the Common Lisp Foundation, dedicated to propogating 
the CL programming language, and is a Director on the Association of Lisp Users.")
	(:p "Dave has spoken and delivered papers at numerous
	   Conferences in the U.S., Europe and Japan.  The
	   presentations have focused on KBE issues and the Lisp programming
           language. Below are some representative papers.")
	(:p ((:a :href "http://downloads.genworks.com/kbe2008.pdf") 
	     "2008 SAE Aerospace Conference, presented in Wichita, Kansas"))
	(:p ((:a :href "http://downloads.genworks.com/kbe2007.pdf") 
	     "2007 AIAA/ATIO Conference, presented in Belfast, N. Ireland"))
	(:p ((:a :href "http://downloads.genworks.com/hsc.pdf") "2005 Huntsville Simulation Conference"))))

      (:h3 "Teodor-Gelu \"Ted\" Chiciudean") 
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Documentation, Geometry Primitives Development, Support")
	(:p "\"Ted\" founded KE-Works S.R.L. Romania in 2009, which is a Value-Added General Reseller for
Genworks GenDL. At that time, he was a PhD Researcher at Delft University of Technology in the department 
of Systems Engineering and Aircraft Design. In 2011 he became engaged directly with Genworks International 
as a consultant for Documentation, Geometry Primitives Development and Support.")
	(:p "\"Ted\" holds a Dipl-Ing. \"M.Sc.\" degree in Aircraft Propulsion System, from 
Politehnica University of Bucharest, Faculty of Aerospace Engineering, and a Doctor degree in 
Chemical Engineering \"Composite Materials\" from Politehnica University Bucharest, Faculty of 
Applied Chemistry and Materials Science.")))
      
      (:h3 "Paul Tarvydas") 
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "GenDL Porting and Symmetric Multiprocessing")
	(:p "Mr. Tarvydas has a background in Physics and Electrical Engineering from the 
University of Toronto. He is involved with improving ANSI Common Lisp standards compliance of the 
GenDL source code to facilitate the porting of GenDL to additional CL platforms. Paul also
has expertise in Symmetric Multiprocessing with CL and helping to make SMP capabilities 
seamlessly available in the GenDL environment.")))

      (:h3 "David Cooper Sr.") 
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Vice-President and General Manager")
	(:p "Mr Cooper's background is in the law.  His legal background is extensive, 
based mainly around Contracts and Insurance law. He has been in practice continuously 
for 50 years, is a graduate of the University of Michigan Law School, is the past-president 
of the organized civil defense bar in the State of Michigan, and has been continuously 
listed in the elite publication Best Lawyers in America since that publication's 
inception in the late 1980's (less than 1% of practicing attorneys are included in 
Best Lawyers). Mr Cooper has also recently been added to Super Lawyers as a specialist 
in Insurance Coverage and Contracts Law.")
	(:p "At Genworks, He performs a managerial role for the company, and acts as 
General Counsel reviewing contracts and in general keeping Genworks' business 
relationships running smoothly.")))

      
      (:h3 "Kai Yan Cooper") 
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Accounts Manager and Licensing Administrator")
	(:p "Kai handles bookkeeping, customer relations, and license tracking for 
the GenDL product. Kai will also act as a language liaison for Genworks activities 
in the Chinese market. Kai studied English language at Changchun University 
in China, obtained a Master's Degree in Accounting from Walsh College in 
Troy, Michigan, worked for several years as a Financial Analyst at a global 
manufacturing firm, and has completed work toward a technical degree in IT and 
Database technology. ")))))))


