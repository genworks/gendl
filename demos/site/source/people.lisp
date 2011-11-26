(in-package :genworks.com)

(define-object people (base-site-sheet)
  
  :computed-slots ((body-class "people"))
  
  :objects
  ((column-left 
    :type 'sheet-section
    :main-view nil)
   
   (column-right 
    :type 'sheet-section
    :main-view nil)
   
   (column-center 
    :type 'sheet-section
    :main-view
    (with-cl-who-string ()
      ((:div :class "content") 
       (:h2 "People")
       (:h5 "Dave Cooper")
       (:p "President and Head of Product Development") 
       (:p "Genworks International")
       (:p "Dave founded Genworks International in 1997.  Prior to that, he 
           worked a number of years for Ford Motor Company in its 
           KBE [Knowledge-Based Engineering] Department in Dearborn, MI.")
       (:p "For the first few years after its founding, Genworks served
   	   principally as an independent contract-programmer for area
   	   companies, notably General Motors Corp, Visteon, Ford Motor
   	   Company, and others, working with their respective KBE
   	   tools to perfect and deliver applications. Beginning in the
   	   early 2000s, Genworks shifted its attention to creating and
   	   developing its own independent KBE tool and accompanying
   	   software, efforts which resulted in the development of its
   	   \"GDL\" tool, which stands for General-Purpose Declarative
   	   Language (see the layman's Glossary in our website as
   	   needed).  In a few short years, Genworks, in a measured and
   	   deliberate manner, has developed a growing client-base in
   	   the U.S, UK, and the European continent.")
       (:p "Dave holds B.S and M.S degrees in Computer Science,
	   both from the University of Michigan in Ann Arbor, MI.
	   Included with those degrees were grant programs with
	   Japanese universities (Tokyo and Hiroshima) and a year's
	   study in Freiburg, Germany. He is fluent in the German
	   language.")
       (:p "Dave has spoken and delivered papers at numerous
	   Conferences in the U.S., Europe and Japan.  The
	   presentations have focused on KBE issues, the Lisp programming
           language, and the ICAD Users Group. Below are some representative
           papers, from 2005, 2007, and 2008.")
       (:p ((:a :href "http://downloads.genworks.com/kbe2008.pdf") 
	    "2008 SAE Aerospace Conference, presented in Wichita, Kansas"))
       (:p ((:a :href "http://downloads.genworks.com/kbe2007.pdf") 
	    "2007 AIAA/ATIO Conference, presented in Belfast, N. Ireland"))
       (:p ((:a :href "http://downloads.genworks.com/hsc.pdf") "2005 Huntsville Simulation Conference"))
       

       (:h5 "Teodor-gelu Chiciudean") 
       (:p "Documentation, Geometry Primitives Development, Support")
       (:p "Mr. Chicuidean was engaged for several years with GDL and KBE
work at the TU Delft in the Netherlands. He is currently completing his PhD Dissertation for the 
TU Delft in the area of KBE applied to Wind Turbine design. He has also recently completed a 
PhD in Biochemistry from Bucharest University. Teodor brings a deep understanding of 
KBE training/documentation techniques and geometric modeling.")

       
       (:h5 "Paul Tarvydas") 
       (:p "GDL Porting and Symmetric Multiprocessing")
       (:p "Mr. Tarvydas has a background in Physics and Electrical Engineering from the 
University of Toronto. He is involved with improving ANSI Common Lisp standards compliance of the 
GDL source code to facilitate the porting of GDL to additional CL platforms. Paul also
has expertise in Symmetric Multiprocessing with CL and helping to make SMP capabilities 
seamlessly available in the GDL environment.")
       
       
       (:h5 "David Cooper Sr.") (:p "Vice-President and General Manager")
       (:p "Mr Cooper's background is in the law.  His legal background
	   is extensive, based mainly around Contracts and Insurance
	   law. He has been in practice continuously for 50 years, is
	   a graduate of the University of Michigan Law School, is
	   the past-president of the organized civil defense bar in
	   the State of Michigan, and has been continuously listed
	   in the elite publication Best Lawyers in America since
	   that publication's inception in the late 1980's (less
	   than 1% of practicing attorneys are included in Best
	   Lawyers). Mr Cooper has also recently been added to 
           Super Lawyers as a specialist in INSURANCE COVERAGE AND CONTRACTS LAW.")
       (:p "At Genworks, He performs a managerial role for the
	   company, and acts as General Counsel reviewing contracts
	   and in general keeping Genworks' business relationships
	   running smoothly.")
       (:h5 "Kai Yan Cooper") 
       (:p "Accounts Manager and Licensing Administrator")
       (:p "Kai handles bookkeeping, customer relations, and license 
           tracking for the GDL product. Kai will also act as a 
           language liaison for Genworks activities in the Chinese 
           market. Kai studied English language at Changchun University 
           in China, obtained a Master's Degree in Accounting from Walsh 
           College in Troy, Michigan, worked for several years 
           as a Financial Analyst at a global manufacturing firm, and
           is currently pursuing a technical degree in IT and Database 
           technology. ")

       
       (:h5 "KE-works, Delft, The Netherlands") 
       (:p "Documentation and Design Consulting")
       (:p "KE-works is a KBE implementation company  with roots from the Technical University of Delft in the Netherlands.

 KE-works is periodically engaged to give the GDL web-based development environment design improvements,
 using state-of-the-art principles from ergonomics and Industrial Design.")

       
       
       (:h5 "Alan Clark")
       (:p "Contract Programmer")
       (:p "Alan has decades of experience in software development using Lisp and many other 
languages, as well as computational geometry and interfacing with geometry kernels 
and CAD systems. 

         Alan provides periodic maintenance support for the GDL system, including our interfaces 
         to the SMLib geometry kernel and our automated build process")

       
       )))))

