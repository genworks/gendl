(in-package :www.genworks.com)

(define-object index-html (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Welcome")
   (link-title "Home")
   (right-section-inner-html  (with-cl-who-string ()
				(:H2 "Welcome to Genworks")
				((:DIV :ID "welcome")
				 ((:IMG :SRC "/newsite-static/images/robot-small.gif" :ALT "Pic 1" :CLASS "left" :WIDTH "171"
					:HEIGHT "137"))
				 (:P (:STRONG "Genworks")
				     " provides General-purpose Declarative Language (GDL), a Generative Applicaton Development system 
for creating web-centric Knowledge Based Engineering and Business applications. Based on both ANSI and de-facto standards, GDL is 
generative on many levels, "
				     ((:a :href "http://en.wikipedia.org/wiki/Automatic_programming") "generating")
				     " detailed code while you write high-level definitions, then generating solutions to your problems 
according to those definitions.")
				 "The "
				 (:STRONG " (GDL)")
				 " suite is an open platform that blends the power of Common Lisp and "
				 (:STRONG "NURBS-based") " geometry kernels. "

				 (:P "Genworks GDL comes in a variety of configurations depending on your needs and resources, 
starting from a free open-source distribution through to fully supported packages with proprietary licensing and built with  
high-end commercial components.")

				 
				 (:P (:STRONG "Genworks")
				     " is the first-level vendor for GDL. We work with a network of General Resellers and Value-added Resellers to provide
you with customized services and end-user applications, depending on your precise requirements."))
				
				(:H3 "Company Profile")
				((:DIV :ID "profile")
				 ((:DIV :ID "corp")
				  ((:DIV :ID "corp-img") "Aerospace")
				  (:P (:STRONG "Genworks International")
				   " was founded in November, 1997 as Knowledge Based Solutions, a Michigan Corporation. ")
				  (:P "We operate as an independent entity, free of underlying debt or big-corporate influences.")
				  (:P "Our focus is primarily as a first-level vendor for the GDL suite of tools, rather than domain-specific 
vertical applications or specialized application consulting. This allows us to keep objectives aligned with our customers, and avoid 
potential conflict-of-interest experienced by other KBE vendors who also presume to dominate the market for vertical applications and 
consulting using their own tool.")))))))

  

