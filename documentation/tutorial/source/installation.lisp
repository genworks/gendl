;;
;; Copyright 2002, 2009, 2012 Genworks International
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

(in-package :gendl-doc)

(defparameter *installation*
    `((:chapter :title "Installation")
      
      "Follow Section "
      (:ref "sec:installationofpre-packagedgendl")
      " if your email address is registered with Genworks and you will
install a pre-packaged Gendl distribution including its own Common
Lisp engine.  Gendl is also available as open-source software"
      (:footnote "http://github.com/genworks/Genworks-GDL")
      "; if you
want to use that version, then please refer to Section "
      (:ref "sec:installationofopen-sourcegendl")
      "."
      ((:section :title "Installation of pre-packaged Gendl")
       "This section will take you through the installation of Gendl
from a prepackaged distribution with the Allegro CL Common Lisp engine
and the Slime IDE (based on Gnu Emacs)."
       ((:subsection :title "Download the Software and retrieve a license key")
	((:list :style :enumerate)
	 (:item "Visit the Downloads section of the "
	   (:href "http://genworks.com/newsite" "Genworks Newsite"))
	 (:item "Enter your email address" 
	   (:footnote "if your address is not on file, send mail to licensing@genworks.com")
	   ".")
	 (:item "Download the latest Payload and gpl.zip for Windows"
	   (:footnote "If you already have a gpl.zip from a previous Gendl installation, it is not necessary to download a new one."))

	 (:item "Click to receive license key file by email.")))
       
       ((:subsection :title "Unpack the Distribution")
	"GenDL is currently distributed for all the platforms as a
self-contained ``zip'' file which does not require official
administrator installation.  What follows are general instructions; more up-to-date details
may be found in the email which accompanies the license key file. A five-minute installation video
is also available in the Documentation section of the "
	(:href "http://genworks.com/newsite" "Genworks Newsite") "."
	((:list :style :enumerate)
	 (:item "Unzip the gdl1581... zipfile into a location where you have write permissions")
	 (:item "Unzip the gpl.zip file at the same level as the gdl payload")
	 (:item "Copy the license key file as gdl.lic (for Trial,
	 Student, Professional editions), or devel.lic (for Enterprise edition) into the "
	   (:texttt "program/")
	   " directory within the gdl1581.../ directory."))

	((:image-figure :image-file "gendl-installation.png"
			:caption "Several Gendl versions and one GPL "
			:label "fig:gendl-installation"))


	"So you now should have two directories at the same level: one named "
	(:texttt "gdl1581.../") 
	"(the rest of the name will contain the specific dated build stamp), and a "
	(:texttt "gdl/")  
	"directory at the same level. Note that as seen in Figure "
	(:ref "fig:gendl-installation")
	", it is possible to have several Gendl versions installed, but just a single common "
	(:texttt "gpl/")
	" folder.")
       
       


       ((:subsection :title "Make a Desktop Shortcut")
	((:list :style :enumerate)
	 (:item "Using the ``My Computer'' or ``Computer'' Windows file manager, right-mouse on the "
	   (:texttt "run-gdl.bat")
	   " file.")
	 (:item "Select ``Create Shortcut.''")
	 (:item "Now drag the new ``Run-gdl-shortcut'' icon to your desktop.")))
       
       ((:subsection :title "Populate your Initialization File")
	"The default initialization file for Gendl is called "
	(:texttt "gdlinit.cl")
", "))

      ((:section :title "Installation of open-source Gendl")
       
       "This section is only relevant if you have not received a
pre-packaged Gendl distribution with its own Common Lisp engine.  If
you have received a pre-packaged Gendl distribution, then please skip
this section. In case you want to use the open-source Gendl, you will
use your own Common Lisp installation and fetch Gendl (Genworks-GDL)
using a very powerful and convenient CL package/library manager
called "
       (:emph "Quicklisp") "."
       
       ((:subsection :title "Install and Configure your Common Lisp environment")
	"Gendl is currently tested to build on the following Common Lisp engines:"
	((:list :style :itemize)
	 (:item "Allegro CL (commercial product from Franz Inc, free Express Edition available)")
	 (:item "LispWorks (commercial product from LispWorks Ltd, free Personal Edition available)")
	 (:item "Steel Bank Common Lisp (SBCL) (free open-source project with permissive license)"))
	"Please refer to the documentation for each of these systems for full information on installing 
and configuring the environment. Typically this will include a text editor, either Gnu Emacs with Superior
Lisp Interaction Mode for Emacs (Slime), or a built-in text editing and development environment which 
comes with the Common Lisp system.

As of this writing, a convenient way to set up Emacs with Slime is to use the "
	(:href "http://github.com/quicklisp/quicklisp-slime-helper" "Quicklisp-slime-helper")
	".")

       ((:subsection :title "Load and Configure Quicklisp")
	"As of this writing, Quicklisp is rapidly becoming the defacto standard library manager for Common Lisp."
	((:list :style :itemize)
	 (:item "Visit the " (:href "http://quicklisp.org" "Quicklisp website"))
	 (:item "Follow the instructions there to download the "
	   (:texttt "quicklisp.lisp")
	   " bootstrap file and load it to set up your Quicklisp environment."))))

      ((:section :title "System Startup and Testing")
       
       ((:subsection :title "System Startup")
	((:subsubsection :title "Startup of prepackaged Gendl distribution")
	 "To start a prepackaged system, follow these steps:"
	 ((:list :style :enumerate)
	  (:item "Invoke the "
	    (:texttt "run-gdl.bat")
	    " (Windows), or "
	    (:texttt "run-gdl")
	    " (Linux, MacOS) startup script. This should launch Gnu Emacs with a 
README file displayed by default. Take the time to look through this README file. 
Especially the later part of the file contains information about Emacs keyboard
shortcuts available.")
	 
	  (:item  "In emacs, enter: "
	    (:texttt "M-x glime")
	    ". That is, hold down the ``Meta'' (or ``Alt'') key, and press the ``X'' key, then type ``glime.''
You will see this command shown in the "
	    (:emph "mini-buffer")
	    " at the bottom of the Emacs window, as shown in Figure "
	    (:ref "fig:mini-buffer"))

	  ((:image-figure :image-file "mini-buffer.png"
			:caption "The mini-buffer in Emacs"
			:width "3in" :height "3in"
			:label "fig:mini-buffer"))


	  (:item "press the ``Enter'' key")

	  ((:image-figure :image-file "genworks-gendl-console.png"
			:caption "Genworks Gendl Console"
			:label "fig:genworks-gendl-console"))

	  (:item "On Windows, you will get a new window, named the the "
	    (:indexed "Genworks Gendl Console")
	    ", as shown in Figure "
	    (:ref "fig:genworks-gendl-console")
	    ". This window might start out in minimized form (as an icon at the bottom of your screen). Click on it 
to open it. Watch this console for any errors or warnings. 

The first time you start up, you may see messages in this console for
several minutes while the system is being built (or if you received a
completely pre-built system, the messages may last only a few
seconds).

On Linux or MacOS, there will be a separate Emacs buffer (available
through Emacs' ``Buffers'' menu) where you will see these messages.

The messages will consist of compiling and loading information, followed by copyright and welcome information
for Gendl. After these messages have finished, you should see the following command prompt:"
	    (:p (:texttt (:indexed "gdl-user(1): ")))

	    "The Genworks GenDL console contains a command prompt, but mostly you will use the "
	    (:indexed "*slime-repl...*") 
	    " buffer in Emacs to type commands. The Genworks GenDL console is mainly used for 
displaying output text from the Gendl system and from your application."
	    )))

       ((:subsubsection :title "Startup of open-source Gendl distribution")
	 "To start an Open-source distribution, follow these steps:"
	 ((:list :style :enumerate)
	  (:item "Start your Common Lisp engine and development environment (e.g. SBCL with Emacs and Superior Lisp Interaction Mode for Emacs).")
	  (:item "After Quicklisp is installed and initialized in your system, type: "
	    (:texttt "(ql:quickload :genworks-gdl)")
	    " to get Genworks Gendl installed and loaded in your environment.")

	  (:item "Type the following to initialize the Gendl environment:"
	    (:p (:texttt "(gdl:start-gdl :edition :open-source)"))))))
       

       

       ((:subsection :title "Basic System Test")
	"You may test your installation using the following
checklist. These tests are optional. You may perform some or all of
them in order to ensure that your Gendl is installed correctly and
running smoothly. In your Web Browser (e.g. Google Chrome, Firefox,
Safari, Opera, Internet Explorer), perform the following steps:"
	((:list :style :enumerate)
	 (:item "visit http://localhost:9000/tasty.")
	 (:item "accept default robot:assembly.")
	 (:item "Select ``Add Leaves'' from the Tree menu.")
	 (:item "Click on the top node in the tree.")
	 (:item "Observe the wireframe graphics for the robot as shown in "
	   (:ref "fig:tasty-robot")
	   ".")

	 ((:image-figure :image-file "tasty-robot.png"
			:caption "Robot displayed in Tasty"
			:height "3in" :width "3in"
			:label "fig:tasty-robot"))

	 
	 ((:image-figure :image-file "tasty-robot-x3dom.png"
			:caption "Robot x3dom"
			:height "3in" :width "3in"
			:label "fig:tasty-robot-x3dom"))


	 (:item "Click on the robot to zoom in.")
	 (:item "Select ``Clear View!'' from the View menu.")
	 (:item "Select ``X3DOM'' from the View menu.")
	 (:item "Click on the top node in the tree.")
	 (:item "``Refresh'' or ``Reload'' your browser window (may not be necessary).")
	 (:item "If your browser supports WebGL, you will see the robot in shaded dynamic view as shown in Figure"
	   (:ref "fig:tasty-robot-x3dom")
	   ".")
	 (:item "Select ``PNG'' from the View menu. You will see the wireframe view of the robot as a PNG image.")
	 (:item "Select ``X3D'' from the View menu. If your browser has an X3D plugin installed (e.g. BS Contact), 
you will see the robot in a shaded dynamic view.")))

       
       ((:subsection :title "Full Regression Test")
	(:index "regression tests")
	"The following commands will invoke a full regression test, including a test of the Surface and Solids
primitives provided by the SMLib geometry kernel. Note that the SMLib geometry kernel is only available with
proprietary Gendl licenses --- therefore if you have an open-source or Trial version, you these regression
tests will not all function.

In Emacs at the "
	(:texttt "gdl-user>")
	" prompt in the "
	(:texttt "*slime-repl...*") " buffer, type the following commands:"
	((:list :style :enumerate)
	 (:item (:texttt "(ql:quickload :gdl-regression)"))
	 (:item (:texttt "(gdl-lift-utils::define-regression-tests)"))
	 (:item (:texttt "(gdl-lift-utils::run-regression-tests-pass-fail)"))
	 (:item (:texttt "(pprint gdl-lift-utils::*regression-test-report*)")))))


      ((:section :title "Getting Help and Support")
       "If you run into unexplained errors in the installation and startup process, please contact the following resources:"
       
       ((:list :style :enumerate)
	(:item "Make a posting to the "
	  (:href "http://groups.google.com/group/genworks" "Genworks Google Group"))

	(:item "For pure Common Lisp issues, join the #lisp IRC (Internet Relay Chat) channel and discuss issues there.")

	(:item "Also for Common Lisp issues, follow the comp.lang.lisp Usenet group.")

	(:item "If you are a supported Genworks customer, send email to "
	  (:href "mailto:support@genworks.com" "support@genworks.com"))
	
	(:item "If you are not a supported Genworks customer but you want to report an apparent bug or have other suggestions or inquiries, you may also send email to "
	  (:href "mailto:support@genworks.com" "support@genworks.com")
	  ", but please understand that Genworks cannot guarantee any response or a particular timeframe for any response.")))))




	    
	   
       

