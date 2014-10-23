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
    `((:chapter :title "Installation [GDL and Gendl]")
      
      "Please follow Section "
      (:ref "sec:installationofpre-packagedgdl")
      " if your email address is registered with Genworks and you will
install a pre-packaged Genworks GDL distribution, including its own
Common Lisp engine.  The foundation of Genworks GDL is also available
as open-source software through The Gendl Project"
      (:footnote "http://github.com/genworks/gendl")
      "; if you elect to use that version, then please refer to Section "
      (:ref "sec:installationofopen-sourcegendl")
      "."
      ((:section :title "Installation of pre-packaged GDL")
       "This section will take you through the installation of
Genworks GDL from a prepackaged distribution with the Allegro CL or
LispWorks commercial Common Lisp engine and the Slime IDE (based on
Gnu Emacs)."
       ((:subsection :title "Download the Software and retrieve a license key")
	((:list :style :enumerate)
	 (:item "Visit the Downloads section of the "
	   (:href "http://genworks.com" "Genworks Website") ";")
	 (:item "Enter your email address" 
	   (:footnote "if your address is not on file, send mail to licensing@genworks.com")
	   ";")
	 (:item "Download the latest Payload for Windows, Linux, or Mac;")
	 (:item "Click to receive the license key file by email.")))
       
       ((:subsection :title "Unpack the Distribution")
	"Genworks GDL is currently distributed as a setup executable for Windows,
a ``dmg'' application bundle for Mac, and a self-contained zip file for Linux."
	(:ul
	 (:item "Run the installation executable. Accept the defaults when prompted."
	   (:footnote "For Linux, you have to install emacs and ghostscript yourself. Please use your distribution's package manager to complete this installation."))
	 (:item "Copy the license key file as gdl.lic (for Trial,
	 Student, Professional editions), or devel.lic (for Enterprise edition) into the "
	   (:texttt "program/")
	   " directory within the "
	   (:texttt "gdl/gdl/program/")
	   " directory.")

	 (:item "Launch the application by finding the Genworks program group in the Start menu (Windows), or by double-clicking the application icon (Mac), or by running the "
	   (:texttt "run-gdl")
	   " script (Linux)."))))


      ((:section :title "Installation of open-source Gendl")
       
       "This section is only germane if you have not received a
pre-packaged Gendl or Genworks GDL distribution with its own Common
Lisp engine.  If you have received a pre-packaged Gendl distribution
then you may skip this section. In case you want to use the
open-source Gendl, you will use your own Common Lisp installation and
obtain Gendl (Genworks-GDL) using a powerful and convenient CL
package/library manager called "
       (:emph "Quicklisp") "."
       
       ((:subsection :title "Install and Configure your Common Lisp environment")
	"Gendl is currently tested to build on the following Common Lisp engines:"
	((:list :style :itemize)
	 (:item "Allegro CL (commercial product from Franz Inc, free Express Edition available)")
	 (:item "LispWorks (commercial product from LispWorks Ltd, free Personal Edition available)")
	 (:item "Clozure CL (free CL engine from Clozure Associates, free for all use)")
	 (:item "Steel Bank Common Lisp (SBCL) (free open-source project with permissive license)"))
	"Please refer to the documentation for each of these systems
for full information on installing and configuring the
environment. Typically this will include a text editor, either Gnu
Emacs with Superior Lisp Interaction Mode for Emacs (Slime), or a
built-in text editing and development environment which comes with the
Common Lisp system.

A convenient way to set up Emacs with Slime is to use the "
	(:href "http://github.com/quicklisp/quicklisp-slime-helper" "Quicklisp-slime-helper")
	".")

       ((:subsection :title "Load and Configure Quicklisp")
	"Quicklisp is the defacto standard library manager for Common
Lisp."
	((:list :style :itemize)
	 (:item "Visit the " (:href "http://quicklisp.org" "Quicklisp website"))
	 (:item "Follow the instructions there to download the "
	   (:texttt "quicklisp.lisp")
	   " bootstrap file and load it to set up your Quicklisp environment.")))

       ((:subsection :title "Load and Start Gendl")
	"invoke the following commands at the Common Lisp toplevel ``repl'' prompt:"
	(:ol
	 (:li (:texttt "(ql:quickload :gendl)"))
	 (:li (:texttt "(gendl:start-gendl!)")))))

      ((:section :title "System Testing")
       ((:subsection :title "Basic Sanity Test")
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
	 (:item "Select ``PNG'' from the View menu. You will see the
	 wireframe view of the robot as a PNG image.")
	 (:item "Select ``X3D'' from the View menu. If your browser
has an X3D plugin installed (e.g. BS Contact), you will see the robot
in a shaded dynamic view.")))

       
       ((:subsection :title "Full Regression Test")
	(:index "regression tests")
	"The following commands will invoke a full regression test,
including a test of the Surface and Solids primitives provided by the
SMLib geometry kernel. Note that the SMLib geometry kernel is only
available with proprietary Genworks GDL licenses --- therefore, if you
have open-source Gendl or a lite Trial version of Genworks GDL, these
regression tests will not all function.

In Emacs at the "
	;;(:texttt "gdl-user>")
	(:texttt "gdl-user")
	" prompt in the "
	(:texttt "*slime-repl...*") " buffer, type the following commands:"
	((:list :style :enumerate)
	 (:item (:texttt "(ql:quickload :regression)"))
	 (:item (:texttt "(gdl-lift-utils::define-regression-tests)"))
	 (:item (:texttt "(gdl-lift-utils::run-regression-tests-pass-fail)"))
	 (:item (:texttt "(pprint gdl-lift-utils::*regression-test-report*)")))))


      ((:section :title "Getting Help and Support")
       "If you encounter unexplained errors in the installation and
startup process, please contact the following resources:"
       
       ((:list :style :enumerate)
	(:item "Make a posting to the "
	  (:href "http://groups.google.com/group/genworks" "Genworks Google Group"))

	(:item "Join the #gendl IRC (Internet Relay Chat) channel on
	irc.freenode.net and discuss issues there.")

	(:item "For exclusively Common Lisp issues, join the #lisp
	IRC (Internet Relay Chat) channel on irc.freenode.net and
	discuss issues there.")

	(:item "Also for Common Lisp issues, follow the comp.lang.lisp
	Usenet group.")

	(:item "If you are a supported Genworks customer, send email to "
	  (:href "mailto:support@genworks.com" "support@genworks.com"))
	
	(:item "If you are not a supported Genworks customer but you want to report an apparent bug or have other suggestions or inquiries, you may also send email to "
	  (:href "mailto:support@genworks.com" "support@genworks.com")
	  ", but as a non-customer please understand that Genworks
	  cannot guarantee a response or a particular time frame for a
	  response. Also note that we are not able to offer guaranteed
	  support for Trial and Student licenses ")))))




	    
	   
       

