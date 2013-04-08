
##Gendl

Gendl is an AGPL-licensed Generative Programming and Knowledge Based
Engineering framework, building on concepts dating back to the
1980s. It allows for high-level declarative, object-oriented problem
solving and application development, including but not limited to the
ability to generate and manipulate 3D geometry. To solve a problem in
Gendl, you formulate it using the "define-object" operator, which
allows you to specify inputs, outputs (computed slots), and child
objects, which then gives the ability to generate a "tree" of objects,
useful for decomposing complexity.

A graphical web-based interface, /tasty/, is available for interacting
with your system as it is developed.

The web-based GUI framework used to make tasty (GWL) is also available
for creating your own custom web-based user interfaces. 

### Fundamental KBE Features Provided

- Declarative (non-procedural) Syntax
- Object and value caching (i.e. runtime memorization)
- Dependency-tracking (cached values get recomputed when needed)

Optionally there is available a set of surface- and solid-modeling
primitives which currently depend on SMLib, a commercial geometry
kernel available from Solid Modeling Solutions, Inc.

The Surface (:surf) package (in the surf/ folder) contains all the
high-level Surface and Solid modeling primitives and is available
under the AGPL with the Gendl project. These primitives provide a
protocol for what the objects should be able to do (i.e. what messages
they should answer), but without the SMLib library and associated
middleware available, they will not be able to return any results.
The SMLib kernel and associated middleware are available as part of
the commercial Genworks GDL product Genworks International
(http://genworks.com).

If you have a different favorite solid modeling kernel
(e.g. OpenCascade, Parasolid, Geometros sgCore), then it may be
possible to interface the existing Surface package to that kernel, by
implementing the methods in surf/source/methods.lisp. 

Gendl is distributed under AGPL, which has the requirement that you
release your updates and any applications compiled with Gendl under
AGPL-compatible license (if distributed at all). 

For Proprietary (closed-source) development and distribution, the
commercial Genworks GDL system is available from http://genworks.com.


### Current Requirements:

 1. Common Lisp: Allegro CL 9.0, LispWorks 6.x, SBCL, or CCL (Clozure
    CL). Without web interface, initial ports to ECL, ABCL, and CLISP
    have also been completed. 

 2. Quicklisp (available from http://www.quicklisp.org)

 To load the entire system, you can do it with:

  (ql:quickload :gendl)
  (gendl:start-gendl!)


Now you can do a quick sanity check by visiting:

  localhost:9000/tasty 

in your browser and trying to instantiate the default assembly tree
(robot).

Slime (Superior Lisp Interaction Mode for Emacs) is recommended 
for developing Gendl applications with Emacs.  

Slime is available with:

   (ql:quickload :quicklisp-slime-helper)

Documentation is in documentation/tutorial/pdf/tutorial.pdf 

and this very much an active work in progress. Training tutorials and videos
are also in progress and in their current state are available in the
Documentation section on http://genworks.com.


To help in understanding the role of each module, the overall Gendl
source architecture is partially described below.


### Gendl source code Architecture:

Gendl is separated into layered components, some of which depend on
others. Some components also depend on third-party external libraries,
which are currently handled with the Quicklisp system.

At the core "kernel" is the :gdl package, implemented with files in
the folder gdl/base/. This includes the compiler/expanders for
"define-object" and related macros as well as core primitives such as
vanilla-mixin.

Including the base, there are eight modules supported with Gendl:


* :base - (gendl/base/) Gendl language kernel.


* :cl-lite - (gdl/cl-lite/) Gendl system definitions and loading
      facility. Supplements our use of asdf and quicklisp.


* :regression - (gdl/regression) regression test utilities.


* :geom-base - (gdl/geom-base/) Built-in Gendl wireframe primtives
      and basic geometry/vector manipulation functions.


* :gwl - (gdl/gwl/) Generative Web Language, represent web pages
       using GDL objects, includes Ajax-based web interaction with
       your model.


* :gwl-graphics - (gdl/gwl-graphics/) include graphics rendered
      from geometry object in GWL web pages.


* :surf - (gdl/surf/) surface primitives without underlying
      geometry kernel middleware or implementation.


* :yadd - (gdl/apps/yadd/) self auto-documentation.


### License

Affero Gnu General Public License (http://www.gnu.org/licenses/agpl.txt)



