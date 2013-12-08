
##Gendl®

Gendl® is a Generative Programming and Knowledge Based Engineering
framework, implementing concepts which date back to the 1980s and
which cutting-edge companies have been quietly using to gain
competitive advantage with mission-critical engineering
applications. These concepts required hundreds of thousands of dollars
investment in hardware and software just a couple decades ago. They
are now at your fingertips as an integral part of the Open-Source
ecosystem, running on commodity consumer-grade hardware.

Gendl allows for high-level declarative, object-oriented problem
solving and application development, including but not limited to the
ability to generate and manipulate 3D geometry. To solve a problem in
Gendl, you formulate it using the `define-object` operator, which
allows you to specify inputs, outputs (`computed-slots`), and child
objects, which then gives the ability to generate a "tree" of objects,
useful for decomposing complexity.

A graphical web-based interface, *tasty*, is available for interacting
with your system as it is developed.

The web-based GUI framework used to make tasty (GWL) is also available
for creating your own custom web-based user interfaces. 

### Fundamental KBE Features Provided

- Declarative (non-procedural) Syntax
- Object and value caching (i.e. runtime memorization)
- Dependency-tracking (cached values get recomputed when needed)

Gendl ships with a full set of wireframe 3D and 2D geometry primitives
along with output *lenses* for standard browser-based viewing and
exchange formats such as PDF, SVG, X3D, PNG, as well as DXF.

Optionally there is available a set of surface- and solid-modeling
primitives which currently depend on SMLib, a commercial geometry
kernel available from Solid Modeling Solutions, Inc. SMLib enables the
use of standard CAD data exchange formats such as Iges, STEP, and STL
(for 3D printing). 


### Basic Requirements:

 1. Common Lisp: Allegro CL 9.0, LispWorks 6.x, SBCL, or CCL (Clozure
    CL). Without web interface, initial ports to ECL, ABCL, and CLISP
    have also been completed. 

 2. Quicklisp (available from http://www.quicklisp.org)

 3. Gnu Emacs (recommended Editor/IDE -- native CL Editor/IDEs can
    also be used)

 4. Standard Web Browser (Chrome, Firefox, Safari, IE, Opera). Pick
    one with WebGL support if possible (check
    [here](http://www.x3dom.org/?page_id=9) and
    [here](http://www.x3dom.org/check/) to check for WebGL browser
    support)

 5. Curiosity, Creativity, and Courage


### Installation

 To load the entire system, you can do it with:

```common-lisp
  (ql:quickload :gendl)
  (gendl:start-gendl!)
```

Now you can do a quick sanity check by visiting:

  [http://localhost:9000/tasty](http://localhost:9000/tasty)

in your browser and trying to instantiate the default assembly tree
(robot).

### Emacs Editor/IDE Support

Slime (Superior Lisp Interaction Mode for Emacs) is recommended 
for developing Gendl applications with Emacs.  

Slime is available with:

   (ql:quickload :quicklisp-slime-helper)

`Glime' is our Gendl-specific customizations to Slime. This is
implemented entirely on the swank (Common Lisp) side of things, and
can be loaded with

 ```common-lisp
  (load (compile-file ".../gendl/emacs/glime.lisp"))
 ```

Although not strictly necessary, the file `.../gendl/emacs/gdl.el` is
also provided and can be used as a starting point or reference for
loading Glime and Gendl into an emacs environment.

### Further Documentation

Documentation is published
[here](http://genworks.com/downloads/tutorial.pdf), and this very much
an active work in progress. Training tutorials and videos are also in
progress and in their current state are available in the Documentation
section on http://genworks.com.


### Support/Community

* #gendl on freenode.irc.net (irc://irc.freenode.net/gendl)
* @gendl and @genworks on Twitter
* (common-lisp.net mailing list hopefully/possibly coming soon)
* [Genworks Google Group](http://groups.google.com/group/genworks)
* #lisp, #quicklisp, #emacs channels on irc.freenode.net

### Training/Coaching

* Genworks would like to begin hosting free Gendl/GDL training
  seminars in Metro Detroit and possibly other locations. Please
  contact [Genworks](http://genworks.com) if you are interested in
  being notified of any upcoming seminars.


### Gendl source code Architecture:

Gendl is separated into layered components, some of which depend on
others. Some components also depend on third-party external libraries,
which are currently handled with the Quicklisp system.

At the core "kernel" is the `:gendl` (nickname `:gdl`) package,
implemented with files in the folder `gendl/base/`. This includes the
compiler/expanders for `define-object` and related macros as well as
core primitives such as `vanilla-mixin`.

Including the base, there are eight modules supported with Gendl:


* `:base` - (gendl/base/) Gendl language kernel for compiling
     declarative object definitions and working with them at runtime.

* `:cl-lite` - (gendl/cl-lite/) For compiling and loading directory
     trees as projects. This can also generate ASDF files, and
     supplements the standard use of asdf and quicklisp.

* `:geom-base` - (gendl/geom-base/) Built-in Gendl 3D and 2D wireframe
      primitives and cartesian coordinate system for basic geometry
      generation and manipulation.


* `:gwl` - (gendl/gwl/) Generative Web Language, for representing web pages
       using GDL objects, includes Ajax-based web interaction with
       your model.


* `:gwl-graphics` - (gendl/gwl-graphics/) for including graphics
     rendered from geometry object in GWL web pages.

* `:surf` - (gendl/surf/) NURBS surface and brep/boolean solids
      primitives (these don't do much without the underlying geometry
      kernel middleware or implementation).

* `:tasty` - (gendl/apps/tasty/) web-based testing, tracking, and
   debugging utility. 

* `:yadd` - (gendl/apps/yadd/) for self auto-documentation.

* `:regression` - (gdl/regression) regression test utilities and tests.


### Alternative Geometry Kernels

If you have a different favorite solid modeling kernel
(e.g. OpenCascade, Parasolid, Geometros sgCore), then an interesting
project would be to interface the existing Surface package to that
kernel, by implementing the methods in surf/source/methods.lisp.

The Surface (`:surf`) package (in the `surf/` folder) contains all the
high-level Surface and Solid modeling primitives currently implemented
in Gendl. These primitives provide a protocol for what the objects
should be able to do (i.e. what messages they should answer), but
without the SMLib library and associated middleware available, they
will not be able to return any results.  The SMLib kernel and
associated middleware are available as part of the commercial
Genworks® GDL product from [Genworks®
International](http://genworks.com).


### License

[Affero Gnu General Public License](http://www.gnu.org/licenses/agpl.txt).

The AGPL, has the requirement that you release any derivatives and any
applications compiled with Gendl under AGPL-compatible license (if
distributed at all).


### Proprietary Distribution

For Proprietary (closed-source) development and distribution, the
commercial Genworks® GDL system (including Gendl® technology and,
optionally, commercial CL engines, technical support, and commercial
solid modeling engine) is available from [Genworks](http://genworks.com). 


### Bug Bounty Program

Genworks offers a modest bounty for reporting of bugs on the [Github
Issues
List](https://github.com/genworks/gendl/issues?state=open). Please
contact [Genworks](http://genworks.com) for details.

### Bug Fix Program

Genworks is open to considering bids for providing cleanly mergeable
fixes to bugs listed on the [Github Issues
List](https://github.com/genworks/gendl/issues?state=open). Please
contact [Genworks](http://genworks.com) for details.


