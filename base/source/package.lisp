;;
;; Copyright 2002, 2007, 2013 Genworks International
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

(in-package :common-lisp-user)


(defpackage :gendl-boot (:export #:system-description #:system-home))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or allegro lispworks sbcl ccl abcl clisp)   (error "

D'oh! GenDL is not yet supported on ~a. If you would like to try porting it, start with this file,
gdl/base/common/genworks.lisp. 
Also, PortableAllegroserve is needed for the web framework. 
If you are interested in this effort we would love to hear from you at open-source@genworks.com.

" (lisp-implementation-type))

  
  ;;
  ;; Copied here from ./genworks.lisp for bootstrapping purposes. 
  ;;
  (defun gendl-boot:system-description (system-designator &optional (errorp t))
    (let (description (home (gendl-boot:system-home system-designator errorp)))
      (when home 
	(let ((description-file (merge-pathnames "description.isc" home)))
	  (when (probe-file description-file)
	    (setq description
		  (with-open-file (in description-file) (read in))))))
      (or description (format nil "~a Subsystem" system-designator))))


  (defun gendl-boot:system-home (system-designator &optional (errorp t))
    (if (find-package :asdf)
	(funcall (read-from-string "asdf:system-source-directory") system-designator)
	(when errorp (error "~&glisp:system-home was called, but cannot function because asdf is not loaded.~%")))))



(defpackage :gendl
  ;;
  ;; FLAG -- intention is to change this to :base eventually ---
  ;; "gendl" is name of overall system.
  ;;
  (:nicknames :gdl :genworks :base)
  (:documentation #.(gendl-boot:system-description :base))
  (:use :common-lisp)
  (:shadow #:the)
  (:export #:%bottom-margin%
           #:%left-margin%
           #:%paper-height%
           #:%paper-width%
           #:%projection-vector%
           #:%view-plane-inverse%
           #:%view-plane-normal%
           #:*%format%*
           #:*allowed-part-documentation-keywords*
           #:*color-table*
           #:*color-table-decimal*
           #:*colors-default*
           #:*compile-circular-reference-detection?*
           #:*compile-dependency-tracking?*
           #:*compile-documentation-database?*
           #:*compile-for-dgdl?*
           #:*compile-source-code-database?*
           #:*compile-with-dependency-recording?*
           #:*current-version*
           #:*curve-chords*
           #:*display-controls*
           #:*dummy-version*
           #:*ensure-lists-when-bashing?*
           #:*error-on-reserved-words?*
           #:*force-restore-slot-default?*
	   #:*override-non-settables?*
           #:*gdl-init-functions*
	   #:*gendl-version*
	   #:*gendl-patch-level*
           #:*ghostscript-path*
           #:*gs-path*
	   #:*sort-children?*
           #:*load-documentation-database?*
           #:*load-source-code-database?*
           #:*out-of-bounds-sequence-reference-action*
           #:*paper-size-plist*
           #:*paper-size-table*
           #:*patch-fasls*
           #:*patches-root-dir-default*
           #:*recompile-gdl?*
           #:*remember-previous-slot-values?*
           #:*report-gdl-redefinitions?*
           #:*reserved-word-protected-packages*
           #:*retain-object-identity-on-type-change?*
           #:*rgb-cube-colors*
           #:*run-with-circular-reference-detection?*
           #:*run-with-dependency-recording?*
           #:*run-with-dependency-tracking?*
           #:*skin*
           #:*stream*
           #:*ui-server*
           #:*undeclared-parameters-enabled?*
           #:*zero-epsilon*
           #:+kappa+
           #:+phi+
           #:2pi
           #:alist2plist
           #:always
           #:append-elements
           #:assembly-leaf-mixin
           #:assembly-node-mixin
           #:base-format
           #:base-rule-object
           #:base-spec-sheet
           #:binary-search
           #:cl
           #:cl-dir
           #:cl-lite
           #:clean
           #:clean-dir
           #:copy-files-to
           #:cyclic-nth
           #:decode-for-http
           #:defaulting
           #:defcompanion
           #:define-color-set
           #:define-colored-icon
           #:define-format
           #:define-icon
           #:define-lens
           #:define-object
           #:define-object-amendment
           #:define-object-documentation
           #:define-package
           #:define-skin
           #:define-view
           #:definition-tree
           #:defpart
           #:defwriter
           #:distribute
           #:distribute-dir
           #:div
           #:encode-for-http
           #:ensure-list
           #:evaluate
           #:evaluate-object
           #:expandit
           #:find-messages-used-by
           #:find-messages-which-use
           #:flatten
           #:format-slot
           #:fround-to-nearest
           #:gdl-class
           #:gdl-format-class
           #:gdl-format-symbol
           #:gdl-object-symbol
           #:gdl-object-symbol
           #:gdl-remote
           #:gdl-skin-class
           #:get-version
           #:half
           #:ignore-errors-with-backtrace
           #:index-filter
           #:iso-8601-date
	   #:universal-time-from-iso-8601
	   #:universal-time-to-plist
           #:lastcar
           #:least
           #:let-streams
           #:list-elements
           #:list-hash
           #:list-of-numbers
           #:lookup-color
	   #:load-quicklisp
	   #:load-hotpatches
	   #:*quicklisp-home*
           #:make-canonical-part
           #:make-keyword
	   #:ensure-keyword
           #:make-keyword-sensitive
           #:make-object
           #:make-part
           #:make-self
           #:mapsend
           #:maptree
           #:match-regexp*
           #:max-of-elements
           #:merge-common-keys
           #:message-popup
           #:min-max-search
           #:min-of-elements
           #:most
           #:near-to?
           #:near-zero?
           #:never
           #:null-object
           #:null-part
           #:number-format
           #:number-round
           #:peruse-file
           #:pi/2
           #:plist-keys
           #:plist-values
           #:print-hash
           #:print-messages
           #:print-variables
           #:quantification
           #:query-collect
	   #:query-license-server
           #:read-safe-string
           #:read-snapshot
           #:readable-expression
           ;;#:record-source-file
	   #:register-init-function
           #:remote-object
           #:remove-plist-key
	   #:remove-plist-entry
           #:remove-plist-keys
           #:replace-substring
           #:restore-ui-object
           #:retitle-emacs
           #:retrieve
           #:rgb-cube-colors
           #:round-to-nearest
           #:safe-float
           #:safe-sort
           #:self
           #:send
           #:series
           #:set-format-slot
           #:set-self
           #:split
           #:standard-query
	   #:start-gendl!
           #:status-message
           #:string-append
           #:sum-elements
           #:tak
           #:the
           #:the-child
           #:the-element
           #:the-object
           #:to-double-float
           #:to-single-float
           #:traverse-tree
           #:twice
           #:undefine-object
           #:update
           #:update-gdl
	   #:update-gendl
           #:vanilla-mixin
           #:vanilla-mixin*
           #:vanilla-remote
           #:version-sequence
           #:with-error-handling
           #:with-format
           #:with-format-slots
           #:with-oracle
           #:with-version
           #:write-env
           #:write-objects
           #:write-plist
           #:write-the
           #:write-the-object
           #:xml-reader
           #:^2
	   #:room-report
	   #:*onclick-function*
	   #:*already-loaded-systems*
	   #:*loaded-hotpatches*
	   #:*packages-to-lock*
	   #:*patch-url-base*
	   #:*warn-on-invalid-toplevel-inputs?))


#-(or allegro lispworks sbcl ccl abcl ecl clisp) (error "Need package for mop:validate-superclass for currently running lisp.~%")
(defpackage :glisp
  (:documentation #.(gendl-boot:system-description :glisp))
  (:use :common-lisp)
  (:shadow #:intern)
  (:nicknames :com.genworks.lisp)
  (:import-from #+(or allegro abcl) :mop #+lispworks :hcl #+sbcl :sb-mop  #+ccl :ccl #+(or ecl clisp) :clos
		#:validate-superclass)
  (:export 
   ;;
   ;; Some implemented in gendl/base/source/genworks.lisp, the rest 
   ;; in gendl/glisp/source/genworks.lisp.
   ;;
   ;; FLAG -- alphabetize all these here in this package export list.
   ;;
   #:*external-text-format*
   #:*gdl-home*
   #:*gendl-home*
   #:*gdl-program-home*
   #:*gendl-program-home*
   #:*genworks-source-home*
   #:*gendl-source-home*
   #:*gdl-source-home*
   #:basic-command-line-arguments
   #:begin-redefinitions-ok
   #:current-directory
   #:define-constant
   #:direct-superclasses
   #:direct-superclass-names
   #:display-startup-banner
   #:end-redefinitions-ok
   #:ensure-string
   #:eql-specializer
   #:executable-homedir-pathname
   #:featurep
   #:find-feature-version
   #:gl-class-name
   #:gl-method-specializers
   #:hex-string-to-integer
   #:intern
   #:make-sans-value-equalp-hash-table
   #:make-sans-value-hash-table
   #:make-weak-hash-table
   #:make-versioned-features
   #:set-default-character-width
   #:set-default-float-format
   #:set-default-package
   #:set-defpackage-behavior
   #:set-local-compiler-tweaks
   #:set-features
   #:set-settings
   #:set-window-titles
   #:sexpr-from-file
   #:source-pathname
   #:system-description
   #:system-home
   #:set-genworks-source-home-if-known
   #:upcase
   #:validate-superclass
   #:with-definition-unit
   #:without-package-variance-warnings
   #:w-o-interrupts
   #:xref-off
   #:xref-on
	     
   #:*fasl-extension*
   #:concatenate-fasls
   #:directory-list
   #:file-directory-p
   #:run-command
   #:temporary-folder
   #:temporary-file

   #:find-gs-path
   #:get-pid
   #:run-gs
   #:set-gs-path
   #:run-program	     
   #:run-shell-command	     

   #:*enable-utf8?*
   #:*base64-encode-func*
   #:*base64-decode-func*
   #:class-slots
   #:gc-full
   #:get-backtrace
   #:initialize-multiprocessing
   #:local-port
   #:match-regexp
   #:patches-dir
   #:process-run-function
   #:remote-host
   #:replace-regexp
   #:room-report
   #:slot-definition-name
   #:snap-folder
   #:socket-bytes-written
   #:split-regexp
   #:with-heuristic-case-mode
   #:with-timeout-sym
   #:with-timeout
   #:without-redefinition-warnings


   #:close-old-areas
   #:open-old-areas
   #:get-mem-info
   #:gc-scavenge


   #:copy-directory
   #:copy-file
   #:delete-directory-and-files
   #:dump-memory
   #:implementation-identifier
   #:next-datestamp

   #:autoloaded-packages
   #:package-documentation
   #:function-documentation
   #:variable-documentation

   #:parse-xml

   #:rsync
   ))

(defpackage :geom-base
  (:use :common-lisp :gdl)
  (:shadowing-import-from :gdl #:the)
  (:documentation #.(gendl-boot:system-description :geom-base))
  (:export #:keyed-transform*vector
	   #:with-translated-state
	   #:raphael
	   #:*raphael-translation*
	   #:make-vector
	   #:make-point
	   #:apply-make-point
	   #:merge-display-controls
	   #:*nominal-x-vector*
	   #:*nominal-y-vector*
	   #:*nominal-z-vector*
	   #:*nominal-x-vector-r*
	   #:*nominal-y-vector-r*
	   #:*nominal-z-vector-r*
	   #:*trimetric-normal-vector*
	   #:*trimetric-normal-vector-left*
	   #:*trimetric-normal-vector-right-rear*
	   #:*trimetric-normal-vector-left-rear*
	   #:*left-handed-transform?*
	   #:+lh-identity-matrix+
	   #:+nominal-origin+
	   #:*standard-views*
	   #:point-expression   
	   #:+postnet-bits+   
	   #:*hash-transforms?*
	   #:get-x
	   #:get-y
	   #:get-z
	   #:get-w
	   #:get-u
	   #:get-v
	   #:determinant
	   #:subtract-vectors
	   #:add-vectors
	   #:3d-distance
	   #:scalar*vector
	   #:matrix*vector
	   #:transpose-matrix
	   #:multiply-matrices
	   #:dot-vectors
	   #:alignment
	   #:make-transform
	   #:angle-between-vectors-d
	   #:angle-between-vectors
	   #:unitize-vector
	   #:orthogonal-component
	   #:same-direction-vectors?
	   #:parallel-vectors?
	   #:reverse-vector
	   #:cross-vectors
	   #:length-vector
	   #:zero-vector?
	   #:degree
	   #:radians-to-degrees
	   #:radians-to-grads
	   #:translate-along-vector
	   #:array-to-list
	   #:coincident-point?
	   #:projected-vector
	   #:rotate-point-d
	   #:rotate-point
	   #:rotate-vector
	   #:rotate-vector-d
	   #:inter-circle-sphere
	   #:inter-line-sphere
	   #:inter-line-plane
	   #:translate
	   #:create-obliqueness
	   #:proj-point-on-line
	   #:pythagorize
	   #:roll
	   #:rotation
	   #:transform-and-translate-point
	   #:transform-numeric-point
	   #:quaternion-to-rotation
	   #:quaternion-to-matrix
	   #:matrix-to-quaternion
	   #:matrix-to-rotation
	   #:normalize-quaternion
	   #:degrees-to-radians
	   #:acosd
	   #:asind
	   #:atand
	   #:midpoint
	   #:between?
	   #:curve-parameter-<
	   #:roughly-aligned-vectors?
	   #:distance-to-line
	   #:equi-space-points
	   #:sort-points-along-vector
	   #:bounding-box-from-points
	   #:flatten-lines
	   #:flatten-curves

	   #:arc
	   #:base-object
	   #:base-coordinate-system
	   #:base-geometry-object
	   #:bezier-curve
	   #:box
	   #:bbox
	   #:c-cylinder
	   #:circle
	   #:cone
	   #:cylinder
	   #:ellipse
	   #:global-filleted-polygon-projection
	   #:global-filleted-polyline
	   #:global-polygon-projection
	   #:ifs-output-mixin
	   #:global-polyline
	   #:graph
	   #:l-line
	   #:null-geometric-object
	   #:outline-specialization-mixin
	   #:point
	   #:route-pipe
	   #:sphere
	   #:spherical-cap
	   #:torus
	   #:cut-cylinder	 ;;do later
	   #:filleted-polyline	 ;; do later
	   #:line		 ;;do later
	   #:point 
	   #:polyline		  ;; do later
	   #:polygon-projection	  ;;do later
           
	   #:points-display
           
	   #:note
	   #:text-block
	   #:text-lines
	   #:typeset-block
	   #:typeset-blocks
	   #:base-drawing
	   #:base-view
	   #:document
	   #:horizontal-dimension
	   #:parallel-dimension
	   #:vertical-dimension
	   #:label
	   #:linear-dimension
	   #:leader-line
           
	   #:dxf
	   #:obj
	   #:pdf-multipage
	   #:pdf
	   #:pdf-raw
	   #:png
	   #:jpeg
	   #:vrml
	   #:x3d
	   #:scad
	   #:vector-graphics
             
           
	   #:pie-chart
           

	   #:fillet
	   #:constrained-fillet
	   #:constrained-line
	   #:constrained-arc

	   #:*gs-text-alpha-bits*
	   #:*gs-graphics-alpha-bits*


	   ;;
	   ;; Implemented in gdl/geom-base/annotations/source/
	   ;;

	   #:angular-dimension 
	   #:sample-drawing 
	   #:generate-sample-drawing 
	   #:center-line 
	   #:*break-leaders?*
	   #:print-document
	   
	   ;;
	   ;; Implemented in gdl/geom-base/text/source/
	   ;;
	   #:general-note 
	   #:text-line

	   ;;
	   ;;
	   ;; FLAG -- consider exporting these if requested. 
	   ;;
	   ;;#:3d-vector-to-array
	   ;;#:array-to-3d-vector
           
	   ))


(defpackage :gdl-toplevel (:use))
(defpackage :gdl-rule (:size 25) (:use) 
            (:export #:%not-handled% #:write-env #:write-of #:%unbound%))
(defpackage :gdl-slots (:size 10000) (:use) (:export))
(defpackage :gdl-inputs (:size 10000) (:use) (:export))
(defpackage :gdl-trickle-downs (:use) (:export))
(defpackage :gdl-acc (:size 10000) (:use) (:export))
(defpackage :gdl-format (:use) (:export))
(defpackage :gdl-output (:use) (:export))


(defpackage :surf
  (:shadowing-import-from :gdl #:the)
  (:use :common-lisp :gdl :geom-base)
  (:documentation #.(gendl-boot:system-description :surf))
  (:shadow #:step)
  (:export #:make-geometry-kernel 
           #:*geometry-kernel* 
           #:*finalize-lofted-surfaces?*
           #:*chain-beziers-for-display?*
           #:curve 
           #:iso-curve
           #:trimmed-curve
           #:b-spline-curve 
           #:arc-curve
           #:elliptical-curve
           #:linear-curve
           #:fitted-curve
           #:planar-offset-curve
           ;;#:fillet
           #:projected-curve
           #:approximated-curve
           #:silhouette-curves
           #:dropped-curve
           #:surface 
           #:b-spline-surface
           #:extended-surface
           #:offset-surface
           #:offset-solid
           #:shelled-solid
           #:manifold-solid
           #:fitted-surface
           #:coons-surface
           #:surface-knot-reduction
           #:joined-surfaces
           #:compatible-surfaces
           #:compatible-curves
           #:extended-curve
           #:lofted-surface
           #:ruled-surface
           #:test-fitted-surface
           #:spherical-surface
           #:revolved-surface
           #:revolved-surfaces
           #:planar-surface
           #:rectangular-surface
           #:trimmed-surface
           #:approximated-subsurface
           #:basic-surface
           
           #:split-surface
           
           #:brep
           #:edge
           #:face
           #:iges-reader
           #:native-reader
           #:brep-reader
           #:step-reader
           #:composed-curve
           #:subdivided-curve
           #:composed-curves
           #:decomposed-curve
           #:decomposed-curves
           #:planar-section-curve
           #:planar-section-curves
           #:global-filleted-polyline-curves
           #:global-filleted-polyline-curve
           #:planar-contour-surface
           #:transformed-curve
           #:boxed-curve
           #:boxed-surface
           #:transformed-surface
           #:transformed-solid
           #:stitched-solid
           
           #:extruded-solid
           #:blended-solid
           
           #:get-3d-point-of
           #:get-parameter-of
           
           #:get-other-parameter-of
           #:get-uv-point-of
           #:get-point-on-surface
           #:get-point-on-curve
           #:get-point-on-other-curve
           
           #:%get-point-on-surface
           #:%get-point-on-curve
           
           
           #:iges #:native #:stl 
           
           
           #:step
           
           
           #:box-solid
           #:cone-solid
           #:cylinder-solid
           #:separated-solid
           #:subtracted-solid
           #:merged-solid
           #:validated-solid
           #:united-solid
           #:intersected-solid
           #:swept-solid
           #:brep-intersect
           #:brep-intersect?
           #:regioned-solid
           
           #:merged-brep
           
           #:general-sweep
           #:normal-sweep
           
           #:edge-blend-surface
           
           
           #:with-pinned-values
           #:pin-value-to-range
           
           #:fitted-conic
           
           #:*3d-approximation-tolerance-default*
           #:*boolean-operation-tolerance-default*
           #:*approximation-tolerance-factor*
           #:*brep-tolerance-default*
           #:*angle-tolerance-radians-default*
           #:*display-tolerance*
           #:*3d-tolerance-default*
           #:*brep-wireframe-tessellation?*
           #:*curve-tessellation?*
           #:*crease-angle-default*
           #:*output-units-default*
           #:*isos-default*
           #:*brep-isos-default*
           #:*brep-vrml-timeout*
           #:*nurbs-to-beziers-timeout*
           #:*boolean-allow-multiple-regions?*
           #:*boolean-error-on-invalid-brep?*
           
           #:iwbrep-sew-and-orient
           
           #:test-b-spline-surface
           #:normalized-curve
           #:surf-grid-points
           #:closed-boolean-operation
           #:closed-boolean-separate-operation
           #:global-brep-brep-solve
	   
	   #:non-rational-curve
	   #:spiral-curve
           
           #:get-faces-from-edge
           
           #:*tess-min-number-of-segments*
           #:*tess-max-3d-edge-factor*
           #:*tess-min-parametric-ratio*
           #:*tess-max-chord-height*
           #:*tess-max-angle-degrees*
           #:*tess-min-3d-edge*
           #:*tess-min-edge-ratio-uv*
           #:*tess-max-aspect-ratio*))

(defmacro gdl:define-package (name &rest body)
  `(defpackage ,name 
     (:use :common-lisp :gdl :geom-base :surf)
     (:shadowing-import-from :gdl #:the)
     (:shadowing-import-from :surf #:step)
     ,@body))


(gdl:define-package :gdl-user)


;;
;; FLAG -- stop using all these library packages in here!
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gdl:define-package :gwl
      ;;
      ;; FLAG moved this to a (use-package ...) in
      ;; gdl/gwl/source/genworks.lisp (until we can clean them
      ;; entirely)
      ;; 
      ;;(:use :net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who)
      (:documentation #.(gendl-boot:system-description :gwl))
    (:shadow #:define-package)
    (:export
     #:define-package
     #:remote-object
     #:*req*
     #:*ent*
     #:*html-referrer*
     #:*instance-hash-table*
     #:*instance-finalizers*
     #:*max-node-depth*
     #:*developing?*
     #:*mime-file-types*
     #:*max-id-value*
     #:*server*
     #:*query-plist*
     #:*field-plist*
     #:*ssl-server*
     #:*adsense-code*
     #:*break-on-set-self?*
     #:*jump-to-toplevel-on-set-self?*
     #:*process-cookies?*
     #:*static-home*
     #:encode-root-path
     #:base-html-sheet
     #:skeleton-ui-element
     #:sheet-section
     #:session-control-mixin
     #:base-html-graphics-sheet
     #:color-palette
     #:crawl
     #:html-submit-button
     #:html-button
     #:html-checkbox
     #:html-radio-button
     #:html-select-choices
     #:html-file
     #:html-multi-line-text
     #:html-string
     #:html-password
     #:html-title
     #:html-anchor
     #:html-cell
     #:html-row
     #:html-table
     #:html-static-text
     #:untagify
     #:make-new-session-id
     #:html-format
     #:html-format
     ;;#:define-package
     #:my
     #:my-child
     #:from-my
     #:my-object
     #:defpage
     #:root-path-to-query-arg
     #:query-arg-to-root-path
     #:gwl-make-part
     #:gwl-make-object
     #:replace-substring
     #:custom-base-html-sheet-mixin
     #:fix-lhtml
     #:start-log-maker
     #:clear-log-buffer
     #:*log-report-buffer*
     #:pdf-output-sheet
     #:graphics-preferences
     #:application-mixin
     #:node-mixin
     #:gwl-rule-object
     #:*js-incremental-search*
   
     #:html-form
   
     #:form-mixin
     #:clear-all-instances
     #:clear-instance
   
     #:with-html-form
   
     #:publish-shared
     #:with-all-servers
     #:web-drawing
   
     #:infinite
   
     #:base64-decode-safe
     #:base64-encode-safe
     #:base64-decode-list
     #:base64-encode-list
   
     #:start-gwl
     ;;
     ;; FLAG -- test this in 1575 build
     ;;
     ;;#:update-gdl
     ;;#:*patch-fasls*
   
   
     ;;
     ;;
     ;;

     #:base-form-control
     #:button-form-control
     #:checkbox-form-control
     #:radio-form-control
     #:menu-form-control
     #:text-form-control
     #:number-form-control
     #:password-form-control
     #:file-form-control
     #:hidden-form-control
     #:object-form-control
     #:grid-form-control
   
     #:encode-for-ajax
     #:decode-from-ajax
     #:form-element-processor
   
     #:*clicked-x*
     #:*clicked-y*
   
     ;;
     ;; Ajax stuff
     ;;

     #:base-ajax-sheet
     #:base-ajax-graphics-sheet

     #:*standard-ajax-javascript*
     #:*ajax-timeout*
     #:*bypass-security-check?*
   
     #:with-cl-who
     #:with-cl-who-string
     #:with-htm
     #:publish-gwl-app
     #:publish-string-content
     )))



(defpackage :gwl-graphics 
  (:use :common-lisp)
  (:documentation #.(gendl-boot:system-description :gwl-graphics)))
