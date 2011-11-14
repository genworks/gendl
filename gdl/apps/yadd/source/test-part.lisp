;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(gdl:define-package :yadd-sample (:export #:assembly #:doc))

(in-package :yadd-sample)

(define-object assembly (base-object)
  
  :documentation (:description "Takes a driveline specification along with 
some optimization parameters, and returns a list, :results, describing the top
candidates according to the specified fitness criteria."
                       :author "David J. Cooper Jr. [Genworks]"
                       :examples "Please see code for ld-driveline:assembly for prototypical example.")
  
  :input-slots
  ("Number. Documentation for A" a
   
   "Number. Documentation for B" b
   
   ("Number. Documentation for C" 
    C 25))
  
  :computed-slots
  (("List of Plists (supported). These plists contain descriptions of the top candidates, one 
            plist per candidate. Each plist is of the following form:
            
<pre>
 (:phase-angles &lt;chosen phase angles&gt;
  :crank-angle  &lt;chosen crank angle&gt;
  :pinion-angle &lt;chosen pinion angle&gt;
   ....)
            </pre>
            :implementation The list is collected up from each of the Winners, a quantified set
            of parts. Each Winner computes its own :results-list - please see the documentation
            for Winner for the specifics of this.
            
            "
    results nil)
   
   ("Array of Double-floats (supported). Each element in this array represents a particular
     combination of vertex-list and phase-angle.
            
     :purpose  The top :number-of-solutions from this array are returned.
            
     :implementation This uses the built-in sort function. Some speedup might
      be realized by making one pass through the :score-array looking for
      the top candidates, rather than sorting the whole thing. However,
      this sort has not been observed to be a bottlneck."
    sorted-score-array nil))
  
  :objects
  (("winner, loser.
            :purpose Object to compute details for each of the top candidates.
            :implementation Please see the definition of the object winner."
    :winners 
    :type 'winner
    :sequence (:size 20)))
   
  :functions
  (("List of lists of 3D Points. Computes next possible sets of points from the given state
     :arguments 
     (start-list \"List of 3D Points. Vertex-list to start from\"
     previous-uj-parameters \"Plist. Standard U-Joint parameters plist from previous joint\"
     current-uj-parameters  \"Plist. Standard U-Joint parameters plist from current joint\"
     joint-number \"Integer. Joint ordinal number, indexed starting from 1\"
     number-of-shafts \"Integer. Total number of shafts in design\"
     nominal-rear-suspension-point \"3D Point. Heuristic point at design condition in rear suspension\"
     style \"Keyword Symbol. Hanger Style, either :hanger or :sitter\"
     facing \"Keyword Symbol. Direction hanger is facing, either :forward or :rearward\")"
    find-next-vertices ())

   ("Double-float Number. Recursively Computes angle for hanger and shaft which results
     in exact 90-degree square angle between bearing and shaft.
     :arguments
      (from-point \"3D Point. Previous U-joint Point\"
       hanger-corner \"3D Point. Corner of Hanger (H1 down or up from Hanger-Attach)\"
       hanger-attach \"3D Point. Hanger Attach Point\"
       b2  \"Double-float Number. Bearing Height\"
       style \"Keyword Symbol. Hanger Style, either :hanger or :sitter\"
       facing \"Keyword Symbol. Direction hanger is facing, either :forward or :rearward\"
       hanger-vector-normal \"3D Vector. Perpendicular to Hanger attach plate\")
       :&optional 
       ((angle-to-try 0) \"Angle in Degrees. Changes slightly through each recursion.\"
        (delta 10) \"Angle in Degrees. Amount to change with each recursion. Gets halved with each recursion.\"
        (tolerance 0.001) \"Angle in Degrees. When resultant angle is this close to 90, we say we are done.\"
        (depth 0) \"Integer. Starts at zero and increments one with each recursion. Used to terminate any runaway recursions.\")
       :&key (try \"This is our first try\")

"
    compute-hanger-angle 
    (from-point  hanger-corner hanger-attach b2 style facing hanger-vector-normal
                 &optional (angle-to-try 0) (delta 10) (tolerance 0.001) (depth 0))
    
    (declare (ignore from-point hanger-corner hanger-attach b2 style facing 
                     hanger-vector-normal angle-to-try delta tolerance depth)))))
    
(define-object winner (base-object))

(define-object loser (base-object))

(define-object test (base-object)
  :objects
  ((doc :type 'yadd:object-dokumentation
        :part-symbol-supplied "assembly"
        :part-package-supplied "yadd-test")))





