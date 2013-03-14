;;
;; Copyright 2002-2011 Genworks International 
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

(in-package :gdl-user)

(define-object general-note-drawing (base-drawing)
  
  :objects
  ((main-view :type 'base-view
              :objects (list (the general-notes center-note)))
   
   (general-notes :type 'general-notes)))


(define-object general-notes (base-object)
  
  :input-slots
  ((underline? t)
   
   (text-x-scale 115)
   )
  
  :computed-slots
  ((strings (list "You are in a dimly lit"
                  "room in a cave."
                  "In each direction you look," 
                  "you see twisty little passages,"
                  "all different.")))
  
  :objects
  ((left-note :type 'general-note
              :pass-down (strings underline? text-x-scale))
   
   (center-note :type 'general-note
                :justification :center
                :outline-shape-type :bubble
                :pass-down (strings underline?  text-x-scale))
   
   (right-note :type 'general-note
               :justification :right
               :pass-down (strings underline? text-x-scale))
   
   (left-note-45-degree :type 'general-note
                        :pass-down (strings underline? text-x-scale)
                        :orientation (alignment :rear (rotate-vector-d (the (face-normal-vector :rear))
                                                                       45
                                                                       (the (face-normal-vector :top)))))
   
   (center-note-45-degree :type 'general-note
                          :pass-down (strings underline? text-x-scale)
                          :justification :center
                          :orientation (alignment :rear (rotate-vector-d (the (face-normal-vector :rear))
                                                                         45
                                                                         (the (face-normal-vector :top)))))
   
   (right-note-45-degree :type 'general-note
                        :pass-down (strings underline? text-x-scale)
                        :justification :right
                        :orientation (alignment :rear (rotate-vector-d (the (face-normal-vector :rear))
                                                                       45
                                                                       (the (face-normal-vector :top)))))
   
   ))
   


(define-object multi-column-ron (base-drawing)
  
  :objects
  ((ron-block :type 'ron-paul-block
              :width (half (the width)))
   
   (ron-blocks :type 'typeset-blocks
               :full-block (the ron-block)
               :width (half (the width))
               :length 200)
   
   (ron-views :type 'base-view
              :border-box? t
              :front-margin 0 :left-margin 0
              :sequence (:matrix :lateral 2 :longitudinal (half (the ron-blocks blocks number-of-elements)))
              :flat-index (+ (* (first (the-child index)) 2) (second (the-child index)))
              :objects (list (the ron-blocks (blocks (the-child flat-index))))
              :width (the ron-blocks width)
              :length (the ron-blocks (blocks (the-child flat-index)) length))
   
   
   (full-view :type 'base-view
              :left-margin 0 :front-margin 0
              :border-box? t)))




(define-object ron-paul-block (typeset-block)
  
  :input-slots ((width 250))
  
  :functions
  ((content
    ()
    (typeset:compile-text ()
     (typeset:paragraph ()
                        "
Ronald Ernest Paul (born August 20, 1935) is a Republican United
States Congressman from Lake Jackson, Texas, a physician, a
bestselling author, and the last Republican candidate to withdraw from
the 2008 U.S. presidential election.")
     
     (typeset:paragraph ()
                        "
Originally from the Green Tree suburb of Pittsburgh, Pennsylvania, he
graduated from Gettysburg College in 1957, then studied at Duke
University School of Medicine; after his 1961 graduation and a
residency in obstetrics and gynecology, he became a U.S. Air Force
flight surgeon, serving outside the Vietnam War zone. He later
represented Texas districts in the U.S. House of Representatives
 (1976–1977, 1979–1985, and 1997–present). He entered the 1988
presidential election, running as the Libertarian nominee while
remaining a registered Republican, and placed a distant third.")
     (typeset:paragraph ()
                        "
Paul has been described as conservative, Constitutionalist, and
libertarian.[2] He advocates a foreign policy of nonintervention,
having voted against actions such as the Iraq War Resolution, but in
favor of force against terrorists in Afghanistan. He favors withdrawal
from the North Atlantic Treaty Organization and the United Nations,
citing the dangers of foreign entanglements to national
sovereignty. Having pledged never to raise taxes, he has long
advocated ending the federal income tax, scaling back government
spending, abolishing most federal agencies, and removing military
bases and troops from foreign soil; he favors hard money and opposes
the Federal Reserve. He also opposes the Patriot Act, the federal War
on Drugs, No Child Left Behind, and gun regulation. Paul is strongly
pro-life, and has introduced bills to negate Roe v. Wade, but affirms
states' rights to allow, regulate or ban abortion, rather than federal
jurisdiction.[3]")
     (typeset:paragraph ()
                        "While Paul was a leading 2008 presidential candidate in some
Republican straw polls, he saw substantially less support in landline
opinion polls and in the actual primaries. Strong internet grassroots
support was indicated by the popularity of his name in search queries
and the number of his campaign's YouTube subscriptions. His book, The
Revolution: A Manifesto, became a bestseller immediately upon
release[4][5][6][7][8] and went on to become  on the New York Times
nonfiction best sellers list.[9]")))))
