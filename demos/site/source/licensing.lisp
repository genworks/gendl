;;
;; Copyright 2002-2011, 2012 Genworks International
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

(in-package :genworks.com)

(define-object licensing (base-site-sheet)

  :input-slots (pricing-footer)

  :computed-slots ((body-class "products licensing"))
  
  :objects
  (
   
   (column-left 
    :type 'sheet-section
    :inner-html nil)

   (column-right 
    :type 'sheet-section
    :inner-html nil)

   
   (column-center :type 'sheet-section
		  :inner-html
		  (with-cl-who-string ()
		    ((:div :class "content") 
		     (:h2 "Runtime Licensing") (:h5 "Internal Runtime Licensing")
		     (:p "Enterprise customers who wish to generate and deploy
	   company-internal runtime applications for production use
	   may purchase GDL/GWL Runtime Licenses for this
	   purpose. Multiple runtime licenses are sold in
	   pre-discounted \"packs\" as defined numbers of runtime
	   \"seats.\"")
		     (:p "The deployment can be a single Server or Servers (e.g. web
	   server) with up to the specified total number of
	   simultaneously active user sessions, separately installed
	   single-user deployments, or any combination in between.")
		     (:h5 "Runtime VAR Program") (:h6 "1 VAR Initiation")
		     (:p "Enterprise customers may apply to Genworks to become GDL
	   VARs (Value-added Resellers). Once enrolled as a VAR, a
	   customer becomes entitled to build standalone runtime
	   applications and sell them or the use of them to third
	   parties for profit.")
		     (:p "In order to become activated as a VAR, the customer pays a
	   fee, called a VAR Initiation, for each Enterprise platform
	   (e.g. Windows, Redhat, Linux, etc.) on which runtime
	   applications are to be built and sold. The VAR Initiation
	   is valid for one year and is renewable annually for a
	   percentage based fee.")
		     (:h6 "2 VAR Royalties")
		     (:p "Under a VAR agreement, revenue associated with the sale
	   and/or servicing (e.g. consulting, customization, etc.) or
	   runtime applications using GDL/GWL technology is subject to
	   a percentage-based royalty fee. Other than this
	   percentage-based royalty fee, there are no separate
	   \"runtime license fees\" for VAR-based distribution of
	   runtime applications.")
		     (:p "In some circumstances, the VAR option may also be used for
	   company-internal runtime deployments. As an alternative to
	   paying individual runtime license fees, a company
	   department may choose to become a VAR, then \"sell\" the
	   runtime applications to another department within the same
	   company, or otherwise account for a specific realistic
	   monetary value for the use of the runtime applications. As
	   long as the prices represent reasonable market values and
	   the accounting is reported properly, such distributions can
	   be done under the terms of the VAR royalty rather than
	   requiring explicit runtime license fees.")
		     )))))


