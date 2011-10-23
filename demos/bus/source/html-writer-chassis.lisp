;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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

(in-package :genworks.demos.bus)

(define-lens (html-format chassis) nil

  :output-functions
  ((model-inputs
    ()
    (html (:p
	   (:table
	    (:tr ((:td :bgcolor :yellow) "Front Overhang")
		 (:td
		  ((:input :type :text :size 5 :name :front-overhang :value
			   (the :front-overhang)))))
	    (:tr ((:td :bgcolor :yellow) "Rear Overhang")
		 (:td
		  ((:input :type :text :size 5 :name :rear-overhang :value
			   (the :rear-overhang)))))
	    (:tr ((:td :bgcolor :yellow) "Turn Type")
		 (:td
		  (the (:select-choices :name :turn-type :keys (list :left :right)))))
	    (:tr ((:td :bgcolor :yellow) "Turn Angle")
		 (:td
		  ((:input :type :text :size 5 :name :turn-angle :value
			   (the :turn-angle)))))
	    (:tr ((:td :bgcolor :yellow) "Toe-In Angle")
		 (:td
		  ((:input :type :text :size 5 :name :toe-in-angle :value
			   (the :toe-in-angle)))))
	    (:tr ((:td :bgcolor :yellow) "Camber Angle")
		 (:td
		  ((:input :type :text :size 5 :name :camber-angle :value
			   (the :camber-angle)))))
	    (:tr ((:td :bgcolor :yellow) "Caster Angle")
		 (:td
		  ((:input :type :text :size 5 :name :caster-angle :value
			   (the :caster-angle)))))
	    (:tr ((:td :bgcolor :yellow) "KPI")
		 (:td
		  ((:input :type :text :size 5 :name :kingpin-inclination-angle :value
			   (the :kingpin-inclination-angle)))))
	    (:tr ((:td :bgcolor :yellow) "Tie Rod" :br "Arm Length")
		 (:td
		  (the (:select-choices :name :tie-rod-arm-length :keys
					(list :short :medium :long) :values
					(list "Short" "Medium" "Long")))))
	    (:tr ((:td :bgcolor :yellow) "Tie Rod" :br "Arm Setting")
		 (:td
		  (the (:select-choices :name :tie-rod-arm-setting :keys
					(list :narrow :medium :wide) :values
					(list "Narrow" "Medium" "Wide")))))))
	  (:p ((:input :type :submit :name :subbmit :value " OK ")))))))
