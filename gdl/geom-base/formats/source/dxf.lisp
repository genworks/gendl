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

(in-package :geom-base)





(defparameter *dxf-header*
    "  0
SECTION
  2
TABLES
  0
TABLE
  2
STYLE
  70
1
  0
STYLE
  2
GDLSTYLE
  70
0
  40
0.0
  41
1.0
  50
0.0
  71
0
  42
0.2
  3
TXT
  0
ENDTAB
  0
TABLE
  2
LTYPE
  70
3
  0
LTYPE
  2
DASHED
  70
64
  3
__ __ __ __
  72
65
  73
2
  40
0.75
  49
0.5
  49
-0.25
  0
LTYPE
  2
CENTER
  70
64
  3
___ _ ___ _ ___
  72
65
  73
4
  40
0.75
  49
0.4
  49
-0.1
  49
0.15
  49
-0.1
  0
LTYPE
  2
DASHDOT
  70
64
  3
__ . __ . __
  72
65
  73
4
  40
0.75
  49
0.5
  49
-0.12495
  49
0.0001
  49
-0.12495
  0
ENDTAB
  0
ENDSEC
  0
SECTION
  2
ENTITIES
")


(defparameter *dxf-footer*
    "0
ENDSEC
  0
EOF
")

(define-format dxf (2d-output))



