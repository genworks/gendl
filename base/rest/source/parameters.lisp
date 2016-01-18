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

(in-package :gdl)

(glisp:define-constant 2pi (* 2 pi)
  "Number (Constant). Twice the internal Lisp value for pi.")

(glisp:define-constant pi/2 (/ pi 2)
  "Number (Constant). Half the internal Lisp value for pi.")

(glisp:define-constant +phi+ 1.618
  "Number (Constant). The Golden Ratio.")

(defparameter *zero-epsilon* 0.001
  "Number. The value used to test for closeness to zero in some functions. Defaults to 0.001")

(glisp:define-constant +kappa+ 0.5522847498307936d0)

(defvar *current-version* nil)

(defparameter *force-restore-slot-default?* nil)

(defparameter *override-non-settables?* t)

(defparameter *bias-to-double-float?* nil 
  "Boolean. Indicates whether the following functions should always return double-floats: 
<ul>
<li><tt>half</tt>
<li><tt>twice</tt>
</ul>.

Defaults to nil.
")


(defparameter *color-plist*
    (list :periwinkle "#aaaaff"
          :aqua "#00ffff"
          :aquamarine "#70db93"
          :aquamarine-medium "#32cd99"
          :black "#000000"
          :blue "#0000ff"
          :blue-cadet "#5f9f9f"
          :blue-corn-flower "#42426f"
          :blue-light "#c0d9d9"
          :blue-medium "#3232cd"
          :blue-midnight "#2f2f4f"
          :blue-midnight-new "#00009c"
          :blue-navy "#23238e"
          :blue-neon "#4d4dff"
          :blue-rich "#5959ab"
          :blue-sky "#3299cc"
          :blue-slate "#007fff"
          :blue-slate-dark "#6b238e"
          :blue-slate-medium "#7f00ff"
          :blue-steel "#236b8e"
          :blue-steel-light "#8f8fbd"
          :blue-violet "#9f5f9f"
          :brass "#b5a642"
          :bronze "#8c7853"
          :bronze-ii "#a67d3d"
          :brown "#a62a2a"
          :brown-dark "#5c4033"
          :brown-dark-very "#5c4033"
          :chocolate-bakers "#5c3317"
          :chocolate-semi-sweet "#6b4226"
          :copper "#b87333"
          :copper-cool "#d98719"
          :coral "#ff7f00"
          :cyan "#00ffff"
          :feldspar "#d19275"
          :firebrick "#8e2323"
          :fuchsia "#ff00ff"
          :gold "#cd7f32"
          :gold-bright "#d9d919"
          :gold-old "#cfb53b"
          :goldenrod "#dbdb70"
          :goldenrod-medium "#eaeaae"
          :green "#008000"
          :green-copper "#527f76"
          :green-copper-dark "#4a766e"
          :green-dark "#2f4f2f"
          :green-forest "#238e23"
          :green-forest-medium "#6b8e23"
          :green-hunter "#215e21"
          :green-lime "#32cd32"
          :green-olive-dark "#4f4f2f"
          :green-pale "#8fbc8f"
          :green-sea "#238e68"
          :green-sea-medium "#426f42"
          :green-spring "#00ff7f"
          :green-spring-medium "#7fff00"
          :green-yellow "#93db70"
          :green-yellow2 "#99cc32"
          :gray "#808080"
          :grey "#c0c0c0"
          :grey-dim "#545454"
          :grey-light "#a8a8a8"
          :grey-light-very "#cdcdcd"
          :grey-slate-dark "#2f4f4f"
          :khaki "#9f9f5f"
          :lime "#00ff00"
          :magenta "#ff00ff"
          :maroon "#800000"
          :navy "#000080"
          :olive "#808000"
          :orange "#ff7f00"
          :orange-mandarian "#e47833"
          :orange-mandarin "#e47833"
          :orchid "#db70db"
          :orchid-dark "#9932cd"
          :orchid-medium "#9370db"
          :pink "#bc8f8f"
          :pink-neon "#ff6ec7"
          :pink-spicy "#ff1cae"
          :plum "#eaadea"
          :purple "#800080"
          :purple-dark "#871f78"
          :quartz "#d9d9f3"
          :red "#ff0000"
          :red-indian "#4e2f2f"
          :red-orange "#ff2400"
          :red-violet "#cc3299"
          :red-violet-medium "#db7093"
          :rose-dusty "#856363"
          :salmon "#6f4242"
          :scarlet "#8c1717"
          :sienna "#8e6b23"
          :silver "#c0c0c0"
          :sky-summer "#38b0de"
          :tan "#db9370"
          :tan-dark "#97694f"
          :tan-new "#ebc79e"
          :teal "#008080"
          :thistle "#d8bfd8"
          :turquoise "#adeaea"
          :turquoise-dark "#7093db"
          :turquoise-medium "#70dbdb"
          :violet "#4f2f4f"
          :wheat "#d8d8bf"
          :white "#ffffff"
          :wood-dark "#855e42"
          :wood-light "#e9c2a6"
          :wood-medium "#a68064"
          :yellow "#ffff00")
  
  "Plist. Maps color name keywords to string containing HTML-style hex RGB color value,
e.g. \"#FFFFFF\" for pure white.")



(defun color-compare (color1 color2)
  (let ((r1 (glisp:hex-string-to-integer (subseq color1 1 3)))
        (r2 (glisp:hex-string-to-integer (subseq color2 1 3)))
        (g1 (glisp:hex-string-to-integer (subseq color1 3 5)))
        (g2 (glisp:hex-string-to-integer (subseq color2 3 5)))
        (b1 (glisp:hex-string-to-integer (subseq color1 5 7)))
        (b2 (glisp:hex-string-to-integer (subseq color2 5 7))))
    (< (+ r1 g1 b1) (+ r2 g2 b2))))

(defparameter *color-plist-sorted*
    (let ((names (plist-keys *color-plist*))
          (hexes (plist-values *color-plist*)))
      (let* ((pairs (mapcar #'list names hexes))
             (sorted-pairs (sort pairs #'color-compare :key #'second)))
        (mapcan #'identity sorted-pairs))))


(defparameter *color-table* 
    (let ((ht (make-hash-table)))
      (mapc #'(lambda(key val)
                (setf (gethash key ht) val)) (plist-keys *color-plist-sorted*) (plist-values *color-plist-sorted*)) ht)
  "Hash Table. Built from the <tt>*color-plist*</tt>, this hash table is keyed on the same keys.")



(defparameter *paper-size-plist*
    (list :letter (list :verbose-name "Letter (8.5x11)"
                        :width 612 :height 792)
          :letter-landscape (list :verbose-name "Letter Landscape (11x8.5)"
                        :width 792 :height 612)
          :tabloid (list :verbose-name "Tabloid (11x17)"
                         :width 792 :height 1224)
          :ledger  (list :verbose-name "Ledger (17x11)"
                         :width 1224 :height 792)
          :legal   (list :verbose-name "Legal (8.5x14)"
                         :width 612 :height 1008)
          :legal-landscape   (list :verbose-name "Legal Landscape (14x8.5)"
                                   :width 1008 :height 612)
          :executive (list :verbose-name "Exec. (7.5x10)"
                           :width 540 :height 720)
          :a3  (list :verbose-name "A3 (11.69x16.53)"
                     :width 841.68 :length 1190.16)
          :a4 (list :verbose-name "A4 (8.26x11.69)"
                    :width 594.72 :height 841.68)
          :a5 (list :verbose-name "A5 (5.83x8.26)"
                    :width 419.76 :height 594.72)
          :b4 (list :verbose-name "B4 (9.83x13.93)"
                    :width 707.76 :height 1001.96)
          :b5 (list :verbose-name "B5 (7.17x10.12)"
                    :width 516.24 :height 728.64)))

(defparameter *paper-size-table*
    (let ((ht (make-hash-table)))
      (mapc #'(lambda(key val)
                (setf (gethash key ht) val)) 
            (plist-keys *paper-size-plist*) 
            (plist-values *paper-size-plist*)) ht))



(defun div (&rest nums)
  "Floating-point number. Divides using rational division and converts the result (which may be a pure rational number) 
to a floating-point number.

:arguments (numerator \"Number.\"
            denominator \"Number.\")

:&optional (more-denominators \"(&rest). More numbers to divide by.\")"
  (to-double-float
   (case (length nums)
     (1 (/ (first nums)))
     (2 (/ (first nums) (second nums)))
     (otherwise (/ (apply #'div (butlast nums)) (lastcar nums))))))

(defparameter *rgb-cube-colors*
    (let ((colors (list "FF" "CC" "99" "66" "33" "00")) result)
      (dolist (red colors (coerce (nreverse result) 'vector))
        (dolist (green colors)
          (dolist (blue colors)
            (push (format nil "#~a~a~a" red green blue) result))))))


(defun rgb-cube-colors (&key (components (list "FF" "CC" "99" "66" "33" "00")))
  (let (result)
    (dolist (red components (coerce (nreverse result) 'vector))
      (dolist (green components)
        (dolist (blue components)
          (push (format nil "#~a~a~a" red green blue) result))))))


(defparameter *color-table-decimal*
    (let ((ht (make-hash-table :test #'equalp)))
      (mapc #'(lambda(key val)
                (setf (gethash key ht) (list (to-single-float (div (glisp:hex-string-to-integer (subseq val 1 3)) 255))
                                             (to-single-float (div (glisp:hex-string-to-integer (subseq val 3 5)) 255))
                                             (to-single-float (div (glisp:hex-string-to-integer (subseq val 5 7)) 255)))))
            (plist-keys *color-plist-sorted*) (plist-values *color-plist-sorted*))
      
      (mapc #'(lambda(key)
                (setf (gethash key ht) (list (to-single-float (div (glisp:hex-string-to-integer (subseq key 1 3)) 255))
                                             (to-single-float (div (glisp:hex-string-to-integer (subseq key 3 5)) 255))
                                             (to-single-float (div (glisp:hex-string-to-integer (subseq key 5 7)) 255)))))
            (coerce *rgb-cube-colors* 'list))
      ht)
  "Hash table. Same as <tt>*color-table*</tt> except the results are returned as a list of three 
decimal integers (for Red, Green, Blue) in the range of 0-254.")


(defparameter *display-controls* nil)

(defparameter *eager-setting-enabled?* nil)

(defparameter *leaf-resets* nil)

(defparameter *ensure-lists-when-bashing?* nil
  "Boolean. Determines whether lists are enforced to stay as lists with set-slot-if-needed method of vanilla-mixin. Default is nil.")

(defparameter *out-of-bounds-sequence-reference-action* :error
  "Keyword symbol :warn, :error, or :silent. Determines what happens when you try to access
a member of a GDL sequence which is out of bounds. If :warn or :silent, an out-of-bounds
reference will simply return nil. If :error, it will throw an error as was the old behavior. 
Defaults to :error.")


(defparameter *remember-previous-slot-values?* nil
  "Boolean. Determines whether the system keeps track of previous slot values 
  (accessible with previous-value function) after bashings are done. Leave this
set to nil to improve memory performance.")

(defparameter *onclick-function* nil)


(defparameter *with-format-if-exists* :supersede
"keyword symbol. Establishes the default for the :if-exists
 format-slot of the base-format. If you want to change this default
 behavior, you can override this parameter globally or bind it
 dynamically. Alternatively you can specify a different value
 for :if-exists in the call to with-format. Valid keywords are the
 same as for Common Lisp open or with-open-file. Default
 is :supersede.
:example
<pre>

  (let ((*with-format-if-exists* :error))
    (with-format (x3d  \"/tmp/box.x3d\") 
      (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output)))

  (with-format (x3d \"/tmp/box.x3d\" :if-exists :error)
    (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output) 

</pre>
 "

)


(defparameter *with-format-if-does-not-exist* :create
"keyword symbol. Establishes the default for the :if-does-not-exist
 format-slot of the base-format. If you want to change this default
 behavior, you can override this parameter globally or bind it
 dynamically. Alternatively you can specify a different value
 for :if-does-not-exist in the call to with-format. Valid keywords are
 the same as for Common Lisp open or with-open-file. Default
 is :create.
:example
<pre>

  (let ((*with-format-if-does-not-exist* :error))
    (with-format (x3d  \"/tmp/box.x3d\") 
      (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output)))

  (with-format (x3d \"/tmp/box.x3d\" :if-does-not-exist :error)
    (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output) 

</pre>
 "
)

(defparameter *with-format-direction :output
"keyword symbol. Establishes the default for the :direction
 format-slot of the base-format. If you want to change this default
 behavior, you can override this parameter globally or bind it
 dynamically. Alternatively you can specify a different value
 for :direction in the call to with-format. Valid keywords are the
 same as for Common Lisp open or with-open-file. Default
 is :output. Normally this should not be changed in user code.")



(defparameter *with-format-external-format* glisp:*external-text-format*
  "External-format. The default for the :external-format format-slot
  for the base format. Defaults to glisp:*external-text-format*.")

;;
;; FLAG -- determine what this should really be, i.e. establish a glisp:*element-type-default*
;;
(defparameter *with-format-element-type* nil
"Element-type. The default for the :element-type format-slot
  for the base format. Defaults to nil. Needs a better default.")
