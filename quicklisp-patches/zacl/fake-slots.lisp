;;;; fake-slots.lisp

(in-package #:zacl)

;;;
;;; "Fake" slots are simulating slot values and accessors with slot
;;; data stored outside the actual instance. It's for things like
;;; EXCL::STREAM-PROPERTY-LIST, where the property list of the stream
;;; is implemented as a slot in Allegro CL, but it's more convenient
;;; to store it outside the instance on non-Allegro CL.
;;;
;;; External slot data is stored in a weak hash table keyed on the
;;; instance.
;;;

(defvar *fake-slots-table*
  (make-hash-table))

(defun ensure-fake-slot-table (slot-name class-name)
  (let ((class-table (gethash class-name *fake-slots-table*)))
    (unless class-table
      (setf class-table
            (setf (gethash class-name *fake-slots-table*)
                  (make-hash-table))))
    (let ((slot-table (gethash slot-name class-table)))
      (unless slot-table
        (setf slot-table
              (setf (gethash slot-name class-table)
                    (make-weak-hash-table))))
      slot-table)))

(defun fake-slot-table (slot-name class-name)
  (gethash slot-name (gethash class-name *fake-slots-table*)))

(defmacro def-fake-slot (slot-name class-name
                         &key accessor (default-value nil default-value-p))
  (unless accessor
    (setf accessor slot-name))
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (ensure-fake-slot-table ',slot-name ',class-name))
     (defgeneric ,accessor (,class-name)
       (:method ((object ,class-name))
         (let ((table (fake-slot-table ',slot-name ',class-name)))
           (multiple-value-bind (value foundp)
               (gethash object table)
             (cond (foundp value)
                   ,@ (if default-value-p
                          `((t
                             (setf (gethash object table) ,default-value)))
                          `((t
                             (slot-unbound (class-of object) object ',slot-name)))))))))
     (defgeneric (setf ,accessor) (,class-name new-value)
       (:method (new-value (object ,class-name))
         (let ((table (fake-slot-table ',slot-name ',class-name)))
           (setf (gethash object table) new-value))))))
