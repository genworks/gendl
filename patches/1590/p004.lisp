(in-package :gdl)

(#+allegro
 excl:without-package-locks #-allegro progn
 (#+allegro
  excl:without-redefinition-warnings #-allegro progn
  (define-object quantification ()
    :documentation (:description "A quantification is an aggregate created as a result of specifying <tt>:sequence (:size ...))</tt> or
<tt>:sequence (:indices ...))</tt> in an <tt>:objects</tt> specification. Usually, the elements of a quantified set are referenced by using
extra parentheses around the message in the reference chain and using the index number. But the aggregate itself also supports certain
messages, documented here. One message, <tt>number-of-elements</tt>, is not listed in the normal messages section because it is 
internal. It can be used, and returns an integer representing the cardinality of the aggregate.")

    :computed-slots
    ((index nil)

     ("GDL Object. Returns the first element of the aggregate."
      first nil)
   
     ("GDL Object. Returns the last element of the aggregate."
      last  nil)))))
