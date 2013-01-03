(in-package :training-g102)

#+nil
(http:export-url #u"/training/g102/images/"
 :image-directory
 :pathname (translate-logical-pathname "genworks:web;training;g102;images;")
 :recursive-p t
 :immediate-export t)


#+nil
(http:export-url #u"/training/g102/examples/"
 :directory
 :pathname (translate-logical-pathname "genworks:web;training;g102;examples;")
 :immediate-export t
 :recursive-p t)


#+nil
(http:export-url #u"/training/g102/exercises/"
 :directory
 :pathname (translate-logical-pathname "genworks:web;training;g102;exercises;")
 :immediate-export t
 :recursive-p t)
