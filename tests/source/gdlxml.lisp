(in-package :gdl-lift-tests)

#+nil
(define-object xmlgdl-test (gdlxml:xml2gdl)
  
  :computed-slots ((xml-file (merge-pathnames "../data/test-file.xml" *input-files-pathname*))))

