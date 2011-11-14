
Next Steps:
===========


gdl-base:
=========

3. Add basic tests for the core kernel (start with test-parts/ directory).

4. Port to SBCL and CCL and maybe SCL and others

5. Try for inclusion in Quicklisp distro. 



gwl, geom-nurbs, surf, yadd, tasty:
===================================

2. Remove :use's for :net.aserve, :net.aserve.client, :cl-who, etc from :gwl package 
   and explicitly put the package prefixes on symbols from those packages. 

3. Confirm that the owned instance-urls are being unpublished when we
   do any kind of update! of base-html-sheet or derivative.



Other
=====

 o All pending historical Trac tickets
