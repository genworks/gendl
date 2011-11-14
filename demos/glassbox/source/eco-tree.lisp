(in-package :gwl-user)


(define-object eco-tree (base-html-sheet)
  
  :computed-slots
  ((additional-header-content (with-cl-who-string  ()
				((:script :type "text/javascript"
					  :src "/static/3rdpty/ecotree/ECOTree.js"))
				((:link :type "text/css"
					:rel "stylesheet"
					:href "/static/3rdpty/ecotree/ECOTree.css"))
				(:style " .copy {
				font-family : \"Verdana\";				
				font-size : 10px;
				color : #CCCCCC;
			}")
				
				
				((:script  :type "text/javascript")
				 
				 "

  var t = null;
	
	function CreateTree() {
				t = new ECOTree('t','sample2');						
				t.config.iRootOrientation = ECOTree.RO_LEFT;
				t.config.defaultNodeWidth = 112;
				t.config.defaultNodeHeight = 20;
				t.config.iSubtreeSeparation = 20;
				t.config.iSiblingSeparation = 10;										
				t.config.linkType = 'B';
				t.config.useTarget = false;
				t.config.nodeFill = ECOTree.NF_GRADIENT;
				t.config.colorStyle = ECOTree.CS_LEVEL;
				t.config.levelColors = [\"#966E00\",\"#BC9400\",\"#D9B100\",\"#FFD700\"];
				t.config.levelBorderColors = [\"#FFD700\",\"#D9B100\",\"#BC9400\",\"#966E00\"];
				t.config.nodeColor = \"#FFD700\";
				t.config.nodeBorderColor = \"#FFD700\";
				t.config.linkColor = \"#FFD700\";
				t.add(1,-1,'species',null,null,\"#F08080\");
				t.add(2,1,'plants');
				t.add(3,1,'fungi');
				t.add(4,1,'lichens');
				t.add(5,1,'animals');
				t.add(6,2,'mosses');
				t.add(7,2,'ferns');
				t.add(8,2,'gymnosperms');
				t.add(9,2,'dicotyledons');
				t.add(10,2,'monocotyledons');
				t.add(11,5,'invertebrates');
				t.add(12,5,'vertebrates');
				t.add(13,11,'insects');
				t.add(14,11,'molluscs');
				t.add(15,11,'crustaceans');		
				t.add(16,11,'others');								
				t.add(17,12,'fish');
				t.add(18,12,'amphibians');
				t.add(19,12,'reptiles');		
				t.add(20,12,'birds');								
				t.add(21,12,'mammals');												
				t.UpdateTree();
			}				"
				  
				  ))  :uncached)
   
   
   
   (main-sheet-body (with-cl-who-string ()
		      (:h4 "ECOTree Simple Tree 4&nbsp;")
		      ((:div :id "sample2")))
		    :uncached)))


(define-lens (html-format eco-tree) ()
  :output-functions
  ((main-sheet 
    ()
    (with-cl-who (:indent t)
      (:html
       (:head (str (the additional-header-content)))
       ((:body :onload "CreateTree();")
	(str (the main-sheet-body))))))))


(publish-gwl-app "/eco-tree" "gwl-user::eco-tree")
				



				
				
				
				
				
				
