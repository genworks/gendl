/** X3DOM Runtime, http://www.x3dom.org/ 1.8.1 - 0c742a1a981f8c0a9cbb7059f36c3a6c4cb9fec6 - Fri Sep 27 18:22:15 2019 +0200 */
x3dom.registerNodeType("X3DParametricGeometryNode","NURBS",defineClass(x3dom.nodeTypes.X3DGeometryNode,function(ctx){x3dom.nodeTypes.X3DParametricGeometryNode.superClass.call(this,ctx);},{}));x3dom.registerNodeType("X3DNurbsSurfaceGeometryNode","NURBS",defineClass(x3dom.nodeTypes.X3DParametricGeometryNode,function(ctx){x3dom.nodeTypes.X3DParametricGeometryNode.superClass.call(this,ctx);this.addField_SFInt32(ctx,'uDimension',0);this.addField_SFInt32(ctx,'vDimension',0);this.addField_SFInt32(ctx,'uOrder',3);this.addField_SFInt32(ctx,'vOrder',3);this.addField_SFFloat(ctx,'uTessellation',0.0);this.addField_SFFloat(ctx,'vTessellation',0.0);this.addField_MFDouble(ctx,'uKnot',[]);this.addField_MFDouble(ctx,'vKnot',[]);this.addField_MFDouble(ctx,'weight',[]);this.addField_SFNode('controlPoint',x3dom.nodeTypes.X3DCoordinateNode);this.addField_SFBool(ctx,'uClosed',false);this.addField_SFBool(ctx,'vClosed',false);this.addField_SFNode('texCoord',x3dom.nodeTypes.X3DTextureCoordinateNode);this.addField_SFBool(ctx,'solid',false);this.addField_SFBool(ctx,'normalPerVertex',true);this._needReRender=true;this.basisFunsCache=new Map();this.uv=[];this.lastTime=-1;},{nodeChanged:function(){this._needReRender=true;this._vf.ccw=false;this._vf.solid=false;this._vf.useGeoCache=false;if(!this._hasCoarseMesh){var its=this.createCoarseITS(this);this._mesh=its._mesh;this._hasCoarseMesh=true;}
if(this._vf.uKnot.length!==this._vf.uDimension+this._vf.uOrder)
this._vf.uKnot=this.createDefaultKnots(this._vf.uDimension,this._vf.uOrder);if(this._vf.vKnot.length!==this._vf.vDimension+this._vf.vOrder)
this._vf.vKnot=this.createDefaultKnots(this._vf.vDimension,this._vf.vOrder);var T=[];if(this._cf.trimmingContour&&this._cf.trimmingContour.nodes.length){var len=this._cf.trimmingContour.nodes.length;for(var i=0;i<len;i++){var c2dnode=this._cf.trimmingContour.nodes[i];if(c2dnode._cf.children){T[i]=[];var trim=c2dnode._cf.children.nodes;for(var j=0;j<trim.length;j++){var tc=trim[j];if(!tc._vf.order){tc._vf.order=2;}
if(!tc._vf.knot){var knots=[];knots.push(0);knots.push(0);for(var k=2;k<tc._vf.controlPoint.length;k++)
knots.push(k-1);knots.push(knots[knots.length-1]+1);knots.push(knots[knots.length-1]);tc._vf.knot=knots;}
T[i].push([tc._vf.controlPoint.length-1,tc._vf.order-1,tc._vf.knot,tc._vf.controlPoint,tc._vf.weight]);}}}}
var onmessage=function(e){if(e.data.length>=3&&e.data[5]>this.caller.lastTime){if(this.caller.uv.length){var data=e.data[1];var point=new x3dom.fields.MFVec3f();for(var i=0;i<data.length;i++)
point.push(new x3dom.fields.SFVec3f(data[i][0],data[i][1],data[i][2]));this.caller._mesh._positions[0]=point.toGL();}else{var its=this.caller.createITS(e.data,this.caller);this.caller.workerTask=null;this.caller._mesh=its._mesh;if(this.caller._nameSpace){var tasks=x3dom.tessWorkerPool.taskQueue.length;var x3de=this.caller._nameSpace.doc._x3dElem;x3de.runtime.canvas.progressText=tasks==0?"":"Tesselation tasks: "+tasks;}}
if(this.caller._cleanupGLObjects)
this.caller._cleanupGLObjects(true);Array.forEach(this.caller._parentNodes,function(node){node.setAllDirty();});this.caller.basisFunsCache=e.data[3];this.caller.uv=e.data[4];this.caller.lastTime=e.data[5];}}
var coordNode=this._cf.controlPoint.node;var startmessage=[this._vf.uDimension-1,this._vf.vDimension-1,this._vf.uOrder-1,this._vf.vOrder-1,this._vf.uKnot,this._vf.vKnot,coordNode.getPoints(),this._vf.weight,this._vf.uTessellation,this._vf.vTessellation,T,this.basisFunsCache,this.uv,performance.now()];if(this.workerTask)
this.workerTask.discard=true;this.workerTask=new x3dom.WorkerTask(x3dom.tessWorkerScript,this,onmessage,startmessage);x3dom.tessWorkerPool.addWorkerTask(this.workerTask);},fieldChanged:function(fieldName){if(fieldName=='order'||fieldName=='knot'||fieldName.includes('Tessellation')){this.basisFunsCache=new Map();this.uv=[];this.nodeChanged();return}else if(this.uv.length)
this.nodeChanged();},createDefaultKnots:function(n,o){var knots=Array(n+o).fill(0);for(var k=o;k<n;k++)
knots[k]=(k-1)/(n-1);for(var k=knots.length-o;k<knots.length;k++)
knots[k]=1;return knots;},createCoarseITS:function(node){var w=node._vf.uDimension;var h=node._vf.vDimension;var coordNode=node._cf.controlPoint.node;var its=new x3dom.nodeTypes.IndexedTriangleSet();its._nameSpace=node._nameSpace;its._vf.solid=false;its._vf.ccw=false;its._cf.texCoord=node._cf.texCoord;var ind=[],i1=0,i2=w;for(var i=0;i<h-1;i++){for(var j=0;j<w-1;j++){ind.push(i1);ind.push(i1+1);ind.push(i2);ind.push(i2);ind.push(i1+1);ind.push(i2+1);i1++;i2++;}
i1++;i2++;}
its._vf.index=ind;its.addChild(coordNode);its.nodeChanged();its._xmlNode=node._xmlNode;return its;},createITS:function(data,node){var its=new x3dom.nodeTypes.IndexedTriangleSet();its._nameSpace=node._nameSpace;its._vf.normalPerVertex=node._vf.normalPerVertex;its._vf.solid=false;its._vf.ccw=false;its._vf.index=data[0];var co=new x3dom.nodeTypes.Coordinate();co._nameSpace=node._nameSpace;co._vf.point=new x3dom.fields.MFVec3f();for(var i=0;i<data[1].length;i++)
co._vf.point.push(new x3dom.fields.SFVec3f(data[1][i][0],data[1][i][1],data[1][i][2]));its.addChild(co);if(node._cf.texCoord.node!==null)its._cf.texCoord=node._cf.texCoord;else{var tc=new x3dom.nodeTypes.TextureCoordinate();tc._nameSpace=node._nameSpace;tc._vf.point=new x3dom.fields.MFVec2f();for(var i=0;i<data[2].length;i++)
tc._vf.point.push(new x3dom.fields.SFVec2f(data[2][i][0],data[2][i][1]));its.addChild(tc);}
its.nodeChanged();its._xmlNode=node._xmlNode;return its;}}));x3dom.registerNodeType("NurbsPatchSurface","NURBS",defineClass(x3dom.nodeTypes.X3DNurbsSurfaceGeometryNode,function(ctx){x3dom.nodeTypes.NurbsPatchSurface.superClass.call(this,ctx);this._needReRender=true;},{nodeChanged:function(){x3dom.nodeTypes.NurbsTrimmedSurface.prototype.nodeChanged.call(this);return;}}));x3dom.registerNodeType("NurbsCurve","NURBS",defineClass(x3dom.nodeTypes.X3DParametricGeometryNode,function(ctx){x3dom.nodeTypes.NurbsCurve.superClass.call(this,ctx);this.addField_SFInt32(ctx,'order',3);this.addField_MFDouble(ctx,'knot',[]);this.addField_SFNode('controlPoint',x3dom.nodeTypes.X3DCoordinateNode);this.addField_MFDouble(ctx,'weight',[]);this.addField_SFInt32(ctx,'tessellation',0);this.addField_SFBool(ctx,'closed',false);this.points=[];this.uList=[];this.basisFunsCache={};this.ils=new x3dom.nodeTypes.IndexedLineSet();this.ils.addChild(new x3dom.nodeTypes.Coordinate());},{nodeChanged:function(){this._needReRender=true;this._vf.useGeoCache=false;if(!this._hasCoarseMesh){var ils=this.createCoarseILS(this);this._mesh=ils._mesh;this._hasCoarseMesh=true;}
this.generateGeometry();},fieldChanged:function(fieldName){Array.forEach(this._parentNodes,function(node){node._dirty.positions=true;node.invalidateVolume();});switch(fieldName){case'tessellation':this.uList=[];break;case'knot':case'order':case'closed':this.uList=[];this.basisFunsCache={};break;}
this.generateGeometry();},generateGeometry:function(){this.points=this._cf.controlPoint.node._vf.point;var points=this.points.length;if(this._vf.knot.length!==points+this._vf.order)this.createDefaultKnots();if(this._vf.weight.length!=points)this._vf.weight=Array(points).fill(1.0);var tessPoints=this.calcTessPoints(this._vf.tessellation,points);if(this.uList.length==0)this.uList=this.listPoints(tessPoints,this._vf.knot);var data=this.tessellate();this.createILS(data,this);this._mesh=this.ils._mesh;return;},createDefaultKnots:function(){var knots=Array(this.points.length+this._vf.order).fill(0);for(var k=this._vf.order;k<this.points.length;k++)
knots[k]=k-1;for(var k=knots.length-this._vf.order;k<knots.length;k++)
knots[k]=this.points.length-1;this._vf.knot=knots;},calcTessPoints:function(tess,controls){if(tess>0)return tess+1;if(tess==0)return 2*controls+1;return-tess*controls+1;},listPoints:function(points,knots){var step=knots[knots.length-1]-knots[0];step=step/(points-1);var list=[];for(var i=0;i<points;i++)list.push(knots[0]+i*step);return list;},tessellate:function(){var nurb={dimension:this.points.length-1,u:this.uList,degree:this._vf.order-1,knots:this._vf.knot,points:this.points,weights:this._vf.weight,closed:this._vf.closed};return nurb.u.map(function(u){return this.curvePoint3DH(nurb.dimension,nurb.degree,nurb.knots,nurb.points,nurb.weights,u);},this);},curvePoint3DH:function(n,p,U,P,W,u){var spanu,indu,k,i;var Nu,temp=[0,0,0,0];spanu=this.findSpan(n,p,u,U);Nu=this.basisFuns(spanu,u,p,U);indu=spanu-p;for(k=0;k<=p;k++){i=indu+k;temp[0]+=Nu[k]*P[i].x;temp[1]+=Nu[k]*P[i].y;temp[2]+=Nu[k]*P[i].z;temp[3]+=Nu[k]*W[i];}
return new x3dom.fields.SFVec3f(temp[0]/temp[3],temp[1]/temp[3],temp[2]/temp[3]);},findSpan:function(n,p,u,U){var low,mid,high;if(u>=U[n])return n;if(u<=U[p])return p;low=0;high=n+1;mid=Math.floor((low+high)/2);while(u<U[mid]||u>=U[mid+1]){if(u<U[mid])high=mid;else low=mid;mid=Math.floor((low+high)/2);}
return mid;},basisFuns:function(i,u,p,U){var uKey=Math.floor(u*10e10);if(this.basisFunsCache[uKey])return this.basisFunsCache[uKey];var N=[],left=[],right=[],saved,temp;var j,r;N[0]=1.0;for(j=0;j<=p;j++){left[j]=0;right[j]=0;}
for(j=1;j<=p;j++){left[j]=u-U[i+1-j];right[j]=U[i+j]-u;saved=0.0;for(r=0;r<j;r++){temp=N[r]/(right[r+1]+left[j-r]);N[r]=saved+right[r+1]*temp;saved=left[j-r]*temp;}
N[j]=saved;}
this.basisFunsCache[uKey]=N;return N;},createCoarseILS:function(node){var coordNode=node._cf.controlPoint.node;var ils=new x3dom.nodeTypes.IndexedLineSet();ils._nameSpace=node._nameSpace;var ind=[];for(var i=0;i<coordNode._vf.point.length;i++){ind.push(i);}
ind.push(-1);ils._vf.coordIndex=ind;ils.addChild(coordNode);ils.nodeChanged();ils._xmlNode=node._xmlNode;return ils;},createILS:function(data,node){this.ils._nameSpace=node._nameSpace;this.ils._vf.coordIndex=[];var co=this.ils._cf.coord.node;co._nameSpace=node._nameSpace;co._vf.point=new x3dom.fields.MFVec3f();for(var i=0;i<data.length;i++){co._vf.point.push(data[i]);this.ils._vf.coordIndex.push(i);}
this.ils.nodeChanged();this.ils._xmlNode=node._xmlNode;return this.ils;}}));x3dom.registerNodeType("NurbsPositionInterpolator","NURBS",defineClass(x3dom.nodeTypes.X3DChildNode,function(ctx){x3dom.nodeTypes.NurbsPositionInterpolator.superClass.call(this,ctx);this.addField_SFInt32(ctx,'order',3);this.addField_MFDouble(ctx,'knot',[]);this.addField_SFNode('controlPoint',x3dom.nodeTypes.X3DCoordinateNode);this.addField_MFDouble(ctx,'weight',[]);this.addField_SFFloat(ctx,'set_fraction',0);this.points=[];},{fieldChanged:function(fieldName){if(fieldName==="set_fraction")
{var value=this.getValue(this._vf.set_fraction);this.postMessage('value_changed',value);}},getValue:function(u){this.points=this._cf.controlPoint.node._vf.point;var points=this.points.length;if(this._vf.knot.length!==points+this._vf.order)this.createDefaultKnots();if(this._vf.weight.length!=points)this._vf.weight=Array(points).fill(1.0);return this.curvePoint(u);},createDefaultKnots:function(){var points=this.points.length;var knots=Array(points+this._vf.order).fill(0);for(var k=this._vf.order;k<points;k++)
knots[k]=(k-1)/(points-1);for(var k=knots.length-this._vf.order;k<knots.length;k++)
knots[k]=1;this._vf.knot=knots;},curvePoint:function(u){var nurb={dimension:this.points.length-1,degree:this._vf.order-1,knots:this._vf.knot,points:this.points,weights:this._vf.weight};return x3dom.nodeTypes.NurbsCurve.prototype.curvePoint3DH.call(this,nurb.dimension,nurb.degree,nurb.knots,nurb.points,nurb.weights,u);},findSpan:function(n,p,u,U){return x3dom.nodeTypes.NurbsCurve.prototype.findSpan(n,p,u,U);},basisFuns:function(i,u,p,U){var N=[],left=[],right=[],saved,temp;var j,r;N[0]=1.0;for(j=0;j<=p;j++){left[j]=0;right[j]=0;}
for(j=1;j<=p;j++){left[j]=u-U[i+1-j];right[j]=U[i+j]-u;saved=0.0;for(r=0;r<j;r++){temp=N[r]/(right[r+1]+left[j-r]);N[r]=saved+right[r+1]*temp;saved=left[j-r]*temp;}
N[j]=saved;}
return N;}}));x3dom.registerNodeType("NurbsOrientationInterpolator","NURBS",defineClass(x3dom.nodeTypes.X3DChildNode,function(ctx){x3dom.nodeTypes.NurbsOrientationInterpolator.superClass.call(this,ctx);this.addField_SFInt32(ctx,'order',3);this.addField_MFDouble(ctx,'knot',[]);this.addField_SFNode('controlPoint',x3dom.nodeTypes.X3DCoordinateNode);this.addField_MFDouble(ctx,'weight',[]);this.addField_SFFloat(ctx,'set_fraction',0);this.points=[];this._fractionalShift=0.01;this._downZ=new x3dom.fields.SFVec3f(0,0,-1);},{fieldChanged:function(fieldName){if(fieldName==="set_fraction")
{var value=this.getValue(this._vf.set_fraction);this.postMessage('value_changed',value);}},getValue:function(u){this.points=this._cf.controlPoint.node._vf.point;var points=this.points.length;var knot=this._vf.knot;if(knot.length!==points+this._vf.order)
x3dom.nodeTypes.NurbsPositionInterpolator.prototype.createDefaultKnots.call(this);if(this._vf.weight.length!=points)
this._vf.weight=Array(points).fill(1.0);var uShift=(knot[knot.length-1]-knot[0])*this._fractionalShift;var diff=this.curvePoint(u).subtract(this.curvePoint(u+uShift));return x3dom.fields.Quaternion.rotateFromTo(this._downZ,diff);},curvePoint:function(u){return x3dom.nodeTypes.NurbsPositionInterpolator.prototype.curvePoint.call(this,u);},findSpan:function(n,p,u,U){return x3dom.nodeTypes.NurbsCurve.prototype.findSpan(n,p,u,U);},basisFuns:function(i,u,p,U){return x3dom.nodeTypes.NurbsPositionInterpolator.prototype.basisFuns(i,u,p,U);}}));x3dom.registerNodeType("NurbsSurfaceInterpolator","NURBS",defineClass(x3dom.nodeTypes.X3DChildNode,function(ctx){x3dom.nodeTypes.NurbsSurfaceInterpolator.superClass.call(this,ctx);this.addField_SFInt32(ctx,'uDimension',0);this.addField_SFInt32(ctx,'vDimension',0);this.addField_MFDouble(ctx,'uKnot',[]);this.addField_MFDouble(ctx,'vKnot',[]);this.addField_SFInt32(ctx,'uOrder',3);this.addField_SFInt32(ctx,'vOrder',3);this.addField_SFNode('controlPoint',x3dom.nodeTypes.X3DCoordinateNode);this.addField_MFDouble(ctx,'weight',[]);this.addField_SFVec2f(ctx,'set_fraction',0,0);this.points=[];this._fractionalShift=0.01;},{nodeChanged:function(){this.points=this._cf.controlPoint.node._vf.point;this.pointsLength=this.points.length;},fieldChanged:function(fieldName){if(fieldName==="set_fraction"){var value=this.getValue(this._vf.set_fraction);this.postMessage('position_changed',value.position);this.postMessage('normal_changed',value.normal);}},getValue:function(uv){var u=uv.x,v=uv.y;this.points=this._cf.controlPoint.node._vf.point;var uKnot=this._vf.uKnot;var vKnot=this._vf.vKnot;if(uKnot.length!==this._vf.uDimension+this._vf.uOrder)
uKnot=this.createDefaultKnots(this._vf.uDimension,this._vf.uOrder);if(vKnot.length!==this._vf.vDimension+this._vf.vOrder)
vKnot=this.createDefaultKnots(this._vf.vDimension,this._vf.vOrder);if(this._vf.weight.length!=this.pointsLength)
this._vf.weight=Array(this.pointsLength).fill(1.0);var uShift=(uKnot[uKnot.length-1]-uKnot[0])*this._fractionalShift;var vShift=(vKnot[vKnot.length-1]-vKnot[0])*this._fractionalShift;var uDiff=this.surfacePoint(u+uShift,v).subtract(this.surfacePoint(u,v));var vDiff=this.surfacePoint(u,v+vShift).subtract(this.surfacePoint(u,v));return{position:this.surfacePoint(u,v),normal:uDiff.cross(vDiff).normalize()}},createDefaultKnots:function(n,o){var knots=Array(n+o).fill(0);for(var k=o;k<n;k++)
knots[k]=(k-1)/(n-1);for(var k=knots.length-o;k<knots.length;k++)
knots[k]=1;return knots;},surfacePoint:function(u,v){return this.surfacePoint3DH(this._vf.uDimension-1,this._vf.vDimension-1,this._vf.uOrder-1,this._vf.vOrder-1,this._vf.uKnot,this._vf.vKnot,this.points,this._vf.weight,u,v);},findSpan:function(n,p,u,U){return x3dom.nodeTypes.NurbsCurve.prototype.findSpan(n,p,u,U);},basisFuns:function(i,u,p,U){return x3dom.nodeTypes.NurbsPositionInterpolator.prototype.basisFuns(i,u,p,U);},surfacePoint3DH:function(n,m,p,q,U,V,P,W,u,v)
{var spanu,spanv,indu,indv,l,k,i,j=0;var Nu,Nv,C=[],Cw=[0.0,0.0,0.0,0.0],temp=[];spanu=this.findSpan(n,p,u,U);Nu=this.basisFuns(spanu,u,p,U);spanv=this.findSpan(m,q,v,V);Nv=this.basisFuns(spanv,v,q,V);indu=spanu-p;for(l=0;l<=q;l++)
{indv=spanv-q+l;for(k=0;k<4;k++)
temp[j+k]=0.0;for(k=0;k<=p;k++)
{i=indu+k+(indv*(n+1));temp[j+0]+=Nu[k]*P[i].x;temp[j+1]+=Nu[k]*P[i].y;temp[j+2]+=Nu[k]*P[i].z;temp[j+3]+=Nu[k]*W[i];}
j+=4;}
j=0;for(l=0;l<=q;l++)
{Cw[0]+=Nv[l]*temp[j+0];Cw[1]+=Nv[l]*temp[j+1];Cw[2]+=Nv[l]*temp[j+2];Cw[3]+=Nv[l]*temp[j+3];j+=4;}
for(j=0;j<3;j++)
C[j]=Cw[j]/Cw[3];return new x3dom.fields.SFVec3f(C[0],C[1],C[2]);}}));x3dom.registerNodeType("NurbsCurve2D","NURBS",defineClass(x3dom.nodeTypes.X3DGroupingNode,function(ctx){x3dom.nodeTypes.NurbsCurve2D.superClass.call(this,ctx);this.addField_SFInt32(ctx,'order',3);this.addField_MFDouble(ctx,'knot',[]);this.addField_MFVec2f(ctx,'controlPoint',[]);this.addField_MFDouble(ctx,'weight',[]);this.addField_SFBool(ctx,'closed',false);},{}));x3dom.registerNodeType("NurbsTrimmedSurface","NURBS",defineClass(x3dom.nodeTypes.X3DNurbsSurfaceGeometryNode,function(ctx){x3dom.nodeTypes.NurbsTrimmedSurface.superClass.call(this,ctx);this.addField_MFNode('trimmingContour',x3dom.nodeTypes.Contour2D);this._needReRender=true;},{}));x3dom.registerNodeType("ContourPolyline2D","NURBS",defineClass(x3dom.nodeTypes.X3DGroupingNode,function(ctx){x3dom.nodeTypes.ContourPolyline2D.superClass.call(this,ctx);this.addField_MFVec2f(ctx,'controlPoint',[]);},{}));x3dom.registerNodeType("Contour2D","NURBS",defineClass(x3dom.nodeTypes.X3DGroupingNode,function(ctx){x3dom.nodeTypes.Contour2D.superClass.call(this,ctx);this.addField_MFNode('children',x3dom.nodeTypes.X3DChildNode);},{}));(function(){x3dom.tessWorkerScript=URL.createObjectURL(new Blob(['('+tessWorker.toString()+')()'],{type:'application/javascript'}));function tessWorker(){onmessage=function(e){if(e.data[11]){basisFunsCache=e.data[11];}else{basisFunsCache=new Map();}
var tess=new Tessellator(e.data);if(e.data[12]&&e.data[12].length){tess.deform();}
else{tess.adjustThresholds(e.data[8],e.data[9]);if(e.data[10]&&e.data[10].length){tess.initTrims();}else{tess.tloops=null;}
tess.tessellate();}
if(tess.have_transferables){var indices=new Uint32Array(tess.indices.length);for(var i=0;i<tess.indices.length;i++)
indices[i]=tess.indices[i];postMessage(indices.buffer,[indices.buffer]);var coords=new Float64Array(tess.coordinates.length);for(var i=0;i<tess.coordinates.length;i++)
coords[i]=tess.coordinates[i];postMessage(coords.buffer,[coords.buffer]);var tcoords=new Float64Array(tess.texcoords.length);for(var i=0;i<tess.texcoords.length;i++)
tcoords[i]=tess.texcoords[i];postMessage(tcoords.buffer,[tcoords.buffer]);}else{postMessage([tess.indices,tess.coordinates,tess.texcoords,basisFunsCache,tess.uv,e.data[13]]);}
close();}
function findSpan(n,p,u,U)
{var low,mid,high;if(u>=U[n])
return n;if(u<=U[p])
return p;low=0;high=n+1;mid=Math.floor((low+high)/2);while(u<U[mid]||u>=U[mid+1])
{if(u<U[mid])
high=mid;else
low=mid;mid=Math.floor((low+high)/2);}
return mid;}
function basisFuns(i,u,p,U)
{var uHash=Object.values(U).toString()+Math.floor(u*10e10);if(basisFunsCache.has(uHash))return basisFunsCache.get(uHash);var N=[],left=[],right=[],saved,temp;var j,r;N[0]=1.0;for(j=0;j<=p;j++){left[j]=0;right[j]=0;}
for(j=1;j<=p;j++)
{left[j]=u-U[i+1-j];right[j]=U[i+j]-u;saved=0.0;for(r=0;r<j;r++)
{temp=N[r]/(right[r+1]+left[j-r]);N[r]=saved+right[r+1]*temp;saved=left[j-r]*temp;}
N[j]=saved;}
basisFunsCache.set(uHash,N);return N;}
function surfacePoint3DH(n,m,p,q,U,V,P,W,u,v)
{var spanu,spanv,indu,indv,l,k,i,j=0;var Nu,Nv,C=[],Cw=[0.0,0.0,0.0,0.0],temp=[];spanu=findSpan(n,p,u,U);Nu=basisFuns(spanu,u,p,U);spanv=findSpan(m,q,v,V);Nv=basisFuns(spanv,v,q,V);indu=spanu-p;for(l=0;l<=q;l++)
{indv=spanv-q+l;for(k=0;k<4;k++)
temp[j+k]=0.0;for(k=0;k<=p;k++)
{i=indu+k+(indv*(n+1));temp[j+0]+=Nu[k]*P[i].x;temp[j+1]+=Nu[k]*P[i].y;temp[j+2]+=Nu[k]*P[i].z;temp[j+3]+=Nu[k]*W[i];}
j+=4;}
j=0;for(l=0;l<=q;l++)
{Cw[0]+=Nv[l]*temp[j+0];Cw[1]+=Nv[l]*temp[j+1];Cw[2]+=Nv[l]*temp[j+2];Cw[3]+=Nv[l]*temp[j+3];j+=4;}
for(j=0;j<3;j++)
C[j]=Cw[j]/Cw[3];return C;}
function surfacePoint3D(n,m,p,q,U,V,P,u,v)
{var spanu,spanv,indu,indv,l,k,i,j=0;var Nu,Nv,C=[0.0,0.0,0.0],temp=[];spanu=findSpan(n,p,u,U);Nu=basisFuns(spanu,u,p,U);spanv=findSpan(m,q,v,V);Nv=basisFuns(spanv,v,q,V);indu=spanu-p;for(l=0;l<=q;l++)
{indv=spanv-q+l;temp[j+0]=0.0;temp[j+1]=0.0;temp[j+2]=0.0;for(k=0;k<=p;k++)
{i=indu+k+(indv*(n+1));temp[j+0]+=Nu[k]*P[i].x;temp[j+1]+=Nu[k]*P[i].y;temp[j+2]+=Nu[k]*P[i].z;}
j+=3;}
j=0;for(l=0;l<=q;l++)
{C[0]+=Nv[l]*temp[j+0];C[1]+=Nv[l]*temp[j+1];C[2]+=Nv[l]*temp[j+2];j+=3;}
return C;}
function curvePoint2DH(n,p,U,P,W,u)
{var span,j,k;var N=[],Cw0=0.0,Cw1=0.0,Cw2=0.0;span=findSpan(n,p,u,U);N=basisFuns(span,u,p,U);for(j=0;j<=p;j++)
{k=(span-p+j);Cw0+=N[j]*P[k].x;Cw1+=N[j]*P[k].y;Cw2+=N[j]*W[k];}
return[Cw0/Cw2,Cw1/Cw2];}
function curvePoint2D(n,p,U,P,u)
{var span,j,k;var N=[],C0=0.0,C1=0.0;span=findSpan(n,p,u,U);N=basisFuns(span,u,p,U);for(j=0;j<=p;j++)
{k=(span-p+j);C0+=N[j]*P[k].x;C1+=N[j]*P[k].y;}
return[C0,C1];}
function Tessellator(nurb){this.bbox_split=4.0;this.use_objectspace=true;this.edge_thresh=0.1;this.trim_thresh=0.1;this.split_bias=0.5;this.skew_thresh=0.01;this.max_rec=5;this.w=nurb[0];this.h=nurb[1];this.p=nurb[2];this.q=nurb[3];this.U=nurb[4];this.V=nurb[5];this.P=nurb[6];this.W=nurb[7];this.tloops=nurb[10];this.useUV=nurb[12];this.surfaceHash=[];this.indexHash=[];this.curveHash=null;this.coordinates=[];this.texcoords=[];this.indices=[];this.uv=[];this.coordIndex=0;this.deform=function(){this.useUV.forEach(function(uv){this.computeSurface(uv,false);},this);}
this.tessellate=function(){if(this.W&&this.W.length!=(this.w+1)*(this.h+1))
this.W={};var u0=this.U[this.p];var u1=this.U[this.U.length-this.p-1];var u05=(u0+u1)*0.5;var v0=this.V[this.q];var v1=this.V[this.V.length-this.q-1];var v05=(v0+v1)*0.5;this.tessTri([[u0,v0],[u0,v05],[u05,v0]]);this.tessTri([[u0,v05],[u05,v05],[u05,v0]]);this.tessTri([[u0,v05],[u0,v1],[u05,v05]]);this.tessTri([[u0,v1],[u05,v1],[u05,v05]]);this.tessTri([[u05,v0],[u05,v05],[u1,v0]]);this.tessTri([[u05,v05],[u1,v05],[u1,v0]]);this.tessTri([[u05,v05],[u05,v1],[u1,v05]]);this.tessTri([[u05,v1],[u1,v1],[u1,v05]]);}
this.adjustThresholds=function(uparam,vparam){if(uparam<0){this.use_objectspace=false;if(vparam>=0.0)
vparam=uparam;var ul=2.0/(this.w+1);var vl=2.0/(this.h+1);ul*=(this.U[this.U.length-this.p-1]-this.U[this.p]);vl*=(this.V[this.V.length-this.q-1]-this.V[this.q]);this.edge_thresh_u=-uparam*ul;this.edge_thresh_v=-vparam*vl;}else{var mi=Number.MAX_VALUE;var mx=-Number.MAX_VALUE;var bb=[mi,mi,mi,mx,mx,mx];for(var i=0;i<this.P.length;i++){if(this.P[i].x<bb[0])bb[0]=this.P[i].x;if(this.P[i].y<bb[1])bb[1]=this.P[i].y;if(this.P[i].z<bb[2])bb[2]=this.P[i].z;if(this.P[i].x>bb[3])bb[3]=this.P[i].x;if(this.P[i].y>bb[4])bb[4]=this.P[i].y;if(this.P[i].z>bb[5])bb[5]=this.P[i].z;}
var ex=Math.sqrt((bb[0]-bb[3])*(bb[0]-bb[3])+
(bb[1]-bb[4])*(bb[1]-bb[4])+
(bb[2]-bb[5])*(bb[2]-bb[5]))/this.bbox_split;if(uparam<=10e-6)
uparam=1.0;this.edge_thresh*=ex*uparam;this.trim_thresh*=ex*uparam;}}
this.tessTri=function(tri){var work=[tri];while(work.length){var cur=work.splice(0,1);var pieces=this.refineTri(cur[0]);work=pieces.concat(work);}}
this.refineTri=function(tri){if(this.tloops&&this.inOut(tri)<0)
return[];var area=tri[0][0]*tri[1][1]-tri[1][0]*tri[0][1]+
tri[1][0]*tri[2][1]-tri[2][0]*tri[1][1]+
tri[2][0]*tri[0][1]-tri[0][0]*tri[2][1];if(area<0)
area=-area;var a=[],b=[];a[0]=tri[0][0]-tri[1][0];a[1]=tri[0][1]-tri[1][1];var max_ed=a[0]*a[0]+a[1]*a[1];a[0]=tri[1][0]-tri[2][0];a[1]=tri[1][1]-tri[2][1];max_ed+=a[0]*a[0]+a[1]*a[1];a[0]=tri[2][0]-tri[0][0];a[1]=tri[2][1]-tri[0][1];max_ed+=a[0]*a[0]+a[1]*a[1];area/=max_ed;if(area<=this.skew_thresh){this.diceTri(tri);return[];}
var eds=[];max_ed=0.0;for(var i=0;i<3;i++){var j0=(i+1)%3;var j1=(i+2)%3;a=tri[j0];b=tri[j1];eds[i]=this.splitEdge(a,b);if(eds[i]>max_ed)
max_ed=eds[i];}
max_ed*=this.split_bias;var m=[],mv=[],co=0;for(var i=0;i<3;i++){var j0=(i+1)%3;var j1=(i+2)%3;if((eds[i]>this.edge_thresh)&&(eds[i]>=max_ed)){co++;mv[i]=[];mv[i][0]=0.5*(tri[j0][0]+tri[j1][0]);mv[i][1]=0.5*(tri[j0][1]+tri[j1][1]);m[i]=i-3;}else{if(eds[j0]>eds[j1]){mv[i]=tri[j0];m[i]=j0;}else{mv[i]=tri[j1];m[i]=j1;}}}
var res=[];if(co){co++;var j=co;for(var i=0;i<3;i++){var j0=(i+1)%3;var j1=(i+2)%3;if((m[j1]!=i)&&(m[j0]!=i)){res[--j]=[tri[i],mv[j1],mv[j0]];}}
if(j){if((m[0]==m[1])||(m[1]==m[2])||(m[2]==m[0])){return[];}
res[0]=[mv[0],mv[1],mv[2]];}
return res;}
this.trimFinal(tri);return[];}
this.diceTri=function(tri){var cv=[];cv[0]=(tri[0][0]+tri[1][0]+tri[2][0])/3.0;cv[1]=(tri[0][1]+tri[1][1]+tri[2][1])/3.0;this.computeSurface(cv);for(var ed=0;ed<3;ed++){var divs=[],d=[];var e1=(ed+1)%3;d[0]=tri[e1][0]-tri[ed][0];d[1]=tri[e1][1]-tri[ed][1];divs[0]=1.0;var beg=0.0;while(divs.length){var a=[],b=[];var end=divs[0];a[0]=tri[ed][0]+d[0]*beg;a[1]=tri[ed][1]+d[1]*beg;b[0]=tri[ed][0]+d[0]*end;b[1]=tri[ed][1]+d[1]*end;if(this.splitEdge(a,b)>this.edge_thresh){divs.splice(0,0,0.5*(beg+end));}else{this.computeSurface(a);this.computeSurface(b);var slice=[cv,a,b];this.trimFinal(slice);divs.splice(0,1);beg=end;}}}}
this.computeSurface=function(uv,nooutput){var indu=Math.floor(uv[0]*10e10);var indv=Math.floor(uv[1]*10e10);if(this.surfaceHash[indu]){var memoizedPoint=this.surfaceHash[indu][indv];if(memoizedPoint)
return memoizedPoint;}
var pnt;if(Object.keys(this.W).length){pnt=surfacePoint3DH(this.w,this.h,this.p,this.q,this.U,this.V,this.P,this.W,uv[0],uv[1]);}else{pnt=surfacePoint3D(this.w,this.h,this.p,this.q,this.U,this.V,this.P,uv[0],uv[1]);}
if(this.curveHash)
return pnt;if(!this.surfaceHash[indu])
this.surfaceHash[indu]=[];this.surfaceHash[indu][indv]=pnt;if(nooutput)
return pnt;if(!this.indexHash[indu])
this.indexHash[indu]=[];this.indexHash[indu][indv]=this.coordIndex;this.coordIndex++;this.coordinates.push(pnt);this.texcoords.push([uv[0],uv[1]]);this.uv.push(uv);return pnt;}
this.computeCurve=function(loop,seg,u){var indu=Math.floor(u*10e10);if(this.curveHash[loop][seg]){var memoizedPoint=this.curveHash[loop][seg][indu];if(memoizedPoint)
return memoizedPoint;}
var pnt,crv=this.tloops[loop][seg];if(crv[4]&&crv[4].length){pnt=curvePoint2DH(crv[0],crv[1],crv[2],crv[3],crv[4],u);}else{pnt=curvePoint2D(crv[0],crv[1],crv[2],crv[3],u);}
if(!this.curveHash[loop][seg])
this.curveHash[loop][seg]=[];this.curveHash[loop][seg][indu]=pnt;return pnt;}
this.splitEdge=function(a,b){var pa=this.computeSurface(a);var pb=this.computeSurface(b);if(!this.use_objectspace){var dist=Math.sqrt((a[0]-b[0])*(a[0]-b[0])/this.edge_thresh_u+
(a[1]-b[1])*(a[1]-b[1])/this.edge_thresh_v);return dist;}
if(Math.abs(pa[0]-pb[0])>10e-6||Math.abs(pa[1]-pb[1])>10e-6||Math.abs(pa[2]-pb[2])>10e-6){var lab=Math.sqrt((pa[0]-pb[0])*(pa[0]-pb[0])+
(pa[1]-pb[1])*(pa[1]-pb[1])+
(pa[2]-pb[2])*(pa[2]-pb[2]));if(lab<this.edge_thresh){var m=[a[0]+(b[0]-a[0])*0.5,a[1]+(b[1]-a[1])*0.5];var pm=this.computeSurface(m,false);var lamb=0.0;if(Math.abs(pa[0]-pm[0])>10e-6||Math.abs(pa[1]-pm[1])>10e-6||Math.abs(pa[2]-pm[2])>10e-6){lamb+=Math.sqrt((pa[0]-pm[0])*(pa[0]-pm[0])+
(pa[1]-pm[1])*(pa[1]-pm[1])+
(pa[2]-pm[2])*(pa[2]-pm[2]))}
if(Math.abs(pb[0]-pm[0])>10e-6||Math.abs(pb[1]-pm[1])>10e-6||Math.abs(pb[2]-pm[2])>10e-6){lamb+=Math.sqrt((pb[0]-pm[0])*(pb[0]-pm[0])+
(pb[1]-pm[1])*(pb[1]-pm[1])+
(pb[2]-pm[2])*(pb[2]-pm[2]))}
if(lamb>lab*1.01)
lab+=lamb;}
return lab;}
else
return 0.0;}
this.splitCenter=function(tri){var cv=[];this.curveHash=[];cv[0]=(tri[0][0]+tri[1][0]+tri[2][0])/3.0;cv[1]=(tri[0][1]+tri[1][1]+tri[2][1])/3.0;var ed=0.0;for(var i=0;i<3;i++)
ed+=this.splitEdge(tri[i],cv);this.curveHash=null;if(ed*0.5>this.edge_thresh){this.computeSurface(cv);return true;}
return false;}
this.renderFinal=function(tri){for(var i=0;i<3;i++){var uv=tri[i];var indu=Math.floor(uv[0]*10e10);var indv=Math.floor(uv[1]*10e10);if(this.indexHash[indu])
this.indices.push(this.indexHash[indu][indv]);}}
this.intersectTrim=function(p1,p2,prev){var sl=0,ss=0;if(prev){sl=prev[2];ss=prev[3]+1;}
for(var ilp=sl;ilp<this.ttloops.length;ilp++){var lp=this.ttloops[ilp];for(var k=ss;k<lp.length-1;k++){var p3=lp[k];var p4=lp[k+1];if(((Math.abs(p1[0]-p3[0])<10e-6)&&(Math.abs(p1[1]-p3[1])<10e-6))||((Math.abs(p1[0]-p4[0])<10e-6)&&(Math.abs(p1[1]-p4[1])<10e-6)))
return[];if(((Math.abs(p2[0]-p3[0])<10e-6)&&(Math.abs(p2[1]-p3[1])<10e-6))||((Math.abs(p2[0]-p4[0])<10e-6)&&(Math.abs(p2[1]-p4[1])<10e-6)))
return[];var den=((p2[0]-p1[0])*(p4[1]-p3[1])-
(p2[1]-p1[1])*(p4[0]-p3[0]));if(Math.abs(den)<10e-6){continue;}
var r=((p1[1]-p3[1])*(p4[0]-p3[0])-
(p1[0]-p3[0])*(p4[1]-p3[1]))/den;if((r<10e-6)||(r>(1.0-10e-6)))
continue;var s=((p1[1]-p3[1])*(p2[0]-p1[0])-
(p1[0]-p3[0])*(p2[1]-p1[1]))/den;if((s<10e-6)||(s>(1.0-10e-6)))
continue;return[p1[0]+r*(p2[0]-p1[0]),p1[1]+r*(p2[1]-p1[1]),ilp,k];}
if(prev)
return[];}
return[];}
this.renderDiced=function(p0,p1,p2,tm,ip0,ip1){var ip0s=ip0;var ip1s=ip1;if(((p0[0]-ip0[0])*(p0[0]-ip0[0])+(p0[1]-ip0[1])*(p0[1]-ip0[1]))>((p0[0]-ip1[0])*(p0[0]-ip1[0])+(p0[1]-ip1[1])*(p0[1]-ip1[1]))){ip0s=ip1;ip1s=ip0;}
this.renderFinal([p0,tm,p2]);this.renderFinal([p0,ip0s,tm]);this.renderFinal([p1,p2,tm]);this.renderFinal([p1,tm,ip1s]);}
this.renderComplex=function(tri,ip0,ip1,ip2){this.diceTri([tri[0],ip0,ip2]);this.diceTri([ip0,tri[1],ip1]);this.diceTri([ip0,ip1,ip2]);this.diceTri([ip2,ip1,tri[2]]);}
this.renderTrimmed=function(tri){var t=0.3;var ip0=this.intersectTrim(tri[0],tri[1]);var ip1=this.intersectTrim(tri[1],tri[2]);var ip2=this.intersectTrim(tri[2],tri[0]);var len=(ip0.length>0)+(ip1.length>0)+(ip2.length>0);if(len==1){var ip=[ip0,ip1,ip2];for(var i=0;i<3;i++)
if(ip[i].length){if(((Math.abs(tri[i][0]-ip[i][0])<10e-6)&&(Math.abs(tri[i][1]-ip[i][1])<10e-6))||((Math.abs(tri[(i+1)%3][0]-ip[i][0])<10e-6)&&(Math.abs(tri[(i+1)%3][1]-ip[i][1])<10e-6))){this.renderFinal(tri);return;}}
if(ip0.length){var ip01=this.intersectTrim(tri[0],tri[1],ip0);if(ip01.length){var tm=this.ttloops[ip01[2]][ip01[3]];this.computeSurface(tm);this.computeSurface(ip0);this.computeSurface(ip01);if(this.inOut([tri[2],[tri[2][0]+(tri[1][0]-tri[2][0])*t,tri[2][1]+(tri[1][1]-tri[2][1])*t],[tri[2][0]+(tri[0][0]-tri[2][0])*t,tri[2][1]+(tri[0][1]-tri[2][1])*t]])>0){this.renderDiced(tri[0],tri[1],tri[2],tm,ip0,ip01);}else{this.renderFinal([ip0,tm,ip01]);}
return;}}else if(ip1.length){var ip11=this.intersectTrim(tri[1],tri[2],ip1);if(ip11.length){var tm=this.ttloops[ip11[2]][ip11[3]];this.computeSurface(tm);this.computeSurface(ip1);this.computeSurface(ip11);if(this.inOut([tri[0],[tri[0][0]+(tri[1][0]-tri[0][0])*t,tri[0][1]+(tri[1][1]-tri[0][1])*t],[tri[0][0]+(tri[2][0]-tri[0][0])*t,tri[0][1]+(tri[2][1]-tri[0][1])*t]])>0){this.renderDiced(tri[1],tri[2],tri[0],tm,ip1,ip11);}else{this.renderFinal([ip1,tm,ip11]);}
return;}}else if(ip2.length){var ip21=this.intersectTrim(tri[2],tri[0],ip2);if(ip21.length){var tm=this.ttloops[ip21[2]][ip21[3]];this.computeSurface(tm);this.computeSurface(ip2);this.computeSurface(ip21);if(this.inOut([tri[1],[tri[1][0]+(tri[0][0]-tri[1][0])*t,tri[1][1]+(tri[0][1]-tri[1][1])*t],[tri[1][0]+(tri[2][0]-tri[1][0])*t,tri[1][1]+(tri[2][1]-tri[1][1])*t]])>0){this.renderDiced(tri[2],tri[0],tri[1],tm,ip2,ip21);}else{this.renderFinal([ip2,tm,ip21]);}
return;}}
if(this.max_rec){this.max_rec--;this.diceTri(tri);this.max_rec++;return;}}
if(len!=2){if(len==3){this.renderComplex(tri,ip0,ip1,ip2);return;}else{var out=0;if(this.inOut([tri[0],[tri[0][0]+(tri[1][0]-tri[0][0])*t,tri[0][1]+(tri[1][1]-tri[0][1])*t],[tri[0][0]+(tri[2][0]-tri[0][0])*t,tri[0][1]+(tri[2][1]-tri[0][1])*t]])<0)
out++;if(this.inOut([tri[1],[tri[1][0]+(tri[0][0]-tri[1][0])*t,tri[1][1]+(tri[0][1]-tri[1][1])*t],[tri[1][0]+(tri[2][0]-tri[1][0])*t,tri[1][1]+(tri[2][1]-tri[1][1])*t]])<0)
out++;if(this.inOut([tri[2],[tri[2][0]+(tri[1][0]-tri[2][0])*t,tri[2][1]+(tri[1][1]-tri[2][1])*t],[tri[2][0]+(tri[0][0]-tri[2][0])*t,tri[2][1]+(tri[0][1]-tri[2][1])*t]])<0)
out++;if(out>0){return;}
this.renderFinal(tri);}}else{if(!ip0.length){this.computeSurface(ip1);this.computeSurface(ip2);if(this.inOut([tri[2],[tri[2][0]+(ip2[0]-tri[2][0])*t,tri[2][1]+(ip2[1]-tri[2][1])*t],[tri[2][0]+(ip1[0]-tri[2][0])*t,tri[2][1]+(ip1[1]-tri[2][1])*t]])<=0){this.renderFinal([tri[0],ip1,ip2]);this.renderFinal([tri[0],tri[1],ip1]);}else{this.renderFinal([tri[2],ip2,ip1]);}}else{if(!ip1.length){this.computeSurface(ip0);this.computeSurface(ip2);if(this.inOut([tri[0],[tri[0][0]+(ip2[0]-tri[0][0])*t,tri[0][1]+(ip2[1]-tri[0][1])*t],[tri[0][0]+(ip0[0]-tri[0][0])*t,tri[0][1]+(ip0[1]-tri[0][1])*t]])<=0){this.renderFinal([tri[1],ip2,ip0]);this.renderFinal([tri[1],tri[2],ip2]);}else{this.renderFinal([tri[0],ip0,ip2]);}}else{this.computeSurface(ip0);this.computeSurface(ip1);if(this.inOut([tri[1],[tri[1][0]+(ip0[0]-tri[1][0])*t,tri[1][1]+(ip0[1]-tri[1][1])*t],[tri[1][0]+(ip1[0]-tri[1][0])*t,tri[1][1]+(ip1[1]-tri[1][1])*t]])<=0){this.renderFinal([tri[0],ip0,ip1]);this.renderFinal([tri[0],ip1,tri[2]]);}else{this.renderFinal([tri[1],ip1,ip0]);}}}}
return;}
this.inOut=function(tri){var a=[],ad=[];var cl=[];var cg=[];var ndx=[];for(var i=0;i<3;i++){a[i]=[];var va=tri[i];var vb=tri[(i+1)%3];a[i][0]=va[1]-vb[1];a[i][1]=vb[0]-va[0];ad[i]=a[i][0]*va[0]+a[i][1]*va[1];cl[i]=cg[i]=0;ndx[i]=(Math.abs(a[i][0])>Math.abs(a[i][1]))?1:0;}
for(var ilp=0;ilp<this.ttloops.length;ilp++){var lp=this.ttloops[ilp];for(var k=0;k<lp.length-1;k++){var p0=lp[k];var p1=lp[k+1];var ni=0;for(var i=0;i<3;i++){var d0=p0[0]*a[i][0]+p0[1]*a[i][1]-ad[i];var d1=p1[0]*a[i][0]+p1[1]*a[i][1]-ad[i];ni+=(d0<0)?1:-1;if((d0<0)?(d1<0):(d1>=0))
continue;var ip=(p1[ndx[i]]*d0-p0[ndx[i]]*d1)/(d0-d1);var ba=ip<tri[i][ndx[i]];var bb=ip<tri[(i+1)%3][ndx[i]];if(ba&&bb){cl[i]++;}else{if(!(ba||bb)){cg[i]++;}else{return 0;}}}
if((ni==3)||(ni==-3))
return 0;}}
if((cl[0]&1)&&(cl[1]&1)&&(cl[2]&1))return 1;if(!((cl[0]&1)||(cl[1]&1)||(cl[2]&1)))return-1;return 0;}
this.refineTrim=function(loop,u1,u2){var pa=this.computeCurve(loop,Math.floor(u1),u1);var pb=this.computeCurve(loop,Math.floor(u2),u2);return(this.splitEdge(pa,pb)>this.edge_thresh);}
this.initTrims=function(){this.ttloops=[];this.curveHash=[];var edt=this.edge_thresh;this.edge_thresh=this.trim_thresh;for(var ilp=0;ilp<this.tloops.length;ilp++){var lp=this.tloops[ilp];this.curveHash[ilp]=[];this.ttloops[ilp]=[];var ttus=[];var pnts=[];var ue;for(var j=0;j<lp.length;j++){var U=lp[j][2];var uf=1.0/(U[U.length-1]-U[0]);if(Math.abs(1.0-uf)>10e-12)
for(var i=0;i<U.length;i++)
U[i]=(U[i]*uf);var ud=U[0]-j;if(Math.abs(ud)>10e-12)
for(var i=0;i<U.length;i++)
U[i]=(U[i]-ud);var ui=lp[j][1];var u=U[ui];ue=U[(U.length-ui-1)];while(u<ue){this.ttloops[ilp].push(this.computeCurve(ilp,j,u));ttus.push(u);ui++;while(Math.abs(u-U[ui])<10e-6)
ui++;u=U[ui];}}
var tlp=this.ttloops[ilp];var x=0;while(x<tlp.length){var p0u=ttus[x];var p0seg=Math.floor(p0u);var y=x+1;if(y==tlp.length){y=0;}
var p1u=ttus[y];if(lp[p0seg][1]>1&&this.refineTrim(ilp,p0u,p1u)){var um;if(y==0){um=0.5*(p0u+ue);y=x+1;}else{um=0.5*(p0u+p1u);}
var v=this.computeCurve(ilp,p0seg,um);tlp.splice(y,0,v)
ttus.splice(y,0,um);}else{if(y==0)
break;x=y;}}
tlp.push(tlp[0]);}
this.curveHash=null;this.edge_thresh=edt;}
this.trimFinal=function(tri){if(this.tloops){if(this.inOut(tri)>=0){this.renderTrimmed(tri);return;}else{var t=0.3;var out=0;if(this.inOut([tri[0],[tri[0][0]+(tri[1][0]-tri[0][0])*t,tri[0][1]+(tri[1][1]-tri[0][1])*t],[tri[0][0]+(tri[2][0]-tri[0][0])*t,tri[0][1]+(tri[2][1]-tri[0][1])*t]])<0)
out++;if(this.inOut([tri[1],[tri[1][0]+(tri[0][0]-tri[1][0])*t,tri[1][1]+(tri[0][1]-tri[1][1])*t],[tri[1][0]+(tri[2][0]-tri[1][0])*t,tri[1][1]+(tri[2][1]-tri[1][1])*t]])<0)
out++;if(this.inOut([tri[2],[tri[2][0]+(tri[1][0]-tri[2][0])*t,tri[2][1]+(tri[1][1]-tri[2][1])*t],[tri[2][0]+(tri[0][0]-tri[2][0])*t,tri[2][1]+(tri[0][1]-tri[2][1])*t]])<0)
out++;if(out>0){return;}}}
this.renderFinal(tri);}}}})();x3dom.WorkerPool=function(size){var _this=this;this.taskQueue=[];this.workerQueue=[];this.poolSize=size;this.init=function(){for(var i=0;i<this.poolSize;i++){_this.workerQueue.push(new x3dom.WorkerThread(_this));}}
this.addWorkerTask=function(workerTask){if(_this.workerQueue.length>0){var workerThread=_this.workerQueue.shift();workerThread.run(workerTask);}else{_this.taskQueue.push(workerTask);}}
this.freeWorkerThread=function(workerThread){if(_this.taskQueue.length>0){var workerTask=_this.taskQueue.shift();if(workerTask.discard)
return this.freeWorkerThread(workerThread);workerThread.run(workerTask);}else{_this.workerQueue.push(workerThread);}}};x3dom.WorkerThread=function(workerPool){var _this=this;this.workerPool=workerPool;this.workerTask={};this.run=function(workerTask){_this.workerTask=workerTask;var worker=new Worker(workerTask.script);worker.caller=workerTask.caller;worker.onmessage=function(e){_this.workerTask.callback(e);_this.workerPool.freeWorkerThread(_this);}
worker.postMessage(workerTask.startMessage);}};x3dom.WorkerTask=function(script,caller,callback,msg){this.script=script;this.caller=caller;this.callback=callback;this.startMessage=msg;};(function(){var poolSize=1;if(navigator.hardwareConcurrency){poolSize=Math.max(1,navigator.hardwareConcurrency-1);}
x3dom.tessWorkerPool=new x3dom.WorkerPool(poolSize);x3dom.tessWorkerPool.init();})();