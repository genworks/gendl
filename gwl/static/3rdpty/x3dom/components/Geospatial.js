/** X3DOM Runtime, http://www.x3dom.org/ 1.8.1 - 0c742a1a981f8c0a9cbb7059f36c3a6c4cb9fec6 - Fri Sep 27 18:22:15 2019 +0200 */
x3dom.registerNodeType("GeoCoordinate","Geospatial",defineClass(x3dom.nodeTypes.X3DCoordinateNode,function(ctx){x3dom.nodeTypes.GeoCoordinate.superClass.call(this,ctx);this.addField_MFVec3f(ctx,'point',[]);this.addField_MFString(ctx,'geoSystem',['GD','WE']);this.addField_SFNode('geoOrigin',x3dom.nodeTypes.GeoOrigin);},{elipsoideParameters:{'AA':['Airy 1830','6377563.396','299.3249646'],'AM':['Modified Airy','6377340.189','299.3249646'],'AN':['Australian National','6378160','298.25'],'BN':['Bessel 1841 (Namibia)','6377483.865','299.1528128'],'BR':['Bessel 1841 (Ethiopia Indonesia...)','6377397.155','299.1528128'],'CC':['Clarke 1866','6378206.4','294.9786982'],'CD':['Clarke 1880','6378249.145','293.465'],'EA':['Everest (India 1830)','6377276.345','300.8017'],'EB':['Everest (Sabah & Sarawak)','6377298.556','300.8017'],'EC':['Everest (India 1956)','6377301.243','300.8017'],'ED':['Everest (W. Malaysia 1969)','6377295.664','300.8017'],'EE':['Everest (W. Malaysia & Singapore 1948)','6377304.063','300.8017'],'EF':['Everest (Pakistan)','6377309.613','300.8017'],'FA':['Modified Fischer 1960','6378155','298.3'],'HE':['Helmert 1906','6378200','298.3'],'HO':['Hough 1960','6378270','297'],'ID':['Indonesian 1974','6378160','298.247'],'IN':['International 1924','6378388','297'],'KA':['Krassovsky 1940','6378245','298.3'],'RF':['Geodetic Reference System 1980 (GRS 80)','6378137','298.257222101'],'SA':['South American 1969','6378160','298.25'],'WD':['WGS 72','6378135','298.26'],'WE':['WGS 84','6378137','298.257223563']},fieldChanged:function(fieldName){if(fieldName=="point"||fieldName=="geoSystem"){Array.forEach(this._parentNodes,function(node){node.fieldChanged("coord");});}},isLogitudeFirst:function(geoSystem){for(var i=0;i<geoSystem.length;++i)
if(geoSystem[i]=='longitude_first')
return true;return false;},getElipsoideCode:function(geoSystem)
{for(var i=0;i<geoSystem.length;++i)
{var code=geoSystem[i];if(this.elipsoideParameters[code])
return code;}
return'WE';},getElipsoide:function(geoSystem)
{return this.elipsoideParameters[this.getElipsoideCode(geoSystem)];},getReferenceFrame:function(geoSystem)
{for(var i=0;i<geoSystem.length;++i)
{var code=geoSystem[i];if(code=='GD'||code=='GDC')
return'GD';if(code=='GC'||code=='GCC')
return'GC';if(code=='UTM')
return'UTM';else
x3dom.debug.logError('Unknown GEO system: ['+geoSystem+']');}
return'GD';},getUTMZone:function(geoSystem)
{for(var i=0;i<geoSystem.length;++i)
{var code=geoSystem[i];if(code[0]=='Z')
return code.substring(1);}
x3dom.debug.logError('no UTM zone but is required:'+geoSystem);},getUTMHemisphere:function(geoSystem)
{for(var i=0;i<geoSystem.length;++i)
{var code=geoSystem[i];if(code=='S')
return code;}
return'N';},isUTMEastingFirst:function(geoSystem)
{for(var i=0;i<geoSystem.length;++i)
{var code=geoSystem[i];if(code=='easting_first')
return true;}
return false;},UTMtoGC:function(geoSystem,coords)
{var utmzone=this.getUTMZone(geoSystem);if(utmzone<1||utmzone>60||utmzone===undefined)
return x3dom.debug.logError('invalid UTM zone: '+utmzone+' in geosystem '+geoSystem);var hemisphere=this.getUTMHemisphere(geoSystem);var eastingFirst=this.isUTMEastingFirst(geoSystem);var elipsoide=this.getElipsoide(geoSystem);var a=elipsoide[1];var f=1/elipsoide[2];var k0=0.9996;var b=a*(1-f);var esq=(1-(b/a)*(b/a));var e=Math.sqrt(esq);var e0=e/Math.sqrt(1-esq);var e0sq=esq/(1-esq);var zcm=3+6*(utmzone-1)-180;var e1=(1-Math.sqrt(1-esq))/(1+Math.sqrt(1-esq));var e1sq=e1*e1;var output=new x3dom.fields.MFVec3f();var rad2deg=180/Math.PI;var f3o64=3/64;var f5o256=5/256;var f27o32=27/32;var f21o16=21/16;var f55o32=55/32;var f151o96=151/96;var f1097o512=1097/512;var fmua=1-esq*(0.25+esq*(f3o64+f5o256*esq));var fphi11=e1*(1.5-f27o32*e1sq);var fphi12=e1sq*(f21o16-f55o32*e1sq);var current,x,y,z,M,mu,phi1,cosphi1,C1,tanphi1,T1,T1sq;var esinphi1,oneesinphi1,N1,R1,D,Dsq,C1sq,phi,lng;for(var i=0;i<coords.length;++i)
{x=(eastingFirst?coords[i].x:coords[i].y);y=(eastingFirst?coords[i].y:coords[i].x);M=(hemisphere=="S"?(y-10000000):y)/k0;mu=M/(a*fmua);phi1=mu+fphi11*Math.sin(2*mu)+fphi12*Math.sin(4*mu);phi1=phi1+e1*(e1sq*(Math.sin(6*mu)*f151o96+Math.sin(8*mu)*f1097o512));cosphi1=Math.cos(phi1);C1=e0sq*cosphi1*cosphi1;tanphi1=Math.tan(phi1);T1=tanphi1*tanphi1;T1sq=T1*T1;esinphi1=e*Math.sin(phi1);oneesinphi1=1-esinphi1*esinphi1;N1=a/Math.sqrt(oneesinphi1);R1=N1*(1-esq)/oneesinphi1;D=(x-500000)/(N1*k0);Dsq=D*D;C1sq=C1*C1;phi=Dsq*(0.5-Dsq*(5+3*T1+10*C1-4*C1sq-9*e0sq)/24);phi=phi+Math.pow(D,6)*(61+90*T1+298*C1+45*T1sq-252*e0sq-3*C1sq)/720;phi=phi1-(N1*tanphi1/R1)*phi;lng=D*(1+Dsq*((-1-2*T1-C1)/6+Dsq*(5-2*C1+28*T1-3*C1sq+8*e0sq+24*T1sq)/120))/cosphi1;current=new x3dom.fields.SFVec3f();current.x=zcm+rad2deg*lng;current.y=rad2deg*phi;current.z=coords[i].z;output.push(current);}
var GDgeoSystem=new x3dom.fields.MFString();GDgeoSystem.push("GD");GDgeoSystem.push(this.getElipsoideCode(geoSystem));GDgeoSystem.push("longitude_first");return this.GDtoGC(GDgeoSystem,output);},GCtoUTM:function(geoSystem,coords){var coordsGD=this.GCtoGD(geoSystem,coords);var utmzone=this.getUTMZone(geoSystem);if(utmzone<1||utmzone>60||utmzone===undefined)
return x3dom.debug.logError('invalid UTM zone: '+utmzone+' in geosystem '+geoSystem);var hemisphere=this.getUTMHemisphere(geoSystem);var eastingFirst=this.isUTMEastingFirst(geoSystem);var elipsoide=this.getElipsoide(geoSystem);var a=elipsoide[1];var f=1/elipsoide[2];var k0=0.9996;var b=a*(1-f);var esq=(1-(b/a)*(b/a));var e=Math.sqrt(esq);var e0sq=esq/(1-esq);var M0=0;var deg2rad=Math.PI/180;var zcmrad=(3+6*(utmzone-1)-180)*deg2rad;var coordsUTM=new x3dom.fields.MFVec3f();var N,T,C,A,M,x,y,phi,lng,cosphi,tanphi,Asq;var i,current;var fMphi=1-esq*(1/4+esq*(3/64+5*esq/256));var fM2phi=esq*(3/8+esq*(3/32+45*esq/1024));var fM4phi=esq*esq*(15/256+esq*45/1024);var fM6phi=esq*esq*esq*(35/3072);for(i=0;i<coordsGD.length;++i){current=new x3dom.fields.SFVec3f();phi=coordsGD[i].y*deg2rad;lng=coordsGD[i].x*deg2rad;cosphi=Math.cos(phi);tanphi=Math.tan(phi);N=a/Math.sqrt(1-Math.pow(e*Math.sin(phi),2));T=Math.pow(tanphi,2);C=e0sq*Math.pow(cosphi,2);A=(lng-zcmrad)*cosphi;M=phi*fMphi;M=M-Math.sin(2*phi)*fM2phi;M=M+Math.sin(4*phi)*fM4phi;M=M-Math.sin(6*phi)*fM6phi;M=M*a;Asq=A*A;x=k0*N*A*(1+Asq*((1-T+C)/6+Asq*(5-T*(18+T)+72*C-58*e0sq)/120));x=x+500000;y=k0*(M-M0+N*tanphi*(Asq*(0.5+Asq*((5-T+9*C+4*C*C)/24+Asq*(61-T*(58+T)+600*C-330*e0sq)/720))));if(y<0){if(hemisphere=="N"){x3dom.debug.logError('UTM zone in northern hemisphere but coordinates in southern!');}
y=10000000+y;}
current.x=eastingFirst?x:y;current.y=eastingFirst?y:x;current.z=coordsGD[i].z;coordsUTM.push(current);}
return coordsUTM;},GDtoGC:function(geoSystem,coords){var output=new x3dom.fields.MFVec3f();var elipsoide=this.getElipsoide(geoSystem);var A=elipsoide[1];var eccentricity=elipsoide[2];var longitudeFirst=this.isLogitudeFirst(geoSystem);var A2=A*A;var F=1.0/eccentricity;var C=A*(1.0-F);var C2=C*C;var C2oA2=C2/A2;var Eps2=F*(2.0-F);var radiansPerDegree=0.0174532925199432957692;var i,current,source_lat,source_lon,slat,slat2,clat,Rn,RnPh;for(i=0;i<coords.length;++i)
{current=new x3dom.fields.SFVec3f();source_lat=radiansPerDegree*(longitudeFirst==true?coords[i].y:coords[i].x);source_lon=radiansPerDegree*(longitudeFirst==true?coords[i].x:coords[i].y);slat=Math.sin(source_lat);slat2=slat*slat;clat=Math.cos(source_lat);Rn=A/Math.sqrt(1.0-Eps2*slat2);RnPh=Rn+coords[i].z;current.x=RnPh*clat*Math.cos(source_lon);current.y=RnPh*clat*Math.sin(source_lon);current.z=(C2oA2*Rn+coords[i].z)*slat;output.push(current);}
return output;},GCtoGD:function(geoSystem,coords){var output=new x3dom.fields.MFVec3f();var rad2deg=180/Math.PI;var ellipsoide=this.getElipsoide(geoSystem);var a=ellipsoide[1];var f=1/ellipsoide[2];var b=a*(1-f);var esq=(1-(b/a)*(b/a));var eps=esq/(1-esq);var i,current,x,y,z,p,q,lat,nu,elev,lon;for(i=0;i<coords.length;++i){x=coords[i].x;y=coords[i].y;z=coords[i].z;p=Math.sqrt(x*x+y*y);q=Math.atan((z*a)/(p*b));lat=Math.atan((z+eps*b*Math.pow(Math.sin(q),3))/(p-esq*a*Math.pow(Math.cos(q),3)));nu=a/Math.sqrt(1-esq*Math.pow(Math.sin(lat),2));elev=p/Math.cos(lat)-nu;lon=Math.atan2(y,x);current=new x3dom.fields.SFVec3f();current.x=lon*rad2deg;current.y=lat*rad2deg;current.z=elev;output.push(current);}
return output;},GEOtoGC:function(geoSystem,geoOrigin,coords){var referenceFrame=this.getReferenceFrame(geoSystem);if(referenceFrame=='GD')
return this.GDtoGC(geoSystem,coords);else if(referenceFrame=='UTM')
return this.UTMtoGC(geoSystem,coords);else if(referenceFrame=='GC')
{if(geoOrigin.node)
{var copy=new x3dom.fields.MFVec3f();for(var i=0;i<coords.length;++i)
{var current=new x3dom.fields.SFVec3f();current.x=coords[i].x;current.y=coords[i].y;current.z=coords[i].z;copy.push(current);}
return copy;}
else
return coords;}
else{x3dom.debug.logError('Unknown geoSystem: '+geoSystem[0]);return new x3dom.fields.MFVec3f();}},GCtoGEO:function(geoSystem,geoOrigin,coords){var referenceFrame=this.getReferenceFrame(geoSystem);if(referenceFrame=='GD'){var coordsGD=this.GCtoGD(geoSystem,coords);if(!this.isLogitudeFirst(geoSystem)){var currentx;for(var i=0;i<coordsGD.length;++i){currentx=coordsGD[i].x;coordsGD[i].x=coordsGD[i].y;coordsGD[i].y=currentx;}}
return coordsGD;}
else if(referenceFrame=='UTM')
return this.GCtoUTM(geoSystem,coords);else if(referenceFrame=='GC')
{if(geoOrigin.node)
{var copy=new x3dom.fields.MFVec3f();for(var i=0;i<coords.length;++i)
{var current=new x3dom.fields.SFVec3f();current.x=coords[i].x;current.y=coords[i].y;current.z=coords[i].z;copy.push(current);}
return copy;}
else
return coords;}
else{x3dom.debug.logError('Unknown geoSystem: '+geoSystem[0]);return new x3dom.fields.MFVec3f();}},OriginToGC:function(geoOrigin)
{var geoCoords=geoOrigin.node._vf.geoCoords;var geoSystem=geoOrigin.node._vf.geoSystem;var point=new x3dom.fields.SFVec3f();point.x=geoCoords.x;point.y=geoCoords.y;point.z=geoCoords.z;var temp=new x3dom.fields.MFVec3f();temp.push(point);var origin=this.GEOtoGC(geoSystem,geoOrigin,temp);return origin[0];},GCtoX3D:function(geoSystem,geoOrigin,coords)
{var gc=coords;if(geoOrigin.node)
{var origin=this.OriginToGC(geoOrigin);var matrix=x3dom.fields.SFMatrix4f.translation(origin.negate());if(geoOrigin.node._vf.rotateYUp)
{var rotmat=x3dom.nodeTypes.GeoLocation.prototype.getGeoRotMat(geoSystem,origin).inverse();matrix=rotmat.mult(matrix);}
for(var i=0;i<coords.length;++i)
gc[i]=matrix.multMatrixPnt(coords[i]);}
return gc;},GEOtoX3D:function(geoSystem,geoOrigin,coords)
{var gc=this.GEOtoGC(geoSystem,geoOrigin,coords);return this.GCtoX3D(geoSystem,geoOrigin,gc);},getPoints:function()
{return this.GEOtoX3D(this._vf.geoSystem,this._cf.geoOrigin,this._vf.point);}}));x3dom.registerNodeType("GeoElevationGrid","Geospatial",defineClass(x3dom.nodeTypes.X3DGeometryNode,function(ctx){x3dom.nodeTypes.GeoElevationGrid.superClass.call(this,ctx);this.addField_SFNode('texCoord',x3dom.nodeTypes.X3DTextureCoordinateNode);this.addField_MFString(ctx,'geoSystem',['GD','WE']);this.addField_SFVec3f(ctx,'geoGridOrigin',0,0,0);this.addField_MFDouble(ctx,'height',0,0);this.addField_SFBool(ctx,'ccw',true);this.addField_SFDouble(ctx,'creaseAngle',0);this.addField_SFInt32(ctx,'xDimension',0);this.addField_SFDouble(ctx,'xSpacing',1.0);this.addField_SFFloat(ctx,'yScale',1);this.addField_SFInt32(ctx,'zDimension',0);this.addField_SFDouble(ctx,'zSpacing',1.0);this.addField_SFNode('geoOrigin',x3dom.nodeTypes.GeoOrigin);this.addField_SFBool(ctx,'lit',true);},{nodeChanged:function()
{var geoSystem=this._vf.geoSystem;var geoOrigin=this._cf.geoOrigin;var height=this._vf.height;var yScale=this._vf.yScale;var xDimension=this._vf.xDimension;var zDimension=this._vf.zDimension;var xSpacing=this._vf.xSpacing;var zSpacing=this._vf.zSpacing;var geoGridOrigin=this._vf.geoGridOrigin;if(height.length!==(xDimension*zDimension))
x3dom.debug.logError('GeoElevationGrid: height.length('+height.length+') != x/zDimension('+xDimension+'*'+zDimension+')');var longitude_first=x3dom.nodeTypes.GeoCoordinate.prototype.isLogitudeFirst(geoSystem);var easting_first=x3dom.nodeTypes.GeoCoordinate.prototype.isUTMEastingFirst(geoSystem);var ccw=this._vf.ccw;var delta_x=1/(xDimension-1);var delta_z=1/(zDimension-1);var numTexComponents=2;var texCoordNode=this._cf.texCoord.node;var texPoints;if(x3dom.isa(texCoordNode,x3dom.nodeTypes.MultiTextureCoordinate)){if(texCoordNode._cf.texCoord.nodes.length)
texCoordNode=texCoordNode._cf.texCoord.nodes[0];}
if(texCoordNode){if(texCoordNode._vf.point){texPoints=texCoordNode._vf.point;if(x3dom.isa(texCoordNode,x3dom.nodeTypes.TextureCoordinate3D)){numTexComponents=3;}}}
var positions=new x3dom.fields.MFVec3f();var texCoords=new x3dom.fields.MFVec2f();for(var z=0;z<zDimension;++z)
for(var x=0;x<xDimension;++x)
{var tex_coord=new x3dom.fields.SFVec2f();tex_coord.x=x*delta_x;tex_coord.y=z*delta_z;texCoords.push(tex_coord);var coord=new x3dom.fields.SFVec3f();if(longitude_first||easting_first)
{coord.x=x*xSpacing;coord.y=z*zSpacing;}
else
{coord.x=z*zSpacing;coord.y=x*xSpacing;}
coord.z=height[(z*xDimension)+x]*yScale;coord=coord.add(geoGridOrigin);positions.push(coord);}
var indices=new x3dom.fields.MFInt32();for(var z=0;z<(zDimension-1);z++)
{for(var x=0;x<(xDimension-1);x++)
{var p0=x+(z*xDimension);var p1=x+(z*xDimension)+1;var p2=x+((z+1)*xDimension)+1;var p3=x+((z+1)*xDimension);if(ccw)
{indices.push(p0);indices.push(p1);indices.push(p2);indices.push(p0);indices.push(p2);indices.push(p3);}
else
{indices.push(p0);indices.push(p3);indices.push(p2);indices.push(p0);indices.push(p2);indices.push(p1);}}}
var transformed=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoX3D(geoSystem,geoOrigin,positions);if(this._vf.creaseAngle<=x3dom.fields.Eps){var that=this;(function(){var indicesFlat=new x3dom.fields.MFInt32(),positionsFlat=new x3dom.fields.MFVec3f(),texCoordsFlat=new x3dom.fields.MFVec2f();if(texPoints){that.generateNonIndexedTriangleData(indices,transformed,null,texPoints,null,positionsFlat,null,texCoordsFlat,null);}
else{that.generateNonIndexedTriangleData(indices,transformed,null,texCoords,null,positionsFlat,null,texCoordsFlat,null);}
for(var i=0;i<positionsFlat.length;++i){indicesFlat.push(i);}
that._mesh._indices[0]=indicesFlat.toGL();that._mesh._positions[0]=positionsFlat.toGL();that._mesh._texCoords[0]=texCoordsFlat.toGL();that._mesh._numTexComponents=2;})();this._mesh.calcNormals(0);}
else{this._mesh._indices[0]=indices.toGL();this._mesh._positions[0]=transformed.toGL();if(texPoints){this._mesh._texCoords[0]=texPoints.toGL();}
else{this._mesh._texCoords[0]=texCoords.toGL();}
this._mesh._numTexComponents=numTexComponents;this._mesh.calcNormals(Math.PI);}
this._mesh._invalidate=true;this._mesh._numFaces=this._mesh._indices[0].length/3;this._mesh._numCoords=this._mesh._positions[0].length/3;},generateNonIndexedTriangleData:function(indices,positions,normals,texCoords,colors,newPositions,newNormals,newTexCoords,newColors)
{for(var i=0;i<indices.length;i+=3){var i0=indices[i],i1=indices[i+1],i2=indices[i+2];if(positions){var p0=new x3dom.fields.SFVec3f(),p1=new x3dom.fields.SFVec3f(),p2=new x3dom.fields.SFVec3f();p0.setValues(positions[i0]);p1.setValues(positions[i1]);p2.setValues(positions[i2]);newPositions.push(p0);newPositions.push(p1);newPositions.push(p2);}
if(normals){var n0=new x3dom.fields.SFVec3f(),n1=new x3dom.fields.SFVec3f(),n2=new x3dom.fields.SFVec3f();n0.setValues(normals[i0]);n1.setValues(normals[i1]);n2.setValues(normals[i2]);newNormals.push(n0);newNormals.push(n1);newNormals.push(n2);}
if(texCoords){var t0=new x3dom.fields.SFVec2f(),t1=new x3dom.fields.SFVec2f(),t2=new x3dom.fields.SFVec2f();t0.setValues(texCoords[i0]);t1.setValues(texCoords[i1]);t2.setValues(texCoords[i2]);newTexCoords.push(t0);newTexCoords.push(t1);newTexCoords.push(t2);}
if(colors){var c0=new x3dom.fields.SFVec3f(),c1=new x3dom.fields.SFVec3f(),c2=new x3dom.fields.SFVec3f();c0.setValues(texCoords[i0]);c1.setValues(texCoords[i1]);c1.setValues(texCoords[i2]);newColors.push(c0);newColors.push(c1);newColors.push(c2);}}}}));x3dom.registerNodeType("GeoLOD","Geospatial",defineClass(x3dom.nodeTypes.X3DBoundedObject,function(ctx){x3dom.nodeTypes.GeoLOD.superClass.call(this,ctx);this.addField_MFString(ctx,'geoSystem',['GD','WE']);this.addField_MFString(ctx,'rootUrl',[]);this.addField_MFString(ctx,'child1Url',[]);this.addField_MFString(ctx,'child2Url',[]);this.addField_MFString(ctx,'child3Url',[]);this.addField_MFString(ctx,'child4Url',[]);this.addField_SFFloat(ctx,'range',10);this.addField_SFInt32(ctx,"level_changed",0);this.addField_SFString(ctx,'referenceBindableDescription',[]);this.addField_SFNode('geoOrigin',x3dom.nodeTypes.GeoOrigin);this.addField_MFNode('rootNode',x3dom.nodeTypes.X3DChildNode);this.addField_SFVec3f(ctx,"center",0,0,0);this._eye=new x3dom.fields.SFVec3f(0,0,0);this._x3dcenter=new x3dom.fields.SFVec3f(0,0,0);this._child1added=false;this._child2added=false;this._child3added=false;this._child4added=false;this._rootNodeLoaded=true;this._childUrlNodes=new x3dom.fields.MFNode(x3dom.nodeTypes.X3DChildNode);this._lastRangePos=-1;},{collectDrawableObjects:function(transform,drawableCollection,singlePath,invalidateCache,planeMask,clipPlanes)
{if(singlePath&&(this._parentNodes.length>1))
singlePath=false;if(singlePath&&(invalidateCache=invalidateCache||this.cacheInvalid()))
this.invalidateCache();planeMask=drawableCollection.cull(transform,this.graphState(),singlePath,planeMask);if(planeMask<=0){return;}
singlePath=false;this.visitChildren(transform,drawableCollection,singlePath,invalidateCache,planeMask,clipPlanes);},visitChildren:function(transform,drawableCollection,singlePath,invalidateCache,planeMask,clipPlanes)
{var i=0,n=0,cnodes,cnode;var mat_view=drawableCollection.viewMatrix;var center=new x3dom.fields.SFVec3f(0,0,0);center=mat_view.inverse().multMatrixPnt(center);this._eye=transform.inverse().multMatrixPnt(center);var len=this._x3dcenter.subtract(this._eye).length();if(len>this._vf.range){i=0;if(!this._rootNodeLoaded){this._rootNodeLoaded=true;}
cnodes=this._cf.rootNode.nodes;}
else{i=1;if(!this._child1added){this._child1added=true;this.addInlineChild(this._vf.child1Url);}
if(!this._child2added){this._child2added=true;this.addInlineChild(this._vf.child2Url);}
if(!this._child3added){this._child3added=true;this.addInlineChild(this._vf.child3Url);}
if(!this._child4added){this._child4added=true;this.addInlineChild(this._vf.child4Url);}
if(this._rootNodeLoaded){this._rootNodeLoaded=false;}
cnodes=this._childUrlNodes.nodes;}
if(i!==this._lastRangePos){this.postMessage('level_changed',i);}
this._lastRangePos=i;n=cnodes.length;if(n&&cnodes)
{var childTransform=this.transformMatrix(transform);for(i=0;i<n;i++){if((cnode=cnodes[i])){cnode.collectDrawableObjects(childTransform,drawableCollection,singlePath,invalidateCache,planeMask,clipPlanes);}}}},addInlineChild:function(url)
{var inline=this.newInlineNode(url);this._childUrlNodes.addLink(inline);},newInlineNode:function(url)
{var inline=new x3dom.nodeTypes.Inline();inline._vf.url=url;inline._nameSpace=this._nameSpace;x3dom.debug.logInfo("add url: "+url);inline.nodeChanged();return inline;},getVolume:function()
{var vol=this._graph.volume;if(!this.volumeValid()&&this._vf.render)
{var child,childVol;for(var i=0,n=this._childNodes.length;i<n;i++)
{if(!(child=this._childNodes[i])||child._vf.render!==true)
continue;childVol=child.getVolume();if(childVol&&childVol.isValid())
vol.extendBounds(childVol.min,childVol.max);}}
return vol;},nodeChanged:function(){var coords=new x3dom.fields.MFVec3f();coords.push(this._vf.center);this._x3dcenter=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoX3D(this._vf.geoSystem,this._cf.geoOrigin,coords)[0];if(!this._cf.rootNode.nodes.length){var inline=this.newInlineNode(this._vf.rootUrl);this._cf.rootNode.addLink(inline);}
this.invalidateVolume();},fieldChanged:function(fieldName){if(fieldName=="render"||fieldName=="range"){this.invalidateVolume();}
if(fieldname=="center"){var coords=new x3dom.fields.MFVec3f();coords.push(this._vf.center);this._x3dcenter=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoX3D(this._vf.geoSystem,this._cf.geoOrigin,coords)[0];this.invalidateVolume();}}}));x3dom.registerNodeType("GeoLocation","Geospatial",defineClass(x3dom.nodeTypes.X3DTransformNode,function(ctx){x3dom.nodeTypes.GeoLocation.superClass.call(this,ctx);this.addField_MFString(ctx,'geoSystem',['GD','WE']);this.addField_SFVec3d(ctx,'geoCoords',0,0,0);this.addField_SFNode('geoOrigin',x3dom.nodeTypes.GeoOrigin);},{nodeChanged:function()
{var position=this._vf.geoCoords;var geoSystem=this._vf.geoSystem;var geoOrigin=this._cf.geoOrigin;this._trafo=this.getGeoTransRotMat(geoSystem,geoOrigin,position);},getGeoRotMat:function(geoSystem,positionGC)
{var coords=new x3dom.fields.MFVec3f();coords.push(positionGC);var positionGD=x3dom.nodeTypes.GeoCoordinate.prototype.GCtoGD(geoSystem,coords)[0];var Xaxis=new x3dom.fields.SFVec3f(1,0,0);var rotlat=180-positionGD.y;var deg2rad=Math.PI/180;var rotUpQuat=x3dom.fields.Quaternion.axisAngle(Xaxis,rotlat*deg2rad);var Zaxis=new x3dom.fields.SFVec3f(0,0,1);var rotlon=90+positionGD.x;var rotZQuat=x3dom.fields.Quaternion.axisAngle(Zaxis,rotlon*deg2rad);return rotZQuat.multiply(rotUpQuat).toMatrix();},getGeoTransRotMat:function(geoSystem,geoOrigin,position)
{var coords=new x3dom.fields.MFVec3f();coords.push(position);var transformed=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoGC(geoSystem,geoOrigin,coords)[0];var rotMat=this.getGeoRotMat(geoSystem,transformed);if(geoOrigin.node)
{var origin=x3dom.nodeTypes.GeoCoordinate.prototype.OriginToGC(geoOrigin);if(geoOrigin.node._vf.rotateYUp)
{var rotMatOrigin=this.getGeoRotMat(geoSystem,origin);return rotMatOrigin.inverse().mult(x3dom.fields.SFMatrix4f.translation(transformed.subtract(origin)).mult(rotMat));}
return x3dom.fields.SFMatrix4f.translation(transformed.subtract(origin)).mult(rotMat);}
else
{return x3dom.fields.SFMatrix4f.translation(transformed).mult(rotMat);}},fieldChanged:function(fieldName)
{if(fieldName=="geoSystem"||fieldName=="geoCoords"||fieldName=="geoOrigin")
{var position=this._vf.geoCoords;var geoSystem=this._vf.geoSystem;var geoOrigin=this._cf.geoOrigin;this._trafo=this.getGeoTransRotMat(geoSystem,geoOrigin,position);this.invalidateVolume();}
else if(fieldName=="render"){this.invalidateVolume();}}}));x3dom.registerNodeType("GeoMetadata","Geospatial",defineClass(x3dom.nodeTypes.X3DInfoNode,function(ctx){x3dom.nodeTypes.GeoMetadata.superClass.call(this,ctx);this.addField_MFString(ctx,'url',[]);this.addField_MFNode('data',x3dom.nodeTypes.X3DInfoNode);this.addField_MFString(ctx,'summary',[]);}));x3dom.registerNodeType("GeoOrigin","Geospatial",defineClass(x3dom.nodeTypes.X3DNode,function(ctx){x3dom.nodeTypes.GeoOrigin.superClass.call(this,ctx);this.addField_MFString(ctx,'geoSystem',['GD','WE']);this.addField_SFVec3d(ctx,'geoCoords',0,0,0);this.addField_SFBool(ctx,'rotateYUp',false);}));x3dom.registerNodeType("GeoPositionInterpolator","Geospatial",defineClass(x3dom.nodeTypes.X3DInterpolatorNode,function(ctx){x3dom.nodeTypes.GeoPositionInterpolator.superClass.call(this,ctx);this.addField_MFString(ctx,'geoSystem',['GD','WE']);this.addField_MFVec3f(ctx,'keyValue',[]);this.addField_SFNode('geoOrigin',x3dom.nodeTypes.GeoOrigin);this.addField_SFBool(ctx,'onGreatCircle',false);},{linearInterpHintKeyValue:function(time,keyHint,key,keyValue,interp){var keylength=key.length;if(time<=key[0])
return[0,keyValue[0]];else if(time>=key[keylength-1])
return[keylength-1,keyValue[keylength-1]];var keyIndexStart=keyHint;var i;for(var ii=0;ii<keylength-1;++ii){i=(keyIndexStart+ii)%keylength;if((key[i]<time)&&(time<=key[i+1]))
return[i,interp(keyValue[i],keyValue[i+1],(time-key[i])/(key[i+1]-key[i]))];i=(keyIndexStart-ii+keylength)%keylength;if((key[i]<time)&&(time<=key[i+1]))
return[i,interp(keyValue[i],keyValue[i+1],(time-key[i])/(key[i+1]-key[i]))];}
return[0,keyValue[0]];},slerp:function(a,b,t){var cosom=a.dot(b)/(a.length()*b.length());var rot1;{rot1=new x3dom.fields.SFVec3f(b.x,b.y,b.z);}
var scalerot0,scalerot1;if((1.0-cosom)>0.00001)
{var omega=Math.acos(cosom);var sinom=Math.sin(omega);scalerot0=Math.sin((1.0-t)*omega)/sinom;scalerot1=Math.sin(t*omega)/sinom;}
else
{scalerot0=1.0-t;scalerot1=t;}
return a.multiply(scalerot0).add(rot1.multiply(scalerot1));},nodeChanged:function(){this._keyValueGC=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoGC(this._vf.geoSystem,this._cf.geoOrigin,this._vf.keyValue);this._keyHint=0;},fieldChanged:function(fieldName)
{if(fieldName==="set_fraction"){var value,indexValue,valueGC,valueX3D,coords;if(this._vf.onGreatCircle){indexValue=this.linearInterpHintKeyValue(this._vf.set_fraction,this._keyHint,this._vf.key,this._keyValueGC,x3dom.nodeTypes.GeoPositionInterpolator.prototype.slerp);this._keyHint=indexValue[0];valueGC=indexValue[1];coords=new x3dom.fields.MFVec3f();coords.push(valueGC);value=x3dom.nodeTypes.GeoCoordinate.prototype.GCtoGEO(this._vf.geoSystem,this._cf.geoOrigin,coords)[0];}
else{indexValue=this.linearInterpHintKeyValue(this._vf.set_fraction,this._keyHint,this._vf.key,this._vf.keyValue,function(a,b,t){return a.multiply(1.0-t).add(b.multiply(t));});this._keyHint=indexValue[0];value=indexValue[1];coords=new x3dom.fields.MFVec3f();coords.push(value);valueGC=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoGC(this._vf.geoSystem,this._cf.geoOrigin,coords)[0];}
coords=new x3dom.fields.MFVec3f();coords.push(valueGC);var GCgeoSystem=new x3dom.fields.MFString();GCgeoSystem.push("GC");GCgeoSystem.push(x3dom.nodeTypes.GeoCoordinate.prototype.getElipsoideCode(this._vf.geoSystem));valueX3D=x3dom.nodeTypes.GeoCoordinate.prototype.GCtoX3D(GCgeoSystem,this._cf.geoOrigin,coords)[0];this.postMessage('value_changed',valueX3D);this.postMessage('geovalue_changed',value);}}}));x3dom.registerNodeType("GeoTransform","Geospatial",defineClass(x3dom.nodeTypes.X3DTransformNode,function(ctx){x3dom.nodeTypes.GeoTransform.superClass.call(this,ctx);this.addField_SFVec3d(ctx,'geoCenter',0,0,0);this.addField_SFRotation(ctx,'rotation',0,0,1,0);this.addField_SFVec3f(ctx,'scale',1,1,1);this.addField_SFRotation(ctx,'scaleOrientation',0,0,1,0);this.addField_SFVec3f(ctx,'translation',0,0,0);this.addField_SFNode('geoOrigin',x3dom.nodeTypes.GeoOrigin);this.addField_MFString(ctx,'geoSystem',['GD','WE']);this.addField_SFBool(ctx,'globalGeoOrigin',false);},{nodeChanged:function()
{this._trafo=this.getGeoTransform();},getGeoTransform:function()
{var geoCenterRotMat,geoCenter,skipGO,scaleOrientMat,geoTransform,coords,geoCenterGC,geoSystem,geoOrigin;geoSystem=this._vf.geoSystem;geoOrigin=this._cf.geoOrigin;geoCenter=this._vf.geoCenter;skipGO=this._vf.globalGeoOrigin;scaleOrientMat=this._vf.scaleOrientation.toMatrix();coords=new x3dom.fields.MFVec3f();coords.push(geoCenter);geoCenterGC=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoGC(geoSystem,geoOrigin,coords)[0];geoCenterRotMat=x3dom.nodeTypes.GeoLocation.prototype.getGeoRotMat(geoSystem,geoCenterGC);geoTransform=x3dom.fields.SFMatrix4f.translation(geoCenterGC).mult(geoCenterRotMat).mult(x3dom.fields.SFMatrix4f.translation(this._vf.translation)).mult(this._vf.rotation.toMatrix()).mult(scaleOrientMat).mult(x3dom.fields.SFMatrix4f.scale(this._vf.scale)).mult(scaleOrientMat.inverse()).mult(geoCenterRotMat.inverse()).mult(x3dom.fields.SFMatrix4f.translation(geoCenterGC.negate()));if(geoOrigin.node)
{var originGC=x3dom.nodeTypes.GeoCoordinate.prototype.OriginToGC(geoOrigin);if(!skipGO)
{geoTransform=geoTransform.mult(x3dom.fields.SFMatrix4f.translation(originGC));}
if(geoOrigin.node._vf.rotateYUp)
{var rotMatOrigin=x3dom.nodeTypes.GeoLocation.prototype.getGeoRotMat(geoSystem,originGC);if(!skipGO)
{geoTransform=geoTransform.mult(rotMatOrigin);}}
geoTransform=x3dom.fields.SFMatrix4f.translation(originGC.negate()).mult(geoTransform);if(geoOrigin.node._vf.rotateYUp)
{geoTransform=rotMatOrigin.inverse().mult(geoTransform);}}
return geoTransform;},fieldChanged:function(fieldName)
{if(fieldName=="geoCenter"||fieldName=="translation"||fieldName=="rotation"||fieldName=="scale"||fieldName=="scaleOrientation")
{this._trafo=this.getGeoTransform();this.invalidateVolume();}
else if(fieldName=="render"){this.invalidateVolume();}}}));x3dom.registerNodeType("GeoViewpoint","Geospatial",defineClass(x3dom.nodeTypes.X3DViewpointNode,function(ctx){x3dom.nodeTypes.GeoViewpoint.superClass.call(this,ctx);this.addField_MFString(ctx,'geoSystem',['GD','WE']);this.addField_SFFloat(ctx,'fieldOfView',0.785398);this.addField_SFRotation(ctx,'orientation',0,0,1,0);this.addField_SFVec3f(ctx,'centerOfRotation',0,0,0);this.addField_SFVec3d(ctx,'position',0,0,100000);this.addField_SFBool(ctx,'headlight',undefined);this.addField_MFString(ctx,'navType',undefined);this.addField_SFFloat(ctx,'speedFactor',1.0);this.addField_SFFloat(ctx,'zNear',-1);this.addField_SFFloat(ctx,'zFar',-1);this.addField_SFBool(ctx,'elevationScaling',true);this.addField_SFNode('geoOrigin',x3dom.nodeTypes.GeoOrigin);this._geoCenterOfRotation=this._vf.centerOfRotation;this._viewMatrix=x3dom.fields.SFMatrix4f.identity();},{activate:function(prev){var viewarea=this._nameSpace.doc._viewarea;if(prev){viewarea.animateTo(this,prev._autoGen?null:prev);}
viewarea._needNavigationMatrixUpdate=true;x3dom.nodeTypes.X3DBindableNode.prototype.activate.call(this,prev);var navi=viewarea._scene.getNavigationInfo();this._initSpeed=navi._vf.speed;this._examineSpeed=navi._vf.speed;this._lastSpeed=navi._vf.speed;this._userSpeedFactor=1.0;this._lastNavType=navi.getType();x3dom.debug.logInfo("initial navigation speed: "+this._initSpeed);if(this._vf.headlight!==undefined){navi._vf.headlight=this._vf.headlight;}
if(this._vf.navType!==undefined){navi._vf.navType=this._vf.navType;}},deactivate:function(prev){var viewarea=this._nameSpace.doc._viewarea;var navi=viewarea._scene.getNavigationInfo();navi._vf.speed=this._examineSpeed;x3dom.debug.logInfo("navigation speed restored to: "+this._examineSpeed);x3dom.nodeTypes.X3DBindableNode.prototype.deactivate.call(this,prev);},nodeChanged:function(){this._stack=this._nameSpace.doc._bindableBag.addBindable(this);this._geoOrigin=this._cf.geoOrigin;this._geoSystem=this._vf.geoSystem;this._position=this._vf.position;this._orientation=this._vf.orientation;this._viewMatrix=this.getInitViewMatrix(this._orientation,this._geoSystem,this._geoOrigin,this._position);this._vf.centerOfRotation=this.getGeoCenterOfRotation(this._geoSystem,this._geoOrigin,this._geoCenterOfRotation);this._projMatrix=null;this._lastAspect=1.0;this._zRatio=10000;this._zNear=this._vf.zNear;this._zFar=this._vf.zFar;this._imgPlaneHeightAtDistOne=2.0*Math.tan(this._vf.fieldOfView/2.0);},fieldChanged:function(fieldName){if(fieldName=="position"||fieldName=="orientation"){this.resetView();}
else if(fieldName=="fieldOfView"||fieldName=="zNear"||fieldName=="zFar"){this._projMatrix=null;this._zNear=this._vf.zNear;this._zFar=this._vf.zFar;this._imgPlaneHeightAtDistOne=2.0*Math.tan(this._vf.fieldOfView/2.0);}
else if(fieldName.indexOf("bind")>=0){this.bind(this._vf.bind);}},setProjectionMatrix:function(matrix)
{this._projMatrix=matrix;},getCenterOfRotation:function(){return this.getCurrentTransform().multMatrixPnt(this._vf.centerOfRotation);},getGeoCenterOfRotation:function(geoSystem,geoOrigin,geoCenterOfRotation){var coords=new x3dom.fields.MFVec3f();coords.push(geoCenterOfRotation);var transformed=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoX3D(geoSystem,geoOrigin,coords);return transformed[0];},isExamineMode:function(navType){return(navType=='examine'||navType=='turntable'||navType=='lookaround'||navType=='lookat');},getViewMatrix:function(){if(this._vf.isActive&&this._vf.elevationScaling){var viewarea=this._nameSpace.doc._viewarea;var navi=viewarea._scene.getNavigationInfo();var navType=navi.getType();if(this.isExamineMode(navType)){if(!this.isExamineMode(this._lastNavType)){navi._vf.speed=this._examineSpeed;}
this._lastNavType=navType;}
else{if(this.isExamineMode(this._lastNavType)){this._examineSpeed=navi._vf.speed;x3dom.debug.logInfo("back from examine mode, resume speed: "+this._lastSpeed);navi._vf.speed=this._lastSpeed;}
this._lastNavType=navType;if(navi._vf.speed!=this._lastSpeed){this._userSpeedFactor*=navi._vf.speed/this._lastSpeed;x3dom.debug.logInfo("interactive speed factor changed: "+this._userSpeedFactor);}
var viewtrafo=viewarea._scene.getViewpoint().getCurrentTransform();viewtrafo=viewtrafo.inverse().mult(this._viewMatrix);var position=viewtrafo.inverse().e3();var geoOrigin=this._geoOrigin;var geoSystem=this._geoSystem;var positionGC=position;if(geoOrigin.node){var origin=x3dom.nodeTypes.GeoCoordinate.prototype.OriginToGC(geoOrigin);if(geoOrigin.node._vf.rotateYUp){var rotmat=x3dom.nodeTypes.GeoLocation.prototype.getGeoRotMat(geoSystem,origin);positionGC=rotmat.multMatrixPnt(position);}
positionGC=positionGC.add(origin);}
var coords=new x3dom.fields.MFVec3f();coords.push(positionGC);var positionGD=x3dom.nodeTypes.GeoCoordinate.prototype.GCtoGD(geoSystem,coords)[0];var elevationSpeed=Math.abs(positionGD.z/10);elevationSpeed=elevationSpeed>1?elevationSpeed:1;navi._vf.speed=elevationSpeed*this._vf.speedFactor*this._userSpeedFactor;this._lastSpeed=navi._vf.speed;}}
return this._viewMatrix;},getInitViewMatrix:function(orientation,geoSystem,geoOrigin,position){var coords=new x3dom.fields.MFVec3f();coords.push(position);var positionGC=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoGC(geoSystem,geoOrigin,coords)[0];var orientMatrix=orientation.toMatrix();var rotMat=x3dom.nodeTypes.GeoLocation.prototype.getGeoRotMat(geoSystem,positionGC);var rotOrient=rotMat.mult(orientMatrix);if(geoOrigin.node){if(geoOrigin.node._vf.rotateYUp){var origin=x3dom.nodeTypes.GeoCoordinate.prototype.OriginToGC(geoOrigin);var rotMatOrigin=x3dom.nodeTypes.GeoLocation.prototype.getGeoRotMat(geoSystem,origin);rotOrient=rotMatOrigin.inverse().mult(rotOrient);}}
var positionX3D=x3dom.nodeTypes.GeoCoordinate.prototype.GEOtoX3D(geoSystem,geoOrigin,coords)[0];return x3dom.fields.SFMatrix4f.translation(positionX3D).mult(rotOrient).inverse();},getFieldOfView:function(){return this._vf.fieldOfView;},resetView:function(){this._viewMatrix=this.getInitViewMatrix(this._vf.orientation,this._vf.geoSystem,this._cf.geoOrigin,this._vf.position);this._vf.centerOfRotation=this.getGeoCenterOfRotation(this._vf.geoSystem,this._cf.geoOrigin,this._geoCenterOfRotation);if(this._nameSpace.doc._viewarea){this._nameSpace.doc._viewarea.resetNavHelpers();}},getNear:function(){return this._zNear;},getFar:function(){return this._zFar;},getImgPlaneHeightAtDistOne:function(){return this._imgPlaneHeightAtDistOne;},getProjectionMatrix:function(aspect)
{var fovy=this._vf.fieldOfView;var zfar=this._vf.zFar;var znear=this._vf.zNear;if(znear<=0||zfar<=0)
{var nearScale=0.8,farScale=1.2;var viewarea=this._nameSpace.doc._viewarea;var scene=viewarea._scene;var min=x3dom.fields.SFVec3f.copy(scene._lastMin);var max=x3dom.fields.SFVec3f.copy(scene._lastMax);var dia=max.subtract(min);var sRad=dia.length()/2;var mat=viewarea.getViewMatrix().inverse();var vp=mat.e3();var translation=new x3dom.fields.SFVec3f(0,0,0),scaleFactor=new x3dom.fields.SFVec3f(1,1,1);var rotation=new x3dom.fields.Quaternion(0,0,1,0),scaleOrientation=new x3dom.fields.Quaternion(0,0,1,0);mat.getTransform(translation,rotation,scaleFactor,scaleOrientation);var minScal=scaleFactor.x,maxScal=scaleFactor.x;if(maxScal<scaleFactor.y)maxScal=scaleFactor.y;if(minScal>scaleFactor.y)minScal=scaleFactor.y;if(maxScal<scaleFactor.z)maxScal=scaleFactor.z;if(minScal>scaleFactor.z)minScal=scaleFactor.z;if(maxScal>1)
nearScale/=maxScal;else if(minScal>x3dom.fields.Eps&&minScal<1)
farScale/=minScal;var sCenter=min.add(dia.multiply(0.5));var vDist=(vp.subtract(sCenter)).length();if(sRad){if(vDist>sRad)
znear=(vDist-sRad)*nearScale;else
znear=0;zfar=(vDist+sRad)*farScale;}
else{znear=0.1;zfar=100000;}
var zNearLimit=zfar/this._zRatio;znear=Math.max(znear,Math.max(x3dom.fields.Eps,zNearLimit));if(zfar>this._vf.zNear&&this._vf.zNear>0)
znear=this._vf.zNear;if(this._vf.zFar>znear)
zfar=this._vf.zFar;if(zfar<=znear)
zfar=znear+1;}
if(this._projMatrix==null)
{this._projMatrix=x3dom.fields.SFMatrix4f.perspective(fovy,aspect,znear,zfar);}
else if(this._zNear!=znear||this._zFar!=zfar)
{var div=znear-zfar;this._projMatrix._22=(znear+zfar)/div;this._projMatrix._23=2*znear*zfar/div;}
else if(this._lastAspect!=aspect)
{this._projMatrix._00=(1/Math.tan(fovy/2))/aspect;this._lastAspect=aspect;}
this._zNear=znear;this._zFar=zfar;return this._projMatrix;}}));