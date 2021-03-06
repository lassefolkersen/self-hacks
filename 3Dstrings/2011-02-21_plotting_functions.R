#
# from "2011-02-21 plotting functions.R"
#
# author: Lasse Folkersen
#
##############################





drawDnaString<-function(from,to,rotationPoint=NULL,maxComplexity=10,verbose=TRUE,structure=loadPDFfile("2011-02-11 3DR/pdb425d.ent"),test=FALSE,startAtom="1",endAtom="227",sequence="ACCGGTACCGGT"){
	#from			a 3D coordinate indicating the starting point
	#to				a 3D coordinate indicating the ending point
	#rotationPoint	NULL for straight lines and a 3D coordinate which is the center of a cirle around which the line will be bent 
	#angle			a numerical between 0 and 2 * pi indicating the angle of curving of the line
	#maxComplexity	a numerical
	if(class(from)!="numeric")stop("Must give 'from' as numeric vector")
	if(class(startAtom)!="character")stop("Must give 'startAtom' as character")
	if(class(endAtom)!="character")stop("Must give 'endAtom' as character")
	if(length(startAtom)!=1)stop("Must give 'startAtom' as a length 1 vector")
	if(length(endAtom)!=1)stop("Must give 'endAtom' as a length 1 vector")
	if(length(from)!=3)stop("Must give 'from' as a length 3 vector")
	if(class(to)!="numeric")stop("Must give 'to' as numeric vector")
	if(length(to)!=3)stop("Must give 'to' as a length 3 vector")
	if(class(maxComplexity)!="numeric")stop("Must give maxComplexity as numeric")
	if(!maxComplexity%in%1:10)stop("Must give maxComplexity as a number between 1 and 10")
	if(class(sequence)!="character")stop("Must give 'sequence' as character vector")
	if(length(sequence)<1)stop("Must give 'sequence' as a vector of length 1 or more")
	
	
	
	#setting colours
	
	
	if(!is.null(rotationPoint)){
		if(class(rotationPoint)!="numeric")stop("Must give 'rotationPoint' as NULL or numeric vector")
		if(length(rotationPoint)!=3)stop("Must give 'rotationPoint' as NULL or a length 3 vector")
		distFrom<-sqrt((from[1]-rotationPoint[1])^2 + (from[2]-rotationPoint[2])^2 + (from[3]-rotationPoint[3])^2)
		distTo<-sqrt((to[1]-rotationPoint[1])^2 + (to[2]-rotationPoint[2])^2 + (to[3]-rotationPoint[3])^2)
		#the rotationPoint, if given, must be at the same distance from 'to' and 'from', so we correct it if it is not
		#this is done by pushing the point in a directin parallel to the to-from line untill the distance to 'to' and 'from' is equal
		if(distFrom != distTo){
			#first we rotate the three points so they are in the XY plane
			s<-data.frame(
					row.names=c("to","from","rotationPoint"),
					x=c(to[1],from[1],rotationPoint[1]),
					y=c(to[2],from[2],rotationPoint[2]),
					z=c(to[3],from[3],rotationPoint[3])
			)
			s<-transposeStructure(s, -from)			
			perpendicularToPlane<-angleAndCrossProduct(to-from,to-rotationPoint)
			rotation<-angleAndCrossProduct(c(0,0,1),perpendicularToPlane[[2]])
			s<-rotateStructure(s, rotation[[1]],rotation[[2]], rotationPoint=c(0,0,0))
			
			
			#then we rotate so toFromLine is the x-axis
			angle<- angleAndCrossProduct(c(1,0,0),as.numeric(s["to",]))[[1]] * sign(s["to","y"])
			if(angle - acos(s["to","x"] / sqrt(s["to","x"]^2 +s["to","y"]^2)) * sign(s["to","y"]))stop("angle2 and angle was !=")
			s2<-rotateStructure(s, -angle, c(0,0,1), rotationPoint=c(0,0,0))
			if(abs(s2["to","y"])>0.0001)stop(paste("s2['to','y'] was", s2["to","y"],"and it should be 0"))
			s2<-rbind(s2,newRotationPoint=c(s2["to","x"]/2,s2["rotationPoint","y"],0))
			
			#and then we rotate back
			s<-rotateStructure(s2, angle, c(0,0,1), rotationPoint=c(0,0,0))
			s<-rotateStructure(s, -rotation[[1]],rotation[[2]], rotationPoint=c(0,0,0))
			s<-transposeStructure(s, from)			
			
			#and get the new rotationPoint
			rotationPoint<-as.numeric(s["newRotationPoint",])
			distFrom<-sqrt(sum((from-rotationPoint)^2))
			distTo<-sqrt(sum((to-rotationPoint)^2))
#			print(paste("distFrom",round(distFrom),"distTo",round(distTo)))
			if((distFrom-distTo)/distFrom > 0.001)stop("Even after correction, the distance from rotationPoint to 'to' and to 'from' is not equal")
			if(verbose)print(paste("rotationPoint corrected to ",paste(round(rotationPoint),collapse=", "),"because there must be equal distance from the to 'to' and to 'from'"))
		}
		circleRadius<-distFrom
	}
	
	dimensions<-c("x","y","z")
	if(class(structure)!="data.frame")stop(paste("structure must be of class data.frame, not",class(structure)))
	for(dimension in dimensions){
		if(!dimension%in%colnames(structure))stop("structure must be a data.frame with the columns x, y, and z")
		if(class(structure[,dimension])!="numeric")stop(paste("The contents of the structure columns x, y, and z must of class numeric, not",class(structure[,dimension])))
	}
	structure<-transposeStructure(structure,-as.numeric(structure[startAtom,c("x","y","z")]))
	DNA_length<-sqrt(sum((structure[endAtom,c("x","y","z")] - structure[startAtom,c("x","y","z")])^2))
	DNA_direction<-as.numeric((structure[endAtom,c("x","y","z")] - structure[startAtom,c("x","y","z")])/DNA_length)
	
	if(verbose)print(paste("Plotting af DNA string of length",round(DNA_length),"from",paste(round(from),collapse=","),"to",paste(round(to),collapse=",")))
	
	
	if(!is.null(rotationPoint)){
		
		#make a structure which is a part-circle in the XY plane with radius circleRadius, and center c(0,0,0),
		topAngle<-angleAndCrossProduct(to-rotationPoint,from-rotationPoint)[[1]]
		curveDistance<-circleRadius *topAngle
		stepsInPartCircle<-curveDistance%/%DNA_length
		rest<-curveDistance%%DNA_length
		radiansToCompleteAPartCircle<- topAngle * (curveDistance - rest) / curveDistance
		template<-data.frame(
				row.names=c("maxDistanceFromToFromLine",seq_len(stepsInPartCircle+1)),
				radians=c(topAngle/2,seq(0,radiansToCompleteAPartCircle, length.out=stepsInPartCircle+1)),
				radius=circleRadius
		)
		template[,"x"]<-cos(template[,"radians"]) * template[,"radius"]
		template[,"y"]<-sin(template[,"radians"]) * template[,"radius"]
		template[,"z"]<-rep(0,nrow(template))
		
		
		#transpose to rotationPoint
		template<-transposeStructure(template,rotationPoint)
		
		#rotate around z-axis to align the start point in a way that the circle would be correct if the toFromLine was equal to the X-axis
		rotation1<-list((3*pi)/2 - topAngle/2,c(0,0,1))
		template<-rotateStructure(template,rotation1[[1]],rotation1[[2]],rotationPoint)
		
		#rotate around the axis which is perpendicular to the from-to line and the X axis
		#debug
		rotation2<-angleAndCrossProduct(to-from,c(1,0,0))
		template<-rotateStructure(template,rotation2[[1]],rotation2[[2]],rotationPoint)
		
		
		#rotate around the the line perpendicular to lines between rotation point and maxDistanceFromToFromLine or halfOfTheToFromLine
		halfOfTheToFromLine<-(to-from)/2 + from
		maxDistanceFromToFromLine<-as.numeric(template["maxDistanceFromToFromLine",dimensions])
		rotation3<-angleAndCrossProduct(halfOfTheToFromLine - rotationPoint,maxDistanceFromToFromLine-rotationPoint)
		template<-rotateStructure(template,rotation3[[1]],rotation3[[2]],rotationPoint)
		
#		print(paste("DEBUGGING: First rotated",signif(rotation1[[1]],3),"around",paste(signif(rotation1[[2]],3),collapse=","),"Second rotated",signif(rotation2[[1]],3),"around",paste(signif(rotation2[[2]],3),collapse=","),"Third rotated",signif(rotation3[[1]],3),"around",paste(signif(rotation3[[2]],3),collapse=",")))
		template<-template[!rownames(template)%in%"maxDistanceFromToFromLine",]
	}else{ # for straight lines
		stop("Straight strings (NULL rotation) have not been implemented at the momemt. Just use a very high distance rotation point")
		distance<-sqrt((from[1]-to[1])^2 + (from[2]-to[2])^2 + (from[3]-to[3])^2)
		steps<-distance%/%DNA_length
		if(distance%%DNA_length!=0){
			extra_length<-DNA_length - distance%%DNA_length
			extra_vector <- extra_length * (to-from) / distance
			to<-to + extra_vector
			if(verbose)print(paste("DNA not divisble in distance - 'to' has been extended to",paste(signif(to,3),collapse=", ")))
			distance<-sqrt((from[1]-to[1])^2 + (from[2]-to[2])^2 + (from[3]-to[3])^2)
		}
		template<-data.frame(
				x=seq(from[1],to[1],length.out=steps),
				y=seq(from[2],to[2],length.out=steps),
				z=seq(from[3],to[3],length.out=steps)
		)
	}
	#getting the sequence
	if(nrow(template) * 12 > nchar(sequence)){
		recyclings<-(nrow(template) * 12) /nchar(sequence) + 1
		sequence<-paste(rep(sequence,recyclings),collapse="")		
		if(verbose)print(paste("sequence given was too short, so it was recycled",recyclings-1,"times"))	
	}
	
	
	#plottting
	if(test){
		plotStructure(template)
		if(!is.null(rotationPoint)){
			p<-projectPoint(rotationPoint)
			draw3DBall(xcenter=p[1],ycenter=p[2],size=p[3],col="red",complexity=10)
		}
	}else{
		for(i in 1:(nrow(template)-1)){
			sequenceHere <- substr(sequence,(i-1)*12+1, i*12)
			startHere <- as.numeric(template[i,dimensions])
			stringDir<-(as.numeric(template[i+1,dimensions]) - startHere)
			rotation<-angleAndCrossProduct(DNA_direction,stringDir) #-changed: c(1,0,0) because c(1,0,0) is the direction of the structure
			structureInstance<-structure	
			transposedStr<-transposeStructure(structureInstance,startHere)
			rotatedStr<-rotateStructure(transposedStr,-rotation[[1]],rotation[[2]],rotationPoint=startHere)
			plotStructure(
					structure=rotatedStr,
					maxComplexity=maxComplexity,
					colorType=sequenceHere)
		}
	}
	#returning the endPoint (which could be different from 'to') and the endDirection
	invisible(
			list(
					endPoint=as.numeric(template[nrow(template),dimensions]),
					endDirection=as.numeric(template[nrow(template),dimensions])-as.numeric(template[nrow(template)-1,dimensions])
			)
	
	)
}






loadPDFfile<-function(filePath){
	if(!file.exists(filePath))stop(paste("Couldn't access file at",filePath))
	allLines<-readLines(filePath)
	atoms<-allLines[grep("^ATOM",allLines)]
	data.frame(
			row.names=sub(" +$","",sub("^ +","",as.character(substr(atoms,7,11)))),
			name=sub(" +$","",sub("^ +","",as.character(substr(atoms,13,16)))),
			resName=sub(" +$","",sub("^ +","",as.character(substr(atoms,18,20)))),
			chainID=substr(atoms,22,22),
			resSeq=as.integer(substr(atoms,23,26)),
			x=as.numeric(substr(atoms,31,38)),
			y=as.numeric(substr(atoms,39,46)),
			z=as.numeric(substr(atoms,47,54)),
			occupancy=as.numeric(substr(atoms,55,60)),
			tempFactor=as.numeric(substr(atoms,61,66)),
			element=sub(" +$","",sub("^ +","",as.character(substr(atoms,77,78)))),
			charge=sub(" +$","",sub("^ +","",as.character(substr(atoms,79,80)))),
			stringsAsFactors=FALSE
	)
}




angleAndCrossProduct<-function(v1,v2){
	rotationAngle<-acos(sum(v1*v2)/(sqrt(v1[1]^2 + v1[2]^2 + v1[3]^2)*sqrt(v2[1]^2 + v2[2]^2 + v2[3]^2)))
	rotationAxis<-c(
			v2[2] * v1[3] - v2[3] * v1[2], 
			v2[3] * v1[1] - v2[1] * v1[3],
			v2[1] * v1[2] - v2[2] * v1[1]
	)
	return(list(rotationAngle,rotationAxis))
	
}




transposeStructure<-function(structure,transposition){
	#moves a structure in the desired direction
	#structure		data.frame with the columns x, y and z
	#transposition	length 3 vector with the transposition vector
	dimensions<-c("x","y","z")
	if(class(structure)!="data.frame")stop(paste("structure must be of class data.frame, not",class(structure)))
	for(dimension in dimensions){
		if(!dimension%in%colnames(structure))stop("structure must be a data.frame with the columns x, y, and z")
		if(class(structure[,dimension])!="numeric")stop(paste("The contents of the structure columns x, y, and z must of class numeric, not",class(structure[,dimension])))
	}
	if(class(transposition)!="numeric")stop(paste("transposition must be of class numeric, not",class(transposition)))
	if(length(transposition)!=3)stop(paste("transposition must be of length 3, not",length(transposition)))
	names(transposition)<-dimensions
	
	for(dimension in dimensions){
		if(transposition[dimension] != 0){
			structure[,dimension]<-structure[,dimension] + transposition[dimension]			
		}
	}
	return(structure)
	
}


rotateStructure<-function(structure,rotationAngle,rotationAxis,rotationPoint=NULL){
	#rotates a structure by a given rotation
	#structure		data.frame with the columns x, y and z
	#rotationAngle	the amount of rotation in radians, using right hand rule
	#rotationAxis	the axis of the rotation given as a vector
	#rotationPoint	the point around which rotation takes place. Will default to the median point of the structure
	dimensions<-c("x","y","z")
	if(class(structure)!="data.frame")stop(paste("structure must be of class data.frame, not",class(structure)))
	for(dimension in dimensions){
		if(!dimension%in%colnames(structure))stop("structure must be a data.frame with the columns x, y, and z")
		if(class(structure[,dimension])!="numeric")stop(paste("The contents of the structure columns x, y, and z must of class numeric, not",class(structure[,dimension])))
	}
	if(is.null(rotationPoint)){
		rotationPoint<-c(median(structure[,"x"]), median(structure[,"y"]), median(structure[,"z"]))
	}
	if(class(rotationPoint)!="numeric")stop(paste("rotationPoint must be of class numeric, not",class(rotationPoint)))
	if(length(rotationPoint)!=3)stop(paste("rotationPoint must be of length 3, not",length(rotationPoint)))
	
	
	if(class(rotationAxis)!="numeric")stop(paste("rotationAxis must be of class numeric, not",class(rotationAxis)))
	if(length(rotationAxis)!=3)stop(paste("rotationAxis must be of length 3, not",length(rotationAxis)))
	if(all(rotationAxis==c(0,0,0))){
		#no rotation needed
		return(structure)
	}
	
	rotationAxis<-rotationAxis/sqrt(rotationAxis[1]^2 + rotationAxis[2]^2 + rotationAxis[3]^2)
	
	if(!class(rotationAngle)%in%c("integer","numeric"))stop(paste("rotationAngle must be of class numeric or integer, not",class(rotationAngle)))
	if(length(rotationAngle)!=1)stop(paste("rotationAngle must be of length 1, not",length(rotationAngle)))
#	if(max(rotationAngle) > 4*pi | min(rotationAngle) < -4*pi)print("The rotationAngle given was more or less than 4 pi. Rotation should be given in radians")
	
	
	
	if(!require("onion"))stop("package 'onion' must be installed for the use of quaternions")
	
	structure<-transposeStructure(structure,-rotationPoint)
	Q2<-quaternion(Re = cos(rotationAngle/2), i = rotationAxis[1] * sin(rotationAngle/2), j = rotationAxis[2] * sin(rotationAngle/2), k = rotationAxis[3] * sin(rotationAngle/2))
	structure[,dimensions]<-rotate(structure[,dimensions], Q2)
	structure<-transposeStructure(structure,rotationPoint)
	return(structure)
}














plotStructure<-function(structure,maxComplexity=10, xlim=c(-200,200),ylim=c(-200,200),colorType="standard"){
	#plotting of a structure, which is a data.frame containing x,y,z coordinates and optionally color information
	#the xlim and ylim is optional, but will omit plotting outside of the window
	#maxComplexity		an integer between 1 and 10 which can limit the max complexity of plotting (put to 1 for speed)
	#colorType			a character that can be either a sequence of 12 leters, in which case it will be taken as nucleote codes				
	
	
	if(class(structure)!="data.frame")stop(paste("structure must be of class data.frame, not",class(structure)))
	for(dimension in c("x","y","z")){
		if(!dimension%in%colnames(structure))stop("structure must be a data.frame with the columns x, y, and z")
		if(class(structure[,dimension])!="numeric")stop(paste("The contents of the structure columns x, y, and z must of class numeric, not",class(structure[,dimension])))
	}
	
	#giving color for atoms if not already given in the structure file
	if(class(colorType)!="character")stop(paste("colorType must be of class character, not",class(colorType)))
	if(length(colorType)!=1)stop(paste("colorType must be of length 1, not",length(colorType)))
	if(nchar(colorType)==12){
		if(any(!unique(strsplit(colorType,"")[[1]])%in%c("A","T","G","C")))stop("Detected a 12-character color sequence input that had non-standard nucleotides (i.e. non-ATCG)")
		if(!nrow(structure)%in%c(486,486/2))stop("DNA nucleotide coding only works when the structure given is the 486 row 'pdb425d.ent' DNA-helix entry, or exactly half of it")
		structure[,"col"]<-"grey90" #backbone color
		basecolors<-c("green","red","dodgerblue","yellow")
		names(basecolors) <- c("A","G","T","C")
		firstStrandBaseID<-list(c(9:18),c(30:37),c(49:56),c(68:78),c(90:100),c(112:120),c(132:141),c(153:160),c(172:179),c(191:201),c(213:223),c(235:243))
		secondStrandBaseID<-list(c(252:261),c(273:280),c(292:299),c(311:321),c(333:343),c(355:363),c(375:384),c(396:403),c(415:422),c(434:444),c(456:466),c(478:486))
		for(base in 1:length(firstStrandBaseID)){
			baseType <-substr(colorType,base,base)
			if(baseType == "T")complement <- "A"
			if(baseType == "A")complement <- "T"
			if(baseType == "C")complement <- "G"
			if(baseType == "G")complement <- "C"
			structure[firstStrandBaseID[[base]],"col"] <- basecolors[baseType]
			if(nrow(structure)==486){
				structure[secondStrandBaseID[[base]],"col"] <- basecolors[complement]
			}
		}
	}else{
		if(!colorType%in%c("standard","predefined"))stop("colorType must be either 'standard', 'predefined', or a character vector with 12 nucleotide letters")
		if(colorType=="standard"){
			if("element"%in%colnames(structure)){
				if(class(structure[,"element"])!="character")stop(paste("structure contained a column element which was not of class character, but",class(structure[,"element"])))
				#giving random colors to all elements
				elementColors<-sample(colors(),length(unique(structure[,"element"])))
				names(elementColors)<-unique(structure[,"element"])
				#revert to fixed colors for  known common elements
				elementColors[c("N","O","C","P")]<-c("dodgerblue","white","blue","red")
				structure[,"col"]<-elementColors[structure[,"element"]]
			}else{
				structure[,"col"]<-"blue"
			}	
		}else{
			if(!"col"%in%colnames(structure))stop("when colorType is given as predefined, the structure must have a column named col")
		}
	}
	
	
	
	
	
	#Plotting every atom
	structure[,"x_proj"]<-vector(length=nrow(structure))
	structure[,"y_proj"]<-vector(length=nrow(structure))
	structure[,"apparent_size"]<-vector(length=nrow(structure))
	for(atom in rownames(structure)){
		projection<-projectPoint(
				c(
						structure[atom,"x"],
						structure[atom,"y"],
						structure[atom,"z"]),
				ballSize=2
		)
		if(!is.null(projection)){
			structure[atom,c("x_proj","y_proj","apparent_size")]<-projection
		}else{
			structure[atom,c("x_proj","y_proj","apparent_size")]<-rep(NA,3)
		}
	}
	
	#removing balls outside picture
	structure<-structure[structure[,"x_proj"] > xlim[1]  & structure[,"x_proj"] < xlim[2] & structure[,"y_proj"] > ylim[1]  & structure[,"y_proj"] < ylim[2],]
	structure<-structure[!is.na(structure[,"x_proj"]),]
	
	if(nrow(structure)>0){
#		#setting complexity of plotting based on nearest atom
#		if(max(structure[,"apparent_size"])>2 * ((xlim[2]-xlim[1])  / 200)){
#			complexity<-10
#		}else{
#			if(max(structure[,"apparent_size"])<0.2*((xlim[2]-xlim[1])  / 200)    ){
#				complexity<-1
#			}else{
#				complexity<-2
#			}
#		}
#		complexity<-min(maxComplexity, complexity)
#		print(paste("Complexity:",complexity))
#		complexity<-2
		
		for(atom in rownames(structure)[order(structure[,"z"],decreasing=T)]){
			draw3DBall(
					structure[atom,"x_proj"],
					structure[atom,"y_proj"],
					structure[atom,"apparent_size"],
					complexity=maxComplexity,
					col=structure[atom,"col"])
		}
	}
}






projectPoint<-function(point,camera_point=c(0,0,-50),camera_rotation=c(0,0,0),viewer_point=c(0,0,-100),ballSize=10){
	#point				x,y,z coordinate of point to plot
	#camera_point		x,y,z coordinate of camera
	#camera_rotation	x,y,z rotation coordinates of camera
	#viewer_point		x,y,z coordinate of viewer
	#ballSize			numeric - the actual size in the same unit of the object viewed
	if(class(point)!="numeric")stop("Must give point as numeric vector")
	if(length(point)!=3)stop("Must give point as a length 3 vector")
	if(class(camera_point)!="numeric")stop("Must give camera_point as numeric vector")
	if(length(camera_point)!=3)stop("Must give camera_point as a length 3 vector")
	if(class(camera_rotation)!="numeric")stop("Must give camera_rotation as numeric vector")
	if(length(camera_rotation)!=3)stop("Must give camera_rotation as a length 3 vector")
	if(class(viewer_point)!="numeric")stop("Must give viewer_point as numeric vector")
	if(length(viewer_point)!=3)stop("Must give viewer_point as a length 3 vector")
	a_x<-point[1]
	a_y<-point[2]
	a_z<-point[3]
	c_x<-camera_point[1]
	c_y<-camera_point[2]
	c_z<-camera_point[3]
	theta_x<-camera_rotation[1]
	theta_y<-camera_rotation[2]
	theta_z<-camera_rotation[3]
	e_x<-viewer_point[1]
	e_y<-viewer_point[2]
	e_z<-viewer_point[3]
	d_x <- cos(theta_x)*(sin(theta_z)*(a_y - c_y) + cos(theta_z) * (a_x - c_x)) - sin(theta_y) * (a_z - c_z)
	d_y <- sin(theta_x)*(cos(theta_y)*(a_z - c_z) + sin(theta_y) * (sin(theta_z) * (a_y - c_y) + cos(theta_z) * (a_x - c_x))) + cos(theta_x)*(cos(theta_z)*(a_y - c_y) - sin(theta_z) * (a_x - c_x))
	d_z <- cos(theta_x) * (cos(theta_y) * (a_z - c_z) + sin(theta_y) * (sin(theta_z) * (a_y - c_y) + cos(theta_z) * (a_x - c_x))) - sin(theta_x) * (cos(theta_z) * (a_y - c_y) - sin(theta_z) * (a_x - c_x))
	
	if(d_z < 0){
		return(NULL)
	}else{
		fieldOfView<-2*atan(1 / abs(e_z))
		distance<-sqrt((a_x-c_x)^2 + (a_y-c_y)^2 + (a_z-c_z)^2)
		apparent_size = ( atan(ballSize / distance) / fieldOfView)
		b_x<-(d_x - e_x) * (e_z / d_z)
		b_y<-(d_y-e_y) * (e_z / d_z)
		return(c(b_x,b_y,apparent_size))
	}
}














draw3DBall<-function(xcenter,ycenter,size,col,complexity){
	#Draws a 3D ball at the (xcenter, ycenter) coordinates of a given size and col
	#xcenter and ycenter should be single integers
	#size can either a single numeric or two numerics. The former 
	#			is faster and uses symbol(), but it doesn't play well with non-square plotwindows
	#complexity is an integer between 1 and 10 giving the level of detail. Should be 10 for large 
	#			balls, but 1 is faster and fine for small balls. 
	if(is.null(xcenter) | is.null(ycenter))return(NULL)
	if(complexity == 1){
		cols<-col
	}else{
		if(!complexity%in%c(2:10))stop("Complexity must be an integer between 1 and 10")
		red<-col2rgb(col)[1]
		green<-col2rgb(col)[2]
		blue<-col2rgb(col)[3]
		hue<-rgb2hsv(red,green,blue)[1]
		saturation<-rgb2hsv(red,green,blue)[2]
		value<-rgb2hsv(red,green,blue)[3]
		darkCols<-hsv(hue,saturation,value*seq(0.4,0.95,0.05*(10/complexity)))
		mainCol<-hsv(hue,saturation,value)
		lightCols<-hsv(hue,saturation*rev(seq(0.5,0.95,0.02*(10/complexity))),value)
		cols<-c(darkCols,mainCol,lightCols)
		
	}
	
	sizes<-seq(1,0.2,length.out=length(cols))
	transpositions<-seq(0,0.51,length.out=length(cols))
	
	if(length(size)==1){	
		for(i in 1:length(cols)){
			symbols(
					x=xcenter+transpositions[i]*size, 
					y = ycenter+transpositions[i]*size, 
					circles=size*sizes[i], 
					inches = FALSE, 
					add = TRUE,
					fg = NULL, 
					bg = cols[i]
			)
		}
	}else{
		if(length(size)!=2)stop("The length of the vector size must either be 1 (circle) or 2 (elipsoid)")
		
		drawCircle<-function(xcenter,ycenter,xsize,ysize,col){
			xs<-vector()
			ys<-vector()
			for(i in seq(0,2*pi,0.05)){
				xs<-c(xs,xsize*cos(i)+xcenter)
				ys<-c(ys,ysize*sin(i)+ycenter)
			}
			polygon(xs, ys, density = NULL, angle = 45,border = NULL, col = col, lty = 0)
		}
		for(i in 1:length(cols)){
			drawCircle(
					xcenter+transpositions[i]*size[1],
					ycenter+transpositions[i]*size[2],
					size[1]*sizes[i],
					size[2]*sizes[i],
					cols[i])
		}
	}
}



