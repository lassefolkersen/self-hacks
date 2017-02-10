rm(list=ls())
maxComplexity<-1
test<-F

dev.off()

source("3Dstrings/2011-02-21_plotting_functions.R")
DNA_structure<-loadPDFfile("3Dstrings/pdb425d.ent")



###########################
#slide "0" just want a little closer DNA look first
###########################


#define camera angle
slideNo <-"00"
steps<-80
# xLeft<-seq(-28,-30,length.out=steps)
# xRight<-seq(-18,0,length.out=steps)
# yBottom<-seq(32,20,length.out=steps)
# yTop<-seq(42,50,length.out=steps)


xLeft<-seq(-28,-40,length.out=steps)
xRight<-seq(-18,-10,length.out=steps)
yBottom<-seq(32,10,length.out=steps)
yTop<-seq(42,40,length.out=steps)




rotation<-seq(-2*pi,0,length.out=steps)
textFade <- c(rep(0,floor(steps*0.7)),seq(0,1,length.out=ceiling(steps*0.3)))

#loop through camera views
for(i in 1:steps){
  if(nchar(i)==1){
    filename <- paste0(slideNo,"_0",i,"_slide.png")
  }else{
    filename <- paste0(slideNo,"_",i,"_slide.png")
  }
  png(filename,width=500,height=500)
  
  #plot the right camera view
  plot.default(x=NULL,y=NULL,xlim=c(xLeft[i],xRight[i]),ylim=c(yBottom[i],yTop[i]),frame.plot=FALSE,axes=FALSE,xlab=" ",ylab=" ")
  
  #plot DNA string
  from<-c(197.839520,   1.049469, 376.255001)
  from<-c(214.59189, -10.76897, 400.00000)
  to<-c(50, -330, 400)
  rotationPoint<-c(124, -260, 100)
  DNA_structure_here<-rotateStructure(DNA_structure, rotationAngle=rotation[i],rotationAxis=c(1,0,0))
  
  
  outPoint2<-drawDnaString(from,to,rotationPoint=rotationPoint,maxComplexity=maxComplexity,test=test,structure=DNA_structure_here,sequence="ACTGACAAA")
  
  #add the letters
  x1<- -30
  x2<- -16
  y1<- 20
  y2<- 48
  bases<-39
  sequence <- strsplit("TGACAAAAC","")[[1]]
  sequence<-rep(sequence,length.out=bases)
  
  
  for(j in 1:bases){
    xHere<-x1 + (x2-x1) * (j / bases)
    yHere<-y1 + (y2-y1) * (j / bases)
    
    col_1<-rgb(r=textFade[i],g=textFade[i],b=textFade[i])
    text(x=xHere,y=yHere,label=sequence[j],col=col_1)
  }
    
  
  dev.off()
  print(i)
}




###########################
#slide 1 -> 2
###########################

#define camera angle
slideNo <-"01"
steps<-80
xLeft<-seq(-40,-100,length.out=steps)
xRight<-seq(-10,30,length.out=steps)
yBottom<-seq(10,-50,length.out=steps)
yTop<-seq(40,90,length.out=steps)


dev.off()
point0X <- seq(600,600,length.out=steps)
point0Y <- seq(-20,-20,length.out=steps)
point0Z <- seq(400,400,length.out=steps)
point1X <- seq(200,200,length.out=steps)
point1Y <- seq(-20,-20,length.out=steps)
Rpoint1X<- seq(400,400,length.out=steps)
Rpoint1Y<- seq(-320,-320,length.out=steps)


point2X <- seq(50,50,length.out=steps)
point2Y <- seq(-330,-330,length.out=steps)
Rpoint2X<- seq(124,124,length.out=steps)
Rpoint2Y<- seq(-260,-260,length.out=steps)


point3X <- seq(-72,-52,length.out=steps)
point3Y <- seq(-524,-324,length.out=steps)
Rpoint3X<- seq(-900,0,length.out=steps)
Rpoint3Y<- seq(-519,-319,length.out=steps)


point4X <- seq(-714,-214,length.out=steps)
point4Y <- seq(-1810,-10,length.out=steps)
Rpoint4X<- seq(-424,-124,length.out=steps)
Rpoint4Y<- seq(-760,-260,length.out=steps)


#define bell curve
x<- seq(-10,10,by=0.1)
y<-dnorm(x)
bellCurve<-data.frame(
  x=x*10,
  y=-y*100,
  z=as.numeric(rep(0,length(x))),
  col="grey90"
)
#loop through camera views
for(i in 1:steps){
  if(nchar(i)==1){
    filename <- paste0(slideNo,"_0",i,"_slide.png")
  }else{
    filename <- paste0(slideNo,"_",i,"_slide.png")
  }
  png(filename,width=500,height=500)
  
  
  plot.default(x=NULL,y=NULL,xlim=c(xLeft[i],xRight[i]),ylim=c(yBottom[i],yTop[i]),frame.plot=FALSE,axes=FALSE,xlab=" ",ylab=" ")
    
    #template for bellCurve
    # plotStructure(bellCurve, maxComplexity=maxComplexity,colorType="predefined")  
    
  
    #drawing the DNA bell-curve    
    # from<-c( 600, -20, 400)
    # to<-c(200, -20, 400)
    # rotationPoint<-c(400, -320, 400)
    from <- c(point0X[i], point0Y[i], point0Z[i])
    to <- c(point1X[i], point1Y[i], 400)
    rotationPoint<-c(Rpoint1X[i], Rpoint1Y[i], 400)
    outPoint1<-drawDnaString(from,to,rotationPoint=rotationPoint,maxComplexity=maxComplexity,structure=DNA_structure,sequence="ACTGACAAA",test=T)

    from<-outPoint1[["endPoint"]] 
    # to<-c(50, -330, 400)
    # rotationPoint<-c(124, -260, 100)
    to <- c(point2X[i], point2Y[i], 400)
    rotationPoint<-c(Rpoint2X[i], Rpoint2Y[i], 100)
    outPoint2<-drawDnaString(from,to,rotationPoint=rotationPoint,maxComplexity=maxComplexity,structure=DNA_structure,sequence="ACTGACAAA",test=test)
    
    
    #this is the summit
    from<-outPoint2[["endPoint"]] 
    # to<-c(-52, -324, 400)
    # rotationPoint<-c(0, -319, 400)
    to <- c(point3X[i], point3Y[i], 400)
    rotationPoint<-c(Rpoint3X[i], Rpoint3Y[i], 400)
    outPoint3<-drawDnaString(from,to,rotationPoint=rotationPoint,maxComplexity=maxComplexity,structure=DNA_structure,sequence="ACTGACAAA",test=test)
  
    from<-outPoint3[["endPoint"]] 
    # to<-c(-214, -10, 400)
    # rotationPoint<-c(-124, -260, 100)
    to <- c(point4X[i], point4Y[i], 400)
    rotationPoint<-c(Rpoint4X[i], Rpoint4Y[i], 100)
    outPoint4<-drawDnaString(from,to,rotationPoint=rotationPoint,maxComplexity=maxComplexity,structure=DNA_structure,sequence="ACTGACAAA",test=test)

  
  # Sys.sleep(1)
  print(i)
  dev.off()
}


###########################
#slide 2 -> 3 -> 4
###########################

#define camera angle
slideNo <-"02"
steps<-140 
xLeft<-seq(-100,0,length.out=steps)
xRight<-seq(30,130,length.out=steps)
yBottom<-seq(-50,-50,length.out=steps)
yTop<-seq(90,90,length.out=steps)


healthyCol<-rgb(r=seq(229,0,length.out=steps),g=seq(229,0,length.out=steps),b=seq(229,255,length.out=steps),maxColorValue=255)
sickCol<-rgb(r=seq(255,255,length.out=steps),g=seq(255,0,length.out=steps),b=seq(255,0,length.out=steps),alpha=seq(255,0,length.out=steps),maxColorValue=255)
sickComplexity <- c(rep(1,floor(steps/3)),round(seq(1,10,length.out=ceiling(steps/3))), rep(10,length.out=ceiling(steps/3)))

#define bell curve 1 and 2
set.seed(3)
l<-200
curve1<-data.frame(p=sort(rnorm(l,mean=0,sd=1)),z=rep(0,l),col="grey90")
for(t in 1:nrow(curve1)){
  curve1[t,"y"]<- -runif(1,min=0,max=dnorm(curve1[t,"p"])*100)
}
curve1[,"x"]<-curve1[,"p"]*10
curve1[,"p"]<-NULL
#add a few more outliers for point about long tail
extra<-data.frame(z=rep(0,12),col=rep("grey90",12),y=rep(0,12),x=c(-35,-29,-25,25,30,35,44,55,60,66,70,90))
curve1<-rbind(curve1,extra)


set.seed(1)
l<-200
curve2<-data.frame(p=sort(rnorm(l,mean=0,sd=1)),z=rep(0,l),col="grey90")
for(t in 1:nrow(curve2)){
  curve2[t,"y"]<- -runif(1,min=0,max=dnorm(curve2[t,"p"])*100)
}
curve2[,"x"]<-curve2[,"p"]*10
curve2[,"p"]<-NULL
#add a few more outliers for point about long tail
extra<-data.frame(z=rep(0,12),col=rep("grey90",12),y=rep(0,12),x=c(-66,-51,-42,-32,-31,-26,28,31,32,43,54,61))
curve2<-rbind(curve2,extra)
curve2[,"x"]<-curve2[,"x"]-10


#loop through camera views
for(i in 1:steps){
  print(i)
  if(nchar(i)==1){
    filename <- paste0(slideNo,"_0",i,"_slide.png")
  }else{
    filename <- paste0(slideNo,"_",i,"_slide.png")
  }
  png(filename,width=500,height=500)
  
  
  plot.default(x=NULL,y=NULL,xlim=c(xLeft[i],xRight[i]),ylim=c(yBottom[i],yTop[i]),frame.plot=FALSE,axes=FALSE,xlab=" ",ylab=" ")
  
  
  curve2[,"col"]<-sickCol[i]
  plotStructure(curve2, maxComplexity=sickComplexity[i],colorType="predefined")  
  curve1[,"col"]<-healthyCol[i]
  plotStructure(curve1, maxComplexity=maxComplexity,colorType="predefined")  
  
  if(i /steps > 0.3){
    legend("topright",legend=c("healthy","sick"),pch=19,col=c("blue","red"),cex=2,bty="n")
    
    
  }
  
  dev.off()
}




###########################
#slide 4 -> 5
###########################

#define camera angle
slideNo <-"04"
steps<-40
xLeft<-seq(0,30,length.out=steps)
xRight<-seq(130,130,length.out=steps)
yBottom<-seq(-50,-35,length.out=steps)
yTop<-seq(90,75,length.out=steps)

curve1[,"col"]<- healthyCol[length(healthyCol)]
curve2[,"col"] <- sickCol[length(healthyCol)]

imputedColour<-"orange"
imputedColourScale<-rgb(r=seq(255,col2rgb(imputedColour)[1],length.out=steps),g=seq(0,col2rgb(imputedColour)[2],length.out=steps),b=seq(0,col2rgb(imputedColour)[3],length.out=steps),maxColorValue=255)

seqColour<-"darkred"
seqColourScale<-rgb(r=seq(255,col2rgb(seqColour)[1],length.out=steps),g=seq(0,col2rgb(seqColour)[2],length.out=steps),b=seq(0,col2rgb(seqColour)[3],length.out=steps),maxColorValue=255)



#loop through camera views
for(i in 1:steps){
  print(i)
  if(nchar(i)==1){
    filename <- paste0(slideNo,"_0",i,"_slide.png")
  }else{
    filename <- paste0(slideNo,"_",i,"_slide.png")
  }
  png(filename,width=500,height=500)
  
  
  plot.default(x=NULL,y=NULL,xlim=c(xLeft[i],xRight[i]),ylim=c(yBottom[i],yTop[i]),frame.plot=FALSE,axes=FALSE,xlab=" ",ylab=" ")
  
  
  
  curve2[curve2[,"x"]< -24 ,"col"] <- imputedColourScale[i]
  curve2[curve2[,"x"]< -50 ,"col"] <- seqColourScale[i]
  
  plotStructure(curve2, maxComplexity=maxComplexity,colorType="predefined")  
  
  
  plotStructure(curve1, maxComplexity=maxComplexity,colorType="predefined")  
  
  if( 0.8 < i/ steps){
    legend("bottomright",legend=c("microarray","imputed microarray","DNA sequencing"),pch=19,col=c("red",imputedColour,seqColour),cex=1.5,bty="n")
    
  }
  
  dev.off()
}





###########################
#slide 6->7
###########################




#define camera angle
slideNo <-"06"
steps<-80
xLeft<-seq(30,-10,length.out=steps)
xRight<-seq(130,135,length.out=steps)
yBottom<-seq(-35,-190,length.out=steps)
yTop<-seq(75,-35,length.out=steps)



#define bell curve
x<- seq(-2,2,by=0.1)
y<-dnorm(x)
bellCurve1<-data.frame(
  x=x*10,
  y=-y*100,
  z=as.numeric(rep(0,length(x))),
  col="blue"
)
bellCurve2<-bellCurve1
bellCurve2[,"col"]<-"red"
bellCurve2[,"x"]<-bellCurve2[,"x"]-10


curveAll1_rotated<-curveAll1<-rbind(curve1,curve2,bellCurve1[,colnames(curve1)],bellCurve2[,colnames(curve1)])




#loop through camera views
for(i in 1:steps){
  print(i)
  if(nchar(i)==1){
    filename <- paste0(slideNo,"_0",i,"_slide.png")
  }else{
    filename <- paste0(slideNo,"_",i,"_slide.png")
  }
  png(filename,width=500,height=500)
  
  
  plot.default(x=NULL,y=NULL,xlim=c(xLeft[i],xRight[i]),ylim=c(yBottom[i],yTop[i]),frame.plot=FALSE,axes=FALSE,xlab=" ",ylab=" ")
  
  #rotate in z plane
  curveAll1_rotated[,"z"] <- -curveAll1[,"x"] * (i / steps) * 2
  curveAll1_rotated[,"y"] <-  curveAll1[,"y"]+ (i / steps) * 80
  
  
  #fade colours a little (to highlight main dot)
  allCols<-rgb2hsv(col2rgb(curveAll1[,"col"]))
  allCols["s",]<-allCols["s",]* (0.5*(steps - i)/steps+0.5)
  allCols["s",]<-allCols["s",]* (0.8*(steps - i)/steps+0.2)
  # allCols["v",]<-allCols["v",]* (0.5*(steps - i)/steps+0.5)
  curveAll1_rotated[,"col"]<-hsv(allCols[1,],allCols[2,],allCols[3,])
  
  plotStructure(curveAll1_rotated, maxComplexity=maxComplexity,colorType="predefined")  
  

  if(i == steps){
    curveAll2_rotated <- curveAll1_rotated
    
    curveAll2_rotated[,"x"] <- curveAll2_rotated[,"x"] - 70
  plotStructure(curveAll2_rotated, maxComplexity=maxComplexity,colorType="predefined")  
  
  }
  
  dev.off()
}


















###########################
#slide 7
###########################




#define camera angle
slideNo <-"07"
steps<-80
xLeft<-seq(-10,-10,length.out=steps)
xRight<-seq(135,135,length.out=steps)
yBottom<-seq(-190,-190,length.out=steps)
yTop<-seq(-35,35,length.out=steps)


#loop through camera views
for(i in 1:steps){
  print(i)
  if(nchar(i)==1){
    filename <- paste0(slideNo,"_0",i,"_slide.png")
  }else{
    filename <- paste0(slideNo,"_",i,"_slide.png")
  }
  png(filename,width=500,height=500)
  
  
  plot.default(x=NULL,y=NULL,xlim=c(xLeft[i],xRight[i]),ylim=c(yBottom[i],yTop[i]),frame.plot=FALSE,axes=FALSE,xlab=" ",ylab=" ")
  
  
  #pick a top-10% in curveAll1
  # p1<-which(order(curveAll1_rotated[,"z"]) == round(0.3*nrow(curveAll1_rotated)))
  # curveAll1_rotated[p1,"col"]<-"black"
  
  #pick a top-30% in curveAll2
  # curveAll2_rotated<-curveAll2_rotated[order(curveAll2_rotated[,"z"]),]
  # curveAll2_rotated[round(0.3*nrow(curveAll2_rotated)),"col"]<-"black"
  
  
  plotStructure(curveAll1_rotated, maxComplexity=maxComplexity,colorType="predefined")  
  plotStructure(curveAll2_rotated, maxComplexity=maxComplexity,colorType="predefined")  

  draw3DBall(128,-73,3.2,complexity=10,col="blue")
  draw3DBall(20,-60,3.2,complexity=10,col="red")
  
  x<-5
  lines(x=c(20+x,35+x),y=c(-20,-20),lwd=2)
  lines(x=c(20+x,35+x),y=c(40,40),lwd=2)
  lines(x=c(20+x,20+x),y=c(-20,40),lwd=2)
  lines(x=c(35+x,35+x),y=c(40,-20),lwd=2)
  lines(x=c(27.5+x-2,27.5+2+x),y=c(30,30),lwd=3,col="red")
  lines(x=c(27.5+x,27.5+x),y=c(35,25),lwd=3,col="red")
  
  
  x<-50
  lines(x=c(20+x,35+x),y=c(-20,-20),lwd=2)
  lines(x=c(20+x,35+x),y=c(40,40),lwd=2)
  lines(x=c(20+x,20+x),y=c(-20,40),lwd=2)
  lines(x=c(35+x,35+x),y=c(40,-20),lwd=2)
  lines(x=c(27.5+x-2,27.5+2+x),y=c(30,30),lwd=3,col="red")
  lines(x=c(27.5+x,27.5+x),y=c(35,25),lwd=3,col="red")
  
  
  dev.off()
}

