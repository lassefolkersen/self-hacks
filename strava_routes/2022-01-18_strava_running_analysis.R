

#Download the strava bulk export file, unzip it, and define the base folder. Then 
#start this script to parse all the info inside and save as a single long-table format R-object
library(XML)
basefolder<-"2022-01-18_strava_running/export_11355130/"
d1<-read.csv(paste0(basefolder,"/activities.csv"))
head(d1)
d4<-data.frame(times=vector(),lats=vector(),lons=vector(),id==vector(),name==vector(),type==vector())
for(i in 1:nrow(d1)){
  id<-d1[i,"Activity.ID"]  
  print(id)
  gpx_path<-paste0(basefolder,"activities/",id,".gpx")
  if(!file.exists(gpx_path))next
  d2<-xmlToList(gpx_path)
  name<-d2[["trk"]][["name"]]
  type<-d2[["trk"]][["type"]]
  trkseg<-d2[["trk"]][["trkseg"]]
  
  times<-sapply(trkseg,function(x){{x[["time"]]}})
  lats<-sapply(trkseg,function(x){{x[[".attrs"]][["lat"]]}})
  lons<-sapply(trkseg,function(x){{x[[".attrs"]][["lon"]]}})
  d3<-data.frame(times,lats,lons,id=rep(id,length(times)),name=rep(name,length(times)),type=rep(type,length(times)))
  
  d4<-rbind(d3,d4)
  
}
save(basefolder,d4,file=paste0(basefolder,"/2022-01-18_import.rdata"))




#clear, load and start over
rm(list=ls())
load("2022-01-18_strava_running/export_11355130/2022-01-18_import.rdata")


#only take runs (no cycling or skating or such)
d4<-d4[d4[,"type"]%in%9,]

#subset to more time-sparse version (basically remove all that are not in at a 
#set time-resoulation, here 20 seconds)
seconds_per_step<-20
for(id in unique(d4[,"id"])){
 w<-which(d4[,"id"]%in%id )
 seconds<-difftime(strptime(d4[w,"times"],"%Y-%m-%dT%H:%M:%SZ"), strptime(d4[w[1],"times"],"%Y-%m-%dT%H:%M:%SZ"),units="secs")
 d4[w,"omit"]<-duplicated(round(seconds / seconds_per_step))
 d4[w,"seconds"]<-seconds
}
d4<-d4[!d4[,"omit"],]


#center the origin of the map, so all run starts the same place
for(id in unique(d4[,"id"])){
  w<-which(d4[,"id"]%in%id )
  d4[w,"lat_zeroed"]<-as.numeric(d4[w,"lats"])-as.numeric(d4[w[1],"lats"])
  d4[w,"lon_zeroed"]<-as.numeric(d4[w,"lons"])-as.numeric(d4[w[1],"lons"])
}



#insert step count - to keep pace in animation
for(id in unique(d4[,"id"])){
  w<-which(d4[,"id"]%in%id )
  d4[w,"step"]<- 1:length(w)
}


#Set plotting limit-parameter
# ((300*seconds_per_step)/60)/60 # that's 1.6 hours!
max_step <- 300
d4<-d4[d4[,"step"]<max_step,]
xlim <- range(d4[,"lon_zeroed"])*0.3
ylim <- range(d4[,"lat_zeroed"])*0.3


#omit a few runs where the gps clearly was broken (can use a text function in
#below plot loop, to find which)
omits<-c(1644682610,395480405,415995387,414122564)
d4<-d4[!d4[,"id"]%in%omits,]


#main plot loop (one png per step)
for(j in 1:max_step){
  #set parameters and blank plotting window
  print(j)
  png(file=paste0(basefolder,j,".png"))
  par(bg = 'darkblue',omi=c(0,0,0,0),mai=c(0,0,0,0))
  plot(NULL,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  
  #plot yellow main dots at step j
  w1<-which(d4[,"step"]%in%j )
  points(
    x=d4[w1,"lat_zeroed"],
    y=d4[w1,"lon_zeroed"],
    pch=19,
    col=rgb(255, 255, 0, 90, maxColorValue=256)
  )

  
  #set trails
  w2<-which(d4[,"step"]<=j)
  points(
    x=d4[w2,"lat_zeroed"],
    y=d4[w2,"lon_zeroed"],
    pch=19,
    cex=0.2,
    col=  rgb(255, 255, 167, 30, maxColorValue=256)
  )

  
  #legend
  one_lat_in_km = 111.321 * cos((56/180)*pi) #put 56 because most runs where at lon 56 - but this is an assumption, do double check
  text(x=xlim[1],y=ylim[1],label=paste0("Minutes: ",round(j*seconds_per_step/60),", KM:"),col="white",adj=0)
  x1=xlim[1] + 0.015
  y1=ylim[1]
  lines(y=c(y1,y1),x=c(x1, x1+ 1/one_lat_in_km),col="white")
  lines(y=c(y1,y1),x=c(x1, x1+ 1/one_lat_in_km),col="white")
  
  #take a break
  Sys.sleep(0.1)
  
  #end plot
  dev.off()
}



#then import in gif-maker og windows movie make or something