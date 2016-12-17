

#2016-11-13 first importing from calendar
rm(list=ls())

setwd("C:/Users/Lasse/Documents/Personal/Diverse/Træning/")
#file<-file("C:/Users/lwf/Desktop/cal/cal.ics","r")
#file<-file("C:/Users/lwf/Desktop/cal/cal3.txt","r")
file<-file("2016-11-13_calender_dump.ics","r")
t<-readLines(file)
close(file)
entries<-grep("^BEGIN",t)
trainings<-data.frame(summary=vector(),date=vector(),stringsAsFactors=FALSE)
for(entry in entries){
  print(entry)
  sumHere<-tHere<-NA
  endNotFound<-TRUE
  i<-0
  while(endNotFound){
    if(i>200)stop("i too high")
    
    i<-i+1	
    if(length(grep("^END",t[i+entry]))!=0){
      endNotFound<-FALSE
    }
  }
  for(j in (entry):(entry+i)){
    if(length(grep("^SUMMARY",t[j]))!=0){
      if(length(grep("^summary\\:training",tolower(t[j])))!=0){
        sumHere<-sub("^SUMMARY\\:","",t[j])
        for(k in (entry):(entry+i)){
          if(length(grep("^DTSTART",t[k]))!=0){
            tHere<-as.Date(sub("^DTSTART;VALUE=DATE\\:","",t[k]),format="%Y%m%d")
            break
          }
        }
        trainings<-rbind(trainings,data.frame(summary=sumHere,date=tHere,stringsAsFactors=FALSE))
      }
    }
  }
}

trainings[,"summary"]<-sub("[cC]ross [cC]ountry","Cross-country",trainings[,"summary"])
trainings[,"summary"]<-sub("[Ll]angrend","Cross-country",trainings[,"summary"])
trainings[,"summary"]<-sub("[sT]tair [mM]aster" ,"Stair-master" ,trainings[,"summary"])
trainings[,"summary"]<-sub("[cC]ross [tT]rainer" ,"Cross-trainer" ,trainings[,"summary"])

#correct known minor errors (forgot 'run' or 'km' or such)
trainings[3,"summary"]<-"Training run 8 km NA"
trainings[4,"summary"]<-"Training run 4 km NA"
trainings[5,"summary"]<-"Training run 3 km NA"
trainings[6,"summary"]<-"Training run 5 km NA"
trainings[7,"summary"]<-"Training run 5 km 18.50"
trainings[8,"summary"]<-"Training Cross-country 13 km 2.00.00"
trainings[9,"summary"]<-"Training run 5 km NA"
trainings[10,"summary"]<-"Training run 5 km 19.32"
trainings[19,"summary"]<-"Training Cross-trainer ~3 km 15 minutes"
trainings[31,"summary"]<-"Training Cross-country 10 km 1.08.00"
trainings[36,"summary"]<-"Training run 10 km 50.03 #premiärmilen"
trainings[43,"summary"]<-"Training run ~7.2 km 35.30 #espergærde rundt"
trainings[71,"summary"]<-"Training run 14.92 km 1.08.00 #avernakø from one end to the other"
trainings[124,"summary"]<-"Training run 6.5 km 31.0 #to work with SJ"
trainings[147,"summary"]<-"Training Cross-trainer ~3 km 15 Minutes"
trainings[148,"summary"]<-"Training Spinning ~17 km 45"
trainings[149,"summary"]<-"Training Cross-trainer ~6 km 30 Minutes"
trainings[251,"summary"]<-"Training run 5 km NA #Threadmill Novo"
trainings[375,"summary"]<-"Training run 5.86 km 26.5"
trainings[500,"summary"]<-"Training run 6.25 km 27.15"
trainings[567,"summary"]<-"Training run 5.41 km 24.37"
trainings[654,"summary"]<-"Training run 7.8 km 35.44"
trainings[340,"summary"]<-"Training run NA km 21 #ca tid - taget fra rådhuse"
trainings[558,"summary"]<-"Training run NA km 32.19"
trainings[363,"summary"]<-"Training run NA km 31.10 #i Frederiksberg have med zombies"
trainings[245,"summary"]<-"Training run 5 km NA #Threadmill Novo"

#categorizing
noApprox<-sub("~","",trainings[,"summary"])
lowerCase<-tolower(noApprox)
noPrefix<-sub("^training ","",lowerCase)
noNumber<-sub(" [0-9].+$","",noPrefix)
noNA<-sub(" na .+$","",noNumber)
noComment <- sub(" #.+$","",noNA)
type<-gsub(" ","",noComment)
table(type)
trainings[,"type"]<-type
unique(type)
# [1] "run" "cross-country" "dj" "cross-trainer" "spinning" "bicycle" "badminton" 
# [8] "ski" "ocr" "tw" 

#get distance
distance<-sub(" km.{0,100}$","",tolower(trainings[,"summary"]))
distance <- sub(" #.+$","",distance)
distance<-sub("^training [a-z -]+ ","",distance)
distance[distance%in%c("training tw","training ski","training dj","training badminton","training spinning")]<-NA
approx<-rep("No",length(distance))
approx[grep("~",distance)]<-"Yes"
distance<-sub("~","",distance)
distance<-sub(":",".",distance)
distance<-as.numeric(distance)
trainings[,"distance"]<-distance
trainings[,"approximate distance"]<-approx

#get time
time<-sub("^.+km {0,1}","",sub(" #.{0,100}$","",tolower(trainings[,"summary"])))
time[time%in%c("na","training dj","","training tw","training ski","training badminton","training spinning")]<-NA
time<-sub("min","",sub(" minutes","",sub("min\\.","",time)))
time<-gsub(":",".",time)
time<-gsub(" ","",time)
readTime<-function(x){
  if(is.na(x))return(x)
  points<-nchar(gsub("[0-9]","",x))
  if(points == 0){ return(as.numeric(x))}
  if(points == 1){ return(as.numeric(x))}
  if(points == 2){
    hours<-as.numeric(sub("\\..+$","",x))
    x<-as.numeric(sub("^.\\.","",x)) + 60*hours
    return(x)
  }
}
for(i in 1:length(time)){
  time[i]<-readTime(time[i])
}
time<-as.numeric(time)
trainings[,"time"]<-time

#get comment
comment<-sub("^.+\\#","",trainings[,"summary"])
comment[-grep("#",trainings[,"summary"])]<-NA
comment<-sub("^ ","",comment)
comment<-gsub("\\\\","",comment)
trainings[,"comment"]<-comment

#get speed
speed<-trainings[,"distance"]/(trainings[,"time"]/60)
trainings[,"speed"]<-speed

#save
trainings[,"summary"]<-NULL
write.table(trainings,file="2016-11-13 trainings calender digest.xls",sep="\t",col.names=TRUE,row.names=FALSE,quote=FALSE)

#merge with additional pre-2012
rm(list=ls())
data1<-read.table("2013-07-01 prior trainings digest.xls",sep="\t",stringsAsFactors=FALSE,header=TRUE)
data2<-read.table("2016-11-13 trainings calender digest.xls",sep="\t",stringsAsFactors=FALSE,header=TRUE,comment.char = "",quote = "")

head(data1)
head(data2)
data2[,"approximate.distance"]<-NULL
data<-rbind(data1,data2)

data<-data[order(data[,"date"]),]

write.table(data,file="2016-11-13 complete training 2007-2016.xls",sep="\t",col.names=TRUE,row.names=FALSE,quote=FALSE)

#plot cumulative distance
rm(list=ls())
data<-read.table("2016-11-13 complete training 2007-2016.xls",sep="\t",stringsAsFactors=FALSE,header=TRUE,comment.char = "",quote = "")

run<-data[data[,"type"]%in%"run",]

run[,"date"]<-as.Date(run[,"date"])
run[,"cdist"]<-0

for(i in 1:nrow(run)){
  if(i != 1){
    run[i,"cdist"]<-run[i-1,"cdist"]
  }
  if(!is.na(run[i,"distance"])){
    run[i,"cdist"]<-run[i,"distance"] + run[i,"cdist"]
  }
}

#set colour
run[is.na(run[,"speed"] ),"speed"]<-mean(run[,"speed"],na.rm=T)
col<-rainbow(11,s=1,v=1,start=0,end=0.15)
# names(col)<-as.character(min(round(run[,"speed"])):max(round(run[,"speed"])))
names(col)<-as.character(7:17)

#set size
run[,"bin"]<-cut(run[,"distance"], breaks=c(0,seq(6,15,1),Inf), labels=seq(5,15,1))
run[,"cex"]<-(run[,"distance"]/max(run[,"distance"],na.rm=T))*2


#main plot function
pdf("2016-11-13 run distance.pdf",width=5,height=5)
plot(
  run[,"date"],run[,"cdist"],
  xlab="",ylab="cumulative distance (km)",
  col=col[as.character(round(run[,"speed"]))],pch=19,
  cex=run[,"cex"],
  main="Running, Lasse"
)


#this is the scale for the colour hue
xl <- min(as.numeric(run[,"date"]),na.rm=T)
yb <- 2000
xr <- xl +200
yt <- 4000
rect(
  xl,
  head(seq(yb,yt,(yt-yb)/10),-1),
  xr,
  tail(seq(yb,yt,(yt-yb)/10),-1),
  col=col
)
text(x = xl +250, y = 150+head(seq(yb,yt,(yt-yb)/10),-1), labels=paste(7:16,"km/h"),cex=0.8,adj=0)


#this is the dot-size scale
xl <- min(as.numeric(run[,"date"]),na.rm=T)+ 365*8
yb <- 100
xr <- xl +200
yt <- 2000
for(i in levels(run[,"bin"])){
  s<-(yt-yb)/length(levels(run[,"bin"]))
  cex<-(as.numeric(i)/max(run[,"distance"],na.rm=T))*2
  points(x=xl,y=s*as.numeric(i)+yb,cex=cex,pch=19,col=col["10"]) 
  text(x=xl+100,y=s*as.numeric(i)+yb,adj=0, label=paste(i,"km"),cex=0.8)
}



dev.off()
