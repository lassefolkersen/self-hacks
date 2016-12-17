#Based on mail download of Nellie pick up time from Rosenhaven (have to use thunderbird, search 'rosenhaven' and click export mails to this folder). Then start R:

rm(lits=ls())
basedir<-"C:/Users/FOLK/Documents/Work/Analysis/2016-11-25_nellie_check_out_times/2016-11-25_all_mails/"
files<-list.files(basedir)
data<-data.frame(direction=vector(),date=vector(),file=vector())

#loop over every single mail in that folder
for(file in files){
  
  #open and read the file
  f<-file(paste0(basedir,file),"r")
  content<-readLines(f)
  close(f)
  subject<-grep("^Subject",content,ignore.case=F,value=T)
  
  #determine if it's check in or check out or other type of mail
  if(subject == "Subject: Nellie Wei Folkersen tjekket ind"){
    direction <- "in"
  }else if(subject == "Subject: Nellie Wei Folkersen tjekket ud"){
    direction <- "out"
  }else{
    # print(subject)
    next
  }
  
  #Handling time zone offset --- note this is horribly difficult because the date stamp is in pacific time
  #and the %&¤#" summer time correction doesn't follow exactly. Lot of trial-and-error testing going on here
  received<-sub("^ +","",content[3])
  string_raw<-sub(" -0.00 \\(P[SD]T\\)","",sub("^.+, ","",received))
  timezone<-sub("\\)$","",sub("^.+\\(","",received))
  timezone_offset<-as.numeric(sub("00$","",sub("^.+ ","",sub(" \\(.+$","",received))))
  time_raw<-sub("^.+ ","",string_raw)
  # hour_raw<-substr(time_raw,1,2)
  # if(hour_raw=="23") hour_raw <- "-01"
  # if(hour_raw=="22") hour_raw <- "-02"
  # if(hour_raw=="21") hour_raw <- "-03"
  # if(hour_raw=="20") hour_raw <- "-04"
  # corrected_hour<-as.numeric(hour_raw) + timezone_offset
  # print(paste(direction,hour_raw,corrected_hour))
  # corrected_hour_char<-as.character(corrected_hour)
  # if(nchar(corrected_hour_char)==1)corrected_hour_char<-paste0("0",corrected_hour_char)
  # rest_raw<-substr(time_raw,3,8)
  # time_raw_2<-paste0(hour_raw,rest_raw)
  date_raw<-sub(" [0-9:]+$","",string_raw)
  # date<-strptime(paste(date_raw,time_raw), tz = "US/Pacific"  , format = "%e %b %Y %H:%M:%S")
  tz<-paste0("Etc/GMT+",abs(timezone_offset))
  date<-as.POSIXct(paste(date_raw,time_raw), tz = tz  , format = "%e %b %Y %H:%M:%S")
  # date<-as.POSIXct(paste(date_raw,time_raw), tz = "US/Pacific"  , format = "%e %b %Y %H:%M:%S")
  format(date,format="%H:%M:%S",tz="Europe/Copenhagen")
  # date_raw<-sub(" -0.00 \\(P[SD]T\\)","",sub("^.+, ","",received))
  # date<-as.Date(date_raw, format = "%e %b %Y %H:%M:%S")
  # date<-strptime(sub("^.+ ","",date_raw), tz = "US/Pacific"  , format = "%H:%M:%S")
  # dtt<-as.POSIXlt(strptime(dt,'%Y-%m-%d %H%M'))
  #ultimately POSIXct seemed to be the best - i.e. the only that didn't introduce weird time-skewings  
  
  #save in data frame
  data<-rbind(data,data.frame(direction=direction,date=date,file=file))
  
  
}



#deduce date
Sys.setlocale(category = "LC_TIME", locale = "English")
data[,"weekday"]<-factor(format(data[,"date"],format="%A",tz="Europe/Copenhagen"),levels=c("Monday","Tuesday","Wednesday","Thursday","Friday"))
data[,"time"]<-format(data[,"date"],format="%H:%M:%S",tz="Europe/Copenhagen")




#insert sheet with other info covariate guesses etc
misc_info_file<-"C:/Users/FOLK/Documents/Work/Analysis/2016-11-25_nellie_check_out_times/2016-11-25_who_pick.txt"
misc_info<-read.table(misc_info_file,header=T,sep="\t",row.names=1)
data<-cbind(data,misc_info[data[,"date"],])



#save data
save(data,file="2016-11-25_Nellie_picking_time.rdata")






#start the plotting/analysis part - first load the extracted data as well as some custome plotting functions
rm(list=ls())
load("Important R-images and cel files/2016-11-22 myfunctions.rdata") #also found in this repository under (misc_files)
load("C:/Users/FOLK/Documents/Work/Analysis/2016-11-25_nellie_check_out_times/2016-11-25_Nellie_picking_time.rdata")


#get time as 10-decimal number 
toDecimal<-function(x) {
  seconds<-as.numeric(substr(x,1,2))*60*60 + as.numeric(substr(x,4,5)) * 60 + as.numeric(substr(x,7,8))
  hours<-seconds / (60*60)
  return(hours)
}
data[,"time_decimal"] <- toDecimal(data[,"time"])


#remove pickups before set time and deliverys after set time
settime<-12
data<-data[!(data[,"time_decimal"]>settime & data[,"direction"]%in%"in")%in%TRUE,]
data<-data[!(data[,"time_decimal"]<settime & data[,"direction"]%in%"out")%in%TRUE,]


#define colours
in_col<-rgb(30/256,144/256,255/256,150/256)
out_col<-rgb(255/256,144/256,0/256,150/256)

#also shades-colours (for slightly offset dot colours)
h<-rgb2hsv(col2rgb(in_col))
h["v",1]<-0.4
in_col_2<-hsv(h["h",1],h["s",1],h["v",1],alpha=1)
h<-rgb2hsv(col2rgb(out_col))
h["v",1]<-0.4
out_col_2<-hsv(h["h",1],h["s",1],h["v",1],alpha=1)
colours<-c(in_col_2,out_col_2)


#group data in a list (because that's how the fun_plot_groupwise_expression_data_20100830 takes them)
#define the pointCol at the same time (that's the point colours)
names(colours) <- c("in","out")
groups<-list()
pointCol <- list()
for(weekday in levels(data[,"weekday"])){
  groups[[substr(weekday,1,3)]] <- data[data[,"weekday"]%in%weekday,"time_decimal"]
  pointCol[[substr(weekday,1,3)]] <- colours[data[data[,"weekday"]%in%weekday,"direction"]]
  names(pointCol[[substr(weekday,1,3)]])<-NULL
}


#open plotting
pdf("2016-11-25_Nellie_picking_time.pdf",width=6,height=4)

#main dot plot
fun_plot_groupwise_expression_data_20100830(groups,ylab="Time",pointPch=19,horizontalScatterSpacing=0.04,main="Nellie Wei Folkersen - B?rnehavehentetider",las=2,plotN=F,pointCol=pointCol,ylim=c(7.5,18))

#add box plots on top
data_in <- data[data[,"direction"]%in%"in",]
boxplot(time_decimal~weekday,data=data_in,add=T,at=1:5,col=in_col,xaxt="n",outline=F)	
data_out <- data[data[,"direction"]%in%"out",]
boxplot(time_decimal~weekday,data=data_out,add=T,at=1:5,col=out_col,xaxt="n",outline=F)	


#calculation1: simple T-test and wilcoxon-statistics for picking data
isNotFriday<-data_out[data_out[,"weekday"]%in%c("Monday","Tuesday","Wednesday","Thursday"),"time_decimal"]
isFriday<-data_out[data_out[,"weekday"]%in%c("Friday"),"time_decimal"]
Diff_out<-signif((mean(isNotFriday) - mean(isFriday))*60,3)
Diff_out_2<-signif((median(isNotFriday) - median(isFriday))*60,3)
P_out_1<-signif(t.test(isFriday,isNotFriday)[["p.value"]],3)
P_out_2<-signif(wilcox.test(isFriday,isNotFriday)[["p.value"]],3)
abline(h=median(isNotFriday),lwd=1,lty=2,col="grey70")

#calculation2: simple T-test and wilcoxon-statistics for deliver data
isNotFriday<-data_in[data_in[,"weekday"]%in%c("Monday","Tuesday","Wednesday","Thursday"),"time_decimal"]
isFriday<-data_in[data_in[,"weekday"]%in%c("Friday"),"time_decimal"]
P_in_1<-signif(t.test(isFriday,isNotFriday)[["p.value"]],3)
P_in_2<-signif(wilcox.test(isFriday,isNotFriday)[["p.value"]],3)
abline(h=median(isNotFriday),lwd=1,lty=2,col="grey70")


#calculation3: linear mixed model calculation for the same (only do 'out')
library(nlme)
weekday<-data[,"weekday"]
time_decimal<-data[,"time_decimal"]
model3<-lme(fixed=time_decimal ~ weekday + picker + season + staffgroup , random  = ~ 1 | picker, data=data, na.action=na.omit, method="REML")
p_out_3<-signif(summary(model3)[["tTable"]]["weekdayFriday","p-value"],3)


#add legend and end plotting
legend(x=0,y=15,legend=c("check-out","check-in"),pch=15,col=c(out_col_2,in_col_2),bty="n")
dev.off()


# get sample size
sampleSize<-round(mean(c(nrow(data_out),nrow(data_in))))

#return main result
paste0("Friday check-out time vs other-day check-out time difference: ",Diff_out_2," minutes (P=",P_out_3,")")

