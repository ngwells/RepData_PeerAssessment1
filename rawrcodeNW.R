setwd("C:/Users/Brian/Documents/COURSERA/class4week2")

dataset<-read.csv("activity.csv")
#dataset$date<-as.Date(dataset$date)
dataset2<-na.omit(dataset)


library(dplyr)
dataset2<-tbl_df(dataset2)
totalstepdata<-dataset2 %>% group_by(date) %>% summarise(totalsteps<-sum(as.numeric(steps)))
print(head(totalstepdata))


hist(totalstepdata$totalsteps,breaks=10)



stepmean<-mean(totalstepdata$totalsteps)
stepmedian<-median(totalstepdata$totalsteps)





dataset2$date<-as.Date(dataset2$date)


meanstepdata<-dataset2 %>% group_by(interval) %>% summarise(totalsteps<-mean(steps))
par(mfrow=c(2,1))
plot(seq(1:dim(dataset2)[1]),dataset2$steps,typ="l",xlab = "Sequence of 5 min Data Points",ylab="Total Steps")
plot(meanstepdata$interval,meanstepdata$totalsteps,typ="l", xlab="Average steps per 5 min Interval",ylab="Average Total Steps")


maxinterval<-meanstepdata[meanstepdata$totalsteps==max(meanstepdata$totalsteps),"interval"]


missingvals<-sum(is.na(dataset2$steps))




completedata <- transform(dataset, steps = ifelse(is.na(dataset$steps), meanstepdata$totalsteps[match(dataset$interval, meanstepdata$interval)], dataset$steps))

totalstepdata2<-completedata %>% group_by(date) %>% summarise(totalsteps=sum(as.numeric(steps)))

hist(totalstepdata2$totalsteps,breaks=10)


stepmean2<-mean(totalstepdata2$totalsteps)
stepmedian2<-median(totalstepdata2$totalsteps)


diffmean<-stepmean-stepmean2
diffmed<-stepmedian-stepmedian2


completedata$day<-weekdays(as.Date(dataset$date))

for( i in 1:dim(completedata)[1])
{
if(completedata$day[i] %in% c("Saturday","Sunday"))(completedata$weekend[i]<-1)else{completedata$weekend[i]<-0}
}

weekenddata<-completedata[completedata$weekend==1,]
weekdaydata<-completedata[completedata$weekend==0,]

meanstepdataWE<-weekenddata %>% group_by(interval) %>% summarise(totalstepsWE=mean(steps))
meanstepdataWD<-weekdaydata %>% group_by(interval) %>% summarise(totalstepsWD=mean(steps))

par(mfrow=c(2,1))
plot(meanstepdataWE$interval,meanstepdataWE$totalsteps,typ="l", main="Weekend Interval Data", xlab="Average steps per 5 min Interval",ylab="Average Total Steps")
plot(meanstepdataWD$interval,meanstepdataWD$totalsteps,typ="l", main="Weekday Interval Data", xlab="Average steps per 5 min Interval",ylab="Average Total Steps")




setwd("C:/Users/Brian/Documents/GitHub/RepData_PeerAssessment1")
knit2html("C:/Users/Brian/Documents/GitHub/RepData_PeerAssessment1/PA1_template.Rmd")
