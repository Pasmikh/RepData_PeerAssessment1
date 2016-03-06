#' ---
#' title: "Reproducible Research. Assignment, week 1"
#' author: "Pavel Mikhaylov"
#' date: "March 4th, 2016"
#' ---
#'Loading packages and data
library(dplyr)
url='https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
tempfile=tempfile()
download.file(url,tempfile)
steps=read.csv(unz(tempfile,'activity.csv'))
#'Calculating number of steps taken per day
stepsNum=group_by(steps,date) %>% summarize(stepsTotal=sum(steps,na.rm=TRUE))
#'Drawing histogram of steps taken per day
hist(stepsNum$stepsTotal,xlab='Number of steps taken per day',main='Histogram of steps taken per day')
#'Calculating mean and median of total number of steps
mean(stepsNum$stepsTotal)
median(stepsNum$stepsTotal)
#'Grouping data by 5 min interval
steps5min=group_by(steps,interval) %>% summarize(stepsAvg=mean(steps,na.rm=T))
#'Formatting intervals for time series plotting
steps5min$interval=sprintf("%04d", steps5min$interval)
steps5min$interval=strptime(steps5min$interval, format="%H%M")
#'Plotting time series
plot(steps5min$interval,steps5min$stepsAvg,type="l",main = 'Number of steps per time of the day'
                                                      ,xlab='Time of the day',
                                                      ylab='Average steps taken')
#'Calculating most active interval
format(steps5min$interval[which.max(steps5min$stepsAvg)],format='%H:%M')
#'Reporting number of NA's
length(steps$interval[which(is.na(steps$steps))])
#'NA's would be filled with mean values of steps taken in the respective interval during other days
#'preparing each interval means for NA's filling
temp=group_by(steps,interval) %>% summarize(stepsAvg=mean(steps,na.rm=T))
#'Function, that merges dataset and table with means per interval. Then it assigns
#'Mean values of intervals for each NA value
fill_na=function(x,means){
      x=merge(x,means,by.x=3,by.y=1,all.x=T)
      x[which(is.na(x[,2])),2]=x[which(is.na(x[,2])),4]
      x
}
#'Creating dataframe with filled NA's
stepsCor=fill_na(steps,temp)
#'Calculating number of steps taken per day
stepsNumCor=group_by(stepsCor,date) %>% summarize(stepsTotal=sum(steps))
#'Drawing histogram of steps taken per day
hist(stepsNumCor$stepsTotal,xlab='Number of steps taken per day',main='Histogram of steps taken per day without NAs')
#'Calculating mean and median of total number of steps corrected
mean(stepsNumCor$stepsTotal)
median(stepsNumCor$stepsTotal)
#'Mean and median numbers are greater with dataset being corrected. That happened, because 
#'steps, that were not recorded did not have input to the total number of steps taken.
#'Removing temp dataframe
rm(temp)
stepsCor$date=strptime(stepsCor$date,format='%Y-%m-%d')
stepsCor$day=as.numeric(format(stepsCor$date,format='%w'))
for(i in 1:17568){stepsCor$dayW[i]=if(stepsCor$day[i]<5){'weekday'}else{'weekend'}}
#'Selecting only useful variables
stepsCor=select(stepsCor,interval,steps,date,dayW)
#'Dplyr doesnt work with POSIXlt, so converting to POSIXct
stepsCor$date=as.POSIXct(stepsCor$date)
#'Preparing data for plotting. Subsetting and formatting both subsets
stepsCorWeekday=filter(stepsCor,dayW=='weekday')
stepsCorWeekend=filter(stepsCor,dayW=='weekend')
steps5minCorWeekday=group_by(stepsCorWeekday,interval) %>% summarize(stepsAvg=mean(steps))
steps5minCorWeekend=group_by(stepsCorWeekend,interval) %>% summarize(stepsAvg=mean(steps))
steps5minCorWeekday$interval=sprintf("%04d", steps5minCorWeekday$interval)
steps5minCorWeekday$interval=strptime(steps5minCorWeekday$interval, format="%H%M")
steps5minCorWeekend$interval=sprintf("%04d", steps5minCorWeekend$interval)
steps5minCorWeekend$interval=strptime(steps5minCorWeekend$interval, format="%H%M")
#'Making a panel plot
par(mfrow=c(2,1))
plot(steps5minCorWeekday$interval,steps5minCorWeekday$stepsAvg,type="l",main = 'Number of steps per interval (weekday)'
     ,xlab='Time of the day',
     ylab='Average steps taken')
plot(steps5minCorWeekend$interval,steps5minCorWeekend$stepsAvg,type="l",main = 'Number of steps per interval (weekend)'
     ,xlab='Time of the day',
     ylab='Average steps taken')
#'
##'R code to reproduce the analyses

```r
library(dplyr)
url='https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
tempfile=tempfile()
download.file(url,tempfile)
steps=read.csv(unz(tempfile,'activity.csv'))
stepsNum=group_by(steps,date) %>% summarize(stepsTotal=sum(steps,na.rm=TRUE))
hist(stepsNum$stepsTotal,xlab='Number of steps taken per day',main='Histogram of steps taken per day')
mean(stepsNum$stepsTotal)
median(stepsNum$stepsTotal)
steps5min=group_by(steps,interval) %>% summarize(stepsAvg=mean(steps,na.rm=T))
steps5min$interval=sprintf("%04d", steps5min$interval)
steps5min$interval=strptime(steps5min$interval, format="%H%M")
plot(steps5min$interval,steps5min$stepsAvg,type="l",main = 'Number of steps per time of the day'
     ,xlab='Time of the day',
     ylab='Average steps taken')
format(steps5min$interval[which.max(steps5min$stepsAvg)],format='%H:%M')
length(steps$interval[which(is.na(steps$steps))])
temp=group_by(steps,interval) %>% summarize(stepsAvg=mean(steps,na.rm=T))
fill_na=function(x,means){
      x=merge(x,means,by.x=3,by.y=1,all.x=T)
      x[which(is.na(x[,2])),2]=x[which(is.na(x[,2])),4]
      x
}
stepsCor=fill_na(steps,temp)
stepsNumCor=group_by(stepsCor,date) %>% summarize(stepsTotal=sum(steps))
hist(stepsNumCor$stepsTotal,xlab='Number of steps taken per day',main='Histogram of steps taken per day without NAs')
mean(stepsNumCor$stepsTotal)
median(stepsNumCor$stepsTotal)
rm(temp)
stepsCor$date=strptime(stepsCor$date,format='%Y-%m-%d')
stepsCor$day=as.numeric(format(stepsCor$date,format='%w'))
for(i in 1:17568){stepsCor$dayW[i]=if(stepsCor$day[i]<5){'weekday'}else{'weekend'}}
stepsCor=select(stepsCor,interval,steps,date,dayW)
stepsCor$date=as.POSIXct(stepsCor$date)
stepsCorWeekday=filter(stepsCor,dayW=='weekday')
stepsCorWeekend=filter(stepsCor,dayW=='weekend')
steps5minCorWeekday=group_by(stepsCorWeekday,interval) %>% summarize(stepsAvg=mean(steps))
steps5minCorWeekend=group_by(stepsCorWeekend,interval) %>% summarize(stepsAvg=mean(steps))
steps5minCorWeekday$interval=sprintf("%04d", steps5minCorWeekday$interval)
steps5minCorWeekday$interval=strptime(steps5minCorWeekday$interval, format="%H%M")
steps5minCorWeekend$interval=sprintf("%04d", steps5minCorWeekend$interval)
steps5minCorWeekend$interval=strptime(steps5minCorWeekend$interval, format="%H%M")
par(mfrow=c(2,1))
plot(steps5minCorWeekday$interval,steps5minCorWeekday$stepsAvg,type="l",main = 'Number of steps per interval (weekday)'
     ,xlab='Time of the day',
     ylab='Average steps taken')
plot(steps5minCorWeekend$interval,steps5minCorWeekend$stepsAvg,type="l",main = 'Number of steps per interval (weekend)'
     ,xlab='Time of the day',
     ylab='Average steps taken')
```
