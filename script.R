# unzip the datafile
unzip("activity.zip", "activity.csv")
?read.csv
activity_data <- read.csv("activity.csv");
colnames(activity_data)
summary(activity_data)
?hist
# re-modelling the data for total number of steps

steps_per_day <- aggregate(activity_data$steps, by =list(activity_data$date), FUN=sum)

library(data.table)
setnames(steps_per_day, c("date", "steps.p.day"))
hist(steps_per_day$steps.p.day, col ="pink",main = "Histogram of total steps taken per day",
     xlab ="steps per day", ylab="frequency")
?hist
?mean
?round
mean_steps.p.day <- round(mean(steps_per_day$steps.p.day, na.rm=TRUE),digits=1)
median_steps.p.day <- median(steps_per_day$steps.p.day, na.rm=TRUE)

step_by_interval <- aggregate(activity_data$steps, by = list(activity_data$interval), FUN= mean, na.rm=TRUE)
setnames(step_by_interval, c("time.interval", "average.steps"))
?plot
plot(step_by_interval$time.interval,step_by_interval$average.steps, type="l",main = "time-series plot of average activity",
     xlab = "time interval", ylab= "average steps")


rows_with_NA <- sum(!complete.cases(activity_data))

?na.handling
is.na(activity_data$steps)
interval_ave_activity <- rep(step_by_interval$average.steps, nrow(steps_per_day))
merge_activity <- cbind(activity_data,interval_ave_activity)
# replace NA values
merge_activity$steps[is.na(merge_activity$steps)] <- merge_activity$interval_ave_activity[is.na(merge_activity$steps)]
?drop
#missing values filled in
activity_data_complete <- merge_activity[,1:3]

steps.p.day.complete <- aggregate(activity_data_complete$steps, by =list(activity_data_complete$date), FUN=sum)

setnames(steps.p.day.complete, c("date","steps"))

mean.steps.p.day.complete <- round(mean(steps.p.day.complete$steps), digits=1)
median.steps.p.day.complete <- round(median(steps.p.day.complete$steps),digits=1)

hist(steps_per_day$steps.p.day, col ="orange",main = "total steps taken per day..no missing values",
     xlab ="steps per day.complete", ylab="frequency")


weekdays(as.Date(activity_data$date))

library(lubridate)
?wday
library(chron)
?is.weekend
is.weekend(as.Date(activity_data$date))

isWeekend <- function(x1){
  if(chron::is.weekend(x1)){
    return("weekend")
  }
  else{
     return("weekday")
  }
}

?tapply
?sapply

day.of.week.activity <- sapply(as.Date(activity_data$date),FUN=isWeekend )

activity.data.week <- cbind(activity_data_complete,day.of.week.activity)
activity.data <- activity.data.week[-c(2)]
library(data.table)
setnames(activity.data,"day.of.week.activity", "day.of.week" )

summary.activity.data <- aggregate(activity.data$steps, by = list(activity.data$interval,activity.data$day.of.week), FUN=mean)

setnames(summary.activity.data, c("interval","day.of.week","steps"))
?as.factor
summary.activity.data$day.of.week <- as.factor(summary.activity.data$day.of.week)
summary(summary.activity.data)

library(lattice)
?xyplot
xyplot(steps~interval|day.of.week,data= summary.activity.data, type= "l",layout=c(1,2), xlab="interval", 
       ylab="average steps", main = "activity comparison weekday and weekend")
