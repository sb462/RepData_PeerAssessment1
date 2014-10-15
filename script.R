# unzip the datafile
unzip("activity.zip", "activity.csv")
?read.csv
activity_data <- read.csv("activity.csv");
colnames(activity_data)
summary(activity_data)
?hist
# re-modelling the data for total number of steps

steps_per_day <- aggregate(activity_data$steps, by =list(activity_data$date), FUN=sum)

