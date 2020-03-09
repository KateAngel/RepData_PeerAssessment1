setwd("E:/DataScience/RepData_PeerAssessment1")


activity <- read.csv("activity.csv")
library(ggplot2)
library(dplyr)
str(activity)
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
#Number of steps per day
total_steps <- with(activity,aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(total_steps) <- c("date", "steps")
total_steps
#Histogram of the total number of steps taken each day
png("plot1.png", width=480, height=480)
hist(total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "red", ylim = c(0,20), breaks = seq(0,25000, by=2500))
dev.off()
#Mean and median of total number of steps taken per day
mean(total_steps$steps, na.rm=TRUE)
median(total_steps$steps, na.rm=TRUE)
#Average daily activity pattern
avg_daily <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
png("plot2.png", width=480, height=480)
h <- ggplot(avg_daily, aes(interval, steps))
h+geom_line(col="red")+ggtitle("Average number of steps")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
dev.off()
#5-minute interval
avg_daily[which.max(avg_daily$steps), ]$interval
#Imputing missing values
#the total number of missing values
sum(is.na(activity$steps))
#filling in all of the missing values
activity$steps_noNA <- ifelse(is.na(activity$steps), round(avg_daily$steps[match(activity$interval, avg_daily$interval)],0), activity$steps)
#a new dataset with the missing data filled in
activity_new <- data.frame(steps=activity$steps_noNA, interval=activity$interval, date=activity$date)
#a histogram of the total number of steps taken each day
total_steps_new <- with(activity_new,aggregate(steps, by = list(date), FUN = sum))
names(total_steps_new) <- c("date", "steps")
png("plot3.png", width=480, height=480)
hist(total_steps_new$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "red", ylim = c(0,30), breaks = seq(0,25000, by=2500))
dev.off()
#Mean and median of total number of steps taken per day
mean(total_steps_new$steps)
median(total_steps_new$steps)
#a new factor variable
activity_new$date <- as.Date(strptime(activity_new$date, format="%Y-%m-%d"))
activity_new$weekday <- weekdays(activity_new$date)
activity_new$daytype <- ifelse(activity_new$weekday=='Saturday' | activity_new$weekday=='Sunday', 'weekend','weekday')
#a panel plot
png("plot4.png", width=480, height=480)
activity_by_days <- aggregate(steps~interval + daytype, activity_new, mean)
ggplot(activity_by_days, aes(x = interval , y = steps, color = daytype)) + geom_line() +
  labs(title = "Average daily steps by type of days", x = "Interval", y = "Average number of steps") +
  facet_wrap(~daytype, ncol = 1, nrow=2)
dev.off()