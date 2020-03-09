---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(x, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(as.POSIXlt(x, tz, ...), tz, ...): unknown timezone
## '%Y-%m-%d'
```
## What is mean total number of steps taken per day?

*Number of steps per day*

```r
total_steps <- with(activity,aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
```

```
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'
```

```r
names(total_steps) <- c("date", "steps")
total_steps
```

```
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'
```

```
##          date steps
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01     0
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04     0
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09     0
## 41 2012-11-10     0
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14     0
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30     0
```
*Histogram of the total number of steps taken each day*

```r
hist(total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "red", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
*Mean and median of total number of steps taken per day*

```r
mean(total_steps$steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total_steps$steps, na.rm=TRUE)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

```r
avg_daily <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)

h <- ggplot(avg_daily, aes(interval, steps))
h+geom_line(col="red")+ggtitle("Average number of steps")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
*5-minute interval*

```r
avg_daily[which.max(avg_daily$steps), ]$interval
```

```
## [1] 835
```
## Imputing missing values

*the total number of missing values*

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
*filling in all of the missing values*

```r
activity$steps_noNA <- ifelse(is.na(activity$steps), round(avg_daily$steps[match(activity$interval, avg_daily$interval)],0), activity$steps)
```
*a new dataset with the missing data filled in*

```r
activity_new <- data.frame(steps=activity$steps_noNA, interval=activity$interval, date=activity$date)
```
*a histogram of the total number of steps taken each day*

```r
total_steps_new <- with(activity_new,aggregate(steps, by = list(date), FUN = sum))
```

```
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'
```

```r
names(total_steps_new) <- c("date", "steps")

hist(total_steps_new$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "red", ylim = c(0,30), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
*Mean and median of total number of steps taken per day*

```r
mean(total_steps_new$steps)
```

```
## [1] 10765.64
```

```r
median(total_steps_new$steps)
```

```
## [1] 10762
```

## Are there differences in activity patterns between weekdays and weekends?

*a new factor variable*


```r
activity_new$date <- as.Date(strptime(activity_new$date, format="%Y-%m-%d"))
```

```
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'
```

```r
activity_new$weekday <- weekdays(activity_new$date)
activity_new$daytype <- ifelse(activity_new$weekday=='Saturday' | activity_new$weekday=='Sunday', 'weekend','weekday')
```
*a panel plot*

```r
activity_by_days <- aggregate(steps~interval + daytype, activity_new, mean)
ggplot(activity_by_days, aes(x = interval , y = steps, color = daytype)) + geom_line() +
  labs(title = "Average daily steps by type of days", x = "Interval", y = "Average number of steps") +
  facet_wrap(~daytype, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
