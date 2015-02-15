---
title: 'Reproducable research: Peer assessment'
author: "Jason"
date: "February 15, 2015"
output: html_document
---
##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Loading and preprocessing the data
Read data into R: 

```r
wristband <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
head(wristband)
```

```
## Error in head(wristband): object 'wristband' not found
```
Process data:

```r
wristband$date <- as.Date(wristband$date, format = "%Y-%m-%d")
```

```
## Error in as.Date(wristband$date, format = "%Y-%m-%d"): object 'wristband' not found
```
Show data:

```r
str(wristband)
```

```
## Error in str(wristband): object 'wristband' not found
```
##What is mean total number of steps taken per day?

```r
steps_per_day <- aggregate(steps ~ date, wristband, sum)
```

```
## Error in eval(expr, envir, enclos): object 'wristband' not found
```

```r
colnames(steps_per_day) <- c("date","steps")
```

```
## Error in colnames(steps_per_day) <- c("date", "steps"): object 'steps_per_day' not found
```

```r
head(steps_per_day)
```

```
## Error in head(steps_per_day): object 'steps_per_day' not found
```

```r
hist(steps_per_day$steps, main = "Histogram of total steps per day", xlab = "steps",ylab="days", freq=TRUE, col = "red")
```

```
## Error in hist(steps_per_day$steps, main = "Histogram of total steps per day", : object 'steps_per_day' not found
```

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_per_day$steps, na.rm=TRUE)
```

```
## Error in mean(steps_per_day$steps, na.rm = TRUE): object 'steps_per_day' not found
```

```r
median(steps_per_day$steps, na.rm=TRUE)
```

```
## Error in median(steps_per_day$steps, na.rm = TRUE): object 'steps_per_day' not found
```

##What is the average daily activity pattern?

```r
avg.steps.on.interval<-tapply(wristband$steps,wristband$interval,"mean",na.rm=TRUE) 
```

```
## Error in tapply(wristband$steps, wristband$interval, "mean", na.rm = TRUE): object 'wristband' not found
```

```r
plot(names(avg.steps.on.interval),avg.steps.on.interval, xlab="interval ID",ylab="steps", type="l")
```

```
## Error in plot(names(avg.steps.on.interval), avg.steps.on.interval, xlab = "interval ID", : object 'avg.steps.on.interval' not found
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? see below calculation

```r
which.max(avg.steps.on.interval)
```

```
## Error in which.max(avg.steps.on.interval): object 'avg.steps.on.interval' not found
```
##Imputing missing values
The missing values are summarized as:

```r
sum(is.na(wristband$steps))
```

```
## Error in eval(expr, envir, enclos): object 'wristband' not found
```

fill missing values with mean:

```r
wristbandfill<-wristband
```

```
## Error in eval(expr, envir, enclos): object 'wristband' not found
```

```r
wristbandfill$steps[is.na(wristband$steps)]<-avg.steps.on.interval[is.na(wristband$steps)]
```

```
## Error in eval(expr, envir, enclos): object 'avg.steps.on.interval' not found
```

```r
head(wristbandfill)
```

```
## Error in head(wristbandfill): object 'wristbandfill' not found
```
histogram plot of new data:

```r
steps_per_day2 <- aggregate(steps ~ date, wristbandfill, sum)
```

```
## Error in eval(expr, envir, enclos): object 'wristbandfill' not found
```

```r
colnames(steps_per_day2) <- c("date","steps")
```

```
## Error in colnames(steps_per_day2) <- c("date", "steps"): object 'steps_per_day2' not found
```

```r
hist(steps_per_day2$steps, main = "Histogram of total steps per day", xlab = "steps",ylab="days", freq=TRUE, col = "red")
```

```
## Error in hist(steps_per_day2$steps, main = "Histogram of total steps per day", : object 'steps_per_day2' not found
```

mean and median of new data:

```r
mean(steps_per_day2$steps, na.rm=TRUE)
```

```
## Error in mean(steps_per_day2$steps, na.rm = TRUE): object 'steps_per_day2' not found
```

```r
median(steps_per_day2$steps, na.rm=TRUE)
```

```
## Error in median(steps_per_day2$steps, na.rm = TRUE): object 'steps_per_day2' not found
```
mean is not affected since we use the mean to fill this slot and calculate mean again. median is affected

##Are there differences in activity patterns between weekdays and weekends?
A new factor variable is created: 

```r
wristband7<-wristbandfill
```

```
## Error in eval(expr, envir, enclos): object 'wristbandfill' not found
```

```r
wristband7$weektime <- as.factor(ifelse(weekdays(wristband$date) %in% 
                        c("Saturday","Sunday"),"weekend", "weekday"))
```

```
## Error in weekdays(wristband$date): object 'wristband' not found
```

```r
summary(wristband7)
```

```
## Error in summary(wristband7): object 'wristband7' not found
```
plot:

```r
StepsByWeektime <- aggregate(steps ~ interval + weektime, data = wristband7, mean)
```

```
## Error in eval(expr, envir, enclos): object 'wristband7' not found
```

```r
names(StepsByWeektime) <- c("interval", "weektime", "steps")
```

```
## Error in names(StepsByWeektime) <- c("interval", "weektime", "steps"): object 'StepsByWeektime' not found
```

```r
#StepsByWeektime$interval <- as.numeric(levels(StepsByWeektime$interval))[StepsByWeektime$interval]
library(lattice)
xyplot(steps ~ interval | weektime, StepsByWeektime,  type = "l", layout = c(1, 2), 
        xlab = "Interval", ylab = "Number of steps")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'StepsByWeektime' not found
```

There is obviouse difference between weekdays and weekends. It appears this user gets up later/goes to bed later during weekends, he/she is also more active during those weekend daytimes.
