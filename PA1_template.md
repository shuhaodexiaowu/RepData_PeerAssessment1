---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
data <- read.csv("./activity.csv")
```

## What is mean total number of steps taken per day?


```r
# Calculate the total number of steps taken per day
data$date <- as.factor(data$date)

data2 <- tapply(data$steps,data$date,sum,na.rm =TRUE)
data2 <- as.data.frame(data2)
print(data2)
```

```
##            data2
## 2012-10-01     0
## 2012-10-02   126
## 2012-10-03 11352
## 2012-10-04 12116
## 2012-10-05 13294
## 2012-10-06 15420
## 2012-10-07 11015
## 2012-10-08     0
## 2012-10-09 12811
## 2012-10-10  9900
## 2012-10-11 10304
## 2012-10-12 17382
## 2012-10-13 12426
## 2012-10-14 15098
## 2012-10-15 10139
## 2012-10-16 15084
## 2012-10-17 13452
## 2012-10-18 10056
## 2012-10-19 11829
## 2012-10-20 10395
## 2012-10-21  8821
## 2012-10-22 13460
## 2012-10-23  8918
## 2012-10-24  8355
## 2012-10-25  2492
## 2012-10-26  6778
## 2012-10-27 10119
## 2012-10-28 11458
## 2012-10-29  5018
## 2012-10-30  9819
## 2012-10-31 15414
## 2012-11-01     0
## 2012-11-02 10600
## 2012-11-03 10571
## 2012-11-04     0
## 2012-11-05 10439
## 2012-11-06  8334
## 2012-11-07 12883
## 2012-11-08  3219
## 2012-11-09     0
## 2012-11-10     0
## 2012-11-11 12608
## 2012-11-12 10765
## 2012-11-13  7336
## 2012-11-14     0
## 2012-11-15    41
## 2012-11-16  5441
## 2012-11-17 14339
## 2012-11-18 15110
## 2012-11-19  8841
## 2012-11-20  4472
## 2012-11-21 12787
## 2012-11-22 20427
## 2012-11-23 21194
## 2012-11-24 14478
## 2012-11-25 11834
## 2012-11-26 11162
## 2012-11-27 13646
## 2012-11-28 10183
## 2012-11-29  7047
## 2012-11-30     0
```

```r
#  Make a histogram of the total number of steps taken each day  
hist(data2$data2)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
barplot(data2$data2) #I guess barplot can reflect the steps per day clearly.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
# Calculate and report the mean and median of the total number of steps taken per day
mean1 <- mean(data2$data2)
print(mean1)
```

```
## [1] 9354.23
```

```r
median1 <- median(data2$data2)
print(median1)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


```r
# Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
data$interval <- as.factor(data$interval)
data3 <- tapply(data$steps,data$interval,mean,na.rm =TRUE)
data3 <- as.data.frame(data3)
plot(x =rownames(data3),y =data3$data3,type ="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxindex <- which.max(data3$data3)
maxinterval <- rownames(data3)[maxindex]
print(maxinterval)
```

```
## [1] "835"
```


## Imputing missing values


```r
#  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
totalofmissing <- sum(is.na(data$steps))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#  Create a new dataset that is equal to the original dataset but with the missing data filled in.
for(i in 1:dim(data)[1]){
        if (is.na(data$steps[i])) data$steps[i]<-data3$data3[which(rownames(data3)==as.character(data$interval[i]))] #  find the missing value and fill it with the average interval value 
        
}
#  the new dataset is just data

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
data4 <- tapply(data$steps,data$date,sum,na.rm =TRUE)
data4 <- as.data.frame(data4)
hist(data4$data4)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean2 <-mean(data4$data4)
median2 <- median(data4$data4)
```


## Are there differences in activity patterns between weekdays and weekends?


```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

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
data <-mutate(data, weekday = weekdays(as.Date(data$date)))
for(i in 1:dim(data)[1]){
        if (data$weekday[i] =="星期六" | data$weekday[i] =="星期日") {data$weekday[i]="weekend"}
        else {data$weekday[i]="weekday"}
}

data$weekday <-as.factor(data$weekday)



attach(data)
data5 <-aggregate(data$steps,by =list(interval,weekday),FUN =mean)
data5$Group.1 <- as.character(data5$Group.1)
par(mfrow =c(2,1))
with(subset(data5, Group.2== "weekend"),plot(Group.1,x,type ="l",col ="blue",main ="weekend",xlab ="interval"))
with(subset(data5, Group.2== "weekday"),plot(Group.1,x,type ="l",col ="blue",main ="weekday",xlab ="interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->





