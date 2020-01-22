---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

## 1. Load the data (i.e. read.csv())


```r
unzip("C:/Users/chenl/Desktop/Documents/PERSONAL INFORMATION/LIANGHE INFORMATION/GITHUB/RepData_PeerAssessment1/activity.zip")
activityData <- read.csv("C:/Users/chenl/Desktop/Documents/PERSONAL INFORMATION/LIANGHE INFORMATION/GITHUB/RepData_PeerAssessment1/activity.csv")
```

## 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
summary(activityData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
names(activityData)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activityData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
pairs(activityData)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is mean total number of steps taken per day?

## 1. Calculate the total number of steps taken per day


```r
stepsPerDay <- aggregate(steps ~ date, data = activityData, sum, na.rm=TRUE)
```

## 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(stepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## 3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanStepsPerDAy <- mean(stepsPerDay$steps)
meanStepsPerDAy
```

```
## [1] 10766.19
```

```r
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay
```

```
## [1] 10765
```

## What is the average daily activity pattern?

## 1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsPerInterval <- aggregate(steps ~ interval, data = activityData, mean, na.rm=TRUE)
plot(steps ~ interval, stepsPerInterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalwithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalwithMaxNbSteps
```

```
## [1] 835
```

## Imputing missing values

## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalValuesMissing <- sum(is.na(activityData$steps))
totalValuesMissing
```

```
## [1] 2304
```

## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
getMeanStepsPerInterval <- function(interval){stepsPerInterval[stepsPerInterval$interval == interval,]$steps}
```

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityDataNoNA <- activityData
for(i in 1:nrow(activityDataNoNA)){if(is.na(activityDataNoNA[i,]$steps)){activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)}}
```

## 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsPerDayNoNA <- aggregate(steps ~ date, data = activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format = "%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)

for(i in 1:nrow(activityDataNoNA)){if(activityDataNoNA[i,]$day %in% c("Saturday", "Sunday")){activityDataNoNA[i,]$day <- "weekend"}else{activityDataNoNA[i,]$day <- "weekday"}}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, data = activityDataNoNA, mean)
```

## 2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, data = stepsByDay, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
