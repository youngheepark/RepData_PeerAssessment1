# Reproducible Research: Peer Assessment 1

```r
library(lubridate)
library(plyr)
```

```
## 
## Attaching package: 'plyr'
## 
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
library(lattice)
```

Loading and preprocessing the data <br>
1. Load the data (i.e. read.csv())

```r
data <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
cdata <- subset(data, !is.na(steps))
```

What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

```r
totalSteps <- ddply(cdata, .(date), summarise, steps = sum(steps))
hist(totalSteps$steps, xlab="steps", main="Histogram of the total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
summary(totalSteps)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
   the average number of steps taken, averaged across all days (y-axis)


```r
sumSteps <- ddply(cdata, .(interval), summarise, steps.mean = mean(steps), steps.max = max(steps))
xyplot(steps.mean ~ interval, sumSteps, type="l", main="The Average number of steps, across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
xyplot(steps.max ~ interval, sumSteps, type="l",  main="The maximum number of steps, across all day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


Imputing missing values <br>
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(data) - nrow(cdata)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
ndata <- data
ndata <- ddply(data, .(interval), transform, stepmean=mean(steps, na.rm=TRUE))
na.idx <- which(is.na(ndata$steps))
ndata$steps[na.idx] <- ndata$stepmean[na.idx]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalSteps2 <- ddply(ndata, .(date), summarise, steps = sum(steps))
hist(totalSteps2$steps, xlab="steps", main="Histogram of the total number of steps (Imputing missing values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Summary

```r
summary(totalSteps2)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

```r
mean(totalSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps2$steps)
```

```
## [1] 10766.19
```


Are there differences in activity patterns between weekdays and weekends? <br>
1. Create a new factor variable in the dataset with two levels
   -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
ndata <- ddply(ndata, .(date), transform, weekday=ifelse(wday(as.Date(date)) %% 7 > 1, "weekday", "weekend"))
weekSteps <- ddply(ndata, c("weekday", "interval"), summarise, steps.mean = mean(steps))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
   and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
xyplot(steps.mean ~ interval|weekday, weekSteps, type="l", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
                         
