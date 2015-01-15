# Reproducible Research: Peer Assessment 1
Jona N  
Jan 13th, 2015  

## Loading and preprocessing the data

. Unzip and loading the data

```r
if(!file.exists("activity.csv")) unzip("activity.zip")
dataset <- read.csv("activity.csv")
summary(dataset)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

```r
head(dataset)
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


## What is mean total number of steps taken per day?

### In this part we're ignoring the missing values in the dataset
### 1. Make a histogram of the total number of steps taken each day
. computing the number of steps taken each day

```r
sumData<-with(dataset, tapply(steps, date, sum))
barplot(sumData, main="Total number of steps taken each day",
        xlab="Date: 2012-10-01 to 2012-11-30", ylab="Steps")
```

<img src="PA1_template_files/figure-html/scatterplot.png" title="plot of chunk scatterplot" alt="plot of chunk scatterplot" style="display: block; margin: auto;" />

### 2. Calculate and report the mean and median total number of steps taken per day

```r
meanData<-with(dataset, tapply(steps, date, mean))
barplot(meanData, main="Average of steps taken per day",
        xlab="Date: 2012-10-01 to 2012-11-30", ylab="Steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-1.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" style="display: block; margin: auto;" />

```r
mean(meanData, na.rm=TRUE)
```

```
## [1] 37.38
```

```r
median(meanData, na.rm=TRUE)
```

```
## [1] 37.38
```


## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
with(aggregate(steps ~ interval, data=dataset, mean, na.rm = T),
     plot(interval, steps, type="l", main="Average daily activity pattern",
     xlab="Interval", ylab="Steps averaged across all days"))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-2.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max(aggregate(steps ~ interval, data=dataset, mean, na.rm = T)$steps)
```

```
## [1] 206.2
```


## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(dataset$steps))
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
. Computing the mean for that 5-minute interval

```r
library("plyr")
tidyData<-dd<-dataset
pp <- ddply(dd, .(interval), summarize, means = mean(steps, na.rm=TRUE))
```

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
. Filling all the missing values of the dataset in new dataset

```r
l<-which(is.na(dataset))
tidyData$steps[l]<-pp$means[pp$interval %in% dataset$interval[l]]
sum(is.na(tidyData$steps))
```

```
## [1] 0
```

```r
head(tidyData)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```

### 4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
. computing the number of steps taken each day

```r
par(mfrow=c(1,2), mar=c(2,2,4,2))

sumDataTidy<-with(tidyData, tapply(steps, date, sum))
barplot(sumDataTidy, main="Total steps taken each day\nWithout missing data",
        xlab="Date: 2012-10-01 to 2012-11-30", ylab="Steps")

sumData<-with(dataset, tapply(steps, date, sum))
barplot(sumData, main="Total steps taken each day\nWith missing data",
        xlab="Date: 2012-10-01 to 2012-11-30", ylab="Steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-7.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />

. computing the mean of steps taken each day

```r
par(mfrow=c(1,2), mar=c(2,2,4,2))

meanDataTidy<-with(tidyData, tapply(steps, date, mean))
barplot(meanDataTidy, main="Average steps taken per day\nWithout missing data",
        xlab="Date: 2012-10-01 to 2012-11-30", ylab="Steps")

meanData<-with(dataset, tapply(steps, date, mean))
barplot(meanData, main="Average steps taken per day\nWith missing data",
        xlab="Date: 2012-10-01 to 2012-11-30", ylab="Steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-8.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />

. Computing the mean and median total number of steps taken per day

```r
mean(meanDataTidy, na.rm=TRUE)
```

```
## [1] 37.38
```

```r
median(meanDataTidy, na.rm=TRUE)
```

```
## [1] 37.38
```



## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
tidyData$week<-as.character(weekdays(as.Date(tidyData$date)))
tidyData$week[!tidyData$week %in% c("sábado","domingo")]<-"weekday"
tidyData$week[tidyData$week %in% c("sábado","domingo")]<-"weekend"
tidyData$week<-factor(tidyData$week, levels = c("weekday", "weekend"))
table(tidyData$week)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
str(tidyData)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ week    : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
par(mfrow=c(2,1), mar=c(4,4,3,2))
with(subset(aggregate(steps ~ interval + week, data = tidyData, mean),week=="weekday"), plot(interval, steps, type="l", main="Average of steps across all weekday day", xlab="Interval", ylab="Steps"))
with(subset(aggregate(steps ~ interval + week, data = tidyData, mean),week=="weekend"), plot(interval, steps, type="l", main="Average of steps across all weekend day", xlab="Interval", ylab="Steps"))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" style="display: block; margin: auto;" />

