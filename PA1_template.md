# Reproducible Research Week 2 Course Project


### Loading data
Data loaded using read.csv()

```r
data<-read.csv("activity.csv")
head(data)
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
library(knitr)
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.3
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
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.3
```


### What is mean total number of steps taken per day?

1. Summing steps of each day into variable totalstepsperday

```r
StepsPerDay<-aggregate(data$steps,list(data$date),sum)
colnames(StepsPerDay)<-c("Date","Steps")
```

2. Creating a histogram of Date vs Total Steps

```r
hsteps<-ggplot(StepsPerDay,aes(Steps))
hsteps+geom_histogram(col="brown",fill="orange")+ggtitle("Steps Per Day HISTOGRAM")+xlab("Steps")+ylab("Frequency Count")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

3. Mean of the number of steps taken per day

```r
smean<-mean(StepsPerDay$Steps,na.rm = TRUE)
smean
```

```
## [1] 10766.19
```

4. Median of the number of steps taken per day

```r
smedian<-median(StepsPerDay$Steps,na.rm = TRUE)
smedian
```

```
## [1] 10765
```


###What is the average daily activity pattern?

1. Making time series plot of steps vs intervals

```r
StepsPerInterval<-aggregate(steps~interval,data,mean,na.action=na.omit)
tsplot<-ggplot(StepsPerInterval,aes(interval,steps))
tsplot+geom_line(col="darkgreen")+ggtitle("Steps vs Intervals")+xlab("Intervals")+ylab("Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

2. The 5-minute interval having maximum no. of steps

```r
max<-filter(StepsPerInterval,steps==max(StepsPerInterval$steps))
```


###Imputing missing values
1. Calculating and reporting total no. of NA values 

```r
mvals<-sum(is.na(data))
mvals
```

```
## [1] 2304
```

2. Replacing missing values 

```r
data$nsteps<-ifelse(is.na(data$steps),round(StepsPerInterval$steps[match(data$interval,StepsPerInterval$interval)],0),data$steps)
```

3. New dataset with filled in missing values

```r
NewData<-data.frame(steps=data$nsteps,date=data$date,interval=data$interval)
head(NewData)
```

```
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

4. Histogram of steps per day with the new dataset 

```r
NewStepsPerDay<-aggregate(NewData$steps,list(NewData$date),sum)
colnames(NewStepsPerDay)<-c("NewDate","NewSteps")

Newhsteps<-ggplot(NewStepsPerDay,aes(NewSteps))
Newhsteps+geom_histogram(color="blue",fill="cyan")+ggtitle("Histogram with new dataset")+xlab("New Steps")+ylab("Frequency Count")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

```r
NewMean<-mean(NewStepsPerDay$NewSteps)
NewMean
```

```
## [1] 10765.64
```

```r
NewMedian<-median(NewStepsPerDay$NewSteps)
NewMedian
```

```
## [1] 10762
```
Comparing both the histograms, we come to know that between 10000 and 12500 steps, there is a sudden rise in frequency because of the replacement of missing values. With a slight variation, the mean and median of the data have decrease since for some values of steps, the frequencies have increased.

###Are there differences in activity patterns between weekdays and weekends?

1.Creating new factor variable to check the day type 

```r
NewData$ActualDate<-as.Date(NewData$date,format="%Y-%m-%d")
NewData$WeekDay<-weekdays(NewData$ActualDate)
NewData$TypeOfDay<-ifelse(NewData$WeekDay=='Saturday' | NewData$WeekDay=='Sunday', 'weekend','weekday')
head(NewData)
```

```
##   steps       date interval ActualDate WeekDay TypeOfDay
## 1     2 2012-10-01        0 2012-10-01  Monday   weekday
## 2     0 2012-10-01        5 2012-10-01  Monday   weekday
## 3     0 2012-10-01       10 2012-10-01  Monday   weekday
## 4     0 2012-10-01       15 2012-10-01  Monday   weekday
## 5     0 2012-10-01       20 2012-10-01  Monday   weekday
## 6     2 2012-10-01       25 2012-10-01  Monday   weekday
```

2. Panel Plot for average steps taken with respect to 5-minute intervals on weekday and weekend 

```r
NewStepsPerInterval<-aggregate(steps~interval+TypeOfDay,NewData,mean,na.action=na.omit)
Newtsplot<-ggplot(NewStepsPerInterval,aes(interval,steps))
Newtsplot+geom_line(col="red")+ggtitle("Steps vs Intervals")+xlab("Intervals")+ylab("Steps")+facet_grid(TypeOfDay~.)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
