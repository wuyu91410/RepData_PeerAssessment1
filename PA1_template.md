# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
dt=read.csv("/users/daniel/downloads/activity.csv")
```

## What is mean total number of steps taken per day?

```r
as=aggregate(steps~date,data=dt,sum,na.action=na.pass)
hist(as$steps)
```

![](PA1_template_files/figure-html/histogram-1.png) 


```r
mean(as$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(as$steps,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
ai=aggregate(steps~interval,data=dt,mean)
plot(ai$interval,ai$steps,type="l")
```

![](PA1_template_files/figure-html/plot-1.png) 


```r
ai[which(ai$steps==max(ai$steps)),]
```

```
##     interval    steps
## 104      835 206.1698
```
So inthe 835 50-minute interval, it contains the maximum number of steps (which is 206.1698) on average across all the days

## Imputing missing values

```r
sum(is.na(dt$step))
```

```
## [1] 2304
```

I use  the mean for that 5-minute interval to fill in the missing data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
m=merge(dt,ai,by="interval")
m$steps.x[is.na(m$steps.x)]=m$steps.y[is.na(m$steps.x)]
m=select(m,date,steps=steps.x,interval)
sum(is.na(m$steps))
```

```
## [1] 0
```


```r
sd=aggregate(steps~date,data=m,sum)
hist(sd$steps)
```

![](PA1_template_files/figure-html/histgram-1.png) 

```r
mean(sd$steps)
```

```
## [1] 10766.19
```

```r
median(sd$steps)
```

```
## [1] 10766.19
```

So mean are different,and the median are different. 
The imputing missing data has impaction on median but not on meanof the estimates of the total daily number of steps

## Are there differences in activity patterns between weekdays and weekends?

```r
m$date=as.Date(m$date)
m$day=ifelse(weekdays(m$date)=="Saturday"|weekdays(m$date)=="Sunday","weekend","weekday")
library(lattice)
aid=aggregate(steps~interval+day,data=m,mean)
xyplot(steps~interval|day,aid,type="l",xlab="Interval",ylab="Number of steps",layout=c(1,2))
```

![](PA1_template_files/figure-html/weekday-1.png) 
