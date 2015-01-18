Reproducible data - Peer Assignment 1
=====================================

To answer the questions, we will need to unpack compressed raw steps data and read it.

```r
unzip('activity.zip')
stepsData <- read.csv('activity.csv')
```

## Part 1 - What is mean total number of steps taken per day?
First, compute the total number of steps taken per day and display the frenquency in a histogram

```r
agg <- aggregate(stepsData$steps, by=list(stepsData$date), FUN=sum, na.rm=TRUE)
hist(agg$x, main="Frequency of Total Steps", xlab="Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

To get the mean and median, we will need to aggregate the number of steps by date

```r
mean1 <- format(round(mean(agg$x), 2), nsmall = 2)
median1 <- format(round(median(agg$x), 2), nsmall = 2)
```

The mean and the median total steps are **9354.23** and **10395.00** respectively.

## Part 2 - What is the average daily activity pattern?
First, we need to find the mean of each time interval across all days

```r
agg2 <- aggregate(stepsData$steps, by=list(stepsData$interval), FUN=mean, na.rm=TRUE)
```

Next, we will need to generate the labels for each time period

```r
timePeriodLabels <- gsub("^([0-9]{2})([0-9]{2})$", '\\1:\\2', sprintf("%04d",stepsData[1:288,]$interval))
agg2$intervalLabel <- timePeriodLabels
```

Then, we plot the timeline

```r
plot(agg2$Group.1, agg2$x, ylab="Steps", xlab="Time Period", type="l", xaxt="n")
axis(1, at=agg2$Group.1, labels=timePeriodLabels)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

The average maximum steps can be obtained by the following:

```r
maximumInterval <- agg2[agg2$x == max(agg2$x),]
```
The maximum average steps is **206.1698113** at interval starting **08:35**.

## Part 3 - Inputing missing values 
Number of missing values

```r
sum(is.na(stepsData$steps))
```

```
## [1] 2304
```

Substitute missing values with their mean steps of the same interval from other days

```r
stepsDataNoNa <- stepsData
stepsDataNoNa$means <- agg2$x
stepsDataNoNa <- within(stepsDataNoNa, steps <- ifelse(is.na(steps), means, steps))
```

Compute the total number of steps taken per day with the substitutions and display the frenquency in a histogram

```r
agg3 <- aggregate(stepsDataNoNa$steps, by=list(stepsDataNoNa$date), FUN=sum, na.rm=TRUE)
hist(agg3$x,main="Frequency of Total Steps (Missing values <- Mean)", xlab="Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

To get the mean and median, we will need to aggregate the number of steps by date

```r
mean3 <- format(round(mean(agg3$x), 2), nsmall = 2)
median3 <- format(round(median(agg3$x), 2), nsmall = 2)
```

The mean and the median total steps are **10766.19** and **10766.19** respectively. With the substitution, more steps are recorded each day as compared to the first part. This has resulted in a higher mean and median.

## Part 4 - Are there differences in activity patterns between weekdays and weekends?
Get the day of week for all steps data entries and add a new factor column for day type

```r
days <- weekdays(as.Date(stepsDataNoNa$date), abbreviate=TRUE)
stepsDataNoNa$day <- ifelse(days=="Sun" | days =="Sat", "weekend", "weekday")
stepsDataNoNa$day <- as.factor(stepsDataNoNa$day)
```

Aggregate data to weekend and weekdays

```r
agg4 <- aggregate(stepsDataNoNa$steps, by=list(stepsDataNoNa$interval,stepsDataNoNa$day), FUN=mean, na.rm=TRUE)
weekdays <- agg4[agg4$Group.2=="weekday",]
weekends <- agg4[agg4$Group.2=="weekend",]
```

Plot the 2 time series

```r
par(mfrow=c(2,1)) 
plot(weekdays$Group.1, weekdays$x, main="Weekdays", ylab="Steps", xlab="Time Period", type="l", xaxt="n")
axis(1, at=weekdays$Group.1, labels=timePeriodLabels)
plot(weekends$Group.1, weekends$x, main="Weekends", ylab="Steps", xlab="Time Period", type="l", xaxt="n")
axis(1, at=weekends$Group.1, labels=timePeriodLabels)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
