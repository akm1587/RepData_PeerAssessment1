Peer Assessment 1
========================================================

This markdown document describes the analysis done on the "activity.zip" file for the Coursera Reproducible Research course, peer assignment 1.

### Loading and Preprocessing the data

First, change the working directory (unique to my directory structure) where 'activity.csv' file is housed. Store the data into a variable named 'dat'. I also chose to convert the dates into the R date class.


```r
setwd("~/datascispec/repdata/")
dat <- read.csv("activity.csv")
dates <- as.Date(dat$date, format = "%Y-%m-%d")
```


### What is mean total number of steps taken per day?

Next, use the aggregate function to calculate the average number of steps per day. Group by the 'steps' column by the date, and taking the sum.


```r
stepsbyday <- aggregate(dat$steps, by = list(dates), sum)
```


Make a histogram of the total number of each steps for each day, which is the second column in 'stepsbyday' (the first is the date).


```r
hist(stepsbyday[, 2], xlab = "Number of Steps", main = "Number of steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


Print the mean and the median of the total steps per day, ignoring any NA's. 


```r
mean(stepsbyday[, 2], na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(stepsbyday[, 2], na.rm = TRUE)
```

```
## [1] 10765
```


Based on these numbers it looks like the average number of steps per day is about 10000.

### What is the average daily activity pattern?

The 'interval' column of dat is the time during the day in 5-minute intervals. I chose to leave it in terms of the interval numbers rather than converting it to hours and minutes to stay consistent with the example plot given.

To look at the number of steps taken in each 5-minute interval, averaged over all days, use aggregate again and group by interval, taking the mean in each interval bin. Setting 'na.rm=TRUE' means that the function 'mean' will remove any NA's.


```r
stepsbyint <- aggregate(dat$steps, list(dat$interval), mean, na.rm = TRUE)
```


Now make a time series plot. 


```r
plot(stepsbyint[, 1], stepsbyint[, 2], type = "l", xlab = "Interval", ylab = "Steps per Interval", 
    main = "Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


To find the part of the day with the most steps, use the 'which.max' function which returns the index of the maximum value. Take that index and see which interval value it corresponds to.


```r
dat$interval[which.max(stepsbyint[, 2])]
```

```
## [1] 835
```


The time with the most steps is 8:35 AM, which makes sense as most people are going to work/school around that time.

### Imputing missing values

The total number of missing values in the dataset is:

```r
sum(is.na(dat$steps))
```

```
## [1] 2304
```


I chose to replace the missing values with the average value of steps in the 5-minute interval. In the code below, 'missingvals' is vector of the indices for which steps is missing. 'stepsfill' is a the new vector with the filled in steps values. 'uniqueintervals' is a vector with the unique interval values.


```r
missingvals <- which(is.na(dat$steps))
stepsfill <- dat$steps
uniqueintervals <- sort(unique(dat$interval))
```


This loop goes through each missing value, stores the interval for that missing value, then finds the corresponding average step value for that interval. That average value is stored in 'stepsfill'.

```
for (i in 1:length(missingvals)) {
        missinginterval <- dat$interval[missingvals[i]]    
        intervalindex <- which(missinginterval==uniqueintervals)        
        stepsfill[missingvals[i]] <- stepsbyint[intervalindex, 2]
}

```

'stepsfill' is the new dataset with the missing values filled in. First, use aggregate to re-calculate the sum of the number of steps for each day.


```r
stepsfillbyday <- aggregate(stepsfill, by = list(dates), sum)
```


Here's a histogram of the new step values.


```r
hist(stepsfillbyday[, 2], xlab = "Number of Steps (NA's removed)", main = "Number of steps per day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


Print the mean and median (the NA's shouldn't pose a problem, but leave na.rm=TRUE anyways).


```r
mean(stepsfillbyday[, 2], na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(stepsfillbyday[, 2], na.rm = TRUE)
```

```
## [1] 10765
```


The mean and median do not change by much, but this is because I chose to use average values to replace missing values.

### Are there differences in activity patterns between weekdays and weekends?

First, convert the dates into the days of the week using the 'weekdays' function. Loop through all 'days' and for each day, store 'Weekend' into week category is the day of the week is Saturday or Sunday, and 'Weekday' otherwise.


```r
days <- weekdays(dates)
weekcategory <- vector()
for (i in 1:length(days)) {
    if (days[i] == "Sunday" | days[i] == "Saturday") {
        weekcategory[i] <- "Weekend"
    } else weekcategory[i] <- "Weekday"
}
```


The 'weekcategory' vector can be used as a factor. The 'stepsweek' data.frame is the average number of steps, grouped by the interval and week category.


```r
stepsweek <- aggregate(stepsfill, by = list(dat$interval, weekcategory), mean)
names(stepsweek) <- c("Interval", "Week", "Steps")
```


Using qplot in ggplot2, plot number of steps vs interval for weekdays and weekends. The 'facet' option plots the results in two panels. First load ggplot2 then plot.


```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```



```r
qplot(Interval, Steps, data = stepsweek, facets = . ~ Week, geom = "line", xlab = "Interval", 
    ylab = "Number of Steps")
```

```
## Error: 'from' must be of length 1
```

