---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
activityData <- read.csv(file="activity.csv", header=TRUE)
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such
## file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

## What is mean total number of steps taken per day?


```r
# The total steps taken per day
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)
```

```
## Error in eval(m$data, parent.frame()): object 'activityData' not found
```

```r
# Histogram of the total number of steps taken per day
hist(totalSteps$steps, main = "Total Steps per Day", xlab = "Number of Steps")
```

```
## Error in hist(totalSteps$steps, main = "Total Steps per Day", xlab = "Number of Steps"): object 'totalSteps' not found
```

```r
# Calculation and a report of the mean and median of total steps taken per day
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
```

```
## Error in mean(totalSteps$steps, na.rm = TRUE): object 'totalSteps' not found
```

```r
medSteps <- median(totalSteps$steps, na.rm = TRUE)
```

```
## Error in median(totalSteps$steps, na.rm = TRUE): object 'totalSteps' not found
```


## What is the average daily activity pattern?


```r
# 2 - Make a time-series plot of the 5-minute interval and the average number of
# steps taken, averaged acoss all days.

library(ggplot2)
meanStepsByInt <- aggregate(steps ~ interval, activityData, mean)
```

```
## Error in eval(m$data, parent.frame()): object 'activityData' not found
```

```r
ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) +
        geom_line() +
        ggtitle("Average Daily Activity Pattern") +
        xlab("5-minute Interval") +
        ylab("Average Number of Steps") +
        theme(plot.title = element_text(hjust = 0.5))
```

```
## Error in ggplot(data = meanStepsByInt, aes(x = interval, y = steps)): object 'meanStepsByInt' not found
```

```r
# 5-minute interval across all days contain the maximum number of steps
maxInt <- meanStepsByInt[which.max(meanStepsByInt$steps),]
```

```
## Error in eval(expr, envir, enclos): object 'meanStepsByInt' not found
```


## Imputing missing values


```r
# Calculation and a report for the total number of missing values in the dataset
missingVals <- is.na(activityData$steps)
```

```
## Error in eval(expr, envir, enclos): object 'activityData' not found
```

```r
# Creating a new dataset that is equal to the original dataset but with 
# the missing data filled in.
imp_activityData <- transform(activityData,
                              steps = ifelse(is.na(activityData$steps),
                                             meanStepsByInt$steps[match(activityData$interval, 
                                                                        meanStepsByInt$interval)],
                                             activityData$steps))
```

```
## Error in transform(activityData, steps = ifelse(is.na(activityData$steps), : object 'activityData' not found
```

```r
#  histogram of the total number of steps taken each day and
# and report the mean and median.
impStepsByInt <- aggregate(steps ~ date, imp_activityData, FUN=sum)
```

```
## Error in eval(m$data, parent.frame()): object 'imp_activityData' not found
```

```r
hist(impStepsByInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")
```

```
## Error in hist(impStepsByInt$steps, main = "Imputed Number of Steps Per Day", : object 'impStepsByInt' not found
```

```r
impMeanSteps <- mean(impStepsByInt$steps, na.rm = TRUE)
```

```
## Error in mean(impStepsByInt$steps, na.rm = TRUE): object 'impStepsByInt' not found
```

```r
impMedSteps <- median(impStepsByInt$steps, na.rm = TRUE)
```

```
## Error in median(impStepsByInt$steps, na.rm = TRUE): object 'impStepsByInt' not found
```

```r
diffMean = impMeanSteps - meanSteps
```

```
## Error in eval(expr, envir, enclos): object 'impMeanSteps' not found
```

```r
diffMed = impMedSteps - medSteps
```

```
## Error in eval(expr, envir, enclos): object 'impMedSteps' not found
```

```r
diffTotal = sum(impStepsByInt$steps) - sum(totalSteps$steps)
```

```
## Error in eval(expr, envir, enclos): object 'impStepsByInt' not found
```


## Are there differences in activity patterns between weekdays and weekends?


```r
# Creating a new factor variable in the dataset with two levels - "weekend" and "weekday"
DayType <- function(date) {
        day <- weekdays(date)
        if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
                return ("weekeday")
        else if (day %in% c('Saturday', 'Sunday'))
                return ("weekend")
        else
                stop ("Invalid Date Format.")
}
imp_activityData$date <- as.Date(imp_activityData$date)
```

```
## Error in as.Date(imp_activityData$date): object 'imp_activityData' not found
```

```r
imp_activityData$day <- sapply(imp_activityData$date, FUN = DayType)
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 'imp_activityData' not found
```

```r
# Making a panel plot containnig a time-series plot of the 5-minute interval
# and the average number of steps taken across all weekdays or weekends
meanStepsByDay <- aggregate(steps ~ interval + day, imp_activityData, mean)
```

```
## Error in eval(m$data, parent.frame()): object 'imp_activityData' not found
```

```r
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + 
        geom_line() +
        facet_grid(day ~ .) +
        ggtitle("Average Daily Activity Pattern") +
        xlab("5-minute Interval") +
        ylab("Average Number of Steps") +
        theme(plot.title = element_text(hjust = 0.5))
```

```
## Error in ggplot(data = meanStepsByDay, aes(x = interval, y = steps)): object 'meanStepsByDay' not found
```
