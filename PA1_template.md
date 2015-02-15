---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
---
Loading and preprocessing the data


```r
data <- read.csv("./activity.csv")

## Sum steps by Date------------------------------------------------------------------------
library(ggplot2)
```

```
## Need help? Try the ggplot2 mailing list: http://groups.google.com/group/ggplot2.
```

```r
sum_by_step <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
```

Plot the result

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```
## [1] 9354.23
```

```
## [1] 10395
```

```
## png 
##   3
```

```
## png 
##   2
```
What is the average daily activity pattern?


```r
## Average daily acitivily------------------------------------------------------------------------

averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
```

Plot the result

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```
##     interval    steps
## 104      835 206.1698
```

```
## png 
##   3
```

```
## png 
##   2
```

Imputing missing values and calculate and report the mean and median total number of steps taken per day


```r
## ----how_many_missing----------------------------------------------------
missing <- is.na(data$steps)
# How many missing
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

```r
## ------------------------------------------------------------------------
# Replace each missing value with the mean value of its 5-minute interval
interval_value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled_result <- data
filled_result$steps <- mapply(interval_value, filled_result$steps, filled_result$interval)
```

Plot the result

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```
## [1] 10766.19
```

```
## [1] 10766.19
```

```
## png 
##   3
```

```
## png 
##   2
```


Are there differences in activity patterns between weekdays and weekends?


```r
filled_result$date <- as.Date(filled_result$date)

## ------------------------------------------------------------------------

week <- factor(weekdays(filled_result$date) %in% c("Saturday","Sunday"), 
               labels=c("weekday","weekend"), ordered=FALSE)

impsteps <- aggregate(filled_result$steps, by=list(interval=filled_result$interval, weekday=week), mean)
```

Plot the result

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```
## png 
##   3
```

```
## png 
##   2
```
