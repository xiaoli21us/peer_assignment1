---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
---
Loading and preprocessing the data

```{r}
data <- read.csv("./activity.csv")

## Sum steps by Date------------------------------------------------------------------------
library(ggplot2)
sum_by_step <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)


```

Plot the result

```{r, echo=FALSE}
qplot(sum_by_step, binwidth=1000, xlab="total number of steps taken each day")
mean(sum_by_step, na.rm=TRUE)
median(sum_by_step, na.rm=TRUE)
```
What is the average daily activity pattern?

```{r}

## Average daily acitivily------------------------------------------------------------------------

averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)

```

Plot the result

```{r, echo=FALSE}
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
averages[which.max(averages$steps),]


```

Imputing missing values and calculate and report the mean and median total number of steps taken per day

```{r}
## ----how_many_missing----------------------------------------------------
missing <- is.na(data$steps)
# How many missing
table(missing)

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

```{r, echo=FALSE}
sum_by_step <- tapply(filled_result$steps, filled_result$date, FUN=sum)
qplot(sum_by_step, binwidth=1000, xlab="total number of steps taken each day")
mean(sum_by_step)
median(sum_by_step)
```


Are there differences in activity patterns between weekdays and weekends?

```{r}

filled_result$date <- as.Date(filled_result$date)

## ------------------------------------------------------------------------

week <- factor(weekdays(filled_result$date) %in% c("Saturday","Sunday"), 
               labels=c("weekday","weekend"), ordered=FALSE)

impsteps <- aggregate(filled_result$steps, by=list(interval=filled_result$interval, weekday=week), mean)

```

Plot the result

```{r, echo=FALSE}
library(ggplot2)
g <- ggplot(impsteps, aes(interval/60, x))
g + geom_line() + facet_grid(weekday ~ .) +
    scale_x_continuous(breaks=0:6*4, labels=paste(0:6*4,":00", sep="")) +
    theme_bw() +
    labs(y="average number of steps in 5-min interval") +
    labs(x="time of day (h)") +
    labs(title="Daily activity pattern")


```
