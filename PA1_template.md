---
title: "Peer Assessment 1"
author: "Murilo Pisciotto"
date: "Thursday, May 14, 2015"
output: html_document
---
# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
```{r}
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "repdata-data-activity.zip", mode="wb")
unzip("repdata-data-activity.zip")
data <- read.csv("activity.csv")
```
## What is mean total number of steps taken per day?
```{r}
library(ggplot2)
total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps, na.rm=TRUE)
median(total_steps, na.rm=TRUE)
```
## What is the average daily activity pattern?
```{r}
library(ggplot2)
average <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=average, aes(x=interval, y=steps)) +
    geom_line() +
    geom_point(colour="red",alpha=2/10) +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```
On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?
```{r}
average[which.max(average$steps),]
```
## Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

```{r how_many_missing}
missing <- is.na(data$steps)
# How many missing
table(missing)
```
The missing values are completed with mean value for that 5-minute
interval.

```{r}
# Replace each missing value with the mean value of its 5-minute interval
value <- function(steps, interval) {
    complet <- NA
    if (!is.na(steps))
        complet <- c(steps)
    else
        complet <- (average[average$interval==interval, "steps"])
    return(complet)
}
fill_data <- data
fill_data$steps <- mapply(value, fill_data$steps, fill_data$interval)
```
With the fill_data set, use histogram for the total number of steps taken each day and compute the mean and median total number of steps.

```{r}
total_steps <- tapply(fill_data$steps, fill_data$date, FUN=sum)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps)
median(total_steps)
```
The mean and median values are higher after imputing missing data. In the original data
there are some days with `steps` values `NA` for 
any `interval`. After replacing missing `steps` values with the mean `steps`
for the associated `interval` value, these 0 values are removed from the plot
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.

```{r}
weekday_weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
fill_data$date <- as.Date(fill_data$date)
fill_data$day <- sapply(fill_data$date, FUN=weekday_weekend)
```
Use facet_grid to compara average number of steps taken
on weekdays and weekends.
```{r}
average <- aggregate(steps ~ interval + day, data=fill_data, mean)
ggplot(average, aes(interval, steps)) +
  geom_line() +
  geom_smooth() +
  geom_point(aes(colour=day),alpha=2/10) +
  facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```
