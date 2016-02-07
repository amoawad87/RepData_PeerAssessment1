# Reproducible Research: Peer Assessment 1


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
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
## Warning: package 'ggplot2' was built under R version 3.2.3
```

## Loading and preprocessing the data

```r
unzip(zipfile="activity.zip")
data_row <- read.csv('activity.csv')

# Remove NA in data

data <- data_row[ with (data_row, { !(is.na(steps)) } ), ]
```

## What is mean total number of steps taken per day?

```r
steps_by_day <- summarise(group_by(data, date), total = sum(steps))
hist(steps_by_day$total, main="Histogram of total number of steps taken per day", 
     xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
mean(steps_by_day$total)
```

```
## [1] 10766.19
```

```r
median(steps_by_day$total)  
```

```
## [1] 10765
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
