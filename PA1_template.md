# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
tf <- unzip("./activity.zip")
activity_data <- read.csv(tf)
activity_data$date<-as.Date(activity_data$date)
```


## What is mean total number of steps taken per day?

```r
library(dplyr)
library(ggplot2)
total_steps_per_day <- activity_data %>% 
                        group_by(date) %>% 
                        summarise(number_steps = sum(steps, na.rm = TRUE))
hist(total_steps_per_day$number_steps, xlab = "number of steps", ylim = c(0,35),
     main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/histogram-1.png) 


```r
mean_steps_per_day <- mean(total_steps_per_day$number_steps)
median_steps_per_day <- median(total_steps_per_day$number_steps)
```

The mean of steps taken per day is 
9354.23. The median of 
steps taken perday is 10395.00.


## What is the average daily activity pattern?

```r
average_steps_by_interval <- activity_data %>% 
                                group_by(interval) %>%
                                summarise(average_steps_by_interval = 
                                            mean(steps, na.rm = TRUE))
g <- ggplot(average_steps_by_interval, 
            aes(x=interval, y=average_steps_by_interval))
g + geom_line() + labs(title ="Average steps by interval", x = "Interval", 
                       y = "Average steps")
```

![](PA1_template_files/figure-html/time series intervals-1.png) 

```r
interval_max_average <- average_steps_by_interval$interval[which.max(
    average_steps_by_interval$average_steps_by_interval)]
```

The interval that contains the max average number of steps is 
835.

## Imputing missing values

```r
total_na <- sum(is.na(activity_data$steps))
```

The total number of missing values in the database is 2304. 


```r
ind_missing_intervals <- which(is.na(activity_data$steps))
#Replace the missing values with rounded step averages for matching interval on 
#other days
interpolated_steps <- replace(activity_data$steps, ind_missing_intervals, floor(average_steps_by_interval$average_steps_by_interval))

activity_data_interpolated <- data.frame(steps = interpolated_steps, 
                                         date = activity_data$date, 
                                         interval = activity_data$interval)
#Create histogram
total_steps_per_day_interpolated <- activity_data_interpolated %>% 
                        group_by(date) %>% 
                        summarise(number_steps = sum(steps, na.rm = TRUE))

hist(total_steps_per_day_interpolated$number_steps, xlab = "number of steps", 
     ylim = c(0,35), 
     main = "Total number of steps taken each day (filling missing values)")
```

![](PA1_template_files/figure-html/filling missing values-1.png) 


```r
mean_steps_per_day_interpolated <- mean(total_steps_per_day_interpolated$number_steps)
median_steps_per_day_interpolated <- median(total_steps_per_day_interpolated$number_steps)
```
The strategy used to fill the missing values was to replace the missing values 
with the rounded number of average steps of matching intervals of other days 
that data was collected.

The average steps taken per day after filling the missing values is 
9354.23, and the median of steps taken per day after filling the 
missing values 10395. The average steps increased by 
14.92%, and 
the median by 
2.37%. 
In terms of impact, the estimated steps created an extra 
85128
steps in the elapsed 61 days of data.

## Are there differences in activity patterns between weekdays and weekends?

```r
weekends <- weekdays(activity_data_interpolated$date) %in% 
    c("Saturday", "Sunday")
activity_data_interpolated$type_day <- factor(
    ifelse(weekends, "weekday", "weekend"))
# Calculate the average number of steps taken, averaged across all weekday days
# and weekend days
average_steps_by_interval <- activity_data_interpolated %>% 
                                     group_by(interval,type_day) %>%
                                     summarise(average_steps_by_interval = 
                                     mean(steps, na.rm = TRUE))
# Plot time series for weekdays and weekends
library(lattice)
xyplot(average_steps_by_interval ~ interval | type_day, 
       data = average_steps_by_interval, layout = c(1, 2), type = "l", 
       ylab = "number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 
