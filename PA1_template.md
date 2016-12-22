# Reproducible Research: Peer Assessment 1
Olga Ermakova  


## Loading and preprocessing the data

Read the data. It is possible to read the original zipped file. Then we print the first 6 lines of data.


```r
  data_fitness <- read.table(file = unz(description = "activity.zip", filename = "activity.csv"), header = T, sep = ",", stringsAsFactors = F)
  head(data_fitness)
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


## What is mean total number of steps taken per day?

For this part we ignore the missing values (NA). We remove the rows with NA using built-in function *complete.cases*. We calculate the total number of steps taken each day with function *tapply*.


```r
  data_fitness_without_na <- data_fitness[complete.cases(data_fitness),]
  steps_per_day = with(data_fitness_without_na, {
    tapply(X = steps, INDEX = date, FUN = sum)
    }
    )
  mean_steps = signif(mean(steps_per_day), digits = 4)
  median_steps = median(steps_per_day)
  barplot(height = steps_per_day
          , names.arg = names(steps_per_day)
          , las = 2, cex.names = 0.8)
```

![](PA1_template_files/figure-html/second-1.png)<!-- -->

We calculate and report the mean = 1.077\times 10^{4} and median = 10765 total number of steps taken per day


## What is the average daily activity pattern?


```r
  averaged_activity = tapply(data_fitness_without_na$steps, data_fitness_without_na$interval, mean)
  plot(x = 1:length(averaged_activity), y = averaged_activity, type = "l", xaxt = "n", xlab = "Interval", ylab = "Averaged Number of Steps", lwd = 2, main = "Average daily activity pattern")
  axis(side = 1, at = 1:length(averaged_activity), labels = names(averaged_activity))
```

![](PA1_template_files/figure-html/third-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
  the_most_active_interval = names(which.max(averaged_activity))
```

It is the interval 835, which contains the maximum number of steps.

## Imputing missing values

We use the mean for the respective 5-minute interval to impute the missing data.


```r
  number_na = nrow(data_fitness) - nrow(data_fitness_without_na)

  imputed_data_fitness = data_fitness
  
  # simple for-loop
  # take the respective value from averaged_activity
  
  for(i in 1:nrow(imputed_data_fitness)){
    if(is.na(imputed_data_fitness$steps[i])){
      imputed_data_fitness$steps[i] <- averaged_activity[as.character(imputed_data_fitness$interval[i])]
      }
  }
```


```r
    steps_per_day_imputed = with(imputed_data_fitness, {
    tapply(X = steps, INDEX = date, FUN = sum)
    }
    )
  mean_steps_imputed = signif(mean(steps_per_day_imputed), digits = 4)
  median_steps_imputed = median(steps_per_day_imputed)
  barplot(height = steps_per_day_imputed
          , names.arg = names(steps_per_day_imputed)
          , las = 2, cex.names = 0.8)
```

![](PA1_template_files/figure-html/sixth-1.png)<!-- -->

We calculate and report the changed mean = 1.077\times 10^{4} and median = 1.0766189\times 10^{4} total number of steps taken per day. The mean value has not changed (not by chance, its math: adding the means of the strata does not change the overall mean) and the median has slightly increased.


## Are there differences in activity patterns between weekdays and weekends?


```r
  imputed_data_fitness$weekdays = sapply(as.Date(imputed_data_fitness$date), weekdays)
  imputed_data_fitness$weekdays = factor(ifelse(imputed_data_fitness$weekdays %in% c("Saturday","Sunday"), yes = "weekend", no = "weekday"))
  
  library(dplyr)
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
  by.weekdays.intervals = group_by(imputed_data_fitness,weekdays,interval)
  summarized.data = summarise(by.weekdays.intervals, mean_steps = mean(steps))
```


```r
  library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
  plot.activity = ggplot(summarized.data, aes(x = interval, y = mean_steps)) + geom_line() + facet_grid(weekdays ~ .)
  plot.activity
```

![](PA1_template_files/figure-html/eights-1.png)<!-- -->

