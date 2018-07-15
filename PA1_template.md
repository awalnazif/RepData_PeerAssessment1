#Peer-graded Assignment: Course Project 1  
##Name: Awal Nazif  
Email address: awalnazif@yahoo.com  
========================================================================  

##Loading the required packages:

```{r packages}
require(dplyr)
require(ggplot2)
```

##Loading and preprocessing the data

Loading and viewing the structure of the dataset using read,csv () and str()

```{r data}
data <- read.csv(file = "activity.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
str(data)
```

Viewing the head (top) and tail (bottom) of the data to ensure the data is loaded properly

```{r head}
head(x = data, n = 10)
tail(x = data, n = 10)
```

Transforming the class of the date variable to date class 

```{r date}
data <- mutate(.data = data, date = as.Date(x = date))
with(data = data, expr = class(date))
```

##The mean total number of steps taken per day

Total number of steps taken per day

```{r sum}
group_by(.data = data, date) %>% summarise(Total = sum(steps, na.rm = TRUE))
```

Histogram of the total number of steps taken each day

```{r histogram}
group_by(.data = data, date) %>% summarise(steps = sum(steps, na.rm = TRUE)) %>% ggplot() + geom_histogram(mapping = aes(steps)) + xlab("Total number of steps taken each day")
```

Mean and Median of the total number of steps taken per day

```{r central tendency}
group_by(.data = data, date) %>% summarise(Mean = mean(x = steps, na.rm = TRUE), Median = median(x = steps, na.rm = TRUE))
```

##The average daily activity pattern

Time series plot of the 5-minute interval and average number of steps taken  

```{r series plot}
group_by(.data = data, interval) %>% summarise(Average = mean(x = steps, na.rm = TRUE)) %>% ggplot() + geom_line(mapping = aes(x = interval, y = Average)) + ylab(label = "Average Steps per Day")
```

The 5-minute interval with the maximum number of steps  

```{r maximim steps}
group_by(.data = data, interval) %>% summarise(Average = mean(x = steps, na.rm = TRUE)) %>% filter(Average == max(Average))
```

The 5-minute interval with maximum number of steps is **835**.

##Imputing missing values  

The total number of missing values in the dataset

```{r missing values}
head(x = filter(.data = data, is.na(steps)), n = 10)
tail(x = filter(.data = data, is.na(steps)), n = 10)
rows <- nrow(filter(.data = data, is.na(steps)))
```

The total number of missing values in the dataset is **`r rows`**.  

Creating a new dataset from the original dataset filling the missing data with the average of the 5-minutes interval

```{r new dataset}
new_data <- group_by(.data = data, interval) %>% mutate(steps = ifelse(test = is.na(steps), yes = mean(x = steps, na.rm = TRUE), no = steps))
head(x = new_data, n = 10)
tail(x = new_data, n = 10)
dim(x = new_data)
```

Mean and Median of the total number of steps taken per day in the new dataset

```{r central tendency 2}
group_by(.data = new_data, date) %>% summarise(Mean = mean(x = steps), Median = median(x = steps))
```

Histogram of the total number of steps taken each day in the new dataset

```{r histogram 2}
group_by(.data = new_data, date) %>% summarise(steps = sum(steps)) %>% ggplot() + geom_histogram(mapping = aes(x = steps)) + xlab("Total number of steps taken each day")
```

##Differences in activity patterns between weekdays and weekends  

Creating a new factor variable in the dataset  

```{r factor variable}
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
mutate(.data = new_data, Day = as.factor(x = ifelse(test = weekdays(date) %in% weekday, yes = "weekday", no = "weekend")))
```

Making a time series panel plot 

```{r panel plot}
mutate(.data = new_data, Day = as.factor(x = ifelse(test = weekdays(date) %in% weekday, yes = "weekday", no = "weekend"))) %>% group_by(interval, Day) %>% summarise(Average = mean(x = steps)) %>% ggplot() + geom_line(mapping = aes(x = interval, y = Average)) + facet_grid(Day ~ .) + ylab(label = "Average Steps Taken")
```
