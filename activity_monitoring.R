library(ggplot2)
# Code for reading in the dataset and/or processing the data
unzip ("activity.zip")
activity <- read.csv("activity.csv")

# Histogram of the total number of steps taken each day
#Aggregate the total steps taken each day
steps.each.day <- aggregate(steps ~ date, data = activity, FUN = sum)
qplot(as.numeric(steps.each.day$steps), geom="histogram", binwidth=1000, xlab = "Total steps taken each day") 

# Mean and median number of steps taken each day
mean(steps.each.day$steps, na.rm=TRUE)
median(steps.each.day$steps, na.rm=TRUE)

# Time series plot of the average number of steps taken
average.steps <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
ggplot(data=average.steps, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken")

# The 5-minute interval that, on average, contains the maximum number of steps
average.steps[which.max(average.steps$steps),]

# Code to describe and show a strategy for imputing missing data
missing.activity <- is.na(activity$steps)
table(missing.activity)

# Histogram of the total number of steps taken each day after missing values are imputed
activity.imputed <- activity
avg.interval <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE, simplify=TRUE)

# Replace missing values with the average steps in that interval
activity.imputed$steps[missing.activity] <- avg.interval[as.character(activity.imputed$interval[missing.activity])]
total.steps <- aggregate(steps ~ date, data = activity.imputed, FUN = sum)

qplot(as.numeric(total.steps$steps), geom="histogram", binwidth=1000, xlab = "Total steps taken each day with missing value imputed") 

#Mean and Median after missing values are imputed
mean(total.steps$steps)
median(total.steps$steps)

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
library(dplyr)
library(lubridate)
activity.imputed$date <- ymd(activity.imputed$date)
activity.imputed <- mutate(activity.imputed, weektype = ifelse(weekdays(activity.imputed$date) == "Saturday" | weekdays(activity.imputed$date) == "Sunday", "weekend", "weekday"))
activity.imputed$weektype <- as.factor(activity.imputed$weektype)
head(activity.imputed)
interval.activity <- activity.imputed %>%
        group_by(interval, weektype) %>%
        summarise(steps = mean(steps))
g <- ggplot(interval.activity, aes(x=interval, y=steps, color = weektype)) +
        geom_line() +
        facet_wrap(~weektype, ncol = 1, nrow=2)
print(g)
