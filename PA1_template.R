##  Reproducible Research: R Code Peer Assessment 1

##  Load and process data
data <- read.csv("activity.csv")

##  What is mean number of steps per day?
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)

##  What is the average daily activity pattern?
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")


##  On average across all the days in the dataset
##  Which 5-minute interval contains the maximum number of steps?
averages[which.max(averages$steps),]

##  Imputing missing values
##  There are many days/intervals where there are missing values (coded as NA)
##  The presence of missing days may introduce bias into 
##  Calculations or summaries of the data
##  If the missing data is not random
missing <- is.na(data$steps)

## How many missing
table(missing)

##  All of the missing values are filled in with mean value for that 5-minute interval
##  Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

##  Using the filled data set make a histogram of 
##  Total number of steps taken each day
##  Calculate the mean and median total number of steps.

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)

##  Determine if there are differences in activity patterns
##  Between weekdays and weekends
##  Using the dataset with filled in values

weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

##  Make a panel plot
##  Plotting average number of steps taken on weekdays and on weekends

averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")