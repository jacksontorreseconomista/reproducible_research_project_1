
unzip("repdata-data-activity.zip")
activity_data <- read.csv("activity.csv", header = TRUE)
actdata <- na.omit(activity_data)

head(actdata)
dim(actdata)
str(actdata)

total.steps <- tapply(actdata$steps, actdata$date, FUN=sum, na.rm=TRUE)
 

hist(total.steps, col = "blue", 
     breaks = 20,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day")

meansteps <- mean(total.steps, na.rm=TRUE)
mediansteps <- median(total.steps, na.rm=TRUE)

print('The mean of total steps per day was ') 
print(meansteps)
print('The median of total steps per day was ') 
print(mediansteps)

library(ggplot2)
averages <- aggregate(x=list(steps=actdata$steps), by=list(interval=actdata$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")

averages[which.max(averages$steps),]
To find the missing values, a filter was performed on the original data set. Getting to the result below.

na_values <- length(which(is.na(activity_data$steps)))
print (na_values)

fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- activity_data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
steps.pd.wt.na <- aggregate(filled.data$steps, 
                                by = list(Steps.Date = filled.data$date), 
                                FUN = "sum")
hist(steps.pd.wt.na$x, col = "green", 
     breaks = 20,
     main = "Total number of steps taken each day (filled data)",
     xlab = "Number of steps per day")
mean(total.steps)
median(total.steps)

weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}

filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)


averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
