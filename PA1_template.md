In this project, the goal is to do a data exploratory and analysis from
a personal activity monitoring device. This mechanism collects data at 5
minute intervals during the day. The observations contain two months of
data from an anonymous individual collected between October and
November, 2012 and englobe the number of steps taken in 5 minute
intervals each day.

1. Loading and preprocessing the data
-------------------------------------

The first step was to unzip and read the file, later eliminating the
lines without observations. Then an analysis of the data set was carried
out to verify the number of variables and observations and the format in
which these observations were recorded.

    unzip("repdata-data-activity.zip")
    activity_data <- read.csv("activity.csv", header = TRUE)
    actdata <- na.omit(activity_data)

What is mean total number of steps taken per day?
-------------------------------------------------

To analyze the total number of steps each day, a grouping of data was
performed and then a histogram was generated.

    total.steps <- tapply(actdata$steps, actdata$date, FUN=sum, na.rm=TRUE)

    hist(total.steps, col = "blue", 
         breaks = 20,
         main = "Total number of steps taken each day",
         xlab = "Number of steps per day")

![](PA1_template_files/figure-markdown_strict/plot_1-1.png)

From this manipulation, the mean and median mean of the total number of
steps per day were calculated.

    meansteps <- mean(total.steps, na.rm=TRUE)
    mediansteps <- median(total.steps, na.rm=TRUE)

    ## [1] "The mean of total steps per day was "

    ## [1] 10766.19

    ## [1] "The median of total steps per day was "

    ## [1] 10765

3. What is the average daily activity pattern?
----------------------------------------------

To find the average daily activity pattern, first it was made the plot
of the number of steps taken averaged across all days, along all 5-min
intervals.

    library(ggplot2)
    averages <- aggregate(x=list(steps=actdata$steps), by=list(interval=actdata$interval),
                          FUN=mean, na.rm=TRUE)
    ggplot(data=averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken")

![](PA1_template_files/figure-markdown_strict/plot_2-1.png)

From the manipulation of the data, the result was the average of the
maximum number of steps in each day in the interval of 5 minutes.

    averages[which.max(averages$steps),]

    ##     interval    steps
    ## 104      835 206.1698

4. Imputing missing values
--------------------------

To find the missing values, a filter was performed on the original data
set. Getting to the result below.

    na_values <- length(which(is.na(activity_data$steps)))
    print (na_values)

    ## [1] 2304

To improve the analysis and the model, the missing values were filled in
with the average of the values, as indicated below.

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

Then the histogram was produced to analyze the results and the mean and
median, which consist of the same result were calculated, and are
approximated to the results without considering the missing values.

    total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
    steps.pd.wt.na <- aggregate(filled.data$steps, 
                                    by = list(Steps.Date = filled.data$date), 
                                    FUN = "sum")
    hist(steps.pd.wt.na$x, col = "green", 
         breaks = 20,
         main = "Total number of steps taken each day (filled data)",
         xlab = "Number of steps per day")

![](PA1_template_files/figure-markdown_strict/plot_3-1.png)

    mean(total.steps)

    ## [1] 10766.19

    median(total.steps)

    ## [1] 10766.19

5. Are there differences in activity patterns between weekdays and weekends?
----------------------------------------------------------------------------

For this question, a new variable was generated in the data set,
identifying whether the date is a weekday or a weekend.

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

Finally, a graph of the number of steps was generated for all 5-minute
intervals, on average between the days of the week and weekends
separately.

    averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")

![](PA1_template_files/figure-markdown_strict/plot_4-1.png)
