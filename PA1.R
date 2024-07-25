#loading necessary libraries
library(readr)
library(tidyverse)

# Specifying the URL of the zip file
zip_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Specify the destination to save the downloaded file in the Git Repo
zip_file <- "activity.zip"

# Download the file
download.file(zip_url, zip_file, mode = "wb")

# Since the file is already in the repo all I need is to
#loading the file
activity <- read_csv("activity.csv")

dim(activity)
head(activity)

activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
str(activity)

#1.calculating total steps taken per day
ts <- activity |> 
        group_by(date) |> 
        summarize(total_steps = sum(steps, na.rm=TRUE))

print(ts, n=61)

# 2.plotting histogram with steps per day
ggplot(data=ts, aes(x=total_steps))+geom_histogram(bins = 61)

# 3. Calculate and report the mean and median of the total number of steps taken per day
summary(ts$total_steps, na.rm=TRUE)


#1. Make a time series
## Calculate the average number of steps per interval
avg_steps <- activity %>%
        group_by(interval) %>%
        summarize(avg_steps = mean(steps, na.rm = TRUE))

str(average_steps)

#Creating a times series plot
ggplot(avg_steps, aes(x = interval, y = avg_steps)) +
        geom_line() +
        labs(title = "Average Number of Steps per 5-Minute Interval",
             x = "5-Minute Interval",
             y = "Average Number of Steps")

#Calculating which day has the maximum average step 
# Find the interval with the maximum average steps
max_interval <- avg_steps %>%
        filter(avg_steps == max(avg_steps, na.rm = TRUE))

print(max_interval)
#ANSWER = interval 835


#######IMPUTING MISSING VALUES

##1.calculating the total number of rows with missing values
total.na <- sum(is.na(activity$steps))

print(total.na)      




# 2. Impute missing values with the previous line's information
# Replace each missing value with the mean value of its 5-minute interval
# Function to fill in the missing values
# Separate valid and NA steps
actv <- activity
actv1 <- filter(actv, !is.na(steps))
actv0 <- filter(actv, is.na(steps))

# Loop to fill NA steps with mean steps for the corresponding interval
for (i in unique(actv0$interval)) {
        actv0[actv0$interval == i, "steps"] <- avg_steps$avg_steps[avg_steps$interval == i]
}

# Combine the data back together
filled_data <- bind_rows(actv1, actv0) %>%
        arrange(date, interval)

# Print the result
head(filled_data)




##7. Histogram of the total number of steps 
##taken each day after missing values are imputed:

agg_step_new <- aggregate(filled_data$steps, by = list(filled_data$date), FUN = "sum")
hist(agg_step_new$x, col="sienna", main = "Histogram plot of Total Number Steps", xlab = "Total Steps")


# Re-calculate mean and median of Total number of steps per day after filling missing values
mnval_new <- mean(agg_step_new$x)
mdval_new <- median(agg_step_new$x)

#ANSWER: Observation: The mean and median values are highrer after 
#replacing missing values comparing original dataset.


#8. Panel plot comparing the average number of steps taken per 
#5-minute interval across weekdays and weekends


# Add a new column 'day' to hold day of the week
actv_new <- mutate(filled_data, day = weekdays(date))
# Add day category as 'weekend' and 'weekday'
actv_new$day_catg <- if_else(actv_new$day %in% c('Sunday','Saturday'), 'weekend', 'weekday')
actv_new$day_catg <- factor(actv_new$day_catg)
str(actv_new)


#Make a panel plot containing a time series plot of the 
#5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

avg_step_week <- aggregate(steps ~ interval + day_catg, data = actv_new, FUN = "mean")
head(avg_step_week)


#creating the graph

g <- ggplot(data = avg_step_week, aes(x = interval, y = steps))
g <- g + facet_grid(day_catg ~ .)
g <- g + geom_line()
g <- g + labs(title = "Average Steps taken across weekday/weekend per Interval")
g <- g + labs(x = "Interval", y = "Average Steps")
print(g)
