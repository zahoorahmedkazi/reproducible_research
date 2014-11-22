#Loading and preprocessing the data
unzip("activity.zip")
activity <- read.csv("activity.csv")


#What is the mean total number of steps taken per day?
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")
mean(steps.date$steps)
median(steps.date$steps)


#What is the average daily activity pattern?
steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps.interval, type = "l")
steps.interval$interval[which.max(steps.interval$steps)]


#Imputing missing values
sum(is.na(activity))
activity <- merge(activity, steps.interval, by = "interval", suffixes = c("",".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")
mean(steps.date$steps)
median(steps.date$steps)

# Are there differences in activity patterns between weekdays and weekends?
daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
  steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$daytype == 
                            type, FUN = mean)
  plot(steps.type, type = "l", main = type)
}