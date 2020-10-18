### Load the data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activity.zip",method="curl")
unzip(zipfile="./data/activity.zip",exdir="./data")
data <- read.csv("./data/activity.csv")
data$date <- as.Date(data$date)


### Find the mean total number of steps taken per day
options(warn=-1)
library(ggplot2)
total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total_steps, binwidth=1000, xlab="Total number of steps taken each day",
      ylab = "Frequency")
mean(total_steps, na.rm=TRUE)
median(total_steps, na.rm=TRUE)

### Find the average daily activity pattern
library(ggplot2)
aver <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                  FUN=mean, na.rm=TRUE)
g<-ggplot(data=aver, aes(x=interval, y=steps)) 
g+ geom_line() +xlab("5-minute interval") + 
  ylab("Average number of steps taken averaged across all days")

aver[which.max(aver$steps),]

### Impute missing values
missing <- is.na(data$steps)
table(missing)
###
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (aver[aver$interval==interval, "steps"])
  return(filled)
}

data_filled<- data
data_filled$steps <- mapply(fill.value, data_filled$steps, data_filled$interval)

total_steps <- tapply(data_filled$steps, data_filled$date, FUN=sum)
qplot(total_steps, binwidth=1000, xlab="Total number of steps taken each day")
mean(total_steps)
median(total_steps)


### Check any difference in activity patterns between weekdays and weekends
Wday_OR_Wend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
}
data_filled$date <- as.Date(data_filled$date)
data_filled$day <- sapply(data_filled$date, FUN=Wday_OR_Wend)

aver <- aggregate(steps ~ interval + day, data=data_filled, mean)
g<-ggplot(aver, aes(interval, steps)) 
g+ geom_line() + facet_grid(day ~ .) +xlab("5-minute Interval") + 
  ylab("Number of steps taken, averaged across all weekday or weekend days")

