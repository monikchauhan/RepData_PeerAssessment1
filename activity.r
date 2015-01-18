
## Loading File
temp <- unz("activity.zip","activity.csv")
activity <- read.csv(temp, header = T, sep = ",")

######What is mean total number of steps taken per day?
echo = TRUE
totalsteps <-aggregate(steps ~ date, activity, sum, na.rm = TRUE)

## 1. Histogram
echo = TRUE
hist(totalsteps$steps,col="blue", xlab = "sum of steps per day", main = "Total no. of steps per day")

## 2.  Mean & Median
###echo = TRUE
mean_totalsteps <- round(mean(totalsteps$steps))
median_totalsteps <- round(median(totalsteps$steps))

print(c("The mean is",mean_totalsteps))
print(c("The median is",median_totalsteps))

###### What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

stepsPerinterval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsPerinterval, type = "l", xlab = "Time Intervals (5-minute)", ylab = "Mean number of steps taken (all Days)", main = "Average number of steps Taken at 5 minute Intervals",  col = "red")

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
MaxStepInterval <- stepsPerinterval[which.max(stepsPerinterval$steps), ]$interval

###### Imputing missing values
## 1. Calculate and report the total number of missing values in the dataset 

missingValues <- sum(!complete.cases(activity))

## 2. Devise a strategy for filling missing values
## missing values will be replaced with mean for that 5-minute interval
MeanStepsPerInterval <- function(interval){
stepsPerinterval[stepsPerinterval$interval==interval,"steps"]
}

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

noNull_activity <- activity

## Filling the missing values with the mean for that 5-minute interval
flag = 0
for (i in 1:nrow(noNull_activity)) {
    if (is.na(noNull_activity[i,"steps"])) {
        noNull_activity[i,"steps"] <- MeanStepsPerInterval(noNull_activity[i,"interval"])
        flag = flag + 1
        }
    }

## 4. Histogram, Mean, Median

#### Histogram
totalStepsPerDay <- aggregate(steps ~ date, data = noNull_activity, sum)
hist(totalStepsPerDay$steps, col = "blue", xlab = "Total Number of Steps", 
     ylab = "Frequency", main = "Histogram of Total Number of Steps taken each Day")

## Mean,Median
newMean <- round(mean(totalStepsPerDay$steps))
newMedian <- round(median(totalStepsPerDay$steps))

print(c("The mean is",newMean))
print(c("The median is",newMedian))


###### Are there differences in activity patterns between weekdays and weekends?

## 1. weekday” and “weekend”

noNull_activity$day <- ifelse(as.POSIXlt(as.Date(noNull_activity$date))$wday%%6 == 
                                    0, "weekend", "weekday")
noNull_activity$day <- factor(noNull_activity$day, levels = c("weekday", "weekend"))

## 2. Panel Plot

steps_interval= aggregate(steps ~ interval + day, noNull_activity, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = steps_interval, aspect = 1/2, 
       type = "l")