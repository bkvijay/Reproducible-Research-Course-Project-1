# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
activity <- read.csv("activity.csv",header = T,sep = ",")
summary(activity)

## What is mean total number of steps taken per day?
?aggregate

act1 <- aggregate(steps ~ date, activity, sum)
?hist
head(act1)
hist(act1$steps, breaks = 30, xlab = "Steps", ylab = "Counts", main = "Number of steps taken" ,col = "yellow")
 ## Mean
mean(act1$steps)
 ## Median
median(act1$steps)


## What is the average daily activity pattern?
act2 <- aggregate(steps ~ interval , act,mean)
head(act2)

## Make a time series plot (i.e.type="l") of the 5-minute interval
## (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(act2$interval, act2$steps,type = "l", xlab = "Intervel", 
     ylab = "Average step taken", main = "Time Series Plot")

## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?
act2[act2$steps ==max(act2$steps),]

# Imputing missing values

## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)
nrow(act [is.na(act$steps)==TRUE,])

## Devise a strategy for filling in all of the missing values in the dataset.
## The strategy does not need to be sophisticated. For example, you could use 
## the mean/median for that day, or the mean for that 5-minute interval, etc.
install.packages("Hmisc")
library(Hmisc)
## Create a new dataset that is equal to the original dataset but with the missing data filled in.
act_new<- act
act_new$steps <- with(act_new, impute(steps, mean))
sub_act <- aggregate(steps ~ date, act_new, sum)
sub_act

## Make a histogram of the total number of steps taken each day and Calculate and 
## report the mean and median total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?
hist(sub_act$steps, breaks = 20,xlab = "Steps",
     ylab = "Counts",main = "New Histogram of Steps after imputing")

mean(sub_act$steps)

median(sub_act$steps)


# Are there differences in activity patterns between weekdays and weekends?

#create factor variable
act$date<-with(act,weekdays(as.Date(act$date)) %in% c('Sunday','Saturday'))

xyplot(steps ~ interval | activity, data = act_new, layout = c(1, 2),
       type = "l",strip=strip.custom(factor.levels=c("WEEKEND","WEEKDAY")))

#create plot
par(mfrow=c(1,2))
hist(act1$steps, breaks=8,main = "Total number of steps per day", 
     xlab = "Steps", col = "red")
hist(sub_act$steps, breaks=8,main = "Total number of steps per day \n with no NA's",
     xlab = "Steps", col = "green")
