# Setting the work directory

setwd("F:/vindo Apple/Reproducible Research/Assigments/Assigment 1/RepData_PeerAssessment1")


# Loading needed package

require(dplyr)
require(lattice)

# Getting files

# Data files are in the Github repository "http://github.com/rdpeng/RepData_PeerAssessment1" that was forked to my repository

#  Getting Data

#The *activity.zip* file from the Github repository for the assingment was unzipped.

unzip("activity.zip")

activity <- read.table("activity.csv", 
                       header = TRUE, 
                       sep = ",", 
                       dec = ".", 
                       na.strings = "NA", 
                       colClasses = c("numeric", "Date", "numeric"), 
                       stringsAsFactors = FALSE)


#What is mean total number of steps taken per day?

## making the histogram of the total number of steps taken each day

## histogram(~steps | factor(date), data = activity)

activity %>%
     group_by(date) %>%
          summarize(sum(steps)) -> TotalStepsPerDay
names(TotalStepsPerDay) <-c("date", "sumSteps")

histogram(~TotalStepsPerDay$sumSteps,
          main =  "Total Number of Steps taken each Day",
          xlab = " Total number of steps taken each day",
          ylab = "Frequency (%)")

## Calculating and reporting the mean and median total number of steps taken per day

mean(TotalStepsPerDay[[2]], na.rm  = TRUE) -> m
median(TotalStepsPerDay[[2]], na.rm  = TRUE) -> md

# What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all days (y-axis)

#The  missing values in the dataset were ignored.

activity %>%
     group_by(interval) %>%
     summarise(mean(steps, na.rm = TRUE)
     ) -> meanStepsBYinterval
names(meanStepsBYinterval) <- c("interval","meanStepsBYinterval")

plot(levels(as.factor(activity$interval)),
     meanStepsBYinterval$meanStepsBYinterval, 
     type = "l",
     xlab = "Interval",
     ylab = "Average number os steps"
     )

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max(meanStepsBYinterval[,2]) -> maxI

meanStepsBYinterval[[which(meanStepsBYinterval[,2] == maxI),1]] -> intevalmaxI


# Imputing missing values

## Calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs)

# Checking which variables cotain NA's

sapply(activity, anyNA)

sum(is.na(activity$steps)) -> sumNA

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## Create a new dataset that is equal to the original dataset but with the missing data filled in

newActivity <- left_join(activity, meanStepsBYinterval, by = "interval")

newActivity %>%
     mutate(newsteps = ifelse(is.na(steps), meanStepsBYinterval, steps)) -> newActivity

# Removing variable meanStepsBYinterval

newActivity$meanStepsBYinterval <- NULL

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

newActivity %>%
     group_by(date) %>%
     summarize(sum(newsteps)) -> newTotalStepsPerDay
names(newTotalStepsPerDay) <-c("date", "newsumSteps")

histogram(~newsumSteps,
          data = newTotalStepsPerDay,
          main = "Average daily activity pattern / newActivity dataset",
          xlab = "Daily total number of steps",
          ylab = "Frequency (%)"
          )

# Calculating and reporting the mean and median total number of steps taken per day

mean(newTotalStepsPerDay[[2]], na.rm  = TRUE) -> mnoNA
median(newTotalStepsPerDay[[2]], na.rm  = TRUE) -> mdnoNA

## Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

matrix(data = c(m,mnoNA,md, mdnoNA),  
       nrow = 2, 
       byrow = FALSE, 
       dimnames = list(Dataset = c("activity", "newActivity"), Statistics = c("Mean", "Median"))
       ) -> mmdActivitynew



#Are there differences in activity patterns between weekdays and weekends?

##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



#wday(newActivity$date, label = TRUE, abbr = FALSE)
# To have the weekdays in English in RStudio and R running in Barzilain Portuguese Windows 10

Sys.setlocale("LC_TIME", "English")

newActivity %>%
     mutate( wkday = factor(
                         ifelse(weekdays(newActivity$date) == "Saturday" | weekdays(newActivity$date) == "Sunday",
                            "weekend",
                            "weekday"
                              )
                         )
     ) -> newActivity

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

newActivity %>%
     group_by(interval, wkday) %>%
     summarise(mean(newsteps, na.rm = TRUE)
     ) -> meanNewStepsBYintervalwkday
names(meanNewStepsBYintervalwkday) <- c("interval", "wkday", "meanNewStepsBYintervalwkday")

xyplot(meanNewStepsBYintervalwkday~interval | wkday,
       data = meanNewStepsBYintervalwkday,
       type = 'l', 
       layout = c(1,2), 
       xlab = "Interval",
       ylab = "Number of steps"
)
