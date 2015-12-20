# Setting the work directory

setwd("F:/vindo Apple/Reproducible Research/Assigments/Assigment 1/RepData_PeerAssessment1")



# Loading needed package

require(dplyr)
require(lattice)
require(lubridate)


# Getting files

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# OR

if(!file.exists("activity.csv")) {
     unzip("activity.zip")
     # unlink(temp)
}


#  Getting Data

activity <- read.table("activity.csv", 
                       header = TRUE, 
                       sep = ",", 
                       dec = ".", 
                       na.strings = "NA", 
                       colClasses = c("numeric", "Date", "numeric"), 
                       stringsAsFactors = FALSE)

# Creating variable hms with the time of the event
activity %>%
     mutate(
          hm = interval
     ) -> activity
sprintf("%04d", activity$hm) -> activity$hm
gsub("(^[0-9]{2})([0-9]{2})$",  "\\1:\\2", activity$hm) -> activity$hm
activity %>%
     mutate(
          hms = hm(hm)
     ) -> activity

# foi substituído pela sprintf...
for(i in seq_along(activity$hm)){
     if(nchar(activity$hml[i]) == 1){
          activity$hm[i] <- paste("000", activity$hm[i], sep = "")
     } else if(nchar(activity$hm[i]) == 2){
          activity$hm[i] <- paste("00", activity$hm[i], sep = "")
     } else if(nchar(activity$hm[i]) == 3){
          activity$hm[i] <- paste("0", activity$hm[i], sep = "")
     }
}
#retirar



# making the histogram of the total number of steps taken each day

# histogram(~steps | factor(date), data = activity)

activity %>%
     group_by(date) %>%
          summarize(sum(steps)) -> TotalStepsPerDay
names(TotalStepsPerDay) <-c("date", "sumSteps")
histogram(~TotalStepsPerDay$sumSteps,
          xlab = " Total number os steps per day",
          ylab = "Frequency (%)")

# Calculating and reporting the mean and median total number of steps taken per day

mean(TotalStepsPerDay[[2]], na.rm  = TRUE) -> m
median(TotalStepsPerDay[[2]], na.rm  = TRUE) -> md


# Está errado
activity %>%
          summarise(mean(steps, na.rm = TRUE),
               median(steps, na.rm = TRUE)
                   ) -> meanANDmedian

meanANDmedian[[1]]
meanANDmedian[[2]]
# remover

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

activity %>%
     group_by(interval) %>%
     summarise(mean(steps, na.rm = TRUE)
     ) -> meanStepsBYinterval

names(meanStepsBYinterval) <- c("interval","meanStepsBYinterval")

plot(levels(as.factor(activity$interval)),
     meanStepsBYinterval$meanStepsBYinterval, 
     type = "l",
     xlab = "Interval",
     ylab = "Average number os steps")
# the following plot is not working
activity %>%
     group_by(hms) %>%
     summarise(mean(steps, na.rm = TRUE)
     ) -> meanStepsBYhms

plot(levels(as.factor(activity$hm)),
     meanStepsBYinterval$`mean(steps, na.rm = TRUE)`, 
     type = "l")
# remove

activity %>%
     group_by(date) %>%
     summarise(mean(steps, na.rm = TRUE)
     ) -> meanBYdate

max(meanStepsBYinterval[,2]) -> maxI

meanStepsBYinterval %>% 
     filter(meanStepsBYinterval[,2] == max(meanStepsBYinterval[,2])) -> intervalMaxSteps
          intervalMaxSteps[[1]]

meanStepsBYinterval %>%          
          slice('which(meanStepsBYinterval[,2] == max(meanStepsBYinterval[,2]))')[[1]]         

slice(meanStepsBYinterval, which(meanStepsBYinterval[,2] == max(meanStepsBYinterval[,2])))




# Imputing missing values

## Calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sapply(activity, anyNA)
sum(is.na(activity$steps))

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## Create a new dataset that is equal to the original dataset but with the missing data filled in

newActivity <- left_join(activity, meanStepsBYinterval, by = "interval")

newActivity %>%
     mutate(newsteps = ifelse(is.na(steps), meanStepsBYinterval, steps)) -> newActivity


## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

newActivity %>%
     group_by(date) %>%
     summarize(sum(newsteps)) -> newTotalStepsPerDay
names(newTotalStepsPerDay) <-c("date", "newsumSteps")
histogram(~newTotalStepsPerDay$newsumSteps)

# Calculating and reporting the mean and median total number of steps taken per day

mean(newTotalStepsPerDay[[2]], na.rm  = TRUE)
median(newTotalStepsPerDay[[2]], na.rm  = TRUE)

## Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?





#Are there differences in activity patterns between weekdays and weekends?

##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



#wday(newActivity$date, label = TRUE, abbr = FALSE)
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







newActivity %>%
     group_by(interval) %>%
     summarise(mean(newsteps, na.rm = TRUE)
     ) -> meanNewStepsBYinterval

names(meanNewStepsBYinterval) <- c("interval","meanNewStepsBYinterval")

xyplot(newActivity$meanStepsBYinterval~newActivity$interval | newActivity$wkday, 
       type = 'l', 
       layout = c(1,2), 
       xlab = "Interval",
       ylab = "Number of steps"
       )

plot(levels(as.factor(newActivity$interval)),
     meanNewStepsBYinterval$meanNewStepsBYinterval, 
     type = "l")

dotplot(interval ~ meanNewStepsBYinterval, 
        data = newActivity,
        groups = wkday,
        layout = c(1, 2),
        auto.key = list( points = FALSE, lines = TRUE)
        )


xyplot(levels(as.factor(interval)) ~ meanNewStepsBYinterval, 
        data = newActivity,
        # groups = wkday,
        #layout = c(1, 2),
        #auto.key = list( points = FALSE, lines = TRUE),
       type = "o"
)

