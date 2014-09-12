data <- read.csv("activity.csv")
data$date <- as.Date(x=data$date) #change format
data$month <- as.numeric(format(data$date,"%m"))
noNAdata <- na.omit(data)
head(noNAdata)
dim(noNAdata)
Sys.setlocale(category="LC_ALL",locale="English") # transfer date from Traditional Chinese to English
library(ggplot2)
# What is mean total number of steps taken per day?
#For the part of the assignment you can ignore the missing values in the dataset.
#1.Make a histogram of the total number of steps taken each day
ggplot(noNAdata, aes(date, steps)) + geom_bar(stat = "identity", colour = "green", fill = "green", width = 0.5)
#2.Calculate and report the mean and median total number of steps taken per day 
totalsteps <- aggregate(x=noNAdata$steps,by=list(date=noNAdata$date),FUN= sum)
m <- mean(totalsteps$x)
m
md <- median(totalsteps$x)
md

# What is the average daily activity pattern ?
#1 Make a time series plot(i.e. type="1") of the 5-minute interval(x-axis) and the 
#average number of steps taken, averaged across all day (y-axis)
avgsteps <- aggregate(x=noNAdata$steps,by=list(interval=noNAdata$interval),FUN= mean)
names(avgsteps)[2] <- "meansteps"
head(avgsteps)
ggplot(avgsteps, aes(interval, meansteps))+geom_line(stat = "identity", color ="green",size=1)+
  labs(title= "The Average Number of Steps taken each 5-minute ", x="5-minute interval", y="Average Number of Steps")

#2 Which 5-minute interval, on average across all the days in the dataset, contains the
#maximum number of steps ?
avgsteps[avgsteps$meansteps == max(avgsteps$meansteps),]
avgsteps[avgsteps$meansteps == max(avgsteps$meansteps),]$meansteps
# Imputing missing values
###1 Calculate and report the total number of missing values in the dataset
#(i.e. the total number of rows with NAS)
sum(is.na(data))
###2 Devise a strategy for filling in all of the missing values in the dataset.
Newdata <- data
#The strategy does not need to be sophisticated. For example, you could use
#the mean/median for that day, or mean for that 5-minute interval,
#Here I choice to devise the mean for that 5-minute interval
### Create a new dataset that is equal to the original dataset but with the missing data filled in. 
for(i in 1:nrow(Newdata)){
  if(is.na(Newdata$steps[i])){
    Newdata$steps[i] <- avgsteps[Newdata$interval[i] == avgsteps$interval,]$meansteps
  }
}
head(Newdata)

### Make a histogram of the total number of steps taken each day
ggplot(Newdata,aes(date,steps))+geom_bar(stat= "identity", color ="blue", fill="blue", width= 0.5)+
  labs(title="The Total Number of Steps for Newdata", x="Date", y="The Total Number of Steps")

### Calculate the mean of the total number of steps taken per day
Newtotalsteps <- aggregate(x=Newdata$steps, by=list(date=Newdata$date),FUN = sum)
names(Newtotalsteps)[2] <- "steps"
head(Newtotalsteps)
Newm <- mean(Newtotalsteps$steps)
Newm
### Calculate the median of the total number of steps taken per day
Newmd <- median(Newtotalsteps$steps)
Newmd

### Do these values differ from the estimates from the first part of the assignment ?
Yes

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
Newm - m
Newmd -md

# So after imputing the missing value the mean of the total number of steps taken per day is
# the same, the median of imputing the missing value is greater.

# Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels-"weekday" and
### weekend" indicating whether a given date is a weekday or weekend day.
head(Newdata)
Newdata$weekdays <- as.factor(format(Newdata$date,"%A"))
levels(Newdata$weekdays)
levels(Newdata$weekdays) <- list(weekday=c("Monday","Thesday","Wednesday","Thursday","Friday"),
                                 weekend=c("Saturday","Sunday"))
levels(Newdata$weekdays)
head(Newdata)

### Make a panel plot containing a time series plot(i.e.type="1") of the
### 5-minute interval(x-axis) and the average number of step taken,
### averaged across all weekday days or weekend days (y-axis)
Newavgsteps <- aggregate(x=Newdata$steps,by=list(interval=Newdata$interval,weekdays=Newdata$weekdays),FUN= mean)
names(Newavgsteps)[3] <- "meansteps"
head(Newavgsteps)
ggplot(Newavgsteps, aes(interval, meansteps))+geom_line(stat = "identity", color ="blue",size=1)+
  facet_grid(. ~ weekdays, scales = "free")+
  labs(title= "The Average Number of Steps taken each 5-minute ", x="5-minute interval", y="Average Number of Steps")