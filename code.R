
# Load the data
data <- read.csv("./activity.csv")

# Calculate the total number of steps taken per day
data$date <- as.factor(data$date)

data2 <- tapply(data$steps,data$date,sum,na.rm =TRUE)
data2 <- as.data.frame(data2)


#  Make a histogram of the total number of steps taken each day  
hist(data2$data2)

barplot(data2$data2) #I guess barplot can reflect the steps per day clearly.

# Calculate and report the mean and median of the total number of steps taken per day
mean1 <- mean(data2$data2)
median1 <- median(data2$data2)


# Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
data$interval <- as.factor(data$interval)
data3 <- tapply(data$steps,data$interval,mean,na.rm =TRUE)
data3 <- as.data.frame(data3)
plot(x =rownames(data3),y =data3$data3,type ="l")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxindex <- which.max(data3$data3)
maxinterval <- rownames(data3)[maxindex]

#  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
totalofmissing <- sum(is.na(data$steps))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#  Create a new dataset that is equal to the original dataset but with the missing data filled in.
for(i in 1:dim(data)[1]){
        if (is.na(data$steps[i])) data$steps[i]<-data3$data3[which(rownames(data3)==as.character(data$interval[i]))] #  find the missing value and fill it with the average interval value 
        
}
#  the new dataset is just data

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
data4 <- tapply(data$steps,data$date,sum,na.rm =TRUE)
data4 <- as.data.frame(data4)
hist(data4$data4)

mean2 <-mean(data4$data4)
median2 <- median(data4$data4)


# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

library(dplyr)
data <-mutate(data, weekday = weekdays(as.Date(data$date)))
for(i in 1:dim(data)[1]){
        if (data$weekday[i] =="星期六" | data$weekday[i] =="星期日") {data$weekday[i]="weekend"}
        else {data$weekday[i]="weekday"}
}

data$weekday <-as.factor(data$weekday)


attach(data)
data5 <-aggregate(data,by =list(interval,weekday),FUN =mean)
data5$Group.1 <- as.character(data5$Group.1)
par(mfrow =c(2,1))
with(subset(data5, Group.2== "weekend"),plot(Group.1,steps,type ="l",col ="blue",main ="weekend",xlab ="interval"))
with(subset(data5, Group.2== "weekday"),plot(Group.1,steps,type ="l",col ="blue",main ="weekday",xlab ="interval"))

