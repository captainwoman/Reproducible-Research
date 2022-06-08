# Setting directory and unzipping file
my_dir <- "/Users/shubh/Downloads"
zip_file <- list.files(path = my_dir , pattern = "*.zip" , full.names = TRUE)
zip_file
ldply(.data = zip_file, .fun = unzip, exdir = my_dir)

# Loading and preprocessing the data

raw_data <- read.csv("activity.csv", header = TRUE)
str(activity)
main_data <- na.omit(raw_data)

# Mean of total number of steps taken per day

stpsperday <- aggregate(main_data$steps, by = list(Steps.Date = main_data$date)
                        , FUN = "sum")
#Histogram with the frequency ot total numbers

hist(stpsperday$x, col="blue",breaks=30,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day")
     
# Mean and median of the total number of steps     

meanstps <- mean(stpsperday[ ,2])
meanstps

medianstps <- median(stpsperday[ ,2])
medianstps

# Average daily activity pattern
 
avg_day <- aggregate(main_data$steps, 
                     by = list(Interval = main_data$interval), 
                     FUN = "mean")
                     
# Plot of average daily activity pattern 

plot(avg_day$Interval, avg_day$x, type= "l", xlab="5-minute interval"
     , ylab= "Average number of steps taken", 
     main= "Average Daily Activity Pattern")
     
# Interval with the maximum number of steps

interval_data <- which.max(avg_day$x)
maxinterval <- avg_day[interval_data, 1]
maxinterval

# Imputing missing values

NAno <- length(which(is.na(raw_data$steps)))
NAno

rawdata_filled <- raw_data
rawdata_filled$steps <- impute(raw_data$steps , mean)


# Histogram with the new frequencies of total number of steps after imputing

newstps_perday <- aggregate(rawdata_filled$steps, 
            by = list(Steps.Date = rawdata_filled$date),  FUN = "sum")

hist(newstps_perday$x, col= "red", breaks= 30,  xlab= "Number of steps per day"
     ,main= "Total number of steps taken each day(filled data)")
     
# Mean and median of the total number of steps after imputing

mean_newstps_perday <- mean(newstps_perday[ ,2])
mean_newstps_perday

median_newstpsperday <- median(newstps_perday[ ,2])
median_newstpsperday


# Differences in activity patterns between weekdays and weekends

rawdata_filled$date <-as.Date(rawdata_filled$date)
rawdata_filled$weekday <- weekdays(rawdata_filled$date)
rawdata_filled$daytype <- ifelse(rawdata_filled$weekday=="Saturday" |
                    rawdata_filled$weekday=="Sunday","Weekend","Weekday")
rawdata_filled$daytype <- factor(rawdata_filled$daytype)


# Plot of weekdays and weekends activity pattern

day_types <- aggregate(steps ~ interval + daytype, data= rawdata_filled,mean)
 ggplot(day_types , aes(interval, steps))+ geom_line()+ facet_grid(daytype ~ .)+
      xlab("5-minute intervals")+ ylab(" Average number of steps taken")+
        ggtitle("Weekdays and Weekends Activity Pattern")

















