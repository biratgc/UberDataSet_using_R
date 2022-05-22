# Reading in the data
uber$WeekdayString <- NA
getwd()
setwd("/Users/biratgc/desktop")
list.files()
uber = read.table(file = "uber_jun2014.txt", header = TRUE)
head(uber)
uber

# Changing Weekdays name from Sunday, Monday, ... to 1, 2, so it is easier to read

install.packages("dplyr")
library(dplyr)
uber <- mutate(uber,
               WeekdayString=
                 case_when(Weekday == 1 ~ "Sunday",
                           Weekday == 2 ~ "Monday",
                           Weekday == 3 ~ "Tuesday",
                           Weekday == 4 ~ "Wednesday",
                           Weekday == 5 ~ "Thursday",
                           Weekday == 6 ~ "Friday",
                           Weekday == 7 ~ "Saturday")
)
uber


colnames(uber)[1] <- "Date" #changing from Date/Time to just Date
uber

uber$Date = as.Date(uber$Date, "%Y-%m-%d") # changing the structure to date
str(uber)


attach(uber)
library(ggplot2)
# Plotting the data
ggplot(uber, aes(`Date`, Price))+
  geom_line(color = "#00AFBB", size = 2) 

# Every week the price goes up at the end but slowly goes down for every week, which means the prices are high during the weekends.


# The following for-loop prints out the range of the prices and durations for each weekday
Weekday1 = c("Sunday","Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday")
priceRange = c(0,0,0,0,0,0,0)
durationRange = c(0,0,0,0,0,0,0)
for(i in 1:7){
  
  Weekday1_data = uber[which(uber$WeekdayString == Weekday1[i]),]
  priceRange[i] = max(Weekday1_data$Price) - min(Weekday1_data$Price)
  durationRange[i] = max(Weekday1_data$Duration)-min(Weekday1_data$Duration)
  print(paste0("Price - ", Weekday1[i] ," : ",priceRange[i]))
  print(paste0("Duration - ", Weekday1[i] , " : ", durationRange[i]))
  
  
}
# Creating a subset for peak times

sub_weekdays = c(6,7,8,9,10,11,12)
subUber612AM = uber[uber$Hour %in% sub_weekdays, ]

sub_weekdays2 = c(18,19,20,21,22,23,0)
subUber612PM = uber[uber$Hour %in% sub_weekdays2, ]

#For Price
subUber612AM$Price = as.numeric(subUber612AM$Price)
mean_1 = mean(subUber612AM$Price)
mean_1

subUber612PM$Price = as.numeric(subUber612PM$Price)
mean_2 = mean(subUber612PM$Price)
mean_2


diff_mean_price = mean_2 - mean_1
diff_mean_price

#For Duration

subUber612AM$Duration = as.numeric(subUber612AM$Duration)
mean_3 = mean(subUber612AM$Duration)
mean_3

subUber612PM$Duration = as.numeric(subUber612PM$Duration)
mean_4 = mean(subUber612PM$Duration)
mean_4

diff_mean_duration = mean_3 - mean_4
diff_mean_duration


# The difference is average prices is 5.694939 which suggests that between the two times, 6 AM-12 PM makes more revenue than the 6 PM - 12AM, however, the duration between these times are similar. 

