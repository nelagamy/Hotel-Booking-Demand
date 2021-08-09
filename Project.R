df<-read.csv("data.csv.csv",header=TRUE) #Reading of the data
new_df = subset(df, select = -c(arrival_date_year,babies,country,market_segment,previous_bookings_not_canceled,is_canceled,adults,children,meal,distribution_channel,is_repeated_guest,previous_cancellations,reserved_room_type,assigned_room_type,booking_changes,deposit_type,agent,company,days_in_waiting_list,customer_type,adr,required_car_parking_spaces,reservation_status,reservation_status_date) )#omitting some of the columns
View(new_df)
final<-na.omit(new_df) #Cleaning of the data
View(final)

#Checking the outliers
plot(final$lead_time) #Checking the outliers using a scatter plot graph
boxplot(final$lead_time,xlab="Lead Time")#Checking the outliers using a box plot graph
boxplot(final$arrival_date_week_number,xlab="Week Number")#Checking the outliers
boxplot(final$arrival_date_day_of_month,xlab="Day Number")#Checking the outliers

boxplot(final$stays_in_weekend_nights,xlab="Weekend Nights")#Checking the outliers
Q<-quantile(final$stays_in_weekend_nights,probs=c(.25,.75),na.rm=FALSE)#Setting the 1st and the 3rd quartile in Q
iqr<-IQR(final$stays_in_weekend_nights)#Calculating the IQR
up <-  Q[2]+1.5*iqr # Upperlimit  
low<- Q[1]-1.5*iqr # Lowerlimit
new<- subset(final, final$stays_in_weekend_nights > (Q[1] - 1.5*iqr) & final$stays_in_weekend_nights < (Q[2]+1.5*iqr))#New data frame without outliers in the specified column
boxplot(as.numeric(new$stays_in_weekend_nights),xlab="Weekend Nights")#The new boxplot without outliers

boxplot(final$stays_in_week_nights,xlab="Week nights")#Checking outliers
Z<-quantile(final$stays_in_week_nights,probs=c(.25,.75),na.rm=FALSE)#Setting the 1st and the 3rd quartile in Z
Iqr<-IQR(final$stays_in_week_nights)#Calculating the IQR
Up <-  Z[2]+1.5*Iqr # Upperlimit  
Low<- Z[1]-1.5*Iqr # Lowerlimit
new2<- subset(final, final$stays_in_week_nights > (Z[1] - 1.5*Iqr) & final$stays_in_week_nights < (Z[2]+1.5*Iqr))#New data frame without outliers in the specified column
boxplot(as.numeric(new2$stays_in_week_nights),xlab="Week Nights")#The new boxplot without outliers

boxplot(final$total_of_special_requests,xlab="Special Requests")#Checking the outliers
F<-quantile(final$total_of_special_requests,probs=c(.25,.75),na.rm=FALSE)#Setting the 1st and the 3rd quartile in F
IQr<-IQR(final$total_of_special_requests)#Calculating the IQR
UP <-  F[2]+1.5*IQr # Upperlimit  
LOw<- F[1]-1.5*IQr # Lowerlimit
new3<- subset(final, final$total_of_special_requests > (F[1] - 1.5*IQr) & final$total_of_special_requests < (F[2]+1.5*IQr))#New data frame without outliers in the specified column
boxplot(as.numeric(new3$total_of_special_requests),xlab="Special Requests")#New boxplot without outliers

#First part
library(tidyverse)
p <- ggplot(final, aes(arrival_date_month))# saving a space in p for a bar chart to see the most frequent month of arriving of the guests
p + geom_bar()+ coord_flip()# Adding the flipped bar chart
mode<-function(x) # To make sure that August is the most frequent month of arriving
{
  u<-unique(x)
  u[which.max(tabulate(match(x,u)))]
}
mode(final$arrival_date_month)

h <- ggplot(final, aes(arrival_date_week_number)) # Checking the most frequent week of arrival
h + geom_histogram(bins=12) # A histogram with 12 bins(bar)
summary(final$arrival_date_week_number)#Finding the six summary numbers

d <- ggplot(final, aes(arrival_date_day_of_month)) # Checking the most frequent day of arrival
d + geom_histogram(bins=12) 
quantile(final$arrival_date_day_of_month)#Finding the five summary numbers

#Second part
e<-ggplot(data=new3, aes(x=hotel, y=total_of_special_requests)) + # A colored bar chart between the hotel variable and the special requests
  geom_bar(stat="identity", color="steelBlue", fill="white")
e

#Last part
max(new2$stays_in_week_nights)-min(new2$stays_in_week_nights) #Finding the range
median(new$stays_in_weekend_nights)#Finding the median
mean(final$lead_time, trim=0.1) # Finding the mean after trimming the outliers
probs<-seq(0,1,0.1)#Setting the prob to 10%
quantile(final$arrival_date_week_number,probs)
mad(final$arrival_date_day_of_month)#Finding the mean absolute deviations of the days of the months
IQR(final$arrival_date_day_of_month)#Finding the interquartile range of the days of the months
cov(new$stays_in_weekend_nights,new$stays_in_weekend_nights)#Finding the variance of the weekend nights
sd(new2$stays_in_week_nights)# Finding the standard deviation of the week nights
mean(final$arrival_date_week_number)#Finding the mean of the week number variable


