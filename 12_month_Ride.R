
getwd()
#Set Working Directory#

#Install Packages#
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("janitor")

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

#Import 12_month_data#
June <- read_csv("202106-divvy-tripdata.csv")
July <- read_csv("202107-divvy-tripdata.csv")
August <- read_csv("202108-divvy-tripdata.csv")
September <- read_csv("202109-divvy-tripdata.csv")
October <- read_csv("202110-divvy-tripdata.csv")
November <- read_csv("202111-divvy-tripdata.csv")
December <- read_csv("202112-divvy-tripdata.csv")
January<- read_csv("202201-divvy-tripdata.csv")
February <- read_csv("202202-divvy-tripdata.csv")
March <- read_csv("202203-divvy-tripdata.csv")
April <- read_csv("202204-divvy-tripdata.csv")
May <- read_csv("202205-divvy-tripdata.csv")
Travel_Data_Cleaned <- read_csv("Travel_Data_Cleaned.csv")

colnames(January)
colnames(February)
colnames(March)
colnames(April)
colnames(May)
colnames(June)
colnames(July)
colnames(August)
colnames(September)
colnames(October)
colnames(Novemeber)
colnames(December)

Travel_Data <- bind_rows(January, February, March, April, May, June, July, August, September, October, November, December)


#Data Cleaning#
Travel_Data2 <- clean_names(Travel_Data)
Travel_Data3 <- remove_empty(Travel_Data2, which = c("rows", "cols"), quiet = FALSE)
Travel_Data_Cleaned <- distinct(Travel_Data3)

Travel_Data_Cleaned$started_at <- as.POSIXct(Travel_Data_Cleaned$started_at, "%m%d%Y", "%H%M%S")
Travel_Data_Cleaned$ended_at <- as.POSIXct(Travel_Data_Cleaned$ended_at, "%m%d%Y", "%H%M%S")

str(Travel_Data_Cleaned)

Travel_Data_Cleaned <- Travel_Data_Cleaned %>%
  mutate(ride_time_m = as.numeric(Travel_Data_Cleaned$ended_at - Travel_Data_Cleaned$started_at) / 60)

Travel_Data_Cleaned <- Travel_Data_Cleaned %>%
  mutate(year_month = paste(strftime(Travel_Data_Cleaned$started_at, "%Y"),
                            "-",
                            strftime(Travel_Data_Cleaned$started_at, "%m"),
                            paste("(",strftime(Travel_Data_Cleaned$started_at, "%b"), ")", sep="")))
unique(Travel_Data_Cleaned$year_month)

str(Travel_Data_Cleaned)

Travel_Data_Cleaned <- Travel_Data_Cleaned %>%
  mutate(weekday = paste(strftime(Travel_Data_Cleaned$ended_at, "%u"), "-", strftime(Travel_Data_Cleaned$ended_at, "%a")))
unique(Travel_Data_Cleaned$weekday)

str(Travel_Data_Cleaned)

Travel_Data_Cleaned <- Travel_Data_Cleaned %>%
  mutate(start_hour = strftime(Travel_Data_Cleaned$ended_at, "%H"))
unique(Travel_Data_Cleaned$start_hour)

write.csv(Travel_Data_Cleaned, file = 'E:/Excel Projects/Case Study 1/Travel_Data_Cleaned.csv')

#Analysis# 
str(Travel_Data_Cleaned)

ggplot(Travel_Data_Cleaned, aes(member_casual, fill = member_casual)) + 
  geom_bar() +
  labs(x = "Users", title = "Number of Users", fill = "User Type")

ggplot(Travel_Data_Cleaned, aes(rideable_type, fill = member_casual)) + 
  geom_bar(position = "dodge") +
  labs(x = "Rideable Types", title = "Most Used Type of Ride", fill = "User Type")

ggplot(Travel_Data_Cleaned, aes(year_month, fill = member_casual)) +
  geom_bar() +
  labs(x = "Months", title = "Number of Users Annually", fill = "User Type") +
  coord_flip()
    
ggplot(Travel_Data_Cleaned, aes(weekday, fill = member_casual)) +
  geom_bar() +
  labs(x = "Day of Week", title = "Number of Users per Day", fill = "User Type")