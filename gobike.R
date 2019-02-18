library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(tidyr)
library(eeptools)

gobike <- fread("input/2017-fordgobike-tripdata.csv")
str(gobike)
head(gobike)

gobike$duration_sec <- as.numeric(gobike$duration_sec)
gobike$member_birth_year <- as.numeric(gobike$member_birth_year)
length(unique(gobike$bike_id))
gobike <- gobike %>%
                mutate(start_time_date = date(start_time)) %>%
                mutate(start_end_time = date(end_time)) %>%
                mutate(age_cal = 2019 - member_birth_year)
gobike_1 <- gobike %>% 
                    mutate(custype = 1) %>%
                    spread(key = user_type, value = custype) %>%
                    mutate(gender = 1)%>%
                    spread(key = member_gender,value = gender)

gobike_1 <- gobike_1[-19]
gobike_1[is.na(gobike_1$Other),]$Other= 0
gobike_1[is.na(gobike_1$Female),]$Female= 0
gobike_1[is.na(gobike_1$Male),]$Male= 0
gobike_1[is.na(gobike_1$age_cal),]$age_cal= round(mean(gobike_1$age_cal,na.rm = TRUE),0)
gobike_1[is.na(gobike_1$Customer),]$Customer = 0
gobike_1[is.na(gobike_1$Subscriber),]$Subscriber = 0

gobike_summary <- gobike_1 %>%
                     group_by(start_time_date)%>%
                    summarise(duration_hrs = mean(duration_sec)/3600,age = mean(age_cal),
                              Female = sum(Female),
                              Male = sum(Male),
                              Other = sum(Other),
                              Customer = sum(Customer),
                              Subscriber = sum(Subscriber),
                              total_rider = n())


