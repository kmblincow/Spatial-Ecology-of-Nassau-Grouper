#Kayla Blincow
#10/18/2019

#Want to make a figure that shows box plot of activitiy metric for each hour 
#in a 24 hour time period
#filtered by detections within an hour, 5 mins, and 2 mins of each other
#only look at HR receiver
#separated by tag
#do a GAM to see what the dealio is---Haven't done this yet

#set working directory
setwd("C:/Users/kmbli/OneDrive - UC San Diego/Nassau Depth/Data")

#load necessary libraries 
library(lubridate)
library(tidyverse)


####Data prep####
#read in the data
DNC <- read.csv("DNC_detections.csv", header = T)

#create datetime column that combines date and time
DNC$datetime <- paste(DNC$Date, DNC$Time)

#convert that object into a time format that R will recognize
DNC$datetime <- as.POSIXct(DNC$datetime, tz = "EST", 
                           format = "%m/%d/%Y %H:%M:%OS")

#make sure that worked okay..
summary(DNC)

#filter my data so that I am only looking at spawning months and home receivers
#non-spawning months
dnsp <- mutate(DNC, Month = month(datetime)) %>% 
  filter(Month > 2)

#home receiver
#tag 61's home receiver has two separate codes, so we will convert those to 
#the same so we make sure to get all the detections we need
dnsp$hydro <- as.numeric(dnsp$hydro)
dnsp$hydro[dnsp$hydro==104923] <- 5682

HR <- dnsp %>% 
  filter(hydro == 5221 | hydro == 5217 | hydro == 8018 | hydro == 5682 | 
           hydro == 5213) %>% 
  group_by(tag, hydro) %>% 
  mutate(counter = n()) %>% 
  group_by(tag) %>% 
  filter(counter == max(counter))

#check that worked
HR %>% group_by(tag, hydro) %>% 
  summarize(n())
#these hydrophone values match up with what I identified as home hydros for 
#each fish


#Now let's create a column that is the difference in time and one that's the 
#difference in depth between detections for each tag number
HR1 <- HR %>% group_by(tag) %>% 
  mutate(diff_dep = depth - lag(depth, default = depth[1]),
         diff_time = datetime - lag(datetime, default = datetime[1]))
#convert out of stupid tibble format
HR1 <- as.data.frame(HR1)
#note difftime is in seconds

#going to need a column that is just the hour, so let's make that
HR1 <- HR1 %>% 
  mutate(hour = hour(datetime))

#finally we need a column for the activity metric (absolute value of change in 
#depth divided by the corresponding change in time)
#filtered for detections that are within an hour of each other
hour <- HR1 %>% 
  filter(diff_time < 3600) %>% #cuz there are 3600 seconds in an hour...
  group_by(tag) %>% 
  mutate(act = abs(diff_dep)/as.numeric(diff_time))

hour <- as.data.frame(hour)

#filtered for detections that are within 5 minutes of each other
mins5 <- HR1 %>% 
  filter(diff_time < 300) %>% #cuz there are 300 seconds in 5 mins...
  group_by(tag) %>% 
  mutate(act = abs(diff_dep)/as.numeric(diff_time))

mins5 <- as.data.frame(mins5)

#filtered for detections that are within 2 minutes of each other
mins2 <- HR1 %>% 
  filter(diff_time < 120) %>% #cuz there are 120 seconds in 2 mins...
  group_by(tag) %>% 
  mutate(act = abs(diff_dep)/as.numeric(diff_time))

mins2 <- as.data.frame(mins2)


#cool, now we need to do some plotting..

####plotting####
####let's start by looking at detections that occur within an hour of each other####

#need hour and tag to be a factor not a numeric
hour$hour <- as.factor(hour$hour)
hour$tag <- as.factor(hour$tag)

#convert NaNs to 0s--they are truly 0s (0 depth change/0 time change)
hour$act[is.na(hour$act)] <- 0

#all tags together
ggplot(hour) +
  geom_boxplot(aes(x = hour, y = act, fill = tag)) +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 18.5) +
  geom_vline(xintercept = 20.5) +
  theme_classic()

#separate tags out
#create plotting function
tagplot <- function(tagnum, data, num) {
  tagnum <- data[data$tag == num,]
  ggplot(tagnum) +
    geom_boxplot(aes(x = hour, y = act), fill = "turquoise4") +
    scale_x_discrete(limits = c(as.character(0:23)))+
    geom_vline(xintercept = 4.5) +
    geom_vline(xintercept = 6.5) +
    geom_vline(xintercept = 18.5) +
    geom_vline(xintercept = 20.5) +
    labs(x = "Hour", y = "Vertical Activity", caption = num) +
    theme_classic()
}

#do ze plots
tagplot(tag41, hour, "41")
tagplot(tag42, hour, "42")
tagplot(tag43, hour, "43")
tagplot(tag44, hour, "44")
tagplot(tag59, hour, "59")
tagplot(tag61, hour, "61")
tagplot(tag63, hour, "63")


####do the same thing for 5 minute interval####
#need hour and tag to be a factor not a numeric
mins5$hour <- as.factor(mins5$hour)
mins5$tag <- as.factor(mins5$tag)

#convert NaNs to 0s--they are truly 0s (0 depth change/0 time change)
mins5$act[is.na(mins5$act)] <- 0

#all tags together
ggplot(mins5) +
  geom_boxplot(aes(x = hour, y = act, fill = tag)) +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 18.5) +
  geom_vline(xintercept = 20.5) +
  theme_classic()

#separate tags out
tagplot(tag41, mins5, "41")
tagplot(tag42, mins5, "42")
tagplot(tag43, mins5, "43")
tagplot(tag44, mins5, "44")
tagplot(tag59, mins5, "59")
tagplot(tag61, mins5, "61")
tagplot(tag63, mins5, "63")

####and for 2 minutes...####
mins2$hour <- as.factor(mins2$hour)
mins2$tag <- as.factor(mins2$tag)

#convert NaNs to 0s--they are truly 0s (0 depth change/0 time change)
mins2$act[is.na(mins2$act)] <- 0

#all tags together
ggplot(mins2) +
  geom_boxplot(aes(x = hour, y = act, fill = tag)) +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 18.5) +
  geom_vline(xintercept = 20.5) +
  theme_classic()

#separate tags out
tagplot(tag41, mins2, "41")
tagplot(tag42, mins2, "42")
tagplot(tag43, mins2, "43")
tagplot(tag44, mins2, "44")
tagplot(tag59, mins2, "59")
tagplot(tag61, mins2, "61")
tagplot(tag63, mins2, "63")


####For context, let's look at total detections per hour for each tag####

#calculate number of detections per hour per day
det <- HR1 %>% 
  group_by(tag, Date, hour) %>% 
  summarize(detnum = n())

#need hour and tag to be a factor not a numeric
det$hour <- as.factor(det$hour)
det$tag <- as.factor(det$tag)



#all tags together
ggplot(det) +
  geom_boxplot(aes(x = hour, y = detnum, fill = tag)) +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 6.5) +
  geom_vline(xintercept = 18.5) +
  geom_vline(xintercept = 20.5) +
  theme_classic()

#separate tags out
#create plotting function for detections
tagplot_det <- function(tagnum, data, num) {
  tagnum <- data[data$tag == num,]
  ggplot(tagnum) +
    geom_boxplot(aes(x = hour, y = detnum), fill = "plum4") +
    scale_x_discrete(limits = c(as.character(0:23)))+
    geom_vline(xintercept = 4.5) +
    geom_vline(xintercept = 6.5) +
    geom_vline(xintercept = 18.5) +
    geom_vline(xintercept = 20.5) +
    labs(x = "Hour", y = "# of Detections on a Given Day", caption = num) +
    theme_classic()
}

tagplot_det(tag41, det, "41")
tagplot_det(tag42, det, "42")
tagplot_det(tag43, det, "43")
tagplot_det(tag44, det, "44")
tagplot_det(tag59, det, "59")
tagplot_det(tag61, det, "61")
tagplot_det(tag63, det, "63")


