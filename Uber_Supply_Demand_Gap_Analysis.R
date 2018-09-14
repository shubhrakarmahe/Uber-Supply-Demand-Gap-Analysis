
# -------------------Assignment - Uber Supply- Demand Gap -------------------

# The aim of analysis is to identify the root cause of the problem 
# (i.e. cancellation and non-availability of cars) and recommend ways to improve 
# the situation. As a result of the analysis, we will be able to present 
# to the client the root cause(s) and possible hypotheses of the problem(s) and 
# recommend ways to improve them. 

# Note: For this assignment, only the trips to and from the airport are being considered.

# ------------------- Load the required libraries --------------------------------------

load.libraries <- c('data.table', 'ggplot2', 'lubridate', 'dplyr', 'reshape2')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs)
sapply(load.libraries, require, character = TRUE)

# -------------------Analyse uber dataset  - Preliminary---------------------------------
uber <- read.csv("Uber Request Data.csv")

# 6745 rows and 6 columns
dim(uber)
names(uber)

head(uber)
tail(uber)

summary(uber)
str(uber)

# Timestamps are in heterogenous format - One of the data issue

# Pickup.point - unordered categorical variable
# Status - unordered categorical variable
# Request.timestamp - interval type of variable 
# Drop.timestamp - interval type of variable

# -------------------Meta data about the source file--------------------------------------

# There are six attributes associated with each request made by a customer:

# Request id: A unique identifier of the request
# Time of request: The date and time at which the customer made the trip request
# Drop-off time: The drop-off date and time, in case the trip was completed 
# Pick-up point: The point from which the request was made
# Driver id: The unique identification number of the driver
# Status of the request: The final status of the trip, that can be either completed, 
#                        cancelled by the driver or no cars available

# ------------------- Data Cleaning and Preparation ------------------------------- 

# 3914 incomplete rows (all the rows having column Drop.timestamp as NA)
sum(!complete.cases(uber)) 

# 1. Duplicated values for uber data frame
# 6745 unique values for Request.id 
# (i.e. no duplicates for uber as Request.id is a unique identifier of given dataset)
uniqueN(uber$Request.id)

# 2. Missing values for uber

# 2.1 checking for NA values; 
# Driver.id - 2650 NA values, Drop.timestamp - 3914 NA values , Request.id - 0,
# Pickup.point - 0, Status - 0, Request.timestamp - 0

# NA values are not replaced with any value. and it is left as it is.
sapply(uber, function(x) length(which(is.na(x))))

#2.2  checking for blank "" values; there are none
sapply(uber, function(x) length(which(x == "")))

# 3. Uppercase lower case mismatch for Pickup.point and Status column

# no mismatch - (2 unique values Airport and City)
table(uber$Pickup.point) 

# no mismatch - (3 unique values - Cancelled, No Cars Available & Trip Completed)
table(uber$Status) 

# create backup for Request.timestamp and Drop.timestamp
uber$Request.timestamp.old <- uber$Request.timestamp
uber$Drop.timestamp.old <- uber$Drop.timestamp

# 4. Heterogenous Date format 
# date is present in two formats in uber dataframe- %d-%m-%Y %H:%M:%S and %d/%m/%Y %H:%M
summary(uber$Request.timestamp)
summary(uber$Drop.timestamp)

# new date format after using dmy_hms() is %Y-%m-%d %h:%m:%s UTC
uber$Request.timestamp <- dmy_hms(uber$Request.timestamp, truncated = 1)
uber$Drop.timestamp <- dmy_hms(uber$Drop.timestamp, truncated = 1)

str(uber)

# ------------------- Derived Metrics ------------------------------- 

# Derive new variables which will be useful for analysis.

# Extract date,time and hour from Request.timestamp and Drop.timestamp
uber$Request.date  <- format(uber$Request.timestamp,"%Y-%m-%d")
uber$Request.time <- format(uber$Request.timestamp,"%H:%M:%S")
uber$Request.hour <- format(uber$Request.timestamp,"%H")

uber$Drop.date  <- format(uber$Drop.timestamp,"%Y-%m-%d")
uber$Drop.time <- format(uber$Drop.timestamp,"%H:%M:%S")
uber$Drop.hour <- format(uber$Drop.timestamp,"%H")

# add Ride.duration as a new column
uber$Ride.duration.mins <- round((uber$Drop.timestamp - uber$Request.timestamp),0)

# add time slot to uber data frame
# 5 time slots - divided on the basis of hour
# 4-7 -early-morning , 8-11 -morning , 12-15 - afternoon, 16-19 - evening, 
# 20-23 - night, 00-03 - late-evening

uber[which(uber$Request.hour %in% c("04","05","06","07")),"Request.time.slot"] <- "early-morning"
uber[which(uber$Request.hour %in% c("08","09","10","11")),"Request.time.slot"] <- "morning"
uber[which(uber$Request.hour %in% c("12","13","14","15")),"Request.time.slot"] <- "afternoon"
uber[which(uber$Request.hour %in% c("16","17","18","19")),"Request.time.slot"] <- "evening"
uber[which(uber$Request.hour %in% c("20","21","22","23")),"Request.time.slot"] <- "night"
uber[which(uber$Request.hour %in% c("00","01","02","03")),"Request.time.slot"] <- "late-night"

# convert these variables to factor as they all are categorical variables
uber$Request.time.slot <- as.factor(uber$Request.time.slot)
uber$Request.date <- as.factor(uber$Request.date)
uber$Request.hour <- as.factor(uber$Request.hour)
uber$Drop.date <- as.factor(uber$Drop.date)
uber$Drop.hour <- as.factor(uber$Drop.hour)

str(uber)
summary(uber)
# ------------------- Univariate Analysis -----------------------------------------------

# All categorical variables and their frequecy are plotted using bar chart

table(uber$Pickup.point)

ggplot(uber, aes(Pickup.point, fill = Pickup.point)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = ..count..),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") + 
  guides(fill = FALSE)
# number of requests from city is 3507, a liitle higher than from airport(i.e 3238)

table(uber$Status)

ggplot(uber, aes(Status, fill = Status)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = ..count..),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") + 
  guides(fill = FALSE)
# count of (no cars available + cancelled) is much higher than "Trip Completed"

table(uber$Request.time.slot)

ggplot(uber, aes(Request.time.slot, fill = Request.time.slot)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = ..count..),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE)
# most of the requests are raised in evening time slot(16:00:00 to 19:59:59) 
# least number of requests at late-night slot(00:00:00 to 03:00:00)

table(uber$Request.date)

ggplot(uber, aes(Request.date, fill = Request.date)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = ..count..),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE)
# not a major difference in the number of requests raised between 2016-07-11 to 2016-07-15

table(uber$Request.hour)

ggplot(uber, aes(Request.hour, fill = Request.hour)) + 
  geom_bar(color = "black") +  
  guides(fill = FALSE)
# most of the requests are raised between 18:00:00 to 18:59:59 
# least number of requests are raised between 01:00:00 to 01:59:59

table(uber$Drop.date)

ggplot(uber, aes(Drop.date, fill = Drop.date)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = ..count..),vjust=-1) +
  guides(fill = FALSE)
# not a major difference in the number of drop between 2016-07-11 to 2016-07-15
# Majority of observations for drop.date are NA

table(uber$Drop.hour)

ggplot(uber, aes(Drop.hour, fill = Drop.hour)) + 
  geom_bar(color = "black") + 
  guides(fill = FALSE)
# there is a dip in the number of drops during 01 am to 4am
# Majority of observations for drop.hour are NA

# ------------------- Segmented Univariate Analysis--------------------------------------

# Status segmented by Pickup point
ggplot(uber, aes(Status, fill = Status)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = ..count..),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") + 
  facet_grid(.~Pickup.point) +
  guides(fill = FALSE)

# Request date segmented by Pickup point and fill by Request time slot
ggplot(uber, aes(Request.date, fill = Request.time.slot)) + 
  geom_bar(color = "black") + 
  facet_grid(.~Pickup.point)
# Geometry used is stacked bar chart and color fill is according to request.time.slot
# From airport - most of the requests are placed in night and evening time slots
# From City - most of the requests are placed in early-morning and morning time slot

# Request hour segmented by request date and pickup point
ggplot(uber, aes(Request.hour, fill = Request.hour)) + 
  geom_bar(color = "black") + 
  facet_grid(Request.date~Pickup.point)
# most of the requests are placed between 5-9 pm from airport
# and 5-9 am from city

#  Drop date segmented by Pickup point and fill by Request time slot
ggplot(uber, aes(Drop.date, fill = Request.time.slot)) + 
  geom_bar(color = "black") + 
  facet_grid(.~Pickup.point)
# lots of Na Values beacuse of cancellation and no cabs available

# Ride duration segmented by request time slot and pickup point
summary(as.numeric(uber$Ride.duration.mins))

ggplot(uber %>% na.omit(), aes(as.numeric(Ride.duration.mins))) + 
  geom_bar(color = "black") +
  facet_grid(Pickup.point~Request.time.slot)
# Minimum ride time is 21 mins & maximum ride time is 83 mins
# late night slot have least number of rides.

# ------------------- Problem Analysis ---------------------------------------------------

# What do you think is the reason for this issue for the supply-demand gap? 
# Write the answer in less than 100 words. 

# Reason for higher rate of Cancellation - 
# 1.  Airport trips from city takes long time to reach airport .
#     Again ,driver have to wait for next trip. 
#     It makes no economical sense if he comes back empty.
#     Based on flight patterns, there is a huge variance on the next trip a driver will get.

# Reason for "No Cabs Available" -
# 1. Most of the drivers will log out for the day before midnight .Hence, supply decreases.

# Plots are accompanied below to support hypothesis

#--------------------Visual Analysis -----------------------------------------------------------

# Visually identify the most pressing problems for Uber. 

#  1. Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; 

# Cancelled - 1264, No Cars Available - 2650 , Trip Completed - 2831
table(uber$Status)

# Graphical presentation - Overall [No. of requests per Status]
# Geometry used is bar chart and color is filled as per Status
ggplot(data = uber,
       aes(Status, fill = Status)) + 
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..),vjust=-1) +
  scale_fill_brewer(palette="Dark2") +
  guides(fill = FALSE) + 
  ylab("") +  
  xlab("") +
  ggtitle("Over-all frequency of requests as per Status") +
  theme(plot.title = element_text(color="black", size=12, face="bold"))


# Graphical presentation - No.of requests per Status displayed for airport & city on facet_grid
# Geometry used is bar chart and color is filled as per Status 
# segmented by pickup point by using facet_grid
ggplot(data = uber,
       aes(Status, fill = Status)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = ..count..),vjust=-1) +
  scale_fill_brewer(palette="Dark2") + 
  facet_grid(. ~ Pickup.point) + 
  guides(fill = FALSE) + 
  ylab("") +  
  xlab("") +
  ggtitle("Frequency of requests segmented by Pickup Point - Airport and City") +
  theme(plot.title = element_text(color="black", size=12, face="bold"))

#------------------------------------------------

# 2. identify the most problematic types of requests (city to airport / airport to city etc.) 
# and the time slots  (early mornings, late evenings etc.) using plots

uber_temp <- uber %>% 
  filter(Status != "Trip Completed") %>%
  group_by(Pickup.point,Request.time.slot,Status) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# Graphical presentation of most problematic types of requests & time slots
# Geometry used is bar chart and color is filled as per Status
# Segmented by Pickup point using facet_grid
ggplot(data = uber_temp, 
       aes(x = Request.time.slot, y = total ,fill = Status)) + 
  geom_bar(color = "black", position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set1") + 
  facet_grid(. ~ Pickup.point) + 
  xlab("") + 
  ylab("") +
  ggtitle("Most problematic types of requests and time-slots") +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

# Most Problematic type of request is "Airport to City"

# Airport to City - Most of the requests have "No Cab Available" status
#                 - for evening time slot (16:00:00 to 19:59:59)
#                 - and night time slot (20:00:00 to 23:59:59)

# City to Aiport - most of the requests are Cancelled by drivers.
#                - for early morning time slot (04:00:00 to 07:59:59)
#                - and morning time slot (08:00:00 to 11:59:59)

#------------------------------------------------

# 3.1 Find out the gap between supply and demand and show the same using plots.

supply.hour <- uber %>% 
  filter(Status == "Trip Completed") %>%
  group_by(Pickup.point,Request.date,Request.time.slot,Request.hour) %>%
  summarise(supply = n()) 

demand.hour <- uber %>% 
  group_by(Pickup.point,Request.date,Request.time.slot,Request.hour) %>%
  summarise(demand = n()) 

gap.hour = merge(supply.hour,demand.hour)

# convert gap.hour data frame  from wide to long format 
gap.hour_long <- melt(gap.hour, id.vars = c("Pickup.point","Request.date","Request.time.slot","Request.hour"))

# Graphical presentation of supply and demand at airport and city.
# Geometry used stacked bar chart to represent supply and demand gap at airport and city respectively
# Color is filled as per supply and demand variable value
ggplot(data = gap.hour_long, 
       aes(x = Request.hour, y = value, fill = variable)) + 
  geom_col() + 
  scale_fill_brewer(palette = "Set2") + 
  facet_grid(Request.date~Pickup.point) + 
  ylab("") + 
  xlab("Request hour") + 
  labs(fill = "") +
  ggtitle("Supply - Demand per hour for each date at Airport and City") +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

# Everyday the pattern is almost same for supply-demand.

#------------------------------------------------

# 3.2 Find the time slots when the highest gap exists

gap.hour$gap <- gap.hour$demand - gap.hour$supply

gap.agg <- gap.hour %>% group_by(Pickup.point,Request.time.slot) %>% 
  summarise(total.supply = sum(supply),total.demand = sum(demand),total.gap = sum(gap)) %>% 
  arrange(desc(total.gap))

print(gap.agg)

# stacked bar chart -geometry used to supply-demand gap per time slot segmented by pickup point
ggplot(data = gap.hour_long, 
       aes(x = Request.time.slot, y = value, fill = variable)) + 
  geom_col() + 
  scale_fill_brewer(palette = "Set2") + 
  facet_grid(.~Pickup.point) + 
  ylab("") + 
  xlab("Request time slot") + 
  labs(fill = "") +
  ggtitle("Supply - Demand per time slot at Airport and City") +
  theme(plot.title = element_text(color="black", size=14, face="bold"))


# highest supply-demand gap from airport is in between evening and night time slot
# highest supply-demand gap from city is in early - morning (04:00:00 to 07:59:59)

#------------------------------------------------
# 3.3 Find the types of requests (city-airport or airport-city) for which the  gap is the most severe in the identified time slots

# Airport to city requests suffers from supply-demand gap in the evening and night time slots

#------------------------------------------------------------------------------------------
# Recommend some ways to resolve the supply-demand gap.
# 1. We can give incentives to drivers to stay longer on the system to avoid "no cabs avaialable".
# 2. We can introduce dynamic pricing of the fare during peak hours to avoid "cab cancellation".

#-------------------------------------------------------------------------------------------
