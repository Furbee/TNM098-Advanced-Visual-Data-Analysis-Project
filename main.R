rm(list = ls())
setwd("~/Documents/DavidTran/Projects/TNM098-Advanced-Visual-Data-Analysis-Project")
source("functions.R")
library(data.table)
library(scales)
library(ggplot2)
library(lubridate)
library(igraph)

## READ DATA FILES ##
data_fri <- fread('comm-data-Fri.csv', header = T, sep = ',')
data_sat <- fread('comm-data-Sat.csv', header = T, sep = ',')
data_sun <- fread('comm-data-Sun.csv', header = T, sep = ',')

# Plot communication based on location and timestamp as x-axis.
# Cluster communication based on location.
# Find out person that communicates with multiple persons under a specific time frame.
# Find out group based on if the communication has been frequent between people.
# Longest common subsequence to determine who communicates with the most people under a sequence.
# Flag suspect person based on communication patterns.

# Plot communication distribution over locations for Friday, Saturday and Sunday as bar chart.
table_fri <- as.data.table(table(data_fri$location))
table_sat <- as.data.table(table(data_sat$location))
table_sun <- as.data.table(table(data_sun$location))
table_fri$day <- c("Friday")
table_sat$day <- c("Saturday")
table_sun$day <- c("Sunday")

com_dist_bar(table_fri, day = "Friday")
com_dist_bar(table_sat, day = "Saturday")
com_dist_bar(table_sun, day = "Sunday")

# Bar charts for each day combined
total <- rbind(table_fri, table_sat, table_sun)
total = melt(total)
ggplot(data=total, aes(x=V1, y=value, fill=day)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label=value),position=position_dodge(width = 1), vjust=-0.3, size=7.5) +
  xlab("Park Locations") +
  ylab("Number of messages") +
  ggtitle("Communication distribution over locations during the weekend") +
  theme(plot.title = element_text(size = 25),text = element_text(size=30))

# Plot external communication during Sunday 
ext <- data_sun
ext <- ext[!ext$to != 'external',]
ext = as.data.table(table(ext$Timestamp, ext$location))
ext <- ext[!(ext$N < 2), ]
ext$date <- ymd_hms(ext$V1)
names(ext)[names(ext) == 'V2'] <- 'location'
p <- ggplot(ext, aes(x = date, y = N, fill = location)) + geom_point(shape = 21,aes(colour = location)) + scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M"))
p + labs(x = "Time") + labs(y = "Number of messages")
p + scale_y_continuous(breaks=seq(0,20,1))

# Plot communication distribution over time for Friday, Saturday and Sunday as linear line plot.
linear_fri <- as.data.table(table(data_fri$Timestamp))
linear_sat <- as.data.table(table(data_sat$Timestamp))
linear_sun <- as.data.table(table(data_sun$Timestamp))
com_dist_linear(linear_fri, day = "Friday")
com_dist_linear(linear_sat, day = "Saturday")
com_dist_linear(linear_sun, day = "Sunday")

# Plot number of messages sent on each location during Friday, Saturday and Sunday as scatter plots
scatter_fri = as.data.table(table(data_fri$Timestamp, data_fri$location))
scatter_sat = as.data.table(table(data_sat$Timestamp, data_sat$location))
scatter_sun = as.data.table(table(data_sun$Timestamp, data_sun$location))

com_dist_scatter_location(scatter_fri, day = "Friday")
com_dist_scatter_location(scatter_sat, day = "Saturday")
com_dist_scatter_location(scatter_sun, day = "Sunday")

# Plot network graph using iGraph package, reveal group of park visitors for each day.
graph_fri <- data_fri[, 2:3]
graph_sat <- data_sat[, 2:3]
graph_sun <- data_sun[, 2:3]

group_graph(graph_fri, day = "Friday")
group_graph(graph_sat, day = "Saturday")
group_graph(graph_sun, day = "Sunday")

## TESTING PHASE ONLY ##
#Testing only. Every 5 minute for one hour with one hour break from 12:00.
test2 <- data_sat
test2 <- test2[!test2$from != 1278894, ]
test2 <- as.data.table(table(test2$from, test2$Timestamp))
test2$date <- ymd_hms(test2$V2)
ggplot(data = test2, aes(x = date, y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M"))+
  xlab("Time") +
  ylab("Number of messages") +
  ggtitle("Communication pattern for ID 1278894 on Saturday") +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=70, hjust=1)) 

test2 <- data_sun
test2 <- test2[!test2$from != 839736, ]
test2 <- as.data.table(table(test2$from, test2$Timestamp))
test2$date <- ymd_hms(test2$V2)
ggplot(data = test2, aes(x = date, y = N)) +
  geom_point(shape = 21, size = 6, stroke = 2, colour = "black", fill = "firebrick2") + 
  scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M"))+
  xlab("Time") +
  ylab("Number of messages") +
  ggtitle("Communication pattern for ID 839736 on Sunday") +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=70, hjust=1)) 