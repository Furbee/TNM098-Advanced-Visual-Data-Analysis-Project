rm(list=ls())
library(data.table)
library(scales)
library(ggplot2)
library(lubridate)
library(dplyr)
library(igraph)


data_fri <- fread('comm-data-Fri.csv', header = T, sep = ',')
data_sat <- fread('comm-data-Sat.csv', header = T, sep = ',')
data_sun <- fread('comm-data-Sun.csv', header = T, sep = ',')

# Plot communication based on location and timestamp as x-axis.
# Cluster communication based on location.
# Find out person that communicates with multiple persons under a specific time frame.
# Find out group based on if the communication has been frequent between people.
# Longest common subsequence to determine who communicates with the most people under a sequence.
# Flag suspect person based on communication patterns.

# Plot communication distribution over locations for Friday as bar chart.
table_fri <- as.data.table(table(data_fri$location)) 
xx <- barplot(table_fri$N, names.arg = table_fri$V1, cex.names = 0.8, ylim = c(0, max(table_fri$N)+30000), main = "Communication distribution over locations for Friday")
text(x = xx, y = table_fri$N, label = table_fri$N, pos = 3, cex = 0.8, col = "red")

# Plot communication distribution over locations for Saturday as bar chart.
table_sat <- as.data.table(table(data_sat$location)) 
xx <- barplot(table_sat$N, names.arg = table_sat$V1, cex.names = 0.8, ylim = c(0, max(table_sat$N)+65000), main = "Communication distribution over locations for Saturday")
text(x = xx, y = table_sat$N, label = table_sat$N, pos = 3, cex = 0.8, col = "red")

# Plot communication distribution over locations for Sunday as bar chart.
table_sun <- as.data.table(table(data_sun$location)) 
xx <- barplot(table_sun$N, names.arg = table_sun$V1, cex.names = 0.8, ylim = c(0, max(table_sun$N)+60000), main = "Communication distribution over locations for Sunday")
text(x = xx, y = table_sun$N, label = table_sun$N, pos = 3, cex = 0.8, col = "red")

# Plot communication distribution over time for Friday as linear line plot.
test_fri <- as.data.table(table(data_fri$Timestamp))
test_fri<-test_fri[!(test_fri$N<100),]
test_fri$date <- ymd_hms(test_fri$V1)
ggplot(data = test_fri, aes(x = date, y = N)) + geom_line() + scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M"))

# Plot communication distribution over time for Saturday as linear line plot.
test_sat <- as.data.table(table(data_sat$Timestamp))
test_sat<-test_sat[!(test_sat$N<100),]
test_sat$date <- ymd_hms(test_sat$V1)
ggplot(data = test_sat, aes(x = date, y = N)) + geom_line() + scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M"))

# Plot communication distribution over time for Sunday as linear line plot.
test_sun <- as.data.table(table(data_sun$Timestamp))
test_sun<-test_sun[!(test_sun$N<100),]
test_sun$date <- ymd_hms(test_sun$V1)
ggplot(data = test_sun, aes(x = date, y = N)) + geom_line() + scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M"))

# Heatmap of total amount of communication for all days.
total <- rbind(data_fri, data_sat, data_sun)
total <- as.data.table(table(total$Timestamp))
total<-total[!(total$N<350),]
total$V1 <- ymd_hms(total$V1)
total$date <- as.Date(total$V1, format = "%Y-%m-%d")
total$hm <- format(total$V1, "%H:%M:%S")
lab <- unique(with(total, paste(format(total$V1, "%H:00"), "00", sep = ":")))

ggplot(data = total, aes(x = date, y = hm)) + 
  geom_tile(aes(fill = N)) + 
  scale_fill_gradientn(colors = c("Yellow", "Orange", "Red"), 
                      limits=range(total$N)) +  
  scale_y_discrete( breaks = lab)

# Plot number of messages sent during the specific day (fri,sat & sunday). 
test2 = as.data.table(table(data_sat$Timestamp,data_sat$location))
test2<-test2[!(test2$N<140),] 
test2$date <- ymd_hms(test2$V1)
names(test2)[names(test2) == 'V2'] <- 'location'
d<-ggplot(test2, aes(x= date, y = N, fill = location)) + geom_point(aes(colour = location)) + scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M"))
d + labs(x = "Time") + labs(y="Number of messages")




## TESTING ONLY ##
library(qgraph)
require(splitstackshape)
test <- data_fri[,2:3]
test <- as.matrix(test)
g <- graph.data.frame(test)
edge=as_ids(E(g))
edge=as.data.frame(edge)
split = as.data.table(cSplit(edge,"edge","|"))
split[,':='(dummy=1,
            key = paste(edge_1,edge_2,sep="_"))]
split = unique(split[,.(edge_1,edge_2,sum(dummy)),by=key])[,2:4,with=F]
lol <- as.data.table(split)
#g <- graph.data.frame(lol)
#plot(g, edge.arrow.size = 0.1,vertex.label=NA)

#Testing only. Every 5 minute for one hour with one hour break from 12:00.
test2 <- data_fri
test2 <- test2[!test2$from != 1278894,]
test2 <- as.data.table(table(test2$from, test2$Timestamp))
test2$date <- ymd_hms(test2$V2)
ggplot(data=test2, aes(x=date, y=N)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + 
  scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M"))