rm(list=ls())
library(data.table)
data_fri <- fread('comm-data-Fri.csv', header = T, sep = ',')
data_sat <- fread('comm-data-Sat.csv', header = T, sep = ',')
data_sun <- fread('comm-data-Sun.csv', header = T, sep = ',')

# Plot communication based on location and timestamp as x-axis.
# Cluster communication based on location.
# Find out person that communicates with multiple persons under a specific time frame.
# Find out group based on if the communication has been frequent between people.
# Longest common subsequence to determine who communicates with the most people under a sequence.
# Flag suspect person based on communication patterns.

#Plot communication distribution over locations for Friday.
table_fri <- as.data.table(table(data_fri$location)) 
xx <- barplot(table_fri$N, names.arg = table_fri$V1, cex.names = 0.8, ylim = c(0, max(table_fri$N)+30000), main = "Communication distribution over locations for Friday")
text(x = xx, y = table_fri$N, label = table_fri$N, pos = 3, cex = 0.8, col = "red")


