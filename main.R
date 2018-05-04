rm(list=ls())
library(data.table)
data_fri <- fread('comm-data-Fri.csv', header = T, sep = ',')
counter = 0

for( i in 1:length(data$from) ){
  counter++
}
