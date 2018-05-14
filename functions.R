com_dist_bar <- function(table_data, day){
  day <- paste("Communication distribution over locations on", day)
  ggplot(data=table_data, aes(x=V1, y=N, fill=V1)) +
    geom_bar(colour="black", stat="identity") + 
    scale_y_continuous(labels = scales::comma) +
    geom_text(aes(label=N), vjust=-0.3, size=3.5) +
    xlab("Park Locations") +
    ylab("Number of messages") + 
    ggtitle(day)
}

com_dist_linear <- function(linear_data, day){
  day <- paste("Total amount of messages sent during", day)
  linear_data <- linear_data[!(linear_data$N < 100), ]
  linear_data$date <- ymd_hms(linear_data$V1)
  ggplot(data = linear_data, aes(x = date, y = N)) + 
    geom_line() + 
    scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M")) +
    xlab("Time") +
    ylab("Number of messages") +
    ggtitle(day) +
    theme(text = element_text(size=30),
          axis.text.x = element_text(angle=70, hjust=1)) 
}

com_dist_scatter_location <- function(scatter_data, day){
  day <- paste("Scatter plot of communication distribution on each location for", day)
  scatter_data <- scatter_data[!(scatter_data$N < 110), ]
  scatter_data$date <- ymd_hms(scatter_data$V1)
  names(scatter_data)[names(scatter_data) == 'V2'] <- 'location'
  ggplot(scatter_data, aes(x = date, y = N, fill = location)) + 
    geom_point(shape = 21, size = 6, stroke = 2, colour = "black",aes(fill = location)) + 
    scale_x_datetime(breaks = date_breaks("30 min"), labels = date_format("%H:%M")) + 
    labs(x = "Time") + labs(y = "Number of messages") + 
    ggtitle(day) +
    theme(text = element_text(size=25),
          axis.text.x = element_text(angle=70, hjust=1)) 
}

group_graph <- function(graph_data, day){
  day <- paste("Group of park visitors who communicates frequently on", day)
  graph_data <- as.matrix(graph_data)
  g <- graph.data.frame(graph_data)
  edge = as_ids(E(g))
  edge = as.data.frame(edge)
  split = as.data.table(cSplit(edge, "edge", "|"))
  split[, ':='(dummy = 1,
               key = paste(edge_1, edge_2, sep = "_"))]
  split = unique(split[, .(edge_1, edge_2, sum(dummy)), by = key])[, 2:4, with =
                                                                     F]
  g <- as.data.table(split)
  g <- g[!g$edge_1 == 1278894,]
  g <- g[!g$edge_2 == 1278894,]
  g <- g[!g$edge_1 == 839736,]
  g <- g[!g$edge_2 == 839736,]
  g <- g[!g$V3 < 30,]
  
  g <- g[!g$V3 < 30,]
  
  g <- graph.data.frame(g)
  SCC <- clusters(g, mode="strong")  
  V(g)$color <- rainbow(SCC$no)[SCC$membership]
  V(g)$label.cex = 0.55
  
  plot(g, mark.groups = split(1:vcount(g), SCC$membership),edge.arrow.size = 0.1, vertex.size = 7)
  title(day,cex.main=2)
}
