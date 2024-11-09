
#### Super easy first look at everything
library('fields')


#### Data
data1 <- read.csv('data/TX_Traffic.csv')
data1 <- data1[data1$CNTY_NM == 'Brazos',]

#### Plotting things
plot(data1$x, data1$y)

#### 
quilt.plot(data1$x, data1$y, data1$AADT_2022)


hist(data1$AADT_2022)


#### Subsetting data to middle 50% of x and y values
xquantl <- quantile(data1$x)[2]
xquantu <- quantile(data1$x)[4]

middlex <- (data1$x > xquantl)&(data1$x < xquantu)


yquantl <- quantile(data1$y)[2]
yquantu <- quantile(data1$y)[4]

middley <- (data1$y > yquantl)&(data1$y < yquantu)
data2 <-data1[(middlex&middley),]


plot(data2$x,data2$y)

quilt.plot(data2$x, data2$y,log(data2$AADT_2022))


## Generate graph 
library(igraph)

dtable <- rdist(cbind(data2$x,data2$y))

thresh <- quantile(dtable[dtable != 0],0.01355932)
#thresh <- quantile(dtable[dtable != 0],0.02)

adj_matrix <- 1*(dtable < thresh )
diag(adj_matrix)<-0

g1 <- graph_from_adjacency_matrix(adj_matrix, mode = 'undirected')

V(g1)$x <- data2$x
V(g1)$y <- data2$y

plot(data2$x, data2$y, col = palette[cut(log(data2$AADT_2022), 100)], pch = c(16))


palette <- colorRampPalette(c("blue","red1"))(100)

plot(g1, vertex.size = 3, vertex.label = NA,
     vertex.color = palette[cut(log(data2$AADT_2022), 100)])


knn
