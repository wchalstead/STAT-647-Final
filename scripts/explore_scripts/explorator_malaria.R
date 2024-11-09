library(dplyr)
library(spdep)
library(fields)
library(sf)
library(ggplot2)
library(maps)
library(httr)
library(jsonlite)

test_data <- read.csv('data/malaria_by_country.csv')

test_data <- test_data[test_data$IND_CODE=='MALARIA_EST_INCIDENCE',]

dim(test_data)

test_data2 <- merge(test_data,spData::world, by.x = 'DIM_GEO_NAME', by.y = 'name_long')

head(test_data2)

sf_data <- st_as_sf(test_data2)

sf_data <- sf_data[sf_data$continent == 'Africa',]

ggplot(sf_data)+geom_sf(aes(fill=VALUE_NUMERIC))

ggplot(sf_data)+geom_sf(aes(fill=log(1+VALUE_NUMERIC*pop/1000)))

ggplot(sf_data)+geom_sf(aes(fill=(VALUE_NUMERIC*pop/1000)))

ggplot(sf_data)+geom_sf(aes(fill=(gdpPercap)))


A <- st_touches(sf_data)
A    <- as.matrix(A)
m    <- rowSums(A)
adj  <- apply(A==1,1,which)
adj[1:5]


W<-as.matrix(A)
nb1<-mat2listw(W, zero.policy = T)
summary(nb1)

s <- st_centroid(sf_data)$geom

pdf('figures/connection.pdf')
map('world',regions = sf_data$DIM_GEO_NAME)
plot.listw(nb1,coords=s,add=T,col=4,lwd=2)
dev.off()



#### Testing nbcost function

x_test <- 1*(st_coordinates(s)[,2] > 7)

sf_data_test <- sf_data
sf_data_test$x_test <- x_test

sf_data_test <- sf_data_test[rowSums(A) != 0,]

ggplot(sf_data_test)+geom_sf(aes(fill=x_test))

nb_test <- poly2nb(sf_data_test)

lcosts <- nbcosts(nb_test, x_test)
nb.w <- nb2listw(nb_test, lcosts, style="B")
mst.bh <- mstree(nb.w,5)

coords <- as.matrix(st_coordinates(s[rowSums(A) != 0,]))

plot(sf_data$geom)
plot(mst.bh, coords, col = 2, cex.lab = .6, cex.circles = 0.035, fg = 'blue', add = T)


k=2 ## three clusters

res1 <- skater(mst.bh[,1:2], x_test, k-1) 
membership = res1$groups
### groups size
table(membership) 

opar <- par(mar=c(0,0,0,0))
plot(res1,  coords, cex.circles=1, cex.lab=.7) 

plot(sf_data$geom)
plot.mst(rbind(res1$edges.groups[[1]]$edge,res1$edges.groups[[2]]$edge), coords, col = 2, cex.lab = .6, cex.circles = 0.035, fg = 'blue', add = T)

plot(sf_data_test$geom, col=heat.colors(length(res1$edg))[res1$groups])

