library(dplyr)
library(spdep)
library(fields)
library(sf)
library(ggplot2)
library(maps)


test_data <- read.csv('data/malaria_by_country.csv')

test_data <- test_data[test_data$IND_CODE=='MALARIA_EST_INCIDENCE',]

dim(test_data)

test_data2 <- merge(test_data,spData::world, by.x = 'DIM_GEO_NAME', by.y = 'name_long')

head(test_data2)

plot(test_data2)

sf_data <- st_as_sf(test_data2)

sf_data <- sf_data[sf_data$continent == 'Africa',]

ggplot(sf_data)+geom_sf(aes(fill=VALUE_NUMERIC))

ggplot(sf_data)+geom_sf(aes(fill=log(1+VALUE_NUMERIC*pop/1000)))

ggplot(sf_data)+geom_sf(aes(fill=(VALUE_NUMERIC*pop/1000)))


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

