library('dplyr')
library('haven')
library('labelled')
library('here')
library('ggplot2')
library('fields')
library('stringr')

## Import data
MISNiger <- read_dta('data/NIPR82FL.dta')


## Getting understanding
labels(MISNiger)


## hml35 is what we want in order to access malaria rapid test results
MISNiger$hml35
MISNiger$hml33


## New shape file for geographic data
sf1 <- sf::read_sf('data/NIGE81FL/NIGE81FL.shp')
plot(sf1)

sf1$LATNUM


## Plotting data
ggplot(spData::world[(spData::world)$name_long == 'Niger',]) +
  geom_sf()+
  stat_sf_coordinates(data = sf1)

geo_data <- read.csv('data/NIGC81FL/NIGC81FL.csv')

names(sf1)


## Attempting to merge
MISNiger$hv001 ### This is the cluster number to merge on with sf1$DHSID
MISReduced <- data.frame(
  cbind(MISNiger$hv001,MISNiger$hml35)
)

names(MISReduced) <- c('ID', 'Malaria')
MISReduced <- na.omit(MISReduced)
MISReduced <- MISReduced[MISReduced$Malaria != 6,]


## New reduced sf object with only important details
sf1$ID <- as.numeric(str_sub(sf1$DHSID,-3))
sf1Reduced <- sf1[,c('ID','geometry','LATNUM','LONGNUM')]

## Merge sf1 with ID to MISreduced
df1 <- merge(sf1Reduced,MISReduced )


quilt.plot(df1$LONGNUM, df1$LATNUM, df1$Malaria)

ggplot(df1)+
  geom_sf(aes(color = Malaria))


