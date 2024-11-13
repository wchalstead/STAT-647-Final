library('dplyr')
library('haven')
library('labelled')
library('here')
library('ggplot2')
library('fields')
library('stringr')
library('spmodel')
library('sf')

### New dataset found:
# https://malariaatlas.org/

## Import data
MISNiger <- read_dta('data/NigerPointData/NIPR82FL.dta')


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


quilt.plot(jitter(df1$LONGNUM), jitter(df1$LATNUM), df1$Malaria)

ggplot(df1,aes(color = as.factor(Malaria), alpha = 0.1))+
  geom_point(aes(x = jitter(LONGNUM, 125), y = jitter(LATNUM,500)))+
  scale_color_manual(values=c('LightGreen','Red'))

ggplot(df1)+
  geom_point(aes(x = jitter(LONGNUM), y = jitter(LATNUM), color = Malaria))


### Now need to extract geographic coefficients
spatial_coef <- read.csv('data/NIGC81FL/NIGC81FL.csv')
spatial_coef$ID <- as.numeric(str_sub(sf1$DHSID,-3))


## Going to reduce this to less coefs
spatial_coef <- spatial_coef[,c(131,11,16,21,26,27,28,33,38,46,66,71,76,82,92,97,120,125,130)]
######################### Can change this ^ part later to include more variables

df1 <- merge(df1, spatial_coef)


plot(df1)


##### Fit basic glm
model1 <- glm(Malaria ~  Rainfall_2020+ Mean_Temperature_2020, 
              data = df1, 
              family = binomial)

summary(model1)
plot(df1$LATNUM, model1$residuals)
plot(df1$LONGNUM, model1$residuals)

quilt.plot(
  (scale(df1$LONGNUM) - min(scale(df1$LONGNUM)))**0.5,
  (scale(df1$LATNUM) - min(scale(df1$LATNUM)))**0.5,
  model1$residuals
)


ggplot(cbind(df1, model1$residuals),aes(color = model1.residuals))+
  geom_sf()+
  scale_color_viridis_c(option = 'turbo')

ggplot(cbind(df1, model1$residuals),aes(color = model1.residuals))+
  geom_point(aes(x = jitter(LONGNUM, 250), y = jitter(LATNUM,250)))+
  scale_color_viridis_c(option = 'turbo')

ggplot(cbind(df1, residuals(model1)),aes(color = residuals.model1.))+
  geom_point(aes(x = jitter(LONGNUM, 250), y = jitter(LATNUM,250)))+
  scale_color_viridis_c(option = 'turbo')


### Step-wise regression example:
step_glm <- glm(Malaria ~  .,
                     data = na.omit(st_drop_geometry(df1)),
                     family = binomial)


both_model <- step(step_glm, direction = "both", trace = 0)

ggplot(cbind(na.omit(df1), both_model$residuals),aes(color = both_model.residuals))+
  geom_point(aes(x = jitter(LONGNUM, 250), y = jitter(LATNUM,250)))+
  scale_color_viridis_c(option = 'turbo')


ggplot(cbind(na.omit(df1), res = (na.omit(df1)$Malaria - both_model$fitted.values)),aes(color = res))+
  geom_point(aes(x = jitter(LONGNUM, 250), y = jitter(LATNUM,250)))+
  scale_color_viridis_c(option = 'turbo')

plot(both_model)



########

### Fit spatial glm
model2 <- spglm(Malaria ~  Rainfall_2020+ Mean_Temperature_2020, 
              data = df1, 
              family = binomial,
              spcov_type = 'matern')

ggplot(cbind(df1, residuals(model2)),aes(color = residuals.model2.))+
  geom_point(aes(x = jitter(LONGNUM, 250), y = jitter(LATNUM,250)))+
  scale_color_viridis_c(option = 'turbo')

plot(df1$LATNUM, residuals(model2))
plot(df1$LONGNUM, residuals(model2))
