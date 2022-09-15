#_______________________________________________________________________________________________________________________________________________
#Setting Working directory and improting data
#_______________________________________________________________________________________________________________________________________________
setwd("/Volumes/Blass The Legend/lifecare")
getwd()
install.packages("readxl")
library(readxl)

# read the distance and the speed datasets from the specific sheets
distance<- read_excel("Lizdata.xlsx", sheet = "distance")
speed<- read_excel("Lizdata.xlsx", sheet = "speed")

#_______________________________________________________________________________________________________________________________________________
#Data manipulation
#_______________________________________________________________________________________________________________________________________________
#combine the two datasets
df<- data.frame(cbind(distance, speed))
df
#subset the df dataframe. for more information: https://www.statology.org/subset-data-frame-in-r/
df1<- df[, c("Distance", "Units", "vehicle", "speed", "Units.1")]
df1
# rename the column names
colnames(df1)<-c("distance", "dist.Units", "vehicle", "speed", "speed.units" )
df1

#replace the "NA", "_", "99", or " blank space" with missing vales
## use the following link to read more about replacing with missing values: https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html or https://www.digitalocean.com/community/tutorials/replace-in-r 
install.packages("naniar")
library(naniar)
str(df1)# check if the column structures 
df1$distance<- as.numeric(as.character(df1$distance))# convert distance from chr to numeric
df1$speed<- as.numeric(as.character(df1$speed))# convert speed from chr to numeric
df1 %>% replace_with_na(replace = list(distance = c(99.0, "_"), speed= c("NA")))# replace the stated entries with mising values
df1$distance[df1$distance== 99.0]<- NA# replace the 99.0 with missing value otherwise considered 
df1
#_______________________________________________________________________________________________________________________________________________
# Get summary statistics of the distance and speed. Source: https://statsandr.com/blog/descriptive-statistics-in-r/
#_______________________________________________________________________________________________________________________________________________

df2<- df1[, c("distance", "speed")]# dataset for only numeric values
summary(df2)# find summary statistics for the df2

df3<- df1[, c("distance", "speed", "vehicle")]# dataset for three variables; distance, speed, vehicle
by(df3, df3$vehicle, summary)# summaries of df3 grouped by the vehicle type

# Find the correlation between speed and distance

# Correlation plot. Source: https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/
cor(df2$distance, df2$speed, use = "pairwise")# this is Pearson Moment correlation, applicable if the sample is large, n >= 30
cor(df2$distance, df2$speed, method = "spearman", use = "pairwise")# one can use="complete", or "everything"
?cor # use this for help to learn more about correlatio

#Combination of correlation coefficients and correlation tests

install.packages("ggstatsplot")
library(ggstatsplot)
ggscatterstats(df3,
               x=distance,
               y=speed,
               bf.message = F)
#_______________________________________________________________________________________________________________________________________________
#Simple Linear Regression using R
#_______________________________________________________________________________________________________________________________________________

library(tidyverse)
library(ggpubr)

# visualization (Scaterplot)

ggplot(df2,
       aes(x=distance, y=speed)) +
       geom_point() + # point geom is used to create scatterplot
       stat_smooth() # aids the eyes in seeing parterns in the pressence of overplotting
                    # NOTE: the grapgh demonstrate a relatively linear relationship between distance and speed

# Combutation of a simple linear model

model<- lm(speed~distance, data = df2)# this gives the beta coefficients (regression coefficients)
model # Interpretation: speed = 2.1287 + 0.3895 distance

# Regression Line

ggplot(df2,
       aes(x=distance, y=speed)) + geom_point() + stat_smooth(method = lm) # scatterplot with the best line of fit

# Model Assessment

## 1. Model Summary

summary(model)

# the unstandardized beta coefficient was found to be insignificant; b=0.3895, p-value = 0.0772 > 0.05.
# it was therefore concluded that there was no signfcant causal linear relationship between distance and speed





