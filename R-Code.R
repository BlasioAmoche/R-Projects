# IMPORTING THE THREE DATASETS FROM EXCEL TO RSTUDIO

# The following codes were used to merge the three datasets where:
# Dry = Dry Harvest vfm
# Wet = Wet Harvest vfm
# Box = Box Placement vfm

# SETING WORK DIRECTORY
setwd("/Volumes/Blass The Legend/R files/Pula files/Pula Worked on Distinct data")

library(readxl)
# We used the "read.excel"function to read the xlsx files from their odirectories

Dry <- read_excel("2 Dry.xlsx") # # Dry = Dry Harvest vfm
Box <- read_excel("2 Box.xlsx") # Box = Box Placement vfm
Wet <- read_excel("2 Wet.xlsx") # Wet = Wet Harvest vfm

# MERGE DATASETS
# First we merge the data using the "merge"function

first_merge <- merge(Box, Dry,
                  by = "ID_CaseID",
                  all.x = TRUE) # merging the first 2 datasets, Dry and Box to form new dataset, data_all

merged_data <- merge(first_merge, Wet,
                      by = "ID_CaseID",
                      all.x = TRUE) # merging the new dataset data_all with the third dataset Wet to form a 
                                    # new dataset called data_overall
# Data_overall is the merged file of the three datasets

View(merged_data) # viewing the dataset
str(merged_data) # checking on the structure of the dataset

#CONVERT CHARACTER VARIABLES TO NUMERIC

# There are dataframes that are in text formart yet supposed to be numeric. 
#These includes: D6_DryWeight_1st, D7_DryWeight2, W5_WetWeight_1st, and W6_WetWeight_2nd 
#We then convert the dataframes columns from text to numeric as follows: 
sapply(merged_data$W6_WetWeight_2nd, class) # checking if the variable is a character, numeric or factor
merged_data$D6_DryWeight_1st<- as.numeric(as.character(merged_data$D6_DryWeight_1st))
# converts D6_DryWeight_1st from character into numeric
merged_data$D7_DryWeight2 <- as.numeric(as.character(merged_data$D7_DryWeight2))
merged_data$W5_WetWeight_1st<- as.numeric(as.character(merged_data$W5_WetWeight_1st))
merged_data$W6_WetWeight_2nd<- as.numeric(as.character(merged_data$W6_WetWeight_2nd))
merged_data$B7_length_1st<- as.numeric(as.character(merged_data$B7_length_1st))
merged_data$B8_width<-as.numeric(as.character(merged_data$B8_width))
merged_data$B19_length_2nd<- as.numeric(as.character(merged_data$B19_length_2nd))
merged_data$B20_width_2nd<- as.numeric(as.character(merged_data$B20_width_2nd))



# DEALING WITH MISSING VALUES (NA)

is.na(merged_data$D6_DryWeight_1st) # Highlights missing vaues
sum(is.na(merged_data$D6_DryWeight_1st))
# Sum of missing values is 65 out of 118 for the variable D6_DryWeight_1st. Only
view(merged_data)


# OUTLIERS

#Using boxplots to assess the presence of outliers
library("ggplot2")

popa<- data.frame(x1= merged_data$D6_DryWeight_1st,
                  x2 = merged_data$D7_DryWeight2,
                  x3 = merged_data$W5_WetWeight_1st,
                  x4 = merged_data$W6_WetWeight_2nd)
boxplot(popa)

boxplot(popa,
        names = c("Dry_weight_1st_bag",
                  "Dry_weight_2nd_bag",
                  "Wet_weight_1st_box",
                  "Wet_weight_2nd_box"))

# CREATING NEW VARIABLES (average mt/hectoare)

# We wish to find the average mt/hectoare 
# Note that 1000 kgs = 1 mt, and 10000 square meters = 1 hectare
# let:
# Ave_Dry_1st_Box = Average yield/hectare for the dry Soybeans from first box calculated as: (weight/1000) / ((length*width)/10,000)
#Ave_Dry_2nd_Box = Average yield/hectare for the dry Soybeans from second box calculated as: (weight/1000) / ((length*width)/10,000)
#Ave_Wet_1st_Box = Average yield/hectare for the wet Soybeans from first box calculated as: (weight/1000) / ((length*width)/10,000)
#Ave_Wet_2nd_Box = Average yield/hectare for the wet Soybeans from second box calculated as: (weight/1000) / ((length*width)/10,000)
#Therefore:
library(tidyverse)
mergerNew<- merged_data
mergerNew<- transform(mergerNew, Ave_Dry_1st_Box = merged_data$D6_DryWeight_1st*10000*1/merged_data$B7_length*1/merged_data$B8_width*1/1000)
mergerNew<- transform(mergerNew, Ave_Dry_2nd_Box = merged_data$D7_DryWeight2*10000*1/merged_data$B19_length_2nd*1/merged_data$B20_width_2nd*1/1000)
mergerNew<- transform(mergerNew, Ave_Wet_1st_Box = merged_data$W5_WetWeight_1st*10000*1/merged_data$B7_length*1/merged_data$B8_width*1/1000)
mergerNew<- transform(mergerNew, Ave_Wet_2nd_Box = merged_data$W6_WetWeight_2nd*10000*1/merged_data$B19_length_2nd*1/merged_data$B20_width_2nd*1/1000)

view(mergerNew)

mergerFinal<- mergerNew
mergerFinal<- transform(mergerFinal, Ave_Yield_Dry = (mergerNew$Ave_Dry_1st_Box + mergerNew$Ave_Dry_2nd_Box)/2)
mergerFinal<- transform(mergerFinal, Ave_Yield_Wet = (mergerNew$Ave_Wet_1st_Box + mergerNew$Ave_Wet_2nd_Box)/2)

view(mergerFinal)

export(mergerFinal, "mergerFinal.xlsx")

# HOW TO GET AVERAGE YIELD PER HECTARE PER DISTRICT
mergerfinal2 <- mergerFinal

na.omit(mergerfinal2)


install.packages("ggregate")
library(ggregate)
library(dplyr)

# Find mean
aggregate(x = mergerFinal$Ave_Yield_Dry,                # Specify data column
         by = list(mergerFinal$B4_District),               # Specify group indicator
         FUN = mean, na.rm = TRUE)                           # Specify function (i.e. mean)

aggregate(x = mergerFinal$Ave_Yield_Wet,                # Specify data column
          by = list(mergerFinal$B4_District),               # Specify group indicator
          FUN = mean, na.rm = TRUE)   




