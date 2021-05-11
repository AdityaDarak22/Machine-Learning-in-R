library(e1071)
library(dplyr)
library(ggplot2)
library(psych)
library(caTools)

# overview of the data
health <- read.csv("D:\\Msc Data Analytics\\Data Mining And Machine Learning\\Final Dataset for Project\\train_2v.csv")
summary(health)

# we have three data types in the dataset: categorical, numerical, IDs
# 12 variables and 43400 observations
str(health)


#-----------------------------------------DATA PREPROCESSING------------------------

# BMI has 3.4% of missing values.
sort(apply(health,2,function(x){sum(is.na(x))/length(x)}*100), decreasing = TRUE)

# Replacing NA with mean value in bmi
health$bmi<-ifelse(is.na(health$bmi), mean(health$bmi, na.rm = TRUE), health$bmi)

# Replacing blanks with "unknown", for smoking status
health$smoking_status[health$smoking_status == ""] <- NA
health$smoking_status <- as.factor(ifelse(is.na(health$smoking_status),"Unknown",paste(health$smoking_status)))     

#-----------------------------------b---------------------------------------

#Eliminating attributes which are of no use (IDs)
health<-subset(health, select=-c(id))
