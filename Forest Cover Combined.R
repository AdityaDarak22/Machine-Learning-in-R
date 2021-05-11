library(mlbench)
library(caret )
library(lattice)
library(dplyr)
library(ggplot2)
library(tidyr)
forest_cover <- read.csv("D:\\Msc Data Analytics\\Data Mining And Machine Learning\\Final Dataset for Project\\covtype.csv")
str(forest_cover)

#----------------------------Checking Missing Values-----------------------------------
sapply(forest_cover,function(x)any(is.na(x)))
# The dataset is been structured really well and there's no missing values

# the cover_type column seems to be unbalanced but we have reasonable number of observations
# for each at least

#-----------------------------Data Preprocessing -----------------------------------------
# We have 4 columns of wildnerness area so by using the gather function from the tidyverse
# package we are combining the wilderness columns into one column name region
# where all the 1 are classified as certain wilderness region
forest_cover <- forest_cover %>%
  gather(key=Region, value=region.indicator,Wilderness_Area1:Wilderness_Area4)%>%
  filter(region.indicator==1) %>%
  select(-region.indicator)

# here we are mentioning the name of the particular wilderness types
# if else is used to check if there is wilderness region present or not if yes then
# fill in with that name otherwise the wilderness 4 name will be written
forest_cover$Region <- ifelse(forest_cover$Region=="Wilderness_Area1","Rawah",
                              ifelse(forest_cover$Region=="Wilderness_Area2","Neota",
                                     ifelse(forest_cover$Region=="Wilderness_Area3","Comanche Peak", 
                                            "Cache la Poudre")))

# changing the region column from character to factor
forest_cover$Region <- as.factor(forest_cover$Region)
str(forest_cover)
table(forest_cover$Region)

# Similarly we will be converting the forest cover type table with the names of each
# cover type
# The names of the 7 cover type are
# 1:- Spruce/Fir
# 2 :- "Lodgepole Pine
# 3 :- Ponderosa Pine
# 4:- Cottonwood/Willow
# 5:- Aspen
# 6:- Douglas-fir
# 7:- Krummholz

# first structuring the table from int to character
forest_cover$Cover_Type <- as.character(forest_cover$Cover_Type)
forest_cover$Cover_Type <- ifelse(forest_cover$Cover_Type==1,"Spruce/Fir",
                                  ifelse(forest_cover$Cover_Type==2,"Lodgepole Pine",
                                         ifelse(forest_cover$Cover_Type==3,"Ponderosa Pine",
                                                ifelse(forest_cover$Cover_Type==4,"Cottonwood/Willow ",
                                                       ifelse(forest_cover$Cover_Type==5,"Aspen ",
                                                              ifelse(forest_cover$Cover_Type==6,"Douglas-fir ",
                                                                     "Krummholz"))))))

# Next we are going to bring all the 40 soil types into a one single column so that our 
# data is simplified for our future use

forest_cover <- forest_cover %>%
  gather(key = Soil, value = soiltype, Soil_Type1:Soil_Type40) %>%
  filter(soiltype == 1) %>%
  select(-soiltype)

forest_cover$Cover_Type <- as.factor(forest_cover$Cover_Type)
str(forest_cover)
forest_cover$Soil <- as.factor(forest_cover$Soil)

#-------------------------------------Visualisation-------------------------------
# theme_bw is used to show the visualisation on white background with black lines
# element blank draws nothing and assigns no space
# For Soil
ggplot(data = forest_cover) + 
  geom_bar(aes(x = Cover_Type, fill = Cover_Type),colour = "black")+
  facet_wrap(~Soil,scales = "free")+
  theme_bw()+
  xlab("Count")+
  ylab("Trees in Cover Type")+
  ggtitle("Cover Type vs Soil")+
  theme(axis.text = element_blank())+
  theme(legend.position = "bottom")

# For Region 
ggplot(data = forest_cover) + 
  geom_bar(aes(x = Cover_Type, fill= Cover_Type),colour = "black")+
  facet_wrap(~Region)+
  theme_bw()+
  xlab("Count")+
  ylab("Trees in Cover Type")+
  ggtitle("Cover Type vs Region")+
  theme(axis.text = element_blank())+
  theme(legend.position = "bottom")

# from the observation we can say that some trees do not exist in particular
# Wilderness Regions

#----------------------------------Data Partition---------------------------
# For Random Forest. I'll use a standard 70/30 split, train the model, then evaluate it and print the confusion matrix.
# here i am taking a sample of 20000 rows out of the total 5,00,000
# just to fasten our querying output
# because taking the whole data is taking a lot of time to generate a output
set.seed(123)
limitrows <- 20000
forest_cover = forest_cover[sample(nrow(forest_cover),limitrows),]
sample_split <- 0.7
model <- sample(nrow(forest_cover),round(sample_split*nrow(forest_cover)))
forest_train <- forest_cover[model,]
forest_test <- forest_cover[-model,]

#-------------------------------------------K-Nearest-Neighbour--------------------------------------

forest_control <- trainControl(method = 'repeatedcv',
                               number = 10,
                               repeats = 3)
set.seed(222)
forest_fit <- train(Cover_Type~Elevation+Aspect+Slope+ Horizontal_Distance_To_Hydrology,
                    data = forest_train,
                    method = 'knn',
                    tuneLength = 20,
                    trControl = forest_control,
                    preProc = c('center','scale'))

forest_fit
plot(forest_fit)
varImp(forest_fit)

forest_pred <- predict(forest_fit, newdata = forest_test)
confusionMatrix(forest_pred,forest_test$Cover_Type, mode = 'prec_recall')


#----------------------------------Applying random forest--------------------------------
library(randomForest)
forest_model <- randomForest(Cover_Type~., 
                             data = forest_train,
                             ntree= 200,
                             importance = TRUE)
#To find out which variable was significant in detecting the cover type we check the importance
Importance <- forest_model$importance
Importance
#Looks like elevation is the most important variable for all of the types of tree.
#The Aspect indicates which way the surface on which the tree is growing faces, 
#which must greatly affect how much sun the tree gets,

# predictions on test data set
prediction <- predict(forest_model,forest_test,type = 'class')

#Confusion Matrix
confusionMatrix(prediction,forest_test$Cover_Type, mode = "prec_recall")
# From the confusion Matrix we can say that our test data predicted the result 
# with an accuracy of 83.27%

# Random Forest Model Turned out to be the best performing model with an accuracy of 83.27%
# in comparison to KNN which had a accuracy of 69.7%