library(caret) # classification and regression 
library(ggplot2) # visualisation
library(lattice) 
library(mice)# Multivariate imputations by chained equations
library(dplyr) # Manipulation 
library(ggExtra)
library(maps)
library(ModelMetrics)
#-----------------------------------Reading the Data------------------------------------
airbnb <- read.csv("D:\\Msc Data Analytics\\Data Mining And Machine Learning\\Final Dataset for Project\\AB_NYC_2019.csv")
summary(airbnb)
str(airbnb)

#------------------Calculating the percentage of data missing-----------------------
per <- function(x) {sum(is.na(x))/length(x)*100}
apply(airbnb,2,per)
# here we can say that reviews_per_month has 20.55% missing data
#Another way of searching for missing data is 
md.pattern(airbnb)

#----------------------------------Removing the NA's from the table--------------------
# here we can say that reviews per month has 10052 null values we would be taking the
# 0 for all the values and fill the null values with 0 
airbnb$reviews_per_month <- ifelse(is.na(airbnb$reviews_per_month),0,
                                   airbnb$reviews_per_month)
summary(airbnb)

# price columns has some value which are zero so we have to remove those values 
airbnb <- airbnb[-which(airbnb$price==0),]


filter_price <- airbnb%>%filter(price <1000)
histogram(filter_price$price)
# from the histogram we can say that the majority price of the property lies in 
# range of 0 to 500 so we are going to remove the outliers 

# there are many outliers value in price column so we are going to select values
# from 0 to 500 
airbnb <- airbnb %>% filter((price >0)&(price<500))

#-------------Removing Unwanted columns which does not showcase any significance-----
airbnb <- select(airbnb,-c(id,name,host_id,host_name,last_review))


#------------------------------------Visualizations----------------------------------
airbnb %>%
  ggplot(aes(x= neighbourhood_group, fill = room_type))+
  geom_bar()+
  xlab("Neighbourhood")+
  ylab("Room type Listings")+
  ggtitle("Room type Listings in Neighbourhood")

ggplot(data=airbnb)+
  geom_point(aes(x=latitude, y=longitude, color=neighbourhood_group))

#------------------------Data Partioning into train and test-------------------------
# call catools library
library(caTools)
set.seed(123)

airbnb_split = sample.split(airbnb$price, SplitRatio = 0.70)
train_airbnb <- subset(airbnb, airbnb_split == TRUE)
test_airbnb <- subset(airbnb, airbnb_split == FALSE)

#----------------------------Multiple Regression Model--------------------------
mlr_model <- lm(price~neighbourhood_group + latitude + longitude + room_type + minimum_nights + availability_365,data = train_airbnb)
summary(mlr_model)


# Since the value of R-squared is less than 0.5 we would be applying logarithmic 
# transformation to the price column 

mlr_model2 <- lm(log(price)~neighbourhood_group + latitude + longitude + room_type + minimum_nights + availability_365,data = train_airbnb)
summary(mlr_model2)

#------------------------------Predicting the values for the test data---------
pred_mlr <- predict(mlr_model2, newdata = test_airbnb)

#Calculating RMSE
rmse <- RMSE(pred_mlr, test_airbnb$price)
rmse

#Calculating MSE
mse <- mse(pred_mlr, test_airbnb$price)
mse

#Calculating MAE
mae <- MAE(pred_mlr, test_airbnb$price)
mae


#--------------------------------------Random Forest Regression-------------------------
library(randomForest)
rf_model <- randomForest(price~neighbourhood_group+room_type+latitude+longitude+
                               minimum_nights+number_of_reviews+reviews_per_month+availability_365,
                             data = train_airbnb,
                             mtry = 3,
                             importance = TRUE)

summary(rf_model)
Importance <- rf_model$importance
Importance


prediction_rf <- predict(rf_model, test_airbnb, type = 'resp')


#Calculating RMSE
rmse <- RMSE(prediction_rf, test_airbnb$price)
rmse

#Calculating MSE
mse <- mse(prediction_rf, test_airbnb$price)
mse

#Calculating MAE
mae <- MAE(prediction_rf, test_airbnb$price)
mae
