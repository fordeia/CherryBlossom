#1. Loading Packages
library(randomForest)
library(ggplot2)
library(readxl)
library(tidyverse)
library(caret)
library(ranger)

#2. Loading Data
CBloom4_24 <-read.table("Cherryblossom_2004-2024.txt",header =TRUE,sep="\t", fill = TRUE)

#3. Data preparation
# Create a new variable by summing two existing variables
CBloom4_24$OceTemp <- 2*CBloom4_24$EL.NINO+CBloom4_24$LA.NINA

#Dropping unwanted variables
CBloom4_24<-CBloom4_24[ , -c(1,3, 4, 5, 6,10,11,12,13,14,15,16)]

#Dropping rows with NA
CBloom4_24<-CBloom4_24[-c(22,23,24,25,26),]

#3. Fitting the random forest model with original dataset. 
set.seed(1237)
CBloom4_24.rf <- randomForest(PEAK ~ ., data=CBloom4_24, importance=TRUE, proximity=TRUE)

sqrt(sum((CBloom4_24.rf$predicted - CBloom4_24$PEAK)^2) / nrow(CBloom4_24))

set.seed(1234)
system.time(rf_fit<-train(PEAK~.,data = CBloom4_24, method ='rf', importance =TRUE, 
                          trControl = trainControl(method = 'cv', number = 10 )))
rf_fit
#Evaluate variable Importance
varImp(rf_fit)

#OOB errors
rf_fit$err.rate[,1]

#Using Ranger
set.seed(25)
system.time(ranger_fit<-train(PEAK~.,data = h_train, method ='ranger', importance ='impurity', 
                          trControl = trainControl(method = 'cv', number = 10 )))
ranger_fit

#Evaluating variance importance
varImp(ranger_fit)





