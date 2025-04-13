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
CBloom4_24.rg <- randomForest(PEAK ~ ., data=CBloom4_24, importance=TRUE,
                        proximity=TRUE)

sqrt(sum((CBloom4_24.rg$predicted - CBloom4_24$PEAK)^2) / nrow(CBloom4_24))

#4. Generating the bootstrap sample
set.seed(25)
bootdataCBloom4_24=CBloom4_24[sample(nrow(CBloom4_24), 1000, replace=TRUE), ]
head(bootdataCBloom4_24)
nrow(bootdataCBloom4_24)
str(bootdataCBloom4_24)

#5. Importing training and testing datasets from the mlr splitting 
h_train <- read_excel("h_train.xlsx")
h_train <- h_train |> mutate(across(c(OceTemp),as.factor))
head(h_train)

h_test <- read_excel("h_test.xlsx")
h_test <- h_test |> mutate(across(c(OceTemp),as.factor))
head(h_test)

#6. Training the random forest model with the bootstrap dataset
set.seed(25)
CBloom4_24.rgBoot <- randomForest(PEAK ~ ., data=h_train, importance=TRUE,
                        proximity=TRUE)

sqrt(sum((CBloom4_24.rgBoot$predicted - h_train$PEAK)^2) / nrow(h_train))

#Extracting feature importance

importance(CBloom4_24.rgBoot)
importance(CBloom4_24.rgBoot, type=1)

#7. Visualize variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(CBloom4_24.rgBoot))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
set.seed(1234)
system.time(rf_fit<-train(PEAK~.,data = h_train, method ='rf', importance =TRUE, 
                          trControl = trainControl(method = 'cv', number = 10 )))
rf_fit
#Evaluate variable Importance
varImp(rf_fit)
importance(rf_fit$finalModel)

#Evaluate model performance on the test set
rf_RMSE <-sqrt(mean((h_test$PEAK - predict(rf_fit, h_test))^2))
rf_RMSE

#Using Ranger
set.seed(25)
system.time(ranger_fit<-train(PEAK~.,data = h_train, method ='ranger', importance ='impurity', 
                          trControl = trainControl(method = 'cv', number = 10 )))
ranger_fit



