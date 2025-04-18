#1. Loading Packages
library(randomForest)
library(ggplot2)
library(readxl)
library(tidyverse)
library(caret)
library(ranger)
library(stats)

#2. Loading Data
CBloom4_24 <-read.table("Cherryblossom_2004-2024.txt",header =TRUE,sep="\t", fill = TRUE)

#3. Data preparation
# Create a new variable by summing two existing variables
CBloom4_24$OceTemp <- 2*CBloom4_24$EL.NINO+CBloom4_24$LA.NINA

#Dropping unwanted variables
CBloom4_24<-CBloom4_24[ , -c(1,3, 4, 5, 6,10,11,12,13,14,15,16)]

#Dropping rows with NA
CBloom4_24<-CBloom4_24[-c(22,23,24,25,26),]

# Separate features (X) and target (y)
X <- CBloom4_24[, -which(names(CBloom4_24) == "PEAK")]  
y <- CBloom4_24$PEAK
# Center and scale the data for PCA
X <- scale(X)

#Principle Component Analysis
# Perform PCA
pca_result <- prcomp(X, scale = FALSE)

#Keep the first 2 components
#pca_data <- as.data.frame(pca_result$x)[,1:2] # Select the first 2 PCs

# Alternatively, keep components based on explained variance
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)
n_components <- which(cumulative_variance >= 0.95)[1] # Keep until 95% explained variance

X_pca <- as.data.frame(pca_result$x[, 1:num_components])
# Now X_pca contains the transformed data

# Set the random seed for reproducibility
set.seed(123)

# Define a train control for cross-validation (e.g., 10-fold CV)
train_control <- trainControl(method = "cv", number = 10)

# Train the Random Forest model
rf_model <- train(X_pca, y, method = "rf", trControl = train_control)

# (Optional) Print the model summary
print(rf_model)

########3. Fitting the random forest model with original dataset. ########################################
set.seed(1237)
CBloom4_24.rf <- randomForest(x = pca_result$x, y = CBloom4_24$PEAK)

sqrt(sum((CBloom4_24.rf$predicted - CBloom4_24$PEAK)^2) / nrow(CBloom4_24))

set.seed(1234)
system.time(rf_fit1<-train(x = pca_result$x, y = CBloom4_24$PEAK, method ='rf', importance =TRUE, 
                          trControl = trainControl(method = 'cv', number = 10 )))
rf_fit1

sqrt(sum((rf_fit1$predicted - CBloom4_24$PEAK)^2) / nrow(CBloom4_24))

#Evaluate variable Importance
varImp(rf_fit1)

#OOB errors
rf_fit$err.rate[,1]

#Using Ranger
set.seed(25)
system.time(ranger_fit<-train(PEAK~.,data = CBloom4_24 , method ='ranger', importance ='impurity', 
                          trControl = trainControl(method = 'cv', number = 10 )))
ranger_fit

#Evaluating variance importance
varImp(ranger_fit)





