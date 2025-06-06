# Loading Packages
library(randomForest)
library(ggplot2)
library(readxl)
library(tidyverse)
library(caret)
library(ranger)
library(stats)
library(vip)


# Loading Data
CBloom4_24 <-read.table("Cherryblossom_2004-2024.txt",header =TRUE,sep="\t", fill = TRUE)

# Data preparation
# Create a new variable by summing two existing variables
CBloom4_24$OceTemp <- 2*CBloom4_24$EL.NINO+CBloom4_24$LA.NINA

#Dropping unwanted variables
CBloom4_24<-CBloom4_24[ , -c(1,3, 4, 5, 6,10,11,12,13,14,15,16)]

#Dropping rows with NA
CBloom4_24<-CBloom4_24[-c(22,23,24,25,26),]

#Test for correlation 
training_explore<-CBloom4_24
training_explore$PEAK<-as.numeric(training_explore$PEAK)
cor_matrix<-abs(cor(training_explore))
diag(cor_matrix)<-0
library(corrplot)
corrplot(cor_matrix, method="square")

# Separate features (X) and target (y)
X <- CBloom4_24[, -which(names(CBloom4_24) == "PEAK")]  
y <- CBloom4_24$PEAK
# Center and scale the data for PCA
X <- scale(X)

#Principle Component Analysis
# Perform PCA
pca_result <- prcomp(X, scale = FALSE)

# Calculate eigenvalues
eigenvalues <- sdev^2

# Print the eigenvalues
print(eigenvalues)

# Access the standard deviations
sdev <- pca_result$sdev

#Scree plot
install.packages("factoextra")
library(factoextra)
fviz_eig(pca_result, addlabels = TRUE)

#Biplot to display the relationship between the PC and independent variables. 
fviz_pca_var(pca_result, col.var = "black")

# return factor scores for first principal component
x_result <- pca_result$x[,"PC1"]

#Keep the first 2 components
#pca_data <- as.data.frame(pca_result$x)[,1:2] # Select the first 2 PCs

# Alternatively, keep components based on explained variance
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)
n_components <- which(cumulative_variance >= 0.95)[1] # Keep until 95% explained variance

X_pca <- as.data.frame(pca_result$x[, 1:n_components])
# Now X_pca contains the transformed data

# Set the random seed for reproducibility
set.seed(123)

# Define a train control for cross-validation (e.g., leave-one-out)
train_control <- trainControl(method = "LOOCV")

# Train the Random Forest model
rf_model <- train(X_pca, y, method = "rf", trControl = train_control)

# (Optional) Print the model summary
print(rf_model)

# Make predictions on the test set
predictions <- predict(rf_model, X_pca)

# Evaluate the model's performance (e.g., RMSE)
rmse <- sqrt(mean((predictions - y)^2))
cat("RMSE:", rmse, "\n")

# You can also use other metrics like R-squared
r_squared <- cor(predictions, y)^2
r_squared
# cat("R-squared:", r_squared, "\n")
# Print the importance of each PCA component
print(rf_model$finalModel$importance)

# Use the vip() function with the trained model object
gini_plot <- vip(rf_model, feature_names = names(X_pca)[-ncol(X_pca)], geom = "point")
gini_plot


#Percentage contribution of each variable to the pca

#correlation between the pca and variables
loadings <- pca_result$rotation
loadings

summary(pca_result)

plot(rf_model) # This plots the overall model performance (e.g., accuracy)

#Extracting variable importance
 variable_importance <- varImp(rf_model)

 plot(variable_importance) # This plots the variable importance scores

#Plotting the loading for PC5
library(factoextra)    
fviz_contrib(pca_result,
                 choice = "var",
                 axes = 5,
                 top = 5, color = 'darkorange3', barfill  = 'blue4',fill ='blue4')

#Plotting the loading for PC3
library(factoextra)    
fviz_contrib(pca_result,
                 choice = "var",
                 axes = 3,
                 top = 5, color = 'darkorange3', barfill  = 'blue4',fill ='blue4')

a <- pca_result$rotation

a
plot(rf_model) # This plots the overall model performance (e.g., accuracy)

#Extracting variable importance
 variable_importance <- varImp(rf_model)

 plot(variable_importance) # This plots the variable importance scores


#Multiple Linear Regression ##########################################################
library(car)
library(boot)
library(MASS)

# Stepwise variable selection
data= CBloom4_24
full_model <- lm(PEAK ~ ., data = data)
stepwise_model <- stepAIC(full_model, direction = "both") # Forward or backward
selected_variables <- coef(stepwise_model)[coef(stepwise_model) != 0]

# Bootstrapping
set.seed(25)
  # Fit MLR model using selected variables
  mlr_model <- lm(data = CBloom4_24, PEAK ~ JAN.RAIN + JAN.TEMP + FEB.TEMP + OceTemp, )

  fit_b<-Boot(mlr_model, R = 1000)
  summary(fit_b)
  confint(fit_b, level = .95)








