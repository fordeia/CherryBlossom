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
# r_squared <- cor(predictions, y)^2
# cat("R-squared:", r_squared, "\n")
# Print the importance of each PCA component
print(rf_model$finalModel$importance)

#Percentage contribution of each variable to the pca

#correlation between the pca and variables
loadings <- pca_result$rotation
loadings

summary(pca_result)

#Multiple Linear Regression ##########################################################
library(car)
library(boot)
library(MASS)

# Fit the linear model
#model_stepwise <- step(lm(PEAK ~., data = CBloom4_24), scope = formula, direction = "forward", k = 2)

# Bootstrapping
#boot_model <- Boot(model_stepwise, R = 1000)

# Analyze the results
#summary(boot_model)
#confint(boot_model, level= .95) # Confidence intervals

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

#MLR with bootstapping 2 ############################################


# Stepwise variable selection
full_model <- lm(PEAK ~ ., data = CBloom4_24)
stepwise_model <- stepAIC(full_model, direction = "both") # Forward or backward
selected_variables <- coef(stepwise_model)[coef(stepwise_model) != 0]

# Bootstrapping
boot_samples <- 1000  # Number of bootstrap samples
boot_results <- matrix(0, nrow = boot_samples, ncol = length(selected_variables))

# Fit MLR models to bootstrap samples
for (i in 1:boot_samples) {
  # Bootstrap sample
  boot_data <- CBloom4_24[sample(nrow(CBloom4_24), replace = TRUE), ]

  # Fit MLR model using selected variables
  mlr_model <- lm(PEAK ~ PEAK ~ JAN.RAIN + JAN.TEMP + FEB.TEMP + OceTemp, data = boot_data)

  # Store coefficient estimates
  boot_results[i, ] <- coef(mlr_model)
}


selected_variables







