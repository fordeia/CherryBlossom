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
CBloom4_24 <- read.table("Cherryblossom_2004-2024.txt", header = TRUE, sep = "\t", fill = TRUE)

# Data preparation
# Create a new variable by summing two existing variables
CBloom4_24$OceTemp <- 2 * CBloom4_24$EL.NINO + CBloom4_24$LA.NINA

# Dropping unwanted variables
CBloom4_24 <- CBloom4_24[, -c(1,3, 4, 5, 6,10,11,12,13,14,15,16)]

# Dropping rows with NA
CBloom4_24 <- CBloom4_24[-c(22,23,24,25,26),]

# Test for correlation 
training_explore <- CBloom4_24
training_explore$PEAK <- as.numeric(training_explore$PEAK)
cor_matrix <- abs(cor(training_explore))
diag(cor_matrix) <- 0
library(corrplot)
corrplot(cor_matrix, method = "square")

# Separate features (X) and target (y)
X <- CBloom4_24[, -which(names(CBloom4_24) == "PEAK")]
y <- CBloom4_24$PEAK

# Center and scale the data for PCA
X <- scale(X)

# Principal Component Analysis
pca_result <- prcomp(X, scale = FALSE)

# Access the standard deviations and eigenvalues
sdev <- pca_result$sdev
eigenvalues <- sdev^2
print(eigenvalues)

# Scree plot
install.packages("factoextra")
library(factoextra)
fviz_eig(pca_result, addlabels = TRUE)

# Biplot of variables
fviz_pca_var(pca_result, col.var = "black")

# Select components explaining 95% of variance
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)
n_components <- which(cumulative_variance >= 0.95)[1]

X_pca <- as.data.frame(pca_result$x[, 1:n_components])

# Set seed and define LOOCV control
set.seed(123)

# 🔹 NEW SECTION ADDED — Enable LOOCV + Save Predictions
train_control <- trainControl(method = "LOOCV", savePredictions = "final")

# Train Random Forest with LOOCV
rf_model <- train(X_pca, y, method = "rf", trControl = train_control)

# Print model summary
print(rf_model)

# 🔹 NEW SECTION ADDED — Extract LOOCV Predictions
pred_data <- rf_model$pred

# Compute RMSE and R-squared from LOOCV
rmse_loocv <- sqrt(mean((pred_data$pred - pred_data$obs)^2))
r2_loocv <- cor(pred_data$pred, pred_data$obs)^2

cat("LOOCV RMSE:", rmse_loocv, "\n")
cat("LOOCV R-squared:", r2_loocv, "\n")

# 🔹 NEW SECTION ADDED — Plot Predicted vs Observed
ggplot(pred_data, aes(x = obs, y = pred)) +
  geom_point(color = "#0099f9", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Predicted vs Observed Values (LOOCV - Random Forest + PCA)",
    x = "Observed PEAK",
    y = "Predicted PEAK"
  )

# Feature importance
print(rf_model$finalModel$importance)

# Gini plot
gini_plot <- vip(rf_model, feature_names = names(X_pca)[-ncol(X_pca)], geom = "point")
gini_plot

# PCA variable contributions
loadings <- pca_result$rotation
summary(pca_result)

# Visualizations
plot(rf_model)
variable_importance <- varImp(rf_model)
plot(variable_importance)

# Loading contributions for PC5
fviz_contrib(pca_result, choice = "var", axes = 5, top = 5,
             color = 'darkorange3', barfill = 'blue4', fill = 'blue4')

# Loading contributions for PC3
fviz_contrib(pca_result, choice = "var", axes = 3, top = 5,
             color = 'darkorange3', barfill = 'blue4', fill = 'blue4')

a <- pca_result$rotation
a

# Multiple Linear Regression Section ########################################
library(car)
library(boot)
library(MASS)

# Stepwise model
data = CBloom4_24
full_model <- lm(PEAK ~ ., data = data)
stepwise_model <- stepAIC(full_model, direction = "both")
selected_variables <- coef(stepwise_model)[coef(stepwise_model) != 0]

# Bootstrapping
set.seed(25)
mlr_model <- lm(data = CBloom4_24, PEAK ~ JAN.RAIN + JAN.TEMP + FEB.TEMP + OceTemp)

fit_b <- Boot(mlr_model, R = 1000)
summary(fit_b)
confint(fit_b, level = .95)
