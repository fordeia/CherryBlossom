# -------------------------------
# Loading Required Packages
# -------------------------------
library(randomForest)
library(ggplot2)
library(readxl)
library(tidyverse)
library(caret)
library(ranger)
library(stats)
library(vip)
library(corrplot)
library(factoextra)
library(car)
library(boot)
library(MASS)

# -------------------------------
# Load Data
# -------------------------------
CBloom4_24 <- read.table("Cherryblossom_2004-2024.txt", header = TRUE, sep = "\t", fill = TRUE)

# -------------------------------
# Data Preparation
# -------------------------------
CBloom4_24$OceTemp <- 2 * CBloom4_24$EL.NINO + CBloom4_24$LA.NINA
CBloom4_24 <- CBloom4_24[, -c(1,3, 4, 5, 6,10,11,12,13,14,15,16)]
CBloom4_24 <- CBloom4_24[-c(22,23,24,25,26),]

# Correlation plot
training_explore <- CBloom4_24
training_explore$PEAK <- as.numeric(training_explore$PEAK)
cor_matrix <- abs(cor(training_explore))
diag(cor_matrix) <- 0
corrplot(cor_matrix, method = "square")

# -------------------------------
# PCA
# -------------------------------
X <- scale(CBloom4_24[, -which(names(CBloom4_24) == "PEAK")])
y <- CBloom4_24$PEAK

pca_result <- prcomp(X, scale = FALSE)
sdev <- pca_result$sdev
eigenvalues <- sdev^2
print(eigenvalues)

# Scree plot
fviz_eig(pca_result, addlabels = TRUE)

# Biplot
fviz_pca_var(pca_result, col.var = "black")

# Select components that explain 95% of variance
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)
n_components <- which(cumulative_variance >= 0.95)[1]

X_pca <- as.data.frame(pca_result$x[, 1:n_components])

# -------------------------------
# Random Forest with LOOCV
# -------------------------------
set.seed(123)
train_control <- trainControl(method = "LOOCV", savePredictions = "final")
rf_model <- train(X_pca, y, method = "rf", trControl = train_control)

print(rf_model)

# Extract LOOCV Predictions
pred_data <- rf_model$pred

# RMSE and R-squared
rmse_loocv <- sqrt(mean((pred_data$pred - pred_data$obs)^2))
r2_loocv <- cor(pred_data$pred, pred_data$obs)^2

cat("Random Forest LOOCV RMSE:", rmse_loocv, "\n")
cat("Random Forest LOOCV R-squared:", r2_loocv, "\n")

# Predicted vs Actual Plot
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
gini_plot <- vip(rf_model, feature_names = names(X_pca)[-ncol(X_pca)], geom = "point")
gini_plot

# PCA contribution plots
loadings <- pca_result$rotation
summary(pca_result)

plot(rf_model)
variable_importance <- varImp(rf_model)
plot(variable_importance)

# Loadings for PC5
fviz_contrib(pca_result, choice = "var", axes = 5, top = 5,
             color = 'darkorange3', barfill = 'blue4', fill = 'blue4')

# Loadings for PC3
fviz_contrib(pca_result, choice = "var", axes = 3, top = 5,
             color = 'darkorange3', barfill = 'blue4', fill = 'blue4')

# -------------------------------
# Bootstrap Multiple Linear Regression (BMLR)
# -------------------------------
data <- CBloom4_24
full_model <- lm(PEAK ~ ., data = data)
stepwise_model <- stepAIC(full_model, direction = "both")
selected_variables <- coef(stepwise_model)[coef(stepwise_model) != 0]

# Fit model with selected variables
mlr_model <- lm(data = CBloom4_24, PEAK ~ JAN.RAIN + JAN.TEMP + FEB.TEMP + OceTemp)

# Bootstrap
set.seed(25)
fit_b <- Boot(mlr_model, R = 1000)
summary(fit_b)
confint(fit_b, level = .95)

# 🔹 NEW: RMSE for BMLR (fitted values vs actuals)
pred_bmlr <- predict(mlr_model)
rmse_bmlr <- sqrt(mean((CBloom4_24$PEAK - pred_bmlr)^2))
cat("BMLR RMSE:", rmse_bmlr, "\n")
