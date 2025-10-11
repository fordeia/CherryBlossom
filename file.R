# Loading Packages
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

# Loading Data
CBloom4_24 <- read.table("Cherryblossom_2004-2024.txt", header = TRUE, sep = "\t", fill = TRUE)

# Data Preparation
CBloom4_24$OceTemp <- 2 * CBloom4_24$EL.NINO + CBloom4_24$LA.NINA
CBloom4_24 <- CBloom4_24[, -c(1, 3, 4, 5, 6, 10, 11, 12, 13, 14, 15, 16)]
CBloom4_24 <- CBloom4_24[-c(22, 23, 24, 25, 26),]

# Correlation Plot
training_explore <- CBloom4_24
training_explore$PEAK <- as.numeric(training_explore$PEAK)
cor_matrix <- abs(cor(training_explore))
diag(cor_matrix) <- 0
corrplot(cor_matrix, method = "square")

# Separate features and target
X <- CBloom4_24[, -which(names(CBloom4_24) == "PEAK")]
y <- CBloom4_24$PEAK
X <- scale(X)

# PCA
pca_result <- prcomp(X, scale = FALSE)
sdev <- pca_result$sdev
eigenvalues <- sdev^2
print(eigenvalues)

# Scree plot
fviz_eig(pca_result, addlabels = TRUE)

# PCA variable biplot
fviz_pca_var(pca_result, col.var = "black")

# Select principal components explaining 95% variance
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)
n_components <- which(cumulative_variance >= 0.95)[1]
X_pca <- as.data.frame(pca_result$x[, 1:n_components])

# Set seed and define LOOCV control
set.seed(123)
train_control <- trainControl(method = "LOOCV", savePredictions = "final")

# Train Random Forest with LOOCV
rf_model <- train(X_pca, y, method = "rf", trControl = train_control)
print(rf_model)

# Extract LOOCV predictions
pred_data <- rf_model$pred
rmse_loocv <- sqrt(mean((pred_data$pred - pred_data$obs)^2))
r2_loocv <- cor(pred_data$pred, pred_data$obs)^2

cat("LOOCV RMSE:", rmse_loocv, "\n")
cat("LOOCV R-squared:", r2_loocv, "\n")

# Predicted vs Actual Plot for RF + LOOCV
ggplot(pred_data, aes(x = obs, y = pred)) +
  geom_point(color = "#0099f9", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Predicted vs Observed (LOOCV - Random Forest + PCA)",
    x = "Observed PEAK",
    y = "Predicted PEAK"
  )

# Variable importance
print(rf_model$finalModel$importance)
gini_plot <- vip(rf_model, feature_names = names(X_pca)[-ncol(X_pca)], geom = "point")
gini_plot

# PCA Loadings
loadings <- pca_result$rotation
summary(pca_result)

# Additional Visuals
plot(rf_model)
variable_importance <- varImp(rf_model)
plot(variable_importance)

# PCA Contribution Plots
fviz_contrib(pca_result, choice = "var", axes = 5, top = 5, color = 'darkorange3', barfill = 'blue4', fill = 'blue4')
fviz_contrib(pca_result, choice = "var", axes = 3, top = 5, color = 'darkorange3', barfill = 'blue4', fill = 'blue4')

# Multiple Linear Regression (Stepwise + Bootstrapping)
full_model <- lm(PEAK ~ ., data = CBloom4_24)
stepwise_model <- stepAIC(full_model, direction = "both")
selected_variables <- coef(stepwise_model)[coef(stepwise_model) != 0]

# Bootstrap Multiple Linear Regression (BMLR)
set.seed(25)
mlr_model <- lm(PEAK ~ JAN.RAIN + JAN.TEMP + FEB.TEMP + OceTemp, data = CBloom4_24)
fit_b <- Boot(mlr_model, R = 1000)
summary(fit_b)
confint(fit_b, level = .95)

# Predicted vs Actual Plot for BMLR
pred_bmlr <- data.frame(
  Observed = CBloom4_24$PEAK,
  Predicted = predict(mlr_model, newdata = CBloom4_24)
)

ggplot(pred_bmlr, aes(x = Observed, y = Predicted)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "darkred", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Predicted vs Observed (Bootstrap MLR)",
    x = "Observed PEAK",
    y = "Predicted PEAK"
  )
