# -------------------------------
# Load Required Packages
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
library(rpart)
library(rpart.plot)

# -------------------------------
# Load Data
# -------------------------------
CBloom4_24 <- read.table("Cherryblossom_2004-2024.txt", header = TRUE, sep = "\t", fill = TRUE)

# Drop rows 22-26
CBloom4_24 <- CBloom4_24[-c(22:26), ]

# Attach March temperatures
mar_temp <- c(42.85, 42.7, 38.75, 41.45, 37.05, 35.35, 40.95, 38.35, 37.6, 40.15, 
              40.1, 44.8, 41.8, 39.3, 38.75, 36.25, 38.3, 39.7, 41.4, 36.85, 40.6)
CBloom4_24$MAR.TEMP <- mar_temp

# -------------------------------
# Data Preparation
# -------------------------------
CBloom4_24$OceTemp <- ifelse(CBloom4_24$EL.NINO == 1, 2, ifelse(CBloom4_24$LA.NINA == 1, 0, 1))
CBloom4_24 <- CBloom4_24[, -c(1,3,4,5,6,10,11,12,13,14,15,16)]
CBloom4_24$PEAK <- as.numeric(CBloom4_24$PEAK)

# -------------------------------
# Correlation Plot
# -------------------------------
cor_matrix <- abs(cor(CBloom4_24))
diag(cor_matrix) <- 0
corrplot(cor_matrix, method = "square")

# -------------------------------
# PCA for Random Forest
# -------------------------------
X <- scale(CBloom4_24[, -which(names(CBloom4_24) == "PEAK")])
y <- CBloom4_24$PEAK

pca_result <- prcomp(X, scale = FALSE)

# Scree plot
fviz_eig(pca_result, addlabels = TRUE)

# Biplot
fviz_pca_var(pca_result, col.var = "black")

# Select components explaining 95% variance
explained_variance <- cumsum((pca_result$sdev^2) / sum(pca_result$sdev^2))
n_components <- which(explained_variance >= 0.95)[1]
X_pca <- as.data.frame(pca_result$x[, 1:n_components])

# -------------------------------
# Random Forest with LOOCV
# -------------------------------
set.seed(123)
rf_control <- trainControl(method = "LOOCV", savePredictions = "final")
rf_model <- train(X_pca, y, method = "rf", trControl = rf_control)

pred_rf <- rf_model$pred
rmse_rf <- sqrt(mean((pred_rf$pred - pred_rf$obs)^2))
r2_rf <- cor(pred_rf$pred, pred_rf$obs)^2

cat("Random Forest LOOCV RMSE:", rmse_rf, "\n")
cat("Random Forest LOOCV R-squared:", r2_rf, "\n")

# Predicted vs Observed Plot
ggplot(pred_rf, aes(x = obs, y = pred)) +
  geom_point(color = "#0099f9", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Random Forest LOOCV Predicted vs Observed",
       x = "Observed PEAK", y = "Predicted PEAK")

# Feature importance
vip(rf_model, geom = "point")

# -------------------------------
# Bootstrap Multiple Linear Regression (BMLR)
# -------------------------------
mlr_model <- lm(PEAK ~ JAN.RAIN + JAN.TEMP + FEB.TEMP + OceTemp, data = CBloom4_24)
set.seed(25)
fit_b <- Boot(mlr_model, R = 1000)
summary(fit_b)
confint(fit_b, level = 0.95)

pred_bmlr <- predict(mlr_model)
rmse_bmlr <- sqrt(mean((CBloom4_24$PEAK - pred_bmlr)^2))
r2_bmlr <- cor(pred_bmlr, CBloom4_24$PEAK)^2

cat("BMLR RMSE:", rmse_bmlr, "\n")
cat("BMLR R-squared:", r2_bmlr, "\n")

# Plot BMLR Predicted vs Observed
ggplot(data.frame(obs = CBloom4_24$PEAK, pred = pred_bmlr),
       aes(x = obs, y = pred)) +
  geom_point(color = "#00BFC4", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "BMLR Predicted vs Observed",
       x = "Observed PEAK", y = "Predicted PEAK")

# -------------------------------
# Decision Tree with LOOCV
# -------------------------------
set.seed(123)
tree_control <- trainControl(method = "LOOCV")
tree_model <- train(PEAK ~ ., data = CBloom4_24, method = "rpart",
                    trControl = tree_control, tuneLength = 10)

pred_tree <- tree_model$pred
rmse_tree <- sqrt(mean((pred_tree$pred - pred_tree$obs)^2))
r2_tree <- cor(pred_tree$pred, pred_tree$obs)^2

cat("Decision Tree LOOCV RMSE:", rmse_tree, "\n")
cat("Decision Tree LOOCV R-squared:", r2_tree, "\n")

# Visualize final decision tree
rpart.plot(tree_model$finalModel,
           type = 2, extra = 101, fallen.leaves = TRUE,
           main = "Decision Tree for PEAK Bloom Prediction")

# Predicted vs Observed Plot
ggplot(pred_tree, aes(x = obs, y = pred)) +
  geom_point(color = "#F8766D", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Decision Tree LOOCV Predicted vs Observed",
       x = "Observed PEAK", y = "Predicted PEAK")

# -------------------------------
# Summary Table of Model Performance
# -------------------------------
model_performance <- data.frame(
  Model = c("Random Forest", "Decision Tree", "BMLR"),
  RMSE = c(round(rmse_rf, 2), round(rmse_tree, 2), round(rmse_bmlr, 2)),
  R_squared = c(round(r2_rf, 3), round(r2_tree, 3), round(r2_bmlr, 3))
)

print(model_performance)
