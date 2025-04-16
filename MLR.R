# read dataset
#install.packages("readxl")
#install.packages("writexl")
library(readxl)

#Loading the datasets
total <- read_excel("bootdataCBloom4_24.xlsx")

h_train <- read_excel("h_train.xlsx")
head(h_train)

h_test <- read_excel("h_test.xlsx")
head(h_test)

#Installing package for melt
library(reshape2)

# Remove target variable
temp_total <- subset(total, select = -c(PEAK))
melt_total <- melt(temp_total)

# Draw boxplot (variable measured on different scale, therefore this is meaningless)
boxplot(data = melt_total, value ~ variable)

# Fit the model and obtain summary
model <- lm(PEAK ~ ., data = h_train)
summary(model)

# Get residuals
lm_residuals <- as.data.frame(residuals(model))

# Visualize residuals
library(ggplot2)
ggplot(lm_residuals, aes(residuals(model))) +
  geom_histogram(fill = "#0099f9", color = "black", binwidth=1) +
  theme_classic() +
  labs(title = "Residuals plot")

# Make predictions on the test set
predictions <- predict(model, h_test)

# Convert to dataframe
eval <- cbind(h_test$PEAK, predictions)
colnames(eval) <- c("Y", "Yhat")
eval <- as.data.frame(eval)
head(eval)

# Evaluate model
mse <- mean((eval$Y - eval$Yhat)^2)
rmse <- sqrt(mse)

#Error rate
sigma(model)/mean(h_test$PEAK)

library(ggplot2)

# Assuming 'model' is your fitted regression model, and 'h_test' is your data frame

ggplot(h_test, aes(x = PEAK, y = predictions )) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  xlab("Observed Values") +
  ylab("Predicted Values") +
  ggtitle("Observed vs. Predicted Values")

#Stepwisse variable selection
library(MASS)
# Fit the full model 
model <- lm(PEAK ~ ., data = h_train)
full.model <- lm(PEAK ~ ., data = h_train)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
