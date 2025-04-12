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
  geom_histogram(fill = "#0099f9", color = "black") +
  theme_classic() +
  labs(title = "Residuals plot")
