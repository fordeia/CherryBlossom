# read dataset
#install.packages("readxl")
#install.packages("writexl")
library(readxl)

h_train <- read_excel("h_train.xlsx")
head(h_train)

h_test <- read_excel("h_test.xlsx")
head(h_test)

# Remove target variable
temp_df <- subset(df, select = -c(Weight))
melt_df <- melt(temp_df)

# Draw boxplot
boxplot(data = melt_df, value ~ variable)
