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

install.packages("reshape")
library(reshape)

# Remove target variable
temp_total <- subset(total, select = -c(PEAK))
melt_total <- melt(temp_total,value.name = "value")

# Draw boxplot
boxplot(data = melt_total, value ~ variable)
