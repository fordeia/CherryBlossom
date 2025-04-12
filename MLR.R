# read dataset
#install.packages("readxl")
#install.packages("writexl")
library(readxl)

h_train <- read_excel("h_train.xlsx")
head(h_train)

h_test <- read_excel("h_test.xlsx")
head(h_test)
