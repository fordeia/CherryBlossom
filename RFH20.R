#install.packages("h2o")
library (h2o)
h2o.init()
h2o.clusterInfo()

#Loading packages
install.packages("tidyverse")
library(tidyverse)

# read dataset, convert categorical variables to factor data type
#install.packages("readxl")
#install.packages("writexl")
library(readxl)
h_train <- read_excel("trainCB.xlsx")
h_test <- read_excel("testCB.xlsx")
