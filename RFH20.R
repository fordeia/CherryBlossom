#install.packages("h2o")
library (h2o)
h2o.init()
h2o.clusterInfo()

#Loading packages
library(tidyverse)

# read dataset, convert categorical variables to factor data type
