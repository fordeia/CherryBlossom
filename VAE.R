install.packages("keras")
library(keras)
use_implementation("tensorflow")
install.packages("tensorflow")
library(tensorflow)
install.packages("reticulate")
library(reticulate)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
use_condaenv("r-reticulate")

conda update r-caret

# Set the seed for Keras/TensorFlow
tensorflow::set_random_seed(123)
mnist<- dataset_mnist()

