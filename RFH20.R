#install.packages("h2o")
library (h2o)
h2o.init()
h2o.clusterInfo()

#Loading packages
#install.packages("tidyverse")
library(tidyverse)

# read dataset, convert categorical variables to factor data type
#install.packages("readxl")
#install.packages("writexl")
library(readxl)
h_train <- read_excel("trainCB.xlsx")
h_test <- read_excel("testCB.xlsx")
h_train <- h_train |> mutate(across(c(OceTemp),as.factor))
h_test <- h_train |> mutate(across(c(OceTemp),as.factor))

#convert the R data.frame into H2O data frame
h_train<- as.h2o(h_train)
h_test<- as.h2o(h_test)

# regression - define features (x) and target (y) 
target <- "PEAK"
features <- setdiff(colnames(h_train), target)

# Cartesian Grid Search
hyper_grid.h2o <- list(ntrees = seq(100, 300, by = 50), 
                       mtries = 2:6
                       # max_depth = seq(10, 30, by = 10), 
                       # min_rows = seq(1, 3, by = 1),
                       # nbins = seq(20, 30, by = 10), 
                       # sample_rate = c(0.55, 0.632, 0.75)
                       )
hyper_grid.h2o

# Number of models
sapply(hyper_grid.h2o, length) %>% prod()

# Train Random Forest Models
system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
                                        # grid_id = "rf_grid1",
                                        x = features,
                                        y = target, 
                                        seed = 1234, 
                                        nfolds = 10, 
                                        training_frame = h_train, 
                                        hyper_params = hyper_grid.h2o,
                                        search_criteria = list(strategy = "Cartesian")))


grid_cartesian

# Collect the results and sort by a model performance metric of choice
grid_perf <- h2o.getGrid(grid_id = "Grid_DRF_h_train_sid_8a1e_6_model_R_1744322960986_3",
                         sort_by = "residual_deviance",
                         decreasing = FALSE)
grid_perf@summary_table

# Best model chosen by validation error 
best_model <- h2o.getModel(grid_perf@model_ids[[1]])
best_model

# Random Discrete Grid Search
hyper_grid.h2o <- list(ntrees = seq(100, 500, by = 50), 
                       mtries = 2:6
                       # max_depth = seq(10, 30, by = 10), 
                       # min_rows = seq(1, 3, by = 1),
                       # nbins = seq(20, 30, by = 10), 
                       # sample_rate = c(0.55, 0.75, 0.05)
                       )

# Number of models
sapply(hyper_grid.h2o, length) %>% prod()

# Set random grid search criteria
search_criteria_2 <- list (strategy = "RandomDiscrete", 
                           max_models = 50, 
                           max_runtime_secs = 10 * 60)

# Train Random Forest Models
system.time(random_grid <- h2o.grid(algorithm = "randomForest",
                                        # grid_id = "rf_grid2",
                                        x = features,
                                        y = target, 
                                        seed = 1234, 
                                        nfolds = 10, 
                                        training_frame = h_train, 
                                        hyper_params = hyper_grid.h2o,
                                        search_criteria = search_criteria_2))

random_grid

# Collect the results and sort by a model performance metric of choice
grid_perf <- h2o.getGrid(grid_id = "Grid_DRF_h_train_sid_8a1e_6_model_R_1744322960986_4",
                         sort_by = "residual_deviance",
                         decreasing = FALSE)
grid_perf@summary_table

# Best model chosen by validation error 
best_model <- h2o.getModel(grid_perf@model_ids[[1]])
best_model

# evaluate variable importance
h2o.varimp(best_model)

# alternatively predict on the test set, confert H20Frame to vector and then calculate RMSE
pred <- h2o.predict(object = best_model, newdata = h_test)
sqrt(mean((as.vector(h_test$PEAK)-as.vector(pred))^2))

# Create the partial dependence plot
pdp <- h2o.pd_plot(best_model, h_test, column = "JAN.RAIN")
print(pdp)
