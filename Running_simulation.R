rm(list = ls())

#setwd(dir = "C:/Users/das70453/OneDrive - Texas Tech University/Thesis/simulation")

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Thesis/simulation") # Macbook


source(file = "required_packages.R")

source(file = "accuracy_supervised_function.R")

source(file = "var_cov_mat_functions.R")

source(file = "data_generation.R")

# The following inputs are required as the input of the functions

number_of_groups = 3

number_of_observations = 1002

number_of_variables <- 10

mean_generation_unif_r <- c(-1, 1)

sigma_for_data <- "toeplitz"

prop_sample_train <- 0.7

data <- data_generation(n_obs = number_of_observations, n_group = number_of_groups, n_var = number_of_variables, mean_unif_limits = mean_generation_unif_r, sigma_type = sigma_for_data)


# Partitioning the data frame into training and test data
index_sample <- sample(x = nrow(data), size = prop_sample_train * nrow(data), replace = F)
train <- data[index_sample,]
test <- data[-index_sample,]

# Checking accuracy
accuracy_supervised_models(train_covariates = train[,(1:number_of_variables)], train_group = train$group, test_covariates = test[,(1:number_of_variables)], test_group = test$group)


