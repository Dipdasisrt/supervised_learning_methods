###############################################################################
###### ********Load library mvtnorm for this function*********###########



data_generation <- function(n_obs, n_group, data_type, mean, sigma, prop_training){
  
  # data_generation function has been created to generate data from a multivariate
  # normal distribution. 
  
  
  # n_obs is number of observations that we wants to generate.
  
  if (n_obs <= 3){
  stop("Number of observations that you want to generate should
                      be greater than 3")
                 }
  
  
  
  # n_group is number of groups that we want to have in our dataset.
  if (n_obs %% n_group > 0)
  {
    stop("Number of observations must be a multiple of number of groups")
  }
  
  
  # data_type determines whether we want to return training/ test/ complete data.
  
  if (data_type != "training" && data_type != "test" && data_type != "complete")
    {
    stop("Enter data_type as either training or test or complete")
  }
  
  
  
  # mean indicates mean vector which will be used to generate the data from
  # multivariate normal
  if (is.vector(mean) == FALSE) 
  {
    stop("mean must be a vector")
  }
  
  
  # sigma indicates variance covariance matrix which will be used to generate 
  # the data from multivariate normal
  if (is.matrix(sigma) == FALSE) 
  {
    stop("sigma must be a matrix")
  }
  
  
  # Checking whether sigma is square matrix
  if ((dim(sigma))[1] != (dim(sigma))[2])
  {
    stop("sigma must be a square matrix")
  }
  
  
  # Checking whether sigma is a symmetrix matrix
  if (isSymmetric(sigma) == FALSE)
  {
    stop("Sigma must be symmetric matrix")
  }
  
  
  # Checking whether the length of mean vector equals the number of rows of the
  # diagonal matrix sigma
  if (length(mean)!= dim(sigma)[1]) 
  {
    stop("mean and sigma have non-conforming size")
  }
  
  
  
  # dim_data determines the dimension of the multivariate data.
  
  dim_data <- length(mean)
  
  if (dim_data <2 || dim_data >15)
  {
    stop("dimension of the multivariate data must be in between 1 to 15")
  }
  
  
  
  # prop_training is the proportion of training data that we want to have in our
  # training data.
  
  if(prop_training <0 || prop_training >1)
  {
  stop("Proportion of training data should be greater or equal to 0 and less 
       than or equal to 1")  
  }
  
  
  # Creating null matrix to store the generated data
  dat <- matrix(NA, nrow = n_obs, ncol = (dim_data+1), byrow = T)
  
  # Number of observations that every groups will have
  obs_per_group <- n_obs/n_group
  
  # Generating the data from multivariate normal distribution
  for (i in 1:n_group)
  {
   dat[(((i-1)*obs_per_group)+1):(i*obs_per_group), 1:dim_data] <- rmvnorm(n = ceiling(obs_per_group), mean = mean, sigma = sigma)
   dat[(((i-1)*obs_per_group)+1):(i*obs_per_group), (dim_data+1)] <- i
  }

 # Converting the data matrix into data frame and naming the group column
 dat <- data.frame(dat)
 colnames(dat) <- c(1:dim_data, "group")
  
 # Partitioning the data frame into training and test data
 index_sample <- sample(x = nrow(dat), size = prop_training * n_obs, replace = F)
 train <- dat[index_sample,]
 test <- dat[-index_sample,]
 
 # Returning the data frame as specified in the function for argument data_type.
 if (data_type == "train") return(train)
 if (data_type == "test") return(test)
 else return(dat)
}

