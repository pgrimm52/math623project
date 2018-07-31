#Lambda value exploration

############################################################
# Title: Main analysis script for 623 project
# Date: 29 JUL 2018
############################################################


##############################
# 1. LOAD DATA
##############################

# Load packages
library(softImpute)
library(tidyverse)
library(ggplot2)

# Load data
setwd("C:/Users/mpeng/Documents/Mary/Georgetown/Math 623 - Sparse Sampling/Final Project/ml-latest-small/")
ratings <- read.csv("ratings.csv", stringsAsFactors = FALSE) %>%
  tbl_df()

ratings_long <- ratings %>%
  select(-timestamp) %>%
  spread(key = movieId, value = rating) # reshape from long to wide

data <- as.matrix(ratings_long[, -1]) # save as matrix, drop redundant userId column

data_full <- data
data_large <- data[1:300, 1:2000]
data_medium <- data[1:100, 1:500]
data_small <- data[1:100, 1:100]
data_very_small <- data[1:10, 1:10]


##############################
# 2. HELPER FUNCTIONS
##############################

calc_error <- function(a, b){
  return(sum((a-b)^2)/length(a))
}

calc_sparsity <- function(mat){
  # Provides fraction of non-missing values
  return(sum(!is.na(mat))/length(mat))
}

fix_ratings <- function(mat){
  # Snaps ratings to 0.5 to 5 (in half unit increments) and fixes extreme values
  mat <- round(mat/0.5)*0.5
  mat[mat<0.5] <- 0.5
  mat[mat>5] <- 5
  return(mat)
}

holdout_sample <- function(mat, frac){
  # Samples a fraction of non-missing indices, saves their values, 
  # and returns matrix without those values
  non_missing <- which(!is.na(mat))
  hold_out <- sort(sample(non_missing, size = frac * length(non_missing)))
  
  actual_data <- mat[hold_out] 
  mat[hold_out] <- NA
  
  return(list(
    indices = hold_out, 
    actual_data = actual_data,
    new_matrix = mat))
}

fill_missing_values <- function(mat, type){
  # Fills in missing values based on different logic
  # 1. Assign random rating
  if (type == "random"){
    mat[is.na(mat)] <- sample((1:10)/2, size=sum(is.na(mat)), replace=TRUE)
  }
  # 2. Assign average non-missing rating for that movie
  if (type == "average"){
    mat <- apply(mat, 2, function(x){
      x[is.na(x)] <- mean(x, na.rm=TRUE)
      return(fix_ratings(x))
    })
    mat[is.na(mat)] <- mean(mat, na.rm=TRUE) # failsafe if there are no entries in a column
    mat <- fix_ratings(mat)
  }
  # 3. Assign movie rating based on overall distribution
  if (type == "distribution"){
    mat[is.na(mat)] <- sample(
      as.numeric(rownames(table(mat))),
      size = sum(is.na(mat)),
      replace = TRUE,
      prob = table(mat))
  }
  return(mat)
}


##############################
# 3. MATRIX COMPLETION
##############################

# Fit model
data_fit <- holdout_sample(data_small, 0.2)

fit <- softImpute(data_fit$new_matrix, 
                  rank.max = 90, 
                  lambda = 3, 
                  type = "svd", maxit = 1000, trace.it = TRUE)

data_small_imputed <- softImpute::complete(data_fit$new_matrix, fit) %>% fix_ratings

# Look at error results
calc_error(
  data_fit$actual_data, 
  data_small_imputed[data_fit$indices])
calc_error(
  data_fit$actual_data,
  fill_missing_values(data_fit$new_matrix, "random")[data_fit$indices])
calc_error(
  data_fit$actual_data,
  fill_missing_values(data_fit$new_matrix, "average")[data_fit$indices])
calc_error(
  data_fit$actual_data,
  fill_missing_values(data_fit$new_matrix, "distribution")[data_fit$indices])

#######################################
# Try different lambdas
#######################################
fit_lambda <- function(sample, holdout_percentage, step_count){
  
  data_fit <- holdout_sample(sample, holdout_percentage)
  
  #Compute sequence of lambda values
  lambda_max <- lambda0(data_fit$new_matrix, maxit=500, trace.it=FALSE)
  lambda_seq <- seq(0.01, lambda_max, by=(lambda_max-0.01)/step_count) 
    
  #Create table to store the values
  error_comparison <- matrix(rep(0, 2*length(lambda_seq)),ncol=2)
  colnames(error_comparison) <- c('lambda', 'MSE')
  
  for(i in seq(1,length(lambda_seq), by=1)) {
    lambda <- lambda_seq[i]
    
    fit <- softImpute(data_fit$new_matrix, 
                      rank.max = 90, 
                      lambda = lambda, 
                      type = "svd", maxit = 1000, trace.it = TRUE)
    
    data_small_imputed <- softImpute::complete(data_fit$new_matrix, fit) %>% fix_ratings
    
    error_comparison[i,1] <- lambda
    error_comparison[i,2] <- calc_error(data_fit$actual_data, data_small_imputed[data_fit$indices])
  
    i <- i+1
  }
  
  return(error_comparison)
}

holdout_10_rank90_data_small <- fit_lambda(data_small, 0.1, step_count = 20)
holdout_20_rank90_data_small <- fit_lambda(data_small, 0.2, step_count = 20)
holdout_30_rank90_data_small <- fit_lambda(data_small, 0.3, step_count = 20)

#Try with a larger dataset
holdout_10_rank90_data_large <- fit_lambda(data_large, 0.1, step_count = 20)
holdout_20_rank90_data_large <- fit_lambda(data_large, 0.2, step_count = 20)
holdout_30_rank90_data_large <- fit_lambda(data_large, 0.3, step_count = 20)

#Plot the relationships
par(mfrow=c(2,3))

plot(holdout_10_rank90_data_small[,1], holdout_10_rank90_data_small[,2], type='l', lwd=2, col='blue',
     xlab = expression(paste(lambda)), ylab='MSE', main = '10% holdout(small)')

plot(holdout_20_rank90_data_small[,1], holdout_20_rank90_data_small[,2], type='l', lwd=2, col='blue',
     xlab = expression(paste(lambda)), ylab='MSE', main = '20% holdout(small)')

plot(holdout_30_rank90_data_small[,1], holdout_30_rank90_data_small[,2], type='l', lwd=2, col='blue',
     xlab = expression(paste(lambda)), ylab='MSE', main = '30% holdout(small)')

plot(holdout_10_rank90_data_large[,1], holdout_10_rank90_data_large[,2], type='l', lwd=2, col='blue',
     xlab = expression(paste(lambda)), ylab='MSE', main = '10% holdout(large)')

plot(holdout_20_rank90_data_large[,1], holdout_20_rank90_data_large[,2], type='l', lwd=2, col='blue',
     xlab = expression(paste(lambda)), ylab='MSE', main = '20% holdout(large)')

plot(holdout_30_rank90_data_large[,1], holdout_30_rank90_data_large[,2], type='l', lwd=2, col='blue',
     xlab = expression(paste(lambda)), ylab='MSE', main = '30% holdout(large)')
