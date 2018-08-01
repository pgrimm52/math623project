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

setwd("C:/Users/mpeng/Documents/Mary/Georgetown/Math 623 - Sparse Sampling/Final Project")

library(caret)
library(RColorBrewer)
library(gridExtra)
library(tidyverse)
library(softImpute)
source("helper_functions.R")
set.seed(1234)

####################################
#Main Algorithm
####################################
fit_model <- function(data, frac, lam, comparison){
  
  # Hold out data
  non_missing <- which(!is.na(data))
  hold_out <- sort(
    sample(non_missing, size = frac * length(non_missing)))
  hold_out_i <- arrayInd(hold_out, dim(data))[, 1]
  hold_out_j <- arrayInd(hold_out, dim(data))[, 2]
  
  real_values <- data[hold_out] 
  data[hold_out] <- NA
  
  # Matrix completion
  data_sparse <- as(data, "Incomplete")
  
  fit <- softImpute(
    data_sparse, 
    rank.max=50, lambda=lam, 
    maxit=200, trace.it=TRUE, type="svd")
  
  fit_debias <- deBias(data_sparse, fit)
  
  m0_results <- fix_ratings(
    impute(
      fit,
      i = hold_out_i,
      j = hold_out_j))
  
  c0 <- calc_error(real_values, m0_results)
  
  m0d_results <- fix_ratings(
    impute(
      fit_debias,
      i = hold_out_i,
      j = hold_out_j))
  
  c0d <- calc_error(real_values, m0d_results) 
  
  # IF REQUESTED: compare to other methods
  c1 <- NA; c2 <- NA; c3 <- NA; c4 <- NA
  if (comparison==TRUE){
    # Dumb completion methods
    m1 <- apply(data, 2, fill_missing_random)
    m1_results <- fix_ratings(m1[hold_out])
    
    m2 <- apply(data, 2, fill_missing_average)
    m2_results <- fix_ratings(m2[hold_out])
    
    m3 <- apply(data, 2, fill_missing_distribution)
    m3_results <- fix_ratings(m3[hold_out])
    
    m4 <- fill_missing_distribution_global(data)
    m4_results <- fix_ratings(m4[hold_out])
    
    # Compare
    c1 <- calc_error(real_values, m1_results)
    c2 <- calc_error(real_values, m2_results)
    c3 <- calc_error(real_values, m3_results)
    c4 <- calc_error(real_values, m4_results)
  }
  # Return results
  return(list(
    matrix_completion = c0,
    matrix_completion_debias = c0d,
    matrix_rank = length(fit$d),
    random_fill = c1,
    average_fill = c2,
    dist_fill = c3,
    dist_glob_fill = c4,
    m0d_results = m0d_results,
    hold_out = hold_out
  ))
}

######################################################
#complete matrix on a small sample 
######################################################
data_medium_fit <- fit_model(
  data = data_medium, 
  lam = 5,
  frac = 0.2,
  comparison = FALSE)

######################################################
#Compare the error between fitted and actual data, and find users with low error
######################################################
test_m0d <- data_medium_fit$m0d_results 
reference <- data_medium[data_medium_fit$hold_out]

#Define success as predicting the same rating as actual data (after applying fix_ratings), error as predicting outside that range
diff <- abs(test_m0d - reference)
index_success <- which(diff == 0)
index_error <- which(diff > 0)

diff[index_success] <- 1
diff[index_error] <- 0

#create a replica of the data_medium matrix to map error back to users
copy_data_medium <- data_medium
copy_data_medium[data_medium_fit$hold_out] <- diff

#Question: How do I then compute the empirical probability of success for a user in my holdout? 

user_in_holdout <- arrayInd(data_medium_fit$hold_out,dim(copy_data_medium))[,1]
movies_in_holdout <- arrayInd(data_medium_fit$hold_out,dim(copy_data_medium))[,2]

#For each user, compute empirical probability of our predictions being in the right range



#Plot the distribution

#Identify the users with the highest empirical probability

#Compare them vs. the average user in terms of: 

  #Number of movies rated in the same genre
  
  #Number of ratings 


######################################################
######################################################

######################################################
######################################################