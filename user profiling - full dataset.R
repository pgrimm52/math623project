################################################
# Script to generate final results for project
################################################

###############
# Load packages/functions
###############
setwd("C:/Users/mpeng/Documents/Mary/Georgetown/Math 623 - Sparse Sampling/Final Project/ml-latest-small/")
ratings <- read.csv("ratings.csv", stringsAsFactors = FALSE) %>%
  tbl_df()

setwd("C:/Users/mpeng/Documents/Mary/Georgetown/Math 623 - Sparse Sampling/Final Project")

library(caret)
library(RColorBrewer)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(softImpute)
source("helper_functions.R")
set.seed(1234)

###############
# Load data
###############

data_long <- read.csv("ml-latest-small/ratings.csv", stringsAsFactors = FALSE) %>% 
	tbl_df() %>%
	dplyr::select(-timestamp)

data_wide <- data_long %>%
	spread(key = movieId, value = rating) %>%
	dplyr::select(-userId) %>%
	as.matrix()


###############
# Core model fitting function
###############

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

fit_model_simple <- function(data, holdout_indices, lam, rank.max){
	
	data_train <- data
	data_train[holdout_indices] <- NA
	
	data_sparse <- as(data_train, "Incomplete")
	
	fit <- softImpute(
		data_sparse, 
		rank.max=rank.max, lambda=lam, 
		maxit=200, trace.it=FALSE, type="svd")
	
	fit <- deBias(data_sparse, fit)
	
	predicted <- fix_ratings(
		impute(
			fit,
			i = arrayInd(holdout_indices, dim(data))[, 1],
			j = arrayInd(holdout_indices, dim(data))[, 2]))
	
	return(list(
		fit_object = fit,
		predicted = predicted,
		actual = data[holdout_indices]
	))
}


###############
# Calculate complete matrix
###############

test_fit <- fit_model(
		data = data_wide, 
		lam = 19, #best_lambda,
		frac = 0.2,
		comparison = FALSE)

###############
# Profile users
###############

#Function for determining proportion of success
compute_pred_accuracy_by_user <- function(fitted_result, actual_data) {
  holdout_list <- fitted_result$hold_out
  test <- fitted_result$m0d_results
  reference <- actual_data[holdout_list]
  
  diff <- abs(test - reference)
  index_success <- which(diff==0)
  index_error <- which(diff > 0)
  
  diff[index_success] <- 1
  diff[index_error] <- 0
  
  #store users in a list, in order of the entries appearing in the holdout
  user_in_holdout <- floor(holdout_list/dim(actual_data)[2]) + 1
  
  #now join user to table indicating whether or not a user's particular preference was correctly predicted
  success_prop_by_user <- data.frame(userId = user_in_holdout, success = diff) %>%
    group_by(userId) %>% 
    summarise(prop = mean(success))
  
  return(list(success_prop_by_user = success_prop_by_user,
              user_in_holdout = user_in_holdout))
}

pred_accuracy_for_full_sample <- compute_pred_accuracy_by_user(test_fit, data_wide)

#Plot distribution
hist(data.frame(pred_accuracy_for_full_sample$success_prop_by_user)$prop, plot=TRUE, breaks=50,xlim=c(0,1), xlab='Proportion of Accurate Predictions',
   ylab = 'Count of Users',
   main='Proportion of accurate predictions by user', col='brown')

#Identify types of users with higher than average empirical probability

#Compare them vs. the average user in terms of: 
 
  #Number of ratings 
  user_rating_cnt <- data.frame(userId = seq(1,dim(data_wide)[1],by=1), 
                                rating_cnt = apply(X = data_wide, 1, FUN = function(x) sum(!is.na(x))))
  
  ratingcnt_predAccuracy <- user_rating_cnt %>%
    inner_join(data.frame(pred_accuracy_for_full_sample$success_prop_by_user), by = 'userId')
  
  plot(x=ratingcnt_predAccuracy$rating_cnt, y=ratingcnt_predAccuracy$prop, xlab = 'Number of Ratings for given user',
       ylab = 'Proportion of accurate predictions', main = 'Prediction Accuracy vs. # Ratings', type='p')
  

  #Number of movies rated in the same genre
