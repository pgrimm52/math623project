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
ratings <- read.csv("ml-latest-small/ratings.csv", stringsAsFactors = FALSE) %>%
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

fix_ratings <- function(vec){
	# Snaps ratings to 0.5 to 5 (in half unit increments) and fixes extreme values
	vec <- round(vec/0.5)*0.5
	vec[vec<0.5] <- 0.5
	vec[vec>5] <- 5
	return(vec)
}

fill_missing_random <- function(vec){
	vec[is.na(vec)] <- sample(
		(1:10)/2, 
		size = sum(is.na(vec)), 
		replace = TRUE)
	return(vec)
}

fill_missing_average <- function(vec){
	if (sum(!is.na(vec)) > 0){
		vec[is.na(vec)] <- mean(vec, na.rm=TRUE)
	} else {
		vec[is.na(vec)] <- sample((1:10)/2, 1)
	}
	return(vec)
}

fill_missing_distribution <- function(vec){
	vals <- as.numeric(rownames(table(vec)))
	probs <- as.numeric(table(vec))
	if (length(vals) > 1){
		vec[is.na(vec)] <- sample(
			vals,
			size = sum(is.na(vec)),
			replace = TRUE,
			prob = probs)
	} else if (length(vals)==1){
		vec[is.na(vec)] <- vals
	} else {
		vec[is.na(vec)] <- sample((1:10)/2, 1)
	}
	return(vec)
}

fill_missing_distribution_global <- function(mat){
	vals <- as.numeric(rownames(table(mat)))
	probs <- as.numeric(table(mat))
	mat[is.na(mat)] <- sample(
		vals, 
		size = sum(is.na(mat)), 
		replace = TRUE, 
		prob = probs)
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

# Try different lambdas
fit_lambda <- function(lambda){
	data_fit <- holdout_sample(data_small, 0.2)
	
	fit <- softImpute(data_fit$new_matrix, 
										rank.max = 90, 
										lambda = lambda, 
										type = "svd", maxit = 1000, trace.it = TRUE)
	
	data_small_imputed <- softImpute::complete(data_fit$new_matrix, fit) %>% fix_ratings
	
	return(calc_error(
		data_fit$actual_data, 
		data_small_imputed[data_fit$indices]))
}

lambda_val <- seq(0, 20, by=1)
sapply(lambda_val, fit_lambda)







