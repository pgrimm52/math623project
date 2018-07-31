################################################
# Script to generate final results for project
################################################

###############
# Load packages/functions
###############

library(tidyverse)
library(softImpute)
source("helper_functions.R")


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
		rank.max=25, lambda=lam, 
		maxit=200, trace.it=FALSE, type="svd")
	
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
		dist_glob_fill = c4
	))
}


###############
# Testing different lambda
###############

lambda_val <- seq(1, 100, by=5)

mse <- sapply(lambda_val, function(x){
	print(paste("Loop for lambda =", x))
	fit_model(
		data = data_wide, 
		lam = x,
		frac = 0.2,
		comparison = FALSE)
})

plot(lambda_val, mse[1, ], col="blue")
lines(lambda_val, mse[2, ], col="blue")


###############
# Comparing to alternative imputation methods
###############

best_lambda <- x
comparison <- fit_model(
	data = data_wide, 
	lam = best_lambda,
	frac = 0.2,
	comparison = TRUE)

abline(h = comparison$c1)
abline(h = comparison$c2)
abline(h = comparison$c3)
abline(h = comparison$c4)
