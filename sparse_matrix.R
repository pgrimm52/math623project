# to test out sparse matrix features

library(softImpute)
library(tidyverse)

data <- read.csv("ml-latest-small/ratings.csv", stringsAsFactors = FALSE) %>% 
	tbl_df()

data_matrix <- data %>%
	select(-timestamp) %>%
	spread(key = movieId, value = rating) %>%
	select(-userId) %>%
	as.matrix()

# use pre-baked scripts

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
	
	m0_results <- fix_ratings(
		impute(
			fit,
			i = hold_out_i,
			j = hold_out_j))
	
	c0 <- calc_error(real_values, m0_results) 
	
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
		random_fill = c1,
		average_fill = c2,
		dist_fill = c3,
		dist_glob_fill = c4
	))
}

fit_model(
	data = data_matrix, 
	lam = 10,
	frac = 0.1,
	comparison = FALSE)

lambda_val <- seq(1, 500, by=5)
sapply(lambda_val, function(x){
	fit_model(
		data = data_matrix, 
		lam = x,
		frac = 0.2,
		comparison = FALSE)
})

