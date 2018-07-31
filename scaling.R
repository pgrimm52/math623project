
source("helper_functions.R")

data <- read.csv("ml-latest-small/ratings.csv", stringsAsFactors = FALSE) %>% 
	tbl_df()

data_long <- data %>%
	select(-timestamp)

data_wide <- data_long %>%
	spread(key = movieId, value = rating) %>%
	select(-userId) %>%
	as.matrix()

data_inc <- as(data_wide, "Incomplete")
data_inc_scale <- biScale(data_inc, col.scale=FALSE, row.scale=FALSE)

fit1 <- softImpute(data_inc, rank.max=50, lambda=50, trace.it=TRUE, type="svd")
fit1_debias <- deBias(data_inc, fit1)
fit1_scale <- softImpute(data_inc_scale, rank.max=50, lambda=50, trace.it=TRUE, type="svd")

str(fit1)
str(deBias(data_inc, fit1))
str(fit1_scale)

impute(fit1, 1, 1)
impute(fit1_debias, 1, 1)
impute(fit1_scale, 1, 1)

# Compare standard, debias, and scaling

fit_model <- function(data, frac, lam, comparison){
	
	# DEBUG
	data = data_wide
	lam = 50
	frac = 0.2
	
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
		rank.max=20, lambda=lam, 
		maxit=200, trace.it=TRUE, type="svd")
	
	fit_debias <- deBias(data_sparse, fit)
	
	# test
	fit_scale <- softImpute(
		biScale(data_sparse, col.scale=FALSE, row.scale=FALSE), 
		rank.max=20, lambda=lam, 
		maxit=200, trace.it=TRUE, type="svd")
		
	# Results
	
	results_standard <- impute(
		fit,
		i = hold_out_i,
		j = hold_out_j)
	
	results_debias<- impute(
		fit_debias,
		i = hold_out_i,
		j = hold_out_j)
	
	results_scale <- impute(
		fit_scale,
		i = hold_out_i,
		j = hold_out_j)
	
	# Results
	return(list(
		standard = calc_error(real_values, results_standard), 
		debias = calc_error(real_values, results_debias), 
		scale = calc_error(real_values, results_scale)
	))
}

fit_model(
	data = data_wide, 
	lam = 50,
	frac = 0.2)

lambda_val <- seq(1, 500, by=5)
sapply(lambda_val, function(x){
	fit_model(
		data = data_matrix, 
		lam = x,
		frac = 0.2,
		comparison = FALSE)
})


# biscale troubleshooting
data <- data_wide

non_missing <- which(!is.na(data))
hold_out <- sort(
	sample(non_missing, size = 0.2 * length(non_missing)))

data[1:10, 1:10]
data[hold_out] <- NA
data[1:10, 1:10]

data_sparse <- as(data, "Incomplete")

biScale(data_sparse, col.scale=FALSE, row.scale=FALSE)

sum((arrayInd(hold_out, dim(data))[, 1] %>% table()) == 1)
sum((arrayInd(hold_out, dim(data))[, 2] %>% table()) == 1)


