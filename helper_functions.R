# Helper functions

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