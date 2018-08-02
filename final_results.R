################################################
# Script to generate final results for project
################################################

###############
# Load packages/functions
###############

library(caret)
library(RColorBrewer)
library(gridExtra)
library(tidyverse)
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
		maxit=200, trace.it=TRUE, type="svd")
	
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
# Lambda selection & alternative imputation methods
###############

lambda_val <- seq(1, 50, by=2)

mse <- sapply(lambda_val, function(x){
	print(paste("Loop for lambda =", x))
	fit_model(
		data = data_wide, 
		lam = x,
		frac = 0.2,
		comparison = FALSE)
})

best_lambda <- lambda_val[which.min(unlist(mse[2,]))]

# Rank of new matrix
cbind(
	lambda_val,
	mse[3, ])

# Alternative imputation at best lambda
comparison <- fit_model(
	data = data_wide, 
	lam = best_lambda,
	frac = 0.2,
	comparison = TRUE)

# Plot all
png('Threshold_size.png')
plot(lambda_val, mse[1, ], 
		 xlab = expression(paste(lambda)), ylab="MSE", main="Thresholding size selection",
		 col="blue", type="l", lwd=3, 
		 ylim=c(1, 5))
lines(lambda_val, mse[2, ], col="green", lwd=3)
abline(v=best_lambda, col="green", lty=2)
abline(h = comparison$random_fill, lty=2, col="black")
abline(h = comparison$dist_glob_fill, lty=5, col="black")
abline(h = comparison$dist_fill, lty=4, col="black")
abline(h = comparison$average_fill, lty=3, col="black")

legend(
	"topright",
	legend = c(
		"SVT algorithm",
		"SVT algorithm (debiased)",
		"Random",
		"Adj random",
		"Adj random/movie",
		"Mean/movie"),
	col = c(
		"blue",
		"green",
		rep("black", 4)),
	lwd = c(
		3, 3,
		1, 1, 1, 1),
	lty = c(
		1, 1, 
		2, 5, 4, 3),
	cex=0.75)
dev.off()

# Extra visualization comparing SVT imputed scores to actual scores
actual <- data_wide[comparison$hold_out]
predicted <- comparison$m0d_results

png("Actual_vs_predicted.png")
ggplot2::qplot(
	jitter(actual, 2),
	jitter(predicted, 2),
	geom="point", 
	alpha=I(0.1),
	xlab = "Actual rating",
	ylab = "Predicted rating") + theme_bw()
dev.off()


###############
# Hold-out fraction selection
###############

frac_val <- seq(0.05, 0.4, by=0.05)

mse_frac <- sapply(frac_val, function(x){
	print(paste("Loop for frac =", x))
	fit_model(
		data = data_wide, 
		lam = best_lambda,
		frac = x,
		comparison = FALSE)
})

png("Best_holdout_size.png")
plot(frac_val, 
		 unlist(mse_frac[2, ]), 
		 xlab = "Holdout fraction", ylab="MSE", 
		 main="Holdout fraction selection",
		 col="green", type="l", lwd=3)
dev.off()


###############
# Create confusion matrix
###############

test_fit <- fit_model(
		data = data_wide, 
		lam = best_lambda,
		frac = 0.2,
		comparison = FALSE)

data_damaged <- data_wide
data_damaged[test_fit$hold_out] <- NA

m1 <- apply(data_damaged, 2, fill_missing_random) %>% fix_ratings
m2 <- apply(data_damaged, 2, fill_missing_average) %>% fix_ratings
m3 <- apply(data_damaged, 2, fill_missing_distribution) %>% fix_ratings
m4 <- fill_missing_distribution_global(data_damaged) %>% fix_ratings

generate_heatmap <- function(cfm){
	# Generates heatmap from caret::confusionMatrix object
	
	# Grab table & convert to proportion
	cfm_prop <- apply(cfm$table, 2, function(x){x/sum(x)})
	# Melt to long form
	ggdata <- reshape2::melt(cfm_prop)
	# Plot heatmap
	ggplot(data = ggdata,
			 aes(x = Reference, y = Prediction, fill = value)) +
	geom_tile() +
	coord_equal() +
	scale_fill_gradientn(
		colours = colorRampPalette(brewer.pal(6, "YlOrRd"))(6), 
		limits = c(0, .4),
		na.value = "#FFFFB2") +
	theme_classic()
}

calc_accuracy_expanded <- function(cfm){
	# Expanded accuracy measure (within 0.5 points)
	d <- seq(12, 89, by=11)
	i <- c(1, 2, d, d-1, d+1, 99, 100)
	return(
		sum(cfm$table[i])/sum(cfm$table))}

reference <- factor(data_wide[test_fit$hold_out])
cfm1 <- confusionMatrix(
	factor(test_fit$m0d_results),
	reference)
cfm2 <- confusionMatrix(
	factor(m1[test_fit$hold_out]),
	reference)
cfm3 <- confusionMatrix(
	factor(m2[test_fit$hold_out]),
	reference)
cfm4 <- confusionMatrix(
	factor(m3[test_fit$hold_out]),
	reference)
cfm5 <- confusionMatrix(
	factor(m4[test_fit$hold_out]),
	reference)

cfm1 %>% calc_accuracy_expanded()
cfm2 %>% calc_accuracy_expanded()
cfm3 %>% calc_accuracy_expanded()
cfm4 %>% calc_accuracy_expanded()
cfm5 %>% calc_accuracy_expanded()

png("Heatmap_grid.png")
grid.arrange(
	cfm1 %>% generate_heatmap(),
 	cfm2 %>% generate_heatmap(),
 	cfm3 %>% generate_heatmap(),
 	cfm4 %>% generate_heatmap(),
 	cfm5 %>% generate_heatmap(),
	nrow=5, ncol=1)
dev.off()


###############
# Alternative sampling strategies
###############

# STILL TO BE FLESHED OUT

# oversample_holdout_indices <- function(data, frac){
# 	# Returns a list of indices to be held out for testing,
# 	# oversamples less prolific viewers
# 	weights <- 1/rowSums(!is.na(data), na.rm=TRUE)
# 	weights[weights == 1] <- 0
# 	
# 	sampling_matrix <- cbind(
# 		index = which(!is.na(data), arr.ind=FALSE),
# 		which(!is.na(data), arr.ind=TRUE),
# 		weights = weights[which(!is.na(data), arr.ind=TRUE)[, 1]])
# 	
# 	holdout <- sample(
# 		x = sampling_matrix[, 1],
# 		size = frac * length(sampling_matrix[, "index"]),
# 		prob = sampling_matrix[, "weights"])
# 	
# 	return(as.vector(sort(holdout)))
# }


###############
# More confusion matrix stuff
###############

rate <- 1 # which cfm metric to plot (here: sensitivity)

# Construct necessary dataset
Rate <- rbind(
  cfm1$byClass[,rate],
	cfm2$byClass[,rate],
	cfm3$byClass[,rate],
	cfm4$byClass[,rate],
	cfm5$byClass[,rate])

Prevalence <- rbind(
	cfm1$byClass[,8],
	cfm2$byClass[,8],
	cfm3$byClass[,8],
	cfm4$byClass[,8],
	cfm5$byClass[,8])
reshape2::melt(Prevalence)

ggdata <- cbind(
	reshape2::melt(Rate),
	Prevalence = reshape2::melt(Prevalence)[,3])

# Give better labels
ggdata$Var1 <- factor(ggdata$Var1)
levels(ggdata$Var1) <- c(
	"SVT algorithm",
	"Random",
	"Adj random",
	"Adj random/movie",
	"Mean/movie")
levels(ggdata$Var2) <- gsub("Class: ", "", levels(ggdata$Var2))

# Plot and save
ggplot(data=ggdata,
			 aes(x = Var2, y = value, fill=Prevalence)) +
	geom_bar(stat="identity", color="black") +
	scale_fill_gradient(low = "white", high="red") +
	facet_grid(Var1 ~ .) +
	theme_classic() +
	xlab("Movie rating") +
	ylab("Sensitivity") +
	ggtitle("Sensitivity by Imputation/Rating\n(color denotes frequency)")
ggsave("Sensitivity.png", width = 7, height = 8)


###############
# Explore users
###############

# Generate data
ho <- gen_holdout_indices(
	data = data_wide,
	frac = 0.2)

fit <- fit_model_simple(
	data = data_wide,
	holdout_indices = ho,
	lam = 19,
	rank.max = 50)

confusionMatrix(
	factor(fit$predicted),
	factor(fit$actual))

# Plot results (distribution of users' narrow vs. expanded success rates)
ho_results <- data.frame(
	user = arrayInd(ho, dim(data_wide))[,1],
	movie = arrayInd(ho, dim(data_wide))[,2],
	fit$predicted,
	fit$actual,
	Success_narrow = (fit$predicted == fit$actual),
	Success_expanded = (abs(fit$predicted-fit$actual)<0.51))

ho_results <- ho_results %>%
	group_by(user) %>%
	summarize(
		Success_narrow = mean(Success_narrow),
		Success_expanded = mean(Success_expanded))

ggplot(data=(ho_results %>% gather(type, value, -user)),
			 aes(x=value, fill=type)) +
	geom_histogram(alpha=0.5, position="identity") + 
	scale_fill_manual(values=c("#1f78b4", "#33a02c")) + 
	theme_classic() +
	xlab("Successful prediction rate for user") +
	ylab("Density") +
	ggtitle("Distribution of Prediction Performance Across Users")
ggsave("User performance distribution.png", width = 7, height = 8)

# Examine users (no good trends emerge)

popularity_criterion <- 50

popular_movies_seen <- rowSums(!is.na(data_wide[, colSums(!is.na(data_wide), na.rm=TRUE)>popularity_criterion]), na.rm=TRUE)
movies_seen <- rowSums(!is.na(data_wide), na.rm=TRUE)
mainstream_viewer <- popular_movies_seen / movies_seen

user_characteristics <- data.frame(
	user = 1:nrow(data_wide),
	movies_seen = movies_seen,
	avg_rating = rowMeans(data_wide, na.rm=TRUE),
	mainstream_viewer = mainstream_viewer,
	high_success = (ho_results$Success_expanded>0.5))

summary(user_characteristics)

ggplot(data=user_characteristics,
			 aes(x=movies_seen, y=avg_rating, color=high_success)) +
	geom_point(alpha=0.5) +
	xlim(c(20, 120))
ggsave("User-1.png")

ggplot(data=user_characteristics,
			 aes(x=mainstream_viewer, y=avg_rating, color=high_success)) +
	geom_point(alpha=0.5)
gsave("User-2.png")

ggplot(data=user_characteristics,
			 aes(x=mainstream_viewer, y=movies_seen, color=high_success)) +
	geom_point(alpha=0.5) +
	ylim(c(0,500))
gsave("User-3.png")

ggplot(data=user_characteristics,
			 aes(x=mainstream_viewer, fill=high_success)) +
	geom_density(alpha=0.5, position="identity")
gsave("User-4.png")

ggplot(data=user_characteristics,
			 aes(x=avg_rating, fill=high_success)) +
	geom_histogram(alpha=0.5, position="identity")
gsave("User-5.png")

ggplot(data=user_characteristics,
			 aes(x=movies_seen, fill=high_success)) +
	geom_histogram(alpha=0.5, position="identity")
gsave("User-6.png")


###############
# Tune lambda to accuracy
###############

ho <- gen_holdout_indices(
	data = data_wide,
	frac = 0.2)

lambda_val <- seq(1, 50, by=2)

tune_lam_accuracy <- sapply(lambda_val, function(x){
	fit <- fit_model_simple(
		data = data_wide,
		holdout_indices = ho,
		lam = x,
		rank.max = 50)
	
	return(confusionMatrix(
		factor(fit$predicted),
		factor(fit$actual))$overall["Accuracy"])
})

# Plot all
png('Threshold_size_accuracy.png')
plot(lambda_val, tune_lam_accuracy, 
		 xlab = expression(paste(lambda)), ylab="Accuracy", main="Thresholding size selection",
		 col="blue", type="l", lwd=3)
dev.off()
