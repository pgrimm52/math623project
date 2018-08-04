#///////////////////////////////////////////////
# Generates all data for final project #########
#///////////////////////////////////////////////


## 1.0 Load packages/functions ####################

rm(list=ls())

library(caret)
library(RColorBrewer)
library(gridExtra)
library(tidyverse)
library(lubridate)
library(softImpute)
source("helper_functions.R")
set.seed(1234)
 

## 2.0 Load data ##########################

links <- read.csv("ml-latest-small/links.csv") %>% tbl_df()
movies <- read.csv("ml-latest-small/movies.csv") %>% tbl_df()
ratings <- read.csv("ml-latest-small/ratings.csv") %>% tbl_df()
tags <- read.csv("ml-latest-small/tags.csv") %>% tbl_df()

data_long <- ratings %>%
	mutate(timestamp = as_datetime(timestamp)) %>%
	# filter(timestamp > ymd("2003-05-16")) %>% # because of data availability issue
	dplyr::select(-timestamp)

data_wide <- data_long %>%
	spread(key = movieId, value = rating) %>%
	dplyr::select(-userId) %>%
	as.matrix()


## 3.0 Core model fitting functions ##########################

gen_holdout_indices <- function(data, frac){
	# Returns a list of indices to be held out for testing
	sampling_matrix <- cbind(
		index = which(!is.na(data), arr.ind=FALSE),
		which(!is.na(data), arr.ind=TRUE))
	
	holdout <- sample(
		x = sampling_matrix[, "index"],
		size = frac * length(sampling_matrix[, "index"]))
	
	return(as.vector(sort(holdout)))
}

fit_model_simple <- function(data, holdout_indices, lam, rank.max=50, debias=FALSE){
	
	data_train <- data
	data_train[holdout_indices] <- NA
	
	data_sparse <- as(data_train, "Incomplete") # sparse matrix object
	
	fit <- softImpute(
		data_sparse, 
		rank.max=rank.max, lambda=lam, 
		maxit=200, trace.it=FALSE, type="svd")
	
	if (debias == TRUE){
		fit <- deBias(data_sparse, fit)
	}

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


## 4.0 Core model fitting (MSE) ##########################

holdout <- gen_holdout_indices(data = data_wide, frac = 0.2)

data_holdout <- data_wide
data_holdout[holdout] <- NA

## 4.1 MSE lambda tuning ##################

lambda_tune <- seq(1, 50, by=2)

mse_tune <- sapply(lambda_tune, function(lam){
	print(paste("Loop for lambda =", lam))
	fit <- fit_model_simple(
		data = data_wide, 
		holdout_indices = holdout, 
		lam = lam)
	
	# rank <- fit$fit_object$d
	mse <- mean((fit$predicted - fit$actual)^2)
	return(mse)
	}
)

mse_tune_debias <- sapply(lambda_tune, function(lam){
	print(paste("Loop for lambda =", lam))
	fit <- fit_model_simple(
		data = data_wide, 
		holdout_indices = holdout, 
		lam = lam,
		debias = TRUE)
	
	# rank <- fit$fit_object$d
	mse <- calc_mse(fit$actual, fit$predicted)
	return(mse)
	}
)

lambda_best <- lambda_tune[which.min(mse_tune)]
lambda_best_debias <- lambda_tune[which.min(mse_tune_debias)]

## 4.2 MSE comparison ##################

# Dumb completion methods
m1 <- apply(data_holdout, 2, fill_missing_random)
m1_results <- fix_ratings(m1[holdout])

m2 <- apply(data_holdout, 2, fill_missing_average)
m2_results <- fix_ratings(m2[holdout])

m3 <- apply(data_holdout, 2, fill_missing_distribution)
m3_results <- fix_ratings(m3[holdout])

m4 <- fill_missing_distribution_global(data_holdout)
m4_results <- fix_ratings(m4[holdout])

c1 <- calc_mse(data_wide[holdout], m1_results)
c2 <- calc_mse(data_wide[holdout], m2_results)
c3 <- calc_mse(data_wide[holdout], m3_results)
c4 <- calc_mse(data_wide[holdout], m4_results)


## 4.3 Plot results ##################

png("figures/Figure_A.png")
plot(lambda_tune, mse_tune, 
		 xlab = expression(paste(lambda)), ylab="MSE",
		 col="green", type="l", lwd=3, 
		 ylim=c(1, 5))
lines(lambda_tune, mse_tune_debias, col="blue", lwd=3)
abline(v = lambda_best_debias, col="blue", lty=2)
abline(h = c1, lwd = 1.5, lty=2, col="black")
abline(h = c4, lwd = 1.5, lty=5, col="black")
abline(h = c3, lwd = 1.5, lty=4, col="brown")
abline(h = c2, lwd = 1.5, lty=3, col="brown")

legend(
	"topright",
	legend = c(
		"SVT algorithm",
		"SVT (debiased)",
		"Random",
		"Emp random",
		"Emp random (movie)",
		"Average (movie)"),
	col = c(
		"green",
		"blue",
		"black", "black", "brown", "brown"), 
	lwd = c(
		3, 3,
		1.5, 1.5, 1.5, 1.5),
	lty = c(
		1, 1, 
		2, 5, 4, 3),
	cex=0.75)
dev.off()


## 5.0 Core model fitting (Accuracy) ##########################

# holdout <- gen_holdout_indices(data = data_wide, frac = 0.2) # re-use previous

## 5.1 Accuracy lambda tuning ##################

acc_tune_debias <- sapply(lambda_tune, function(lam){
	print(paste("Loop for lambda =", lam))
	fit <- fit_model_simple(
		data = data_wide, 
		holdout_indices = holdout, 
		lam = lam,
		debias = TRUE)
	
	# rank <- fit$fit_object$d
	acc <- calc_acc(fit$actual, fit$predicted)
	return(acc)
}
)

lambda_best_debias_acc <- lambda_tune[which.max(acc_tune_debias)]

## 5.2 Confusion matrices ##################

fit_acc <- fit_model_simple(
	data = data_wide,
	holdout_indices = holdout,
	lam = lambda_best_debias_acc,
	debias = TRUE)

cfm1 <- confusionMatrix(
	factor(m1_results),
	factor(data_wide[holdout]))
cfm2 <- confusionMatrix(
	factor(m2_results),
	factor(data_wide[holdout]))
cfm3 <- confusionMatrix(
	factor(m3_results),
	factor(data_wide[holdout]))
cfm4 <- confusionMatrix(
	factor(m4_results),
	factor(data_wide[holdout]))
cfm5 <- confusionMatrix(
	factor(fit_acc$predicted),
	factor(data_wide[holdout]))

png("figures/Figure_B.png")
plot(lambda_tune, acc_tune_debias, 
		 xlab = expression(paste(lambda)), ylab="Accuracy",
		 col="blue", type="l", lwd=3,
		 ylim=c(0.05, 0.25))
abline(v = lambda_best_debias_acc, col="blue", lty=2)
abline(h = cfm1$overall["Accuracy"], lwd = 1.5, lty=2, col="black")
abline(h = cfm4$overall["Accuracy"], lwd = 1.5, lty=5, col="black")
abline(h = cfm3$overall["Accuracy"], lwd = 1.5, lty=4, col="brown")
abline(h = cfm2$overall["Accuracy"], lwd = 1.5, lty=3, col="brown")
legend(
	"topright",
	legend = c(
		"SVT (debiased)",
		"Random",
		"Emp random",
		"Emp random (movie)",
		"Average (movie)"),
	col = c(
		"blue",
		"black", "black", "brown", "brown"), 
	lwd = c(
		3,
		1.5, 1.5, 1.5, 1.5),
	lty = c(
		1, 
		2, 5, 4, 3),
	cex=0.75)
dev.off()

cfm1$overall["Accuracy"]
cfm2$overall["Accuracy"]
cfm3$overall["Accuracy"]
cfm4$overall["Accuracy"]
cfm5$overall["Accuracy"]

# > cfm1$overall["Accuracy"]
# Accuracy 
# 0.09995 
# > cfm2$overall["Accuracy"]
# Accuracy 
# 0.2224 
# > cfm3$overall["Accuracy"]
# Accuracy 
# 0.20565 
# > cfm4$overall["Accuracy"]
# Accuracy 
# 0.1745 
# > cfm5$overall["Accuracy"]
# Accuracy 
# 0.1908 

cfm1 %>% calc_accuracy_expanded()
cfm2 %>% calc_accuracy_expanded()
cfm3 %>% calc_accuracy_expanded()
cfm4 %>% calc_accuracy_expanded()
cfm5 %>% calc_accuracy_expanded() # matrix completion

# > cfm1 %>% calc_accuracy_expanded()
# [1] 0.286
# > cfm2 %>% calc_accuracy_expanded()
# [1] 0.54855
# > cfm3 %>% calc_accuracy_expanded()
# [1] 0.4092
# > cfm4 %>% calc_accuracy_expanded()
# [1] 0.3744
# > cfm5 %>% calc_accuracy_expanded() # matrix completion
# [1] 0.2766


## 5.3 Plot results ##################

# Heatmaps
generate_heatmap <- function(cfm, title){
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
		ggtitle(title) +
		theme_classic()
}

png("figures/Figure_C.png")
grid.arrange(
	cfm5 %>% generate_heatmap("SVT (debiased)"),
	cfm1 %>% generate_heatmap("Random"),
	cfm2 %>% generate_heatmap("Average (movie)"),
	cfm3 %>% generate_heatmap("Emp random (movie)"),
	cfm4 %>% generate_heatmap("Emp random"),
	nrow=3, ncol=2)
dev.off()

# Sensitivity plot

rate <- 1 # which cfm metric to plot (here: sensitivity)

# Construct necessary dataset
Rate <- rbind(
	cfm5$byClass[,rate],
	cfm1$byClass[,rate],
	cfm2$byClass[,rate],
	cfm3$byClass[,rate],
	cfm4$byClass[,rate])

Prevalence <- rbind(
	cfm5$byClass[,8],
	cfm1$byClass[,8],
	cfm2$byClass[,8],
	cfm3$byClass[,8],
	cfm4$byClass[,8])

ggdata <- cbind(
	reshape2::melt(Rate),
	Prevalence = reshape2::melt(Prevalence)[,3])

# Give better labels
ggdata$Var1 <- factor(ggdata$Var1)
levels(ggdata$Var1) <- c(
	"SVT algorithm",
	"Random",
	"Emp random",
	"Emp random (movie)",
	"Average (movie)")
levels(ggdata$Var2) <- gsub("Class: ", "", levels(ggdata$Var2))

# Plot and save
ggplot(data=ggdata,
			 aes(x = Var2, y = value, fill=Prevalence)) +
	geom_bar(stat="identity", color="black") +
	scale_fill_gradient(low = "white", high="red") +
	facet_grid(Var1 ~ .) +
	theme_minimal() +
	xlab("Movie rating") +
	ylab("Sensitivity") +
ggsave("figures/Figure_D.png", width = 7, height = 8)


## 6.0 Explore users ##################

user_data <- data.frame(
	user = arrayInd(holdout, dim(data_wide))[,1],
	movie = arrayInd(holdout, dim(data_wide))[,2],
	fit_acc$predicted,
	fit_acc$actual,
	Success_narrow = (fit_acc$predicted == fit_acc$actual),
	Success_expanded = (abs(fit_acc$predicted-fit_acc$actual)<0.51))
	
user_data <- user_data %>%
	group_by(user) %>%
	summarize(
		Success_narrow = mean(Success_narrow),
		Success_expanded = mean(Success_expanded))

ggplot(data=(user_data %>% gather(type, value, -user)),
			 aes(x=value, fill=type)) +
	geom_histogram(alpha=0.5, position="identity") + 
	scale_fill_manual(
		"Success criterion",
		values=c("#1f78b4", "#33a02c"), 
		labels=c("Expanded (+/- 0.5)", "Narrow (exact)")) + 
	theme_classic() +
	xlab("Accuracy") +
	ylab("Count")
ggsave("figures/Figure_E.png", height=4, width=6, units='in', dpi=600)

# Examine users (no good trends emerge) & plot

popularity_criterion <- 50

popular_movies_seen <- rowSums(!is.na(data_wide[, colSums(!is.na(data_wide), na.rm=TRUE)>popularity_criterion]), na.rm=TRUE)
movies_seen <- rowSums(!is.na(data_wide), na.rm=TRUE)
mainstream_viewer <- popular_movies_seen / movies_seen

user_characteristics <- data.frame(
	user = 1:nrow(data_wide),
	movies_seen = movies_seen,
	avg_rating = rowMeans(data_wide, na.rm=TRUE),
	mainstream_viewer = mainstream_viewer,
	high_success = (user_data$Success_expanded>0.5))

summary(user_characteristics)

ggplot(data=user_characteristics,
			 aes(x=mainstream_viewer, fill=high_success)) +
	geom_density(alpha=0.5, position="identity") +
	scale_fill_manual(
		"User accuracy high (>0.5)?",
		values=c("#FF8C00", "#1E90FF"), 
		labels=c("FALSE", "TRUE")) +
	theme_classic() +
	xlab("Prop of mainstream movies in user's history")
ggsave("figures/Figure_F.png", height=4, width=6, units='in', dpi=600)

ggplot(data=user_characteristics,
			 aes(x=movies_seen, fill=high_success)) +
	geom_density(alpha=0.5, position="identity") +
	xlim(c(0,600)) +
	scale_fill_manual(
		"User accuracy high (>0.5)?",
		values=c("#FF8C00", "#1E90FF"), 
		labels=c("FALSE", "TRUE")) +
	theme_classic() +
	xlab("Movies seen (by user)")
ggsave("figures/Figure_G.png", height=4, width=6, units='in', dpi=600)
