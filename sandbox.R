############
## Play with data and softImpute for project
############

# Prelims
library(softImpute)
library(tidyverse)
library(ggplot2)
setwd("/Users/philipp/Google Drive/Courses/Math 623 Sparsity/project")

# Simple SVD
test <- matrix(1:16, nrow=4)
s <- svd(test)
s$u %*% diag(s$d) %*% t(s$v)

# Play with softImpute
set.seed(1011)
x=matrix(rnorm(30),6,5)
x[sample(1:30,10,replace=FALSE)]=NA
x
fits=softImpute(x,trace=TRUE,type="svd")
fits2=softImpute(x,rank.max=3,lambda=1.9,trace=TRUE,type="svd")

fits$u %*% diag(fits$d) %*% t(fits$v)
x
softImpute::complete(x,fits)

fits2$u %*% diag(fits2$d) %*% t(fits2$v)
x
softImpute::complete(x,fits2)

# Load movielens data
data <- read.csv("ml-latest-small/ratings.csv", stringsAsFactors = FALSE) %>% tbl_df()
str(data)

data %>% count(userId)
data %>% count(movieId)

(data %>% count(userId) %>% nrow()) * (data %>% count(movieId) %>% nrow())

data %>% count(userId, movieId) %>% nrow

(data %>% count(userId, movieId) %>% nrow) / 
	((data %>% count(userId) %>% nrow()) * (data %>% count(movieId) %>% nrow()))

data_matrix <- data %>%
	select(-timestamp) %>%
	spread(key = movieId, value = rating)

############
## Soft Imputing Movielens data
############

x=matrix(rnorm(30),6,5)
x
x_orig <- x
hold_out <- sample(1:30,10,replace=FALSE)
x[hold_out] <- NA
x

fit <- softImpute(x,trace=TRUE,type="svd")
x_complete <- softImpute::complete(x, fit)
x_complete

sum((x_complete[hold_out] - x_orig[hold_out])^2)/length(hold_out)





############
## Soft Imputing Movielens data
############

# Try with a sample of our data
dim(data_matrix)
data_sample <- as.matrix(data_matrix[1:100, 2:501])
# data_sample <- as.matrix(data_matrix)

# Hold-out some data for training
non_missing <- which(!is.na(data_sample))
hold_out <- sample(non_missing, size = 1/6 * length(non_missing))
data_sample_train <- data_sample
data_sample_train[hold_out] <- NA 

# Impute
lambda0(data_sample_train)
fit <- softImpute(data_sample_train, 
									rank.max = 90, 
									lambda = 2, 
									type = "svd", maxit = 1000, trace.it = TRUE)
data_sample_complete <- softImpute::complete(data_sample_train, fit)

# Snap to 0.5 through 5 rating (in half-unit increments)
data_sample_complete <- round(data_sample_complete/0.5)*0.5
data_sample_complete[data_sample_complete<0.5] <- 0.5
data_sample_complete[data_sample_complete>5] <- 5

# Compare modeled hold-out to actual hold-out
sum((data_sample[hold_out] - data_sample_complete[hold_out])^2)/length(hold_out)

# Compare to randomly filled rating

data_random <- data_sample
data_random[hold_out] <- sample(1:10/2, length(hold_out), replace=TRUE)
sum((data_sample[hold_out] - data_random[hold_out])^2)/length(hold_out)


# Visualize fit
cbind(data_sample[hold_out], data_sample_complete[hold_out])
plot(jitter(data_sample_complete[hold_out], 1) ~ jitter(data_sample[hold_out], 1))
abline(0,1, col="blue")
abline(lm(data_sample_complete[hold_out] ~ data_sample[hold_out]), col="red")

# Iterate over many lambda
calc_error <- function(lam){
	fit <- softImpute(data_sample_train, 
										rank.max = 90, 
										lambda = lam, 
										type = "svd", maxit = 1000, trace.it = FALSE)
	data_sample_complete <- softImpute::complete(data_sample_train, fit)
	
	# Snap to 0.5 through 5 rating (in half-unit increments)
	data_sample_complete <- round(data_sample_complete/0.5)*0.5
	data_sample_complete[data_sample_complete<0.5] <- 0.5
	data_sample_complete[data_sample_complete>5] <- 5
	
	# Compare modeled hold-out to actual hold-out
	print(paste("Loop complete for lambda:", lam))
	return(
		cbind(		
			sum((data_sample[hold_out] - data_sample_complete[hold_out])^2)/length(hold_out),
			length(fit$d)
		))
}

lambda_val <- seq(0, 10, by=.5)
sapply(lambda_val, calc_error)



############
## EDA with tags
############

links <- read.csv("ml-latest-small/links.csv") %>% tbl_df()
movies <- read.csv("ml-latest-small/movies.csv") %>% tbl_df()
ratings <- read.csv("ml-latest-small/ratings.csv") %>% tbl_df()
tags <- read.csv("ml-latest-small/tags.csv") %>% tbl_df()

links
movies
ratings
tags

View(tags)

# Summary stats on tags (less then 10% of users/movies)
tags %>% nrow

tags %>% count(userId) %>% nrow
ratings %>% count(userId) %>% nrow
tags %>% count(userId) %>% nrow / ratings %>% count(userId) %>% nrow

tags %>% count(movieId) %>% nrow
movies %>% nrow
tags %>% count(movieId) %>% nrow / movies %>% nrow

# Tags per user
tags %>%
	group_by(userId) %>%
	summarize(n_tags = n()) %>% 
	summary()

# Viz: distribution of tags per user
tags %>%
	group_by(userId) %>%
	summarize(n_tags = n()) %>%
	filter(n_tags < 100) %>%
	qplot(data = ., n_tags, geom="density")

# Viz: number of tags per genre
tags %>%
	select(userId, movieId) %>%
	left_join(movies %>% select(movieId, genres), by="movieId") %>%
	count(genres) %>%
	arrange(desc(n)) %>%
	filter(row_number() <= 25) %>%
	ggplot(aes(x=reorder(genres, n), y=n)) +
  	geom_bar(stat='identity') +
  	coord_flip()

# Adjusted tag prevalence by genre
movies %>%
	select(movieId, genres) %>%
	left_join(
		tags %>% count(movieId),
		by="movieId") %>%
	group_by(genres) %>%
	summarize(
		n_movies = n(),
		n_tags = sum(n, na.rm=TRUE),
		tags_per_movie = n_tags/n_movies) %>%
	arrange(desc(tags_per_movie)) %>%
	filter(row_number()<=25) %>%
	ggplot(aes(x=reorder(genres, tags_per_movie), y=tags_per_movie)) +
  	geom_bar(stat='identity') +
  	coord_flip()

## Top genres only: adjusted tag prevalence by genre
top_genres <- movies %>%
	count(genres) %>%
	arrange(desc(n)) %>%
	filter(row_number() <= 25) %>%
	pull(genres)

movies %>%
	select(movieId, genres) %>%
	left_join(
		tags %>% count(movieId),
		by="movieId") %>%
	filter(genres %in% top_genres) %>%
	group_by(genres) %>%
	summarize(
		n_movies = n(),
		n_tags = sum(n, na.rm=TRUE),
		tags_per_movie = n_tags/n_movies) %>%
	arrange(desc(tags_per_movie)) %>%
	filter(row_number()<=25) %>%
	ggplot(aes(x=reorder(genres, tags_per_movie), y=tags_per_movie)) +
  	geom_bar(stat='identity') +
  	coord_flip()

# tags vs. rating

movies %>%
	select(movieId, genres) %>%
	left_join(
		tags %>% count(movieId),
		by="movieId") %>%
	mutate(n_tags = ifelse(is.na(n), 0, n)) %>%
	select(-n) %>%
	left_join(
		ratings %>%
			group_by(movieId) %>%
			summarize(avg_rating = mean(rating)),
		by="movieId") %>%
	filter(n_tags > 0) %>%
	ggplot(aes(x=avg_rating, y=n_tags)) +
  	geom_point() +
		geom_smooth(method=lm)

# most common tags (weird)
tags %>%
	count(tag) %>%
	arrange(desc(n))















