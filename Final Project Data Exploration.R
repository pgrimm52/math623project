#Final Project Data Exploration

library(tidyverse)
library(dplyr)
library(tidyr)

# DISABLED FOR PHIL
# setwd("C:/Users/mpeng/Documents/Mary/Georgetown/Math 623 - Sparse Sampling/Final Project/ml-latest-small/")
setwd("/Users/philipp/Google Drive/Courses/Math 623 Sparsity/project/ml-latest-small/")

#Import ratings data
  ratings_raw <- read.csv('ratings.csv', header = TRUE)
  ratings_frame <- data.frame(ratings_raw)
  
  ratings_small <- ratings_raw %>%
  	mutate(timestamp = as_datetime(timestamp)) %>%
  	filter(timestamp > ymd("2003-05-16")) %>% # because of data availability issue
  	dplyr::select(-timestamp)
  
  reshaped_ratings <- ratings_frame %>%
    select(-4) %>%
    spread(data=., key = movieId, value = rating)

  #Make sure that the data is in the right format
  reshaped_ratings[1:10,1:50]

  #How many users in total
  dim(reshaped_movie)[1]

  #How many movies in total
  dim(reshaped_movie)[2]
  
  
#Import genre data
  movies_raw <- read.csv('movies.csv', header = TRUE)
  movies_frame <- data.frame(movies_raw[,-2])

  head(movies_frame)
  
  #Turn the genres column into a list to determine how many distinct genres there are 
  lst <- strsplit(as.character(movies_frame$genres),"\\|")
  lvl <- unique(unlist(lst))      
  movies_boolean <- data.frame(movieId=as.numeric(movies_frame$movieId),
                    do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), 
                    stringsAsFactors=FALSE)
  
  head(movies_boolean)
  
  movies_frame %>%
    filter(genres == '(no genres listed)') #18 movies with no genres listed

#create a bar chart of number of movies by genre
  Number_movies_by_genre <- movies_boolean[,-1] %>%
    summarise_all(funs(sum))
  
  barplot(height = as.matrix(Number_movies_by_genre), width = 2, names.arg = colnames(Number_movies_by_genre),
          ylab = 'Number of Movies in Dataset', main = 'Number of Movies by Genre in Dataset', 
          col='blue', las=2)
  
ggdata1 <- movies %>%
	mutate(genres = str_split(genres, "\\|")) %>%
	unnest(genres) %>%
	count(genres) %>%
	slice(-1) %>%
	arrange(desc(n))
  
ggplot(data = ggdata1, aes(x = reorder(genres, n), y = n)) +
	geom_bar(stat = "identity", color="black", alpha=0.5) +
	geom_text(aes(label = n, hjust=-.2)) +
	coord_flip() +
	xlab("Genre") +
	ylab("Movie count\n(includes movies with multiple genres)") +
	theme_classic() +
	theme(axis.text=element_text(size=10))
ggsave("EDA_Figure_A.png")
  
#Bar chart showing average number of ratings per movie in that genre --> how sparsely populated is that 
  #Number of ratings by genre
    #Document the number of observations per movie
    number_of_ratings_by_movie <- dim(reshaped_ratings)[1] - sapply(X = reshaped_ratings[,-1], FUN = function(x) sum(is.na(x)))
    number_of_ratings_by_movie <- data.frame(movieId = as.numeric(rownames(as.matrix(number_of_ratings_by_movie))),
                                            n = number_of_ratings_by_movie)
    
    number_of_ratings_by_movie[1:5,]
    movies_boolean[1:5,]
    
    #Join the previous table to the genre table
    genre_ratings <- number_of_ratings_by_movie %>%
      left_join(movies_boolean, by="movieId")
    
    #Sum by genre - can do this later
  
  #Number of movies by genre 
  Number_movies_by_genre
  
#Distribution of ratings
  observed_ratings <- reshaped_ratings[,-1] %>%
    colSums(is.na(reshaped_ratings[,-1]))
  
  average_ratings <- observed_ratings / number_of_ratings_by_movie
  
  average_ratings[1:5]

  hist(average_ratings, breaks=9, xlim=c(1,5), plot=TRUE, col='green', main='Distribution of Average Rating',
       xlab = 'Rating', ylab = 'Frequency Based on Known Ratings')

ggplot(data = ratings_small, aes(x = rating)) +
	geom_histogram(bins = 10, color="black") +
	theme_classic() +
	xlab("Ratings") + 
	ylab("Count") +
	theme(axis.text=element_text(size=10))

  
  #Distribution of sparsity of data
  max(number_of_ratings_by_movie[,2])
  
  hist(number_of_ratings_by_movie[,2],plot=TRUE,breaks=50, xlim = c(0,340), ylim = c(0,6000),col='green', main='Distribution of Number of Ratings/Movie',
       xlab = 'Count of Ratings / Movie')

ggdata2 <- ratings_small %>%
	count(movieId)

ggplot(data = ggdata2, aes(x = n)) +
	geom_histogram(color="black") +
	theme_classic() +
	xlab("Ratings per movie \n(axis truncated due to long tail)") + 
	ylab("Count") +
	xlim(c(0, 30)) + 
	theme(axis.text=element_text(size=10))
  
  percent_rated_by_movie <- (number_of_ratings_by_movie[,-1]/dim(reshaped_movie)[1])*100

  hist(percent_rated_by_movie, breaks=10, plot=TRUE, main = '% of Users Rated, by Movie', 
       xlab='% of Users Rated Movie', col='green')
  
# num_users <- ratings_small %>% count(userId) %>% nrow()
# ggdata3 <- 
# ratings_small %>% count(movieId)
	