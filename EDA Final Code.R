#EDA Final Code

#########################
#########################
library('tidyverse')
library('dplyr')

##########################
#Import Data
##########################
setwd("C:/Users/mpeng/Documents/Mary/Georgetown/Math 623 - Sparse Sampling/Final Project/ml-latest-small/")

#ratings data
ratings_raw <- read.csv('ratings.csv', header = TRUE) %>%
                data.frame()

reshaped_ratings <- ratings_raw %>%
  select(-4) %>%
  spread(data=., key = movieId, value = rating)

#genre data
movies_data <- read.csv('movies.csv', header = TRUE) %>%
              data.frame()

#######################################
#Figure 1: Distribution of # of Ratings by Movie
#######################################
number_of_ratings_by_movie <- sapply(X = reshaped_ratings[,-1], FUN = function(x) sum(!is.na(x)))

#Phil to regenerate using ggplot
hist(number_of_ratings_by_movie[,2],plot=TRUE,breaks=50, xlim = c(0,340), ylim = c(0,6000),col='green', 
     main='Figure 1: Number of Ratings/Movie',
     xlab = 'Number of Ratings', ylab = 'Number of Movies')


#######################################
#Figure 2: % of total movies rated / user 
#######################################
percent_of_ratings_by_user <- round(apply(X = reshaped_ratings, 1, FUN = function(x) sum(!is.na(x)))/(dim(reshaped_ratings)[2]-1),2) * 100

#Phil to regenerate using ggplot
hist(percent_of_ratings_by_user,plot=TRUE,breaks=50, ylim = c(0,300), col='green', 
     main='Figure 2: % Movies Rated/User',
     xlab = '% of Movies Rated', ylab = 'Number of Users')

#######################################
#Figure 3: Average Rating / Movie
#######################################
avg_observed_ratings <- reshaped_ratings[,-1] %>%
                        colSums(is.na(reshaped_ratings[,-1]))/number_of_ratings_by_movie

#Phil to regenerate using ggplot
hist(round(avg_observed_ratings$n,2), breaks=9, xlim=c(1,5), ylim = c(0,4000), plot=TRUE, col='green', 
     main='Figure 3: Average Rating/Movie',
     xlab = 'Average Rating for Movie', ylab = 'Count of Movies')

###########################################
#Figure 4: Distribution of Ratings by time
###########################################
data_long <- read.csv("ml-latest-small/ratings.csv", stringsAsFactors = FALSE) %>% 
  tbl_df() %>%
  mutate(timestamp = as_datetime(timestamp)) %>%
  filter(timestamp > ymd("2003-05-16")) %>%
  dplyr::select(-timestamp)

#######################################
#Figure 5: Count of Movies by genre
#######################################

#Turn the genres column into a list to determine how many distinct genres there are 
lst <- strsplit(as.character(movies_data$genres),"\\|")
lvl <- unique(unlist(lst))      
movies_boolean <- data.frame(movieId=as.numeric(movies_data$movieId),
                             do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), 
                             stringsAsFactors=FALSE)

#create a bar chart of number of movies by genre
Number_movies_by_genre <- movies_boolean[,-1] %>%
  summarise_all(funs(sum))

#Phil to regenerate chart
barplot(height = as.matrix(Number_movies_by_genre), width = 2, names.arg = colnames(Number_movies_by_genre),
        ylab = 'Number of Movies', main = 'Figure 5: Count of Movies by Genre', 
        col='blue', las=2)

