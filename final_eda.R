data_long
data_wide

links <- read.csv("ml-latest-small/links.csv") %>% tbl_df()
movies <- read.csv("ml-latest-small/movies.csv") %>% tbl_df()
ratings <- read.csv("ml-latest-small/ratings.csv") %>% tbl_df()
tags <- read.csv("ml-latest-small/tags.csv") %>% tbl_df()


# How many user-movie pairs have ratings
calc_sparsity(data_wide)

# How many genres
movies %>% separate(genres, "|")
