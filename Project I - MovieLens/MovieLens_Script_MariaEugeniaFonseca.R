# MovieLens Project
# Author: Maria Eugenia Fonseca
# Date: May 2019
  
# Note: this script took several minutes to run

# Importing data ----------------------------------------------------------
# Note: this first code chunk was provided by the course

#############################################################
# Create edx set, validation set, and submission file
#############################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Descriptive analysis ----------------------------------------------------

# Loading required packages
requiredPackages <- c("tidyverse", "rafalib", "ggpubr", "knitr", "raster")
lapply(requiredPackages, library, character.only = TRUE)

# Histogram of ratings per movie: Some movies are more rated than others
hist_movies <- edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  labs(
    title = "Histogram of ratings per movie",
    x = "Number of ratings per movie", y = "Count", fill = element_blank()
  ) +
  theme_classic()

# Histogram of ratings per user: Some users are rated more movies than others
hist_users <- edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  labs(
    title = "Histogram of ratings per user",
    x = "Number of ratings per user", y = "Count", fill = element_blank()
  ) +
  theme_classic()

ggarrange(hist_movies, hist_users,
  ncol = 2, nrow = 1
)

# Histogram of ratings
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(fill = "steelblue") +
  labs(
    title = "Histogram of ratings",
    x = "Ratings", y = "Count", fill = element_blank()
  ) +
  theme_classic()

# View of all unique genres
unique_genres_list <- str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique()

unique_genres_list

# Creating the long version of both the train and validation datasets with separeted genres
edx_genres <- edx %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)

validation_genres <- validation %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)

# Histogram of ratings per genre
hist_genres <- ggplot(edx_genres, aes(x = reorder(genres, genres, function(x) -length(x)))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Ratings per genre",
    x = "Genre", y = "Counts"
  ) +
  scale_y_continuous(
    labels = paste0(1:4, "M"),
    breaks = 10^6 * 1:4
  ) +
  coord_flip() +
  theme_classic()

hist_genres

# Boxplot of movie ratings per genre
boxplot_genre_ratings <- ggplot(edx_genres, aes(genres, rating)) +
  geom_boxplot(fill = "steelblue", varwidth = TRUE) +
  labs(
    title = "Movie ratings per genre",
    x = "Genre", y = "Rating", fill = element_blank()
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

boxplot_genre_ratings

# Creating the RMSE function
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Analysis section --------------------------------------------------------

# Method: Just the average ------------------------------------------------
mu_hat <- mean(edx$rating)
mod_average <- RMSE(edx$rating, mu_hat)

rmse_results <- tibble(Method = "Just the average", RMSE = mod_average)
kable(rmse_results)

# Method: Movie Effect Model ----------------------------------------------
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

predicted_ratings <- mu_hat + validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  pull(b_i)

predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

mod_m <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Movie Effect Model",
    RMSE = mod_m
  )
)

kable(rmse_results[2, ])

# Method: Movie + User Effects Model --------------------------------------
user_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

mod_m_u <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Movie + User Effects Model",
    RMSE = mod_m_u
  )
)

kable(rmse_results[3, ])

# Method: Movie + User + Genres Effects Model -----------------------------
genres_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))

predicted_ratings <- validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genres_avgs, by = c("genres")) %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)

predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

model_m_u_g <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Movie + User + Genres Effects Model",
    RMSE = model_m_u_g
  )
)

kable(rmse_results[4, ])

# Method: Movie + User + Genres Ind. Effects Model ------------------------
genres_avgs_ind <- edx_genres %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_gInd = mean(rating - mu_hat - b_i - b_u))

predicted_ratings <- validation_genres %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genres_avgs_ind, by = c("genres")) %>%
  mutate(pred = mu_hat + b_i + b_u + b_gInd) %>%
  pull(pred)

predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

model_m_u_gInd <- RMSE(predicted_ratings, validation_genres$rating)
rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Movie + User + Genres Ind. Effects Model",
    RMSE = model_m_u_gInd
  )
)

kable(rmse_results[5, ])

# Method: Movie + User + Genres Ind. + Genre_User Effects Model -----------
genres_user_avgs <- edx_genres %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genres_avgs_ind, by = "genres") %>%
  group_by(genres, userId) %>%
  summarize(b_gu = mean(rating - mu_hat - b_i - b_u - b_gInd))

predicted_ratings <- validation_genres %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genres_avgs_ind, by = c("genres")) %>%
  left_join(genres_user_avgs, c("userId", "genres")) %>%
  mutate(
    b_gu = ifelse(is.na(b_gu), 0, b_gu),
    pred = mu_hat + b_i + b_u + b_gInd + b_gu
  ) %>%
  pull(pred)

predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

model_m_u_gInd_gu <- RMSE(predicted_ratings, validation_genres$rating)
rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Movie + User + Genres Ind. + Genre_User Effects Model",
    RMSE = model_m_u_gInd_gu
  )
)

kable(rmse_results[6, ])

# Method: Regularized Movie + User + Genre Ind. + Movie_Genre + Genre_User Effect Model --------
# Regularized parameter
lambdas <- seq(11.5, 12.5, 0.2)

# Grid search to tune the regularized parameter lambda
rmses <- sapply(lambdas, function(l) {
  mu <- mean(edx$rating)

  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l))

  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + l))

  b_g <- edx_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + l))

  b_gu <- edx_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(userId, genres) %>%
    summarize(b_gu = sum(rating - mu - b_i - b_u - b_g) / (n() + l))

  predicted_ratings <- validation_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_gu, by = c("userId", "genres")) %>%
    mutate(
      b_gu = ifelse(is.na(b_gu), 0, b_gu),
      pred = mu + b_i + b_u + b_g + b_gu
    ) %>%
    pull(pred)
  
  predicted_ratings <- clamp(predicted_ratings, 0.5, 5)

  return(RMSE(predicted_ratings, validation_genres$rating))
})

plot_rmses <- qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]

rmse_results <- bind_rows(
  rmse_results,
  tibble(
    Method = "Regularized Movie + User + Genre Ind. + Movie_Genre + Genre_User Effect Model",
    RMSE = min(rmses)
  )
)

kable(rmse_results[7, ])

# Results section ---------------------------------------------------------

kable(rmse_results)