##Ashley Reinhart
##github.com/anreinhart
##HarvardX: PH125.9x Data Science: MovieLens Capstone


#####################################################
#create edx set, validation set (final hold-out test set) 
#this code provided by HarvardX: PH125.9x
#####################################################
# Note: this process could take a couple of minutes
# STEP 1:
if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(caret)
library(tidyr)
library(stringr)
library(tinytex)
library(kableExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if (!file.exists(dl)) {
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
}

ratings_file <- "ml-10M100K/ratings.dat"
if (!file.exists(ratings_file)) {
  unzip(dl, ratings_file)
}

movies_file <- "ml-10M100K/movies.dat"
if (!file.exists(movies_file)) {
  unzip(dl, movies_file)
}

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE
)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(
    userId = as.integer(userId),
    movieId = as.integer(movieId),
    rating = as.numeric(rating),
    timestamp = as.integer(timestamp)
  )

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE
)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

# To save memory, which my computer has little of
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#####################################################
# Methods and Analysis
#####################################################

## Examining the EDX Data Set
dim(edx)
head(edx,8)

#obtaining distinct counts of users, movies and genre combos
distinct_counts <- edx %>%
  summarise(
    distinct_userIds = n_distinct(userId),
    distinct_movieIds = n_distinct(movieId),
    distinct_combo_genre = n_distinct(genres)
  )
# Print the distinct counts
print(distinct_counts)


#visualizing ratings by users

edx %>%
  group_by(userId) %>% # grouping the data by userId
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color = "black", fill = "#6952b9", bins = 40) +
  scale_x_log10() +
  theme_minimal() + # set theme to minimal
  labs(x = "Ratings", y = "Users", title = "Ratings by Users") + # updating
  #label names
  theme(
    plot.title = element_text(hjust = 0.5), # center title
    axis.text = element_text(size = 10), # adjusting text size
    axis.title = element_text(size = 12), # adjusting the title
    panel.grid.major = element_line(color = "gray", linetype = "dashed"), # add
    #dashed lines
    panel.grid.minor = element_blank() # Removing minor grid lines
  )

# populating rating options
length(unique(edx$rating))

# populating distinct rating options
unique_ratings <- unique(edx$rating)
sort(unique_ratings)

# summarizing ratings
edx %>%
  group_by(rating) %>%
  summarize(ratings_sum = n()) %>%
  arrange(desc(ratings_sum))

# % of ratings that are greater than or equal to a 3 star
ratingpercent <- edx %>% filter(edx$rating >= 3)
nrow(ratingpercent) / length(edx$rating)

#plotting ratings distribution
edx %>%
  ggplot(aes(x = rating)) + # set x axis as the rating
  geom_histogram(binwidth = 0.25, fill = "#6952b9", color = "white") + # add
  #histogram layers
  ggtitle("Distribution of Movie Ratings") + # plot title
  labs(x = "Rating", y = "Number of Ratings") + # set the x and y axis
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # align title
    axis.text = element_text(size = 10), # set text size for axis labs
    axis.title = element_text(size = 12), # set text size for axis titles
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),#major grid lines
    panel.grid.minor = element_blank() # removes minor grid lines
  ) +
  scale_y_continuous(labels = scales::comma) # adjust y-axis scale to use commas for thousands separator


#A function was created so that it could be easily applied to the final_holdout_test data later on. This function extracts the release year from the title, changes the timestamp to the year reviewed, calculates the movie age, and separates the genres. 
cleaning_data <- function(data, timestamp_column, title_column) {
  data <- data %>%
    # extracting release year from title
    mutate(release_year = str_extract({{ title_column }}, "\\((\\d{4})\\)")) %>%
    # remove parentheses from release year
    mutate(release_year = as.integer(str_replace(release_year, "\\((\\d{4})\\)", "\\1"))) %>%
    # convert timestamp column to year and rename it to "review_year"
    mutate(review_year = as.POSIXct({{ timestamp_column }}, origin = "1970-01-01", tz = "EST")) %>%
    mutate(review_year = as.integer(format(review_year, "%Y"))) %>%
    # remove the original timestamp column and add a movie_age column
    select(-{{ timestamp_column }}, review_year) %>%
    mutate(movie_age = as.integer(2024 - release_year)) %>%
    separate_rows(genres, sep = "\\|")
  
  return(data)
}

edx <- cleaning_data(edx, timestamp, title) # applying function to edx data


#Validation function executed properly
# Filter and print records where release_year is greater than 2024 or less than 1900 to validate function exceuted properly
edx %>%
  group_by(movieId, title, release_year) %>%
  filter(release_year > 2024 | release_year < 1900) %>%
  distinct(release_year)

# validating that the function correctly extracted the release year from movies with 4 digit integers in the title
desired_movie_id <- 53953

edx %>% filter(movieId == desired_movie_id) %>%
  slice(1)


#plotting reviews per year bar chart

edx %>%
  ggplot(aes(x = review_year)) + # sett x axis to review year
  geom_bar(fill = "#6952b9", color = "black") + # setting to barplot
  ggtitle("User Ratings Per Year") + # plot title
  labs(x = "Year", y = "Ratings") + # set x and y axis
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # align title
    axis.text = element_text(size = 10), # sett text size
    axis.title = element_text(size = 12), # setti axis title size
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),#major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  ) +
  scale_y_continuous(labels = scales::comma)  # adjust y-axis scale to use commas for thousands separator


# Calculate the mean rating for each movie_age group, ignoring NA values.
mean_rating_age <- edx %>%
  group_by(movie_age) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE))

# plotting mean_rating_age
ggplot(mean_rating_age, aes(x = movie_age, y = mean_rating)) +
  geom_point(color = "#c4553a", size = 3) + # setting color and size of points
  geom_smooth(method = "loess", color = "#6952b9", method.args = list(span = 0.15, degree = 1)) +
  labs(x = "Movie Age (Years)", y = "Mean Rating", title = "Mean Rating by Movie Age") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # settitle
    axis.text = element_text(size = 10), # set axis text size
    axis.title = element_text(size = 12), # set axis title size
    panel.grid.major = element_line(color = "gray", linetype = "dashed"), #add dashed grid lines
    panel.grid.minor = element_blank() # removing minor grid lines
  )

#Genre counts
n_distinct(edx$genres) # counting distinct genres


# genres distribution
# grouping movies by genre and arranging them in descending order
edx %>%
  group_by(genres) %>%
  summarise(number_movies_genre = n()) %>%
  arrange(desc(number_movies_genre))

# calculating mean rating per genre and populating by descending order
edx %>%
  group_by(genres) %>%
  summarize(mean_rating_by_genre = mean(rating)) %>%
  arrange(desc(mean_rating_by_genre))

# Plotting mean rating by genre with error bars
edx %>%
  group_by(genres) %>%
  summarize(
    n = n(),
    avgerge_rating = mean(rating),
    se = sd(rating) / sqrt(n())
  ) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avgerge_rating)) %>%
  ggplot(aes(
    x = genres, y = avgerge_rating,
    ymin = avgerge_rating - 2 * se, ymax = avgerge_rating + 2 * se
  )) +
  geom_point(color = "#c4553a", size = 3) + #setting color and size of points
  geom_errorbar(color = "black", width = .5) + #setting error bar appearance
  ggtitle("Plot of Average Ratings Based on Genres") +
  labs(x = "Genres", y = "Average Rating") + #adding axis labels
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), #rotate x axis
    axis.text.y = element_text(size = 10), # set y axis
    axis.title = element_text(size = 12), # set axis title
    plot.title = element_text(hjust = 0.5), # set plot title
    panel.grid.major = element_line(color = "gray", linetype = "dashed"), #add dashed grid lines
    panel.grid.minor = element_blank() # removes minor grid lines
  )


#####################################################
#Models
#####################################################

# create train and test partitions from EDX
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index, ]
temp <- edx[test_index, ]

# make sure userId and movieId in test set are also in train set
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# remove temporary files to tidy environment
rm(test_index, temp, removed)


## calacualting RMSE and mu_hat ##

RMSE <- function(true_ratings, predicted_ratings) {
  # calculate the squared differences between true and predicted ratings
  squared_errors <- (true_ratings - predicted_ratings)^2

  # calculate the mean of squared errors, taking care to handle NA values if present
  mean_squared_error <- mean(squared_errors, na.rm = TRUE)

  # return the square root of the mean squared error, which is the RMSE
  return(sqrt(mean_squared_error))
}

mu_hat <- mean(train_set$rating) # Calculate the mean rating of the training set
mu_hat


 
## Model 1: Naive Mean ##

M1_NRMSE <- RMSE(test_set$rating, mu_hat) # calculate NRMSE for Model 1,
#where predictions are based solely on the mean rating.

# create tibble to store results
results_table <- tibble(
  Model_Type = c("Model 1:NRMSE"),
  RMSE = c(M1_NRMSE)
) %>%
  mutate(RMSE = sprintf("%0.4f", RMSE)) # format decimal to 4 places

results_table


##Model 2: Movie Effect Bias##

bi <- train_set %>%
  group_by(movieId) %>% # group the data by movieId
  summarize(b_i = mean(rating - mu_hat)) # calculate the mean of (rating - mu_hat) for each movieId

bi %>%
  ggplot(aes(b_i)) + # plotting by b_i
  geom_histogram(color = "black", fill = "#6952b9") +
  xlab("Movie Bias") +
  ylab("Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # setting the title
    axis.text = element_text(size = 10), # setting text size
    axis.title = element_text(size = 12), # setting axis title size
    panel.grid.major = element_line(color = "gray", linetype = "dashed"), # adding dashed grid lines
    panel.grid.minor = element_blank() # removing minor grid lines
  )


prediction_bi <- mu_hat + test_set %>% # start with mu_hat and the test set data
  left_join(bi, by = "movieId") %>% # join with movie bias data
  .$b_i # extract the bias term 'b_i' as a vector of predicted ratings

# calculate the RMSE for Model 2
M2 <- RMSE(test_set$rating, prediction_bi)

# add results to table
results_table <- results_table %>%
  add_row(Model_Type = "Model 2: Mean & Movie Effects", RMSE = sprintf("%0.4f", M2))

results_table



## Model 3: User Effect Bias ##

bu <- train_set %>%
  left_join(bi, by = "movieId") %>% # join with movie bias data
  group_by(userId) %>% # group the data by userId
  summarize(b_u = mean(rating - mu_hat - b_i)) # calculate the mean of
#(rating - mu_hat - b_i) for each userId

predicted_ratings <- test_set %>%
  left_join(bi, by = "movieId") %>% # join with movie bias data
  left_join(bu, by = "userId") %>% # Join with user bias data
  mutate(pred = mu_hat + b_i + b_u) %>% # calculate the predicted ratings 
  #incorporating both movie and user effects
  pull(pred) # extract the predicted ratings as a vector

# Calculate the RMSE for Model 3
M3 <- RMSE(predicted_ratings, test_set$rating)

# Add results to table
results_table <- results_table %>% 
  add_row(Model_Type = "Model 3: Mean, Movie, & User Effects", RMSE = sprintf("%0.4f", M3))

results_table



## Model 4: Movie Age Bias ##

ba <- train_set %>%
  left_join(bi, by = "movieId") %>% # join with movie bias data
  left_join(bu, by = "userId") %>% # join with user bias data
  group_by(movie_age) %>% # group the data by movie_age
  summarize(b_a = mean(rating - b_i - b_u - mu_hat)) # calculate the mean of
#(rating - b_i - b_u - mu_hat) for each movie age group

# plotting movie_age distribution based on b_a
ba %>%
  ggplot(aes(x = b_a)) + # setting x axid to b_a
  geom_histogram(fill = "#6952b9", color = "black", bins = 35) + # setting fill color
  ggtitle("Movie Age Distribution") +
  labs(x = "Movie Age", y = "Count") + # add axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # set title
    axis.text = element_text(size = 10), # set text size
    axis.title = element_text(size = 12), # set axis title size
    panel.grid.major = element_line(color = "gray", linetype = "dashed"), # adding dashed grid lines
    panel.grid.minor = element_blank() # removing minor grid lines
  )

predictions_ma <- test_set %>%
  left_join(bi, by = "movieId") %>% # join with the movie bias data
  left_join(bu, by = "userId") %>% # join with user bias data
  left_join(ba, by = "movie_age") %>% # join with age bias data
  mutate(predictions = mu_hat + b_i + b_u + b_a) %>% # calculate the predicted ratings incorporating movie, user, and age effects
  .$predictions # extract the predicted ratings as a vector

# calculate the RMSE
M4 <- RMSE(test_set$rating, predictions_ma)

# add results to table
results_table <- results_table %>% 
  add_row(Model_Type = "Model 4: Mean, Movie, User, & Movie Age Effects", RMSE = sprintf("%0.4f", M4))

results_table



## Model 5: Genre Bias ##

bg <- train_set %>%
  left_join(bi, by = "movieId") %>% # join with movie bias data
  left_join(bu, by = "userId") %>% # join with user bias data
  left_join(ba, by = "movie_age") %>% # join with age bias data
  group_by(genres) %>% # Group the data by genres.
  summarize(b_g = mean(rating - b_i - b_u - b_a - mu_hat)) # calculate the mean of
#(rating - b_i - b_u - b_a - mu_hat) for each genre

predictions_mg <- test_set %>%
  left_join(bi, by = "movieId") %>% #  join with movie bias data
  left_join(bu, by = "userId") %>% # join with user bias data
  left_join(ba, by = "movie_age") %>% # join with age bias data
  left_join(bg, by = "genres") %>% # join with genre bias data
  mutate(predictions = mu_hat + b_i + b_u + b_a + b_g) %>% # calculate the predicted ratings 
  #incorporating movie, user, age, and genre effects
  .$predictions # extract the predicted ratings as a vector

# calculate the RMSE
M5 <- RMSE(test_set$rating, predictions_mg)

# add results to table
results_table <- results_table %>% 
  add_row(Model_Type = "Model 5: Mean, Movie, User, Movie Age, & Genre Effects", RMSE = sprintf("%0.4f", M5))

results_table

#####################################################
## Final Results ##
#####################################################

final_holdout_test <- cleaning_data(final_holdout_test, timestamp, title) # applying cleaning function to final_holdout_set

#applying model 4 to final_holdout_set
ba_final_test <- final_holdout_test %>%
  left_join(bi, by = "movieId") %>% # join with movie bias data
  left_join(bu, by = "userId") %>% # join with the user bias data
  group_by(movie_age) %>% # group the data by movie_age
  summarize(b_a = mean(rating - b_i - b_u - mu_hat)) # calculate the mean of
#(rating - b_i - b_u - mu_hat) for each movie age group.

predictions_final <- final_holdout_test %>%
  left_join(bi, by = "movieId") %>% # join with movie bias data
  left_join(bu, by = "userId") %>% # join with user bias data
  left_join(ba_final_test, by = "movie_age") %>% # join with age bias data
  mutate(predictions = mu_hat + b_i + b_u + b_a) %>% # calculate the predicted ratings 
  #incorporating movie, user, and age effects.
  .$predictions # extract the predicted ratings as a vector

# Calculate Final RMSE
final_RMSE <- RMSE(final_holdout_test$rating, predictions_final)

# Print Final RMSE
final_RMSE