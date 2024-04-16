
# install packages
if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
if (!require(knitr)) install.packages("knitr")
library(knitr)
if (!require(kableExtra)) install.packages("kableExtra")
library(kableExtra)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(stringr)) install.packages("stringr")
library(stringr)
if (!require(pander)) install.packages("pander")
library(pander)
if (!require(randomForest)) install.packages("randomForest")
library(randomForest)
if (!require(rpart.plot)) install.packages("rpart.plot ")
library(rpart.plot)
if (!require(rpart)) install.packages("rpart")
library(rpart)
if (!require(tibble)) install.packages("tibble")
library(tibble)
if (!require(flextable)) install.packages("flextable")
library(flextable)


### Loading Data

data_url <- "https://raw.githubusercontent.com/anreinhart/edx-capstone/main/NPHA-doctor-visits.csv"
NPHA_Original <- read.csv(data_url)



key_url <- "https://raw.githubusercontent.com/anreinhart/edx-capstone/main/NPHA_Data_Info%20-%20Sheet1.csv"
NPHA_key <- read.csv(key_url)


# Cleaning Data
NPHA_Original <- NPHA_Original %>%
  rename(Physical.Health = Phyiscal.Health)

str(NPHA_Original)


### Creating Data Frame with Response Values
# function to replace numeric values with descriptions
replace_with_descriptions <- function(df, key_df) {
  for (col_name in colnames(df)) { # iterating over each column
    var_name <- col_name
    mapping <- key_df$Units[key_df$Variable.Name == var_name]  # find matching units from 'key_df' based on 'var_name'
    if (length(mapping) > 0) { # proceed if there are matches in 'key_df'
      unit_str <- key_df$Units[key_df$Variable.Name == var_name] # extract the unit string from 'key_df'
      unit_map <- strsplit(gsub("[{}]", "", unit_str), ", ")[[1]] #spliting the unit string into key-value pairs and create a named vector ('unit_map')
  # creating vector where keys are numeric values and values are corresponding labels
      unit_map <- setNames(
        sapply(unit_map, function(x) {
          key_val <- strsplit(x, ": ")[[1]] # splitting key-value pair
          as.character(key_val[2]) #extracting label (value) and convert to character
        }),
        sapply(unit_map, function(x) {
          key_val <- strsplit(x, ": ")[[1]]# extracting numeric key and convert to numeric type
          as.numeric(key_val[1])
        })
      )
      # Replace column values in 'df' with labels from 'unit_map'
      df[[col_name]] <- factor(df[[col_name]], levels = names(unit_map), labels = unname(unit_map))
    }
  }
  return(df)
}

# applying function to create new data frame
NPHA_transformed <- replace_with_descriptions(NPHA_Original, NPHA_key)

str(NPHA_transformed)

NPHA_transformed %>%
  select(Dental.Health, Trouble.Sleeping) %>%
  slice_head(n = 8)



### Handling NA Values
# summing NAs for Dental.Health
sum(is.na(NPHA_transformed$Dental.Health))

#summing 6s in Dental.Health}
sum(NPHA_Original$Dental.Health == 6, na.rm = TRUE)

#Summing NAs for Trouble.Sleeping
sum(is.na(NPHA_transformed$Trouble.Sleeping))


# checking number of NAs}
sum(is.na(NPHA_transformed))

# Function to remove NAs, 6s, and Trouble.Sleeping column}
remove_column_and_na_rows <- function(df, column_to_remove) {
  # check if the column_to_remove exists in the data frame
  if (column_to_remove %in% colnames(df)) {
    # remove the specified column
    df <- df[, !colnames(df) %in% column_to_remove]
  } else {
    cat(paste("Column", column_to_remove, "does not exist in the dataframe.\n"))
    return(NULL)
  }
  
  # filter out rows with NA values across remaining columns
  df <- df[complete.cases(df), ]
  
  # filter out rows where Dental.Health is 6
  df <- df[df$Dental.Health != 6, ]
  
  return(df)
}

#applying function to transformed dfs
NPHA_transformed <- remove_column_and_na_rows(NPHA_transformed, "Trouble.Sleeping") 

NPHA_Original <- remove_column_and_na_rows(NPHA_Original, "Trouble.Sleeping") 


# checking na removal}
sum(is.na(NPHA_transformed))

# Exploratory Data Analysis 
### _Doctor Visits_
table(NPHA_transformed$Number.of.Doctors.Visited)

### _Age & Doctor Visits_
# create the basic bar chart
ggplot(NPHA_transformed, aes(x = factor(Number.of.Doctors.Visited), fill = Age)) +
  
  # add bars with dodged position and outline
  geom_bar(position = "dodge", color = NA, alpha = 0.8, width = 0.7) +
  
  # add text labels on top of bars (showing counts)
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5, # adjust vertical position of labels
    size = 3, # label size
    color = "black", # text color
    fontface = "bold" # label text style
  ) +
  
  # set fill colors for each Age group
  scale_fill_manual(
    values = c("50-64" = "#4CAF50", "65-80" = "#8C67B5"),
    name = "Age Group" # Customize legend title
  ) +
  labs(
    x = "Number of Doctor Visits",
    y = "Count",
    title = "Doctor Visits by Age Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10)
  )

# removing age column, as there is only one age group
NPHA_Original <- subset(NPHA_Original, select = -Age)
NPHA_transformed <- subset(NPHA_transformed, select = -Age)


### _Gender & Race_
NPHA_transformed %>%
  ggplot(aes(x = factor(Gender, levels = c("Male", "Female")), fill = Gender)) +
  # using factor to specify the order of 'Gender' levels in x-axis
  geom_bar() +
  # creating a bar plot
  scale_fill_manual(values = c("Male" = "#8dd3c7", "Female" = "#fb8072")) +
  # fill colors
  labs(x = "Gender", y = "Count", title = "Gender Distribution") +
  # Label x-axis, y-axis, and plot title
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4, color = "white") +
  # adding count labels above bars, adjust text position and size
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none")



table(NPHA_transformed$Race, NPHA_transformed$Gender)

# creating custome color pallet for race categories
race_palette <- c(
  "#7BA3D0", "#F6AE2D", "#6AB187", "#F26419", "#A08BA5", "#FF7F00",
  "#4B4E6D"
)
# plotting doctor visits by race and gender with tilted x-axis labels
ggplot(NPHA_transformed, aes(x = Number.of.Doctors.Visited, fill = Race)) +
  # add bar chart with dodged bars for each Race category
  geom_bar(position = "dodge", color = NA, size = 0.2) +
  scale_fill_manual(values = race_palette) +
  # facet by Gender to create separate plots for each gender
  facet_wrap(~Gender, scales = "free") +
  labs(
    title = "Doctor Visits by Race and Gender",
    x = "Number of Doctors Visited",
    y = "Count"
  ) +
  # Customize axis and legend labels, and tilt x-axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + # Rotate x-axis labels
  # Remove legend title and customize legend appearance
  guides(fill = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)) +
  
  # Adjust facet labels
  theme(
    strip.background = element_blank(),
    strip.placement = "outside"
  )

### _Average Ratings_
avg_responses <- colMeans(NPHA_Original[3:5])
# obtaining means by col
print(avg_responses)



### _Mental Health_
avg_mental_health <- aggregate(Mental.Health ~ Race + Number.of.Doctors.Visited, data = NPHA_Original, FUN = mean)
# calculating mean of mental.health var grouped by race and number of doctor visits

#creating labels for plot
doctor_labels <- c("0-1 Doctors", "2-3 Doctors", "4 or More Doctors")
race_labels <- c("White Non-Hispanic", "Black Non-Hispanic", "Other Non-Hispanic", "Hispanic", "2+ Races Non-Hispanic")

# plotting tiled heatmap
ggplot(avg_mental_health, aes(x = factor(Race, levels = 1:5, labels = race_labels),# defining x-axis w/ custom labels
                              y = factor(Number.of.Doctors.Visited, levels = 1:3, labels = doctor_labels),  #defining y-axis w/ custom labels
                              fill = Mental.Health)) + #fill color base on avg mental.health value
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") + # color gradient
  labs(x = "Race", y = "Number of Doctors Visited", fill = "Average Mental Health Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #rotatin axis to fit



### _Physical Health_


physhealth_results <- table(NPHA_transformed$Physical.Health)
physhealth_results

percentage_physhealth <- prop.table(physhealth_results) * 100
# combine categories and percentages into a data frame
results_df <- data.frame(
  Response_Category = names(physhealth_results),
  Count = as.numeric(physhealth_results), # convert counts to numeric
  Percentage = percentage_physhealth
)

print(percentage_physhealth)


avg_physical_health <- aggregate(Physical.Health ~ Race + Number.of.Doctors.Visited, data = NPHA_Original, FUN = mean)
# calculating mean of physical.health var grouped by race and number of doctor visits

# Plot heatmap
ggplot(avg_physical_health, aes(x = factor(Race, levels = 1:5, labels = race_labels), # defining x axis w/custom labels
                                y = factor(Number.of.Doctors.Visited, levels = 1:3, labels = doctor_labels), # defining y axis w/custom labels
                                fill = Physical.Health)) + #setting fill to physical.health value
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") + # color gradient
  labs(x = "Race", y = "Number of Doctors Visited", fill = "Average Physical Health Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #tilting axis to fit

### _Dental Health_
ggplot(NPHA_transformed, aes(x = factor(Dental.Health), fill = Race)) +
  
  # plot frequencies
  geom_bar(position = "dodge", stat = "count", width = 0.7) +
  scale_fill_manual(values = race_palette) +
  
  # plot titles and axis labels
  labs(
    title = "Dental Health Rating Distribution",
    x = "Rating",
    y = "Count of Responses",
    fill = "Race"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  guides(fill = guide_legend(reverse = TRUE)) + # reverse the order of legend keys
  coord_flip() # flip coordinates for horizontal bar plot

### _Employment_ 
table(NPHA_transformed$Employment)


#calcualting retired %
Retiredpercent <- NPHA_transformed %>%
  filter(NPHA_transformed$Employment == "Retired") #filtering table to just retired 

(nrow(Retiredpercent) / length(NPHA_transformed$Employment)) * 100


#grouping 'NPHA_Original' by recoded 'Employment' categories and calculate averages
avg_health_by_employment <- NPHA_Original %>%
  group_by(Employment_label = recode(Employment,
    "1" = "Woirking full-time",
    "2" = "Working part-time",
    "3" = "Retired",
    "4" = "Not working at this time"
  )) %>%
  # grouping by label and for visual
  summarise(
    avg_phys_health = mean(Physical.Health, na.rm = TRUE),
    avg_ment_health = mean(Mental.Health, na.rm = TRUE),
    avg_dent_health = mean(Dental.Health, na.rm = TRUE)
  )


kable(avg_health_by_employment)

avg_health_by_employment_visits <- NPHA_Original %>%
  mutate(Employment_label = case_when(
    Employment == 1 ~ "Working full-time",
    Employment == 2 ~ "Working part-time",
    Employment == 3 ~ "Retired",
    Employment == 4 ~ "Not working at this time"
  )) %>%
  # grouping by label and recoding for visual
  group_by(Employment_label, Number.of.Doctors.Visited) %>%
  summarise(
    avg_phys_health = mean(Physical.Health, na.rm = TRUE),
    avg_ment_health = mean(Mental.Health, na.rm = TRUE),
    avg_dent_health = mean(Dental.Health, na.rm = TRUE)
  ) %>%
  ungroup() %>% # removing grouping for further operations
  mutate(Number.of.Doctors.Visited = recode(Number.of.Doctors.Visited, !!!setNames(doctor_labels, 1:3)))


kable(avg_health_by_employment_visits)


### _Sleep_
NPHAtrans_sleep_subset <- NPHA_transformed[, c("Stress.Keeps.Patient.from.Sleeping", "Medication.Keeps.Patient.from.Sleeping", "Pain.Keeps.Patient.from.Sleeping", "Uknown.Keeps.Patient.from.Sleeping", "Bathroom.Needs.Keeps.Patient.from.Sleeping")]
# subetting for visualizaition

# reshape data into long format
NPHASleep_long <- pivot_longer(NPHAtrans_sleep_subset,
  cols = everything(),
  names_to = "Variable",
  values_to = "Response"
)

# coounts of yes * no
sleepcounts <- table(NPHASleep_long$Variable, NPHASleep_long$Response)
sleepcounts <- data.frame(sleepcounts)


# creating custome labels for x axis
sleep_labels <- c(
  "Stress.Keeps.Patient.from.Sleeping" = "Stress",
  "Medication.Keeps.Patient.from.Sleeping" = "Medication",
  "Pain.Keeps.Patient.from.Sleeping" = "Pain",
  "Uknown.Keeps.Patient.from.Sleeping" = "Unknown",
  "Bathroom.Needs.Keeps.Patient.from.Sleeping" = "Bathroom Needs"
)

# color palette
custom_colors <- c("Yes" = "#5E9FDF", "No " = "#E6614F")


# grouped bar chart
ggplot(sleepcounts, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Counts of Yes/No by Variable Keep Patient From Sleeping",
    x = "Variables", y = "Count"
  ) +
   #customize the fill (color) scale 
  scale_fill_manual(
    values = custom_colors, name = "Response",
    labels = c("Yes" = "Yes", "No " = "No")
  ) +
  # customizing x-axis labels using predefined sleep_labels
  scale_x_discrete(labels = sleep_labels) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.key.size = unit(1, "cm")
  ) +
  guides(fill = guide_legend(reverse = TRUE)) # reverse legend order


#prescription meds
table(NPHA_transformed$Prescription.Sleep.Medication)


# Methods and Analysis

## _Correlation_


# Calculate Cramér's V for categorical variables
cramer_v <- function(x, y) {
  tab <- table(x, y)
  chisq <- chisq.test(tab)$statistic
  n <- sum(tab)
  df <- (nrow(tab) - 1) * (ncol(tab) - 1) # degrees of freedom calculation
  
  # Cramer's V with NaN values excluded
  if (is.nan(chisq)) {
    return(NA) # return NA if chisq is NaN
  } else {
    return(sqrt(chisq / (n * df)))
  }
}

# correlations with target variable
correlations <- sapply(NPHA_transformed[-1], function(x) {
  cramer <- cramer_v(NPHA_transformed$Number.of.Doctors.Visited, x)
  ifelse(is.na(cramer), 0, cramer) # replace NaN with 0
})


kable(correlations)


## _Creating Partitions_
set.seed(123) # set seed for reproducibility

index_original <- createDataPartition(NPHA_Original$Number.of.Doctors.Visited, p = 0.7, list = FALSE)

train_original <- NPHA_Original[index_original, ] # 70% of the data for training
test_original <- NPHA_Original[-index_original, ] # Remaining 30% of the data for testing



## Model Seleciton
### _Random Forest_ 

#Turning target to factors
train_original$Number.of.Doctors.Visited <- factor(train_original$Number.of.Doctors.Visited)
test_original$Number.of.Doctors.Visited <- factor(test_original$Number.of.Doctors.Visited)


# function for randomforest
fit_random_forest <- function(train_data, test_data, target_variable, to_exclude = NULL, ntree = 50000) {
  if (!is.null(to_exclude)) {
    x_train <- train_data[, -to_exclude]
    x_test <- test_data[, -to_exclude]
  } else {
    x_train <- train_data
    x_test <- test_data
  }
  
  y_train <- train_data[[target_variable]]
  
  # fitting RF
  set.seed(120) # Setting seed
  classifier_RF <- randomForest(x = x_train, y = y_train, ntree = ntree)
  
  # predicting on test set
  y_pred <- predict(classifier_RF, newdata = x_test)
  
  # confusion matrix
  confusion_mtx <- table(test_data[[target_variable]], y_pred)
  
  # calculate accuracy using confusion matrix
  accuracy <- sum(diag(confusion_mtx)) / sum(confusion_mtx)
  
  return(list(
    model = classifier_RF,
    confusion_matrix = confusion_mtx,
    accuracy = accuracy
  ))
}



to_exclude <- c(1) # index of the target variable to exclude

# calling RF function
result <- fit_random_forest(
  train_data = train_original,
  test_data = test_original,
  target_variable = "Number.of.Doctors.Visited",
  to_exclude = to_exclude
)

# results
result$confusion_matrix # The confusion matrix on actual predictions
result$accuracy # The accuracy of the model



# create an empty tibble for model results
model_results <- tibble(
  Model = character(),
  Accuracy = numeric()
)

# updating model_results tibble
model_results <- bind_rows(model_results, tibble(
  Model = "Random Forest",
  Accuracy = result$accuracy
))

kable(model_results)


# importance plot
importance(result$model)

# variable importance plot
varImpPlot(result$model)

# extracting feature importance scores
feature_importance <- importance(result$model)
# extraxting names of important features based on MeanDecreaseGini values in descending order
important_features <- names(sort(feature_importance[, "MeanDecreaseGini"], decreasing = TRUE))

# selecting top n important features
n <- 5
selected_features <- important_features[1:n]


# subset the training and test data with selected features
train_data_selected <- train_original[, c("Number.of.Doctors.Visited", selected_features)]

test_data_selected <- test_original[, c("Number.of.Doctors.Visited", selected_features)]


# call RF function
result_features <- fit_random_forest(
  train_data = train_data_selected,
  test_data = test_data_selected,
  target_variable = "Number.of.Doctors.Visited",
  to_exclude = to_exclude
)

# results
result_features$model # The trained Random Forest model
result_features$confusion_matrix # The confusion matrix
result_features$accuracy # The accuracy of the model




# updating model_results tibble
model_results <- model_results %>%
  add_row(Model = "Random Forest w/Feature Selection", Accuracy = result_features$accuracy)

kable(model_results)



### _KNN_

# creating function for KNN
train_and_evaluate_knn <- function(train_data, test_data, target_variable, k_values = 15:25,
                                   cv_folds = 10, cv_repeats = 3) {
  # preparing features (X) and target variable (Y) from training dataset
  X <- train_data[, !names(train_data) %in% target_variable] # features excluding target variable
  Y <- train_data[[target_variable]] # tagret variable
  
  # Train KNN model
  set.seed(123) # set seed for reproducibility
  knn_model <- train(X, Y,
                     method = "knn", 
                     trControl = trainControl( #training control for model tuning
                       method = "repeatedcv", # use repeated k-fold cross-validation for model evaluation
                       number = cv_folds, # folds
                       repeats = cv_repeats #times to repeat CV
                     ),
                     tuneGrid = expand.grid(k = k_values) #specify grid k-values to tune
  )
  
  
  # print the best tuned parameter (optimal k)
  knn_best_k <- knn_model$bestTune
  
  # predict on test data
  predictions <- predict(knn_model, newdata = test_data[, !names(test_data) %in% target_variable])
  
  #  model performance
  accuracy <- confusionMatrix(predictions, test_data[[target_variable]])$overall["Accuracy"]
  print(paste("Accuracy with KNN:", accuracy))
  
  # confusion matrix
  conf_matrix <- confusionMatrix(predictions, test_data[[target_variable]])
  
  # return the trained model and evaluation results
  return(list(model = knn_model, accuracy = accuracy, confusion_matrix = conf_matrix))
}




# setting the target variable name
target_variable <- "Number.of.Doctors.Visited"

# calling KNN function
result_knn <- train_and_evaluate_knn(
  train_data = train_original,
  test_data = test_original,
  target_variable = target_variable,
  k_values = 15:25,
  cv_folds = 10,
  cv_repeats = 3
)


result_knn$model # trained KNN model
plot(result_knn$model)



result_knn$confusion_matrix # confusion matrix of predictions
result_knn$accuracy # accuracy of the KNN model


# Updating model_results tibble
model_results <- model_results %>%
  add_row(Model = "KNN", Accuracy = result_knn$accuracy)

kable(model_results)


# create manual balanced sampling
class_levels <- levels(train_original$Number.of.Doctors.Visited)
# choosing the minimum class size for balanced sampli
sample_size <- min(table(train_original$Number.of.Doctors.Visited)) # choosing the minimum class size for balanced sampling

#create empty df top store data
balanced_data <- NULL
#loop thru each class level in class_levels
for (level in class_levels) {
  #subset original data for class level
  level_data <- train_original[train_original$Number.of.Doctors.Visited == level, , drop = FALSE]
  #randomly sampling rows from subet data & combining 
  balanced_data <- rbind(balanced_data, level_data[sample(1:nrow(level_data), sample_size), ])
}

# shuffle the balanced data
balanced_data <- balanced_data[sample(nrow(balanced_data)), ]

# confirm new distribution
table(balanced_data$Number.of.Doctors.Visited)


### _Balanced Weights: Random Forest_


to_exclude <- c(1) # index of the target variable to exclude

# calling function
result_RF_balanced <- fit_random_forest(
  train_data = balanced_data,
  test_data = test_original,
  target_variable = "Number.of.Doctors.Visited",
  to_exclude = to_exclude
)

# results
result_RF_balanced$model # trained Random Forest model
result_RF_balanced$confusion_matrix # confusion matrix
result_RF_balanced$accuracy # accuracy of the model


# importance plot
importance(result_RF_balanced$model)

# variable importance plot
varImpPlot(result_RF_balanced$model)


# Updating the model_results tibble to add a new column
model_results <- model_results %>%
  mutate(`Accuracy w/Balanced Weights` = NA_real_) # Initialize a new column with NA

# assign  accuracy value for "Random Forest" rows in the new column
model_results$`Accuracy w/Balanced Weights`[model_results$Model == "Random Forest"] <- result_RF_balanced$accuracy

# Print
kable(model_results)



### _Balanced Weights: Random Forest w/Feature Selection_
# extracting feature importance scores
feature_importance <- importance(result_RF_balanced$model)

important_features <- names(sort(feature_importance[, "MeanDecreaseGini"], decreasing = TRUE))

# selecting top n important features
n <- 5
selected_features <- important_features[1:n]


# subset the training and test data with selected features
train_data_selected <- balanced_data[, c("Number.of.Doctors.Visited", selected_features)]

test_data_selected <- test_original[, c("Number.of.Doctors.Visited", selected_features)]



# calling function
result_features_balanced <- fit_random_forest(
  train_data = train_data_selected,
  test_data = test_data_selected,
  target_variable = "Number.of.Doctors.Visited",
  to_exclude = to_exclude
)

# results
result_features_balanced$model # trained Random Forest model
result_features_balanced$confusion_matrix #  confusion matrix
result_features_balanced$accuracy # accuracy of the model


# add the second balanced weight value for the "Random Forest" model
# assign the accuracy value for "Random Forest" rows in the new column
model_results$'Accuracy w/Balanced Weights'[model_results$Model == "Random Forest w/Feature Selection"] <- result_features_balanced$accuracy

# Print updated model_results tibble
print(model_results)

### _Balanced Weights: KNN_


# setting target variable
target_variable <- "Number.of.Doctors.Visited"

# calling function
result_knn_balanced <- train_and_evaluate_knn(
  train_data = balanced_data,
  test_data = test_original,
  target_variable = target_variable,
  k_values = 15:25,
  cv_folds = 10,
  cv_repeats = 3
)

# results
result_knn_balanced$model # trained KNN model
result_knn_balanced$accuracy # accuracy of the KNN model
result_knn_balanced$confusion_matrix # confusion matrix of prediction

plot(result_knn_balanced$model)

# adding the balanced weight for KNN
# assign the accuracy value for "KNN" rows in the new column
model_results$'Accuracy w/Balanced Weights'[model_results$Model == "KNN"] <- result_knn_balanced$accuracy

# Print
kable(model_results)

