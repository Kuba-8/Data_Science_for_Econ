setwd("/Users/jakubfridrich/Desktop/EC349 Assignment")
getwd()


library(tidyverse)
library(tibble)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(glmnet)
library(lubridate)
library(text)
library(SentimentAnalysis)
library(tidytext)
library(ggcorrplot)
library(hexbin)

yelp_review_data <- load("/Users/jakubfridrich/Desktop/EC349 Assignment/yelp_review_small.Rda")
yelp_user_data <- load("/Users/jakubfridrich/Desktop/EC349 Assignment/yelp_user_small.Rda")

yelp_business_data <- stream_in(file("/Users/jakubfridrich/Desktop/EC349 Assignment/yelp_academic_dataset_business.json"))
yelp_checkin_data  <- stream_in(file("/Users/jakubfridrich/Desktop/EC349 Assignment/yelp_academic_dataset_checkin.json")) 
yelp_tip_data  <- stream_in(file("/Users/jakubfridrich/Desktop/EC349 Assignment/yelp_academic_dataset_tip.json")) 

print(yelp_review_data)
print(yelp_user_data)

class(review_data_small)
class(user_data_small)

review_data_small_tibble <- as_tibble(review_data_small)
user_data_small_tibble <- as_tibble(user_data_small)

colnames(user_data_small_tibble)
colnames(review_data_small_tibble)

head(yelp_business_data)
head(yelp_checkin_data)
head(yelp_tip_data)

business_data_tibble <- as_tibble(yelp_business_data)
checkin_data_tibble <- as_tibble(yelp_checkin_data)
tip_data_tibble <- as_tibble(yelp_tip_data)

colnames(business_data_tibble)
colnames(checkin_data_tibble)
colnames(tip_data_tibble)

glimpse(user_data_small_tibble)
glimpse(review_data_small_tibble)
glimpse(business_data_tibble)
glimpse(checkin_data_tibble)
glimpse(tip_data_tibble)


# Merging User and Review Data

merged_review_and_user_tibble <- full_join(review_data_small_tibble, user_data_small_tibble, by ="user_id")

head(merged_review_and_user_tibble)

# Filtering out reviews without information on their associated users 

missings_in_columns <- colSums(is.na(merged_review_and_user_tibble))
print(missings_in_columns)

filtered_review_and_user_tibble <- merged_review_and_user_tibble %>%
  filter(!is.na(name))

head(filtered_review_and_user_tibble)
print(filtered_review_and_user_tibble)

# Merging with business data

merged_business_review_and_user_tibble <- full_join(filtered_review_and_user_tibble, business_data_tibble, by ="business_id")

head(merged_business_review_and_user_tibble)
print(merged_business_review_and_user_tibble)

# Preparing and merging the relevant tips data

unique_tip_counts <- tip_data_tibble %>%
  count(business_id, name = "tip_count") %>%
  distinct()

print(unique_tip_counts)
summary(unique_tip_counts)

merged_tip_business_review_and_user_tibble <- left_join(merged_business_review_and_user_tibble, unique_tip_counts, by = "business_id")

print(merged_tip_business_review_and_user_tibble)

# Preparing and merging the relevant checkin data to get the fully merged tibble

checkin_data_tibble <- checkin_data_tibble %>%
  mutate(checkin_timestamp_count = str_count(date, ",") + 1)

print(checkin_data_tibble)

num_checkin_per_business <- checkin_data_tibble %>%
  select(-date)

print(num_checkin_per_business)
summary(num_checkin_per_business)

final_tibble <- left_join(merged_tip_business_review_and_user_tibble, num_checkin_per_business, by = "business_id")

print(final_tibble)
glimpse(final_tibble)


missings_in_columns <- colSums(is.na(final_tibble))
print(missings_in_columns)

# Removing the missing values for the 'text' variable that contains the main body of the reviews

final_tibble_2 <- final_tibble %>%
  filter(!is.na(text))

head(final_tibble_2)
glimpse(final_tibble_2)

missings_in_columns <- colSums(is.na(final_tibble_2))
print(missings_in_columns)

glimpse(final_tibble_2$attributes)

missings_in_columns <- colSums(is.na(final_tibble_2$attributes))
print(missings_in_columns)

# Investigating the attributes dataframe: RestaurantsPriceRange2

unique_values <- final_tibble_2$attributes %>%
  distinct(RestaurantsPriceRange2)

print(unique_values)

final_tibble_2$attributes <- final_tibble_2$attributes %>%
  mutate(RestaurantsPriceRange2 = factor(RestaurantsPriceRange2, levels = c("1", "2", "3", "4")))

glimpse(final_tibble_2$attributes$RestaurantsPriceRange2)
summary(final_tibble_2$attributes$RestaurantsPriceRange2)

# Graphical Analysis 

ggplot(final_tibble_2, aes(x = attributes$RestaurantsPriceRange2, y = stars.x)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Boxplot for the Variation in Stars per review with RestaurantsPriceRange2",
       x = "RestaurantsPriceRange2",
       y = "Stars") +
  theme_minimal()


ggplot(final_tibble_2, aes(x = attributes$RestaurantsPriceRange2, y = stars.x, fill = attributes$RestaurantsPriceRange2)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Violoinplot for the Variation in Stars per review with RestaurantsPriceRange2",
       x = "RestaurantsPriceRange2",
       y = "Stars") +
  theme_minimal()

# Clear distributional differences are present

# Mathemtically

model_ranges <- lm(stars.x ~ attributes$RestaurantsPriceRange2, data = final_tibble_2)

summary(model_ranges)

# Small, though statistically signficant relationship for all coefficients

# Distribution of stars.x with and without missing values being deleted


final_tibble_3 <- final_tibble_2 %>%
  filter(!is.na(attributes$RestaurantsPriceRange2))

missings_in_columns <- colSums(is.na(final_tibble_3))
print(missings_in_columns)

# Underlying distribution remains unchanged:

histogram1 <- ggplot(final_tibble_2, aes(x = stars.x)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram for Stars per review in final_tibble_2")

histogram2 <- ggplot(final_tibble_3, aes(x = stars.x)) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.7) +
  labs(title = "Histogram for Stars per review in final_tibble_3")

print(histogram1)

print(histogram2)

# Difference in means test for stars.x over the two samples

t_test_result <- t.test(final_tibble_2$stars.x, final_tibble_3$stars.x)

print(t_test_result)

# sample mean difference is siginficant, though very small at just around 0.02 of a star


# Now, taking a similar look at another potentially suitable variable: BusinessAcceptsCreditCards

final_tibble_3$attributes <- final_tibble_3$attributes %>%
  mutate(BusinessAcceptsCreditCards = as.logical(BusinessAcceptsCreditCards))

glimpse(final_tibble_3$attributes$BusinessAcceptsCreditCards)
summary(final_tibble_3$attributes$BusinessAcceptsCreditCards)

# low number of missing values, however very little variation

final_tibble_3$attributes <- final_tibble_3$attributes %>%
  select(-BusinessAcceptsCreditCards)

# Looking further at categories:

final_tibble_and_categories <- final_tibble_3 %>%
  separate_rows(categories, sep = ", ") %>%
  mutate(categories = trimws(categories)) %>%
  filter(categories != "") %>%
  count(categories) %>%
  arrange(desc(n))


cat("Number of unique categories: ", nrow(final_tibble_and_categories), "\n")
head(final_tibble_and_categories)

# 992 categories is simply too many categories to include in the dataset

ggplot(head(final_tibble_and_categories, 20), aes(x = reorder(categories, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
  coord_flip() +
  labs(title = "Top 20 Most Popular Categories", x = "Categories", y = "Count")

# Approximately 80% of the businesses are categorised as restaurants, with 13 of the top 20 categories being food related. Therefore, they are all broadly the same categroy of business.


# Consequently, the variable is dropped

final_tibble_3 <- final_tibble_3 %>%
  select(-categories)

glimpse(final_tibble_3)

# Looking at another business attribute: WiFi

unique_values <- final_tibble_3$attributes %>%
  distinct(WiFi) %>%
  count(WiFi) %>%
  arrange(desc(n))

print(unique_values)

final_tibble_3$attributes <- final_tibble_3$attributes %>%
  mutate(WiFi = factor(WiFi, levels = c("'free'", "'no'", "'paid'", "None", "u'free'", "u'no'", "u'paid'")))

glimpse(final_tibble_3$attributes$WiFi)
summary(final_tibble_3$attributes$WiFi)

# Graphs to find out whether WiFi is an important variable

ggplot(final_tibble_3, aes(x = attributes$WiFi, y = stars.x)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Boxplot for the variation in Stars per review with WiFi",
       x = "WiFi",
       y = "Stars") +
  theme_minimal()

ggplot(final_tibble_3, aes(x = attributes$WiFi, y = stars.x, fill = attributes$WiFi)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Violin plot for the variation in Stars per review with WiFi",
       x = "WiFi",
       y = "Stars") +
  theme_minimal()

# Clearly no difference in distribution, when taking into account the sample of each column


# Imputing missed values in the two variables constructed from the checkin and tips data with 0's
# as a missing value just means the business_id was not present in the other two datasets

final_tibble_4 <- final_tibble_3 %>%
  mutate(
    tip_count = if_else(is.na(tip_count), 0, tip_count),
    checkin_timestamp_count = if_else(is.na(checkin_timestamp_count), 0, checkin_timestamp_count)
  )

missings_in_columns <- colSums(is.na(final_tibble_4))
print(missings_in_columns)

# Code for dropping the variables that I do not want to use, largely due to a lack of data

final_tibble_4$attributes <- final_tibble_4$attributes %>% 
  select(-ByAppointmentOnly, -CoatCheck, -WheelchairAccessible, -HappyHour, -RestaurantsTakeOut, -RestaurantsDelivery, -Caters, -OutdoorSeating, -HasTV, -RestaurantsReservations, -DogsAllowed, -Alcohol, -GoodForKids, -RestaurantsAttire, -Ambience,
         -RestaurantsTableService, -RestaurantsGoodForGroups, -DriveThru, -NoiseLevel,
         -GoodForMeal, -BusinessAcceptsBitcoin, -Smoking, -Music, -GoodForDancing, -AcceptsInsurance, -BestNights, -BYOB, -Corkage, -BYOBCorkage, -HairSpecializesIn, -Open24Hours, -RestaurantsCounterService, -AgesAllowed, -DietaryRestrictions, -BikeParking, -BusinessParking, -WiFi)

missings_in_columns <- colSums(is.na(final_tibble_4))
print(missings_in_columns)

# The hours sub-dataframe has relatively few missing values, so could be altered using mean imputation
# However, it does not seem relevant enough to individual reviews.

final_tibble_4$hours <- final_tibble_4$hours %>% 
  select(-Monday, -Tuesday, -Wednesday, -Thursday, -Friday, -Saturday, -Sunday)

missings_in_columns <- colSums(is.na(final_tibble_4))
print(missings_in_columns)

# Adjusting variable types

# date (currently character)

final_tibble_5 <- final_tibble_4 %>%
  mutate(date = ymd_hms(date))

glimpse(final_tibble_5)

# yelping_since (currently character)
final_tibble_5 <- final_tibble_5 %>%
  mutate(yelping_since = ymd_hms(yelping_since))

glimpse(final_tibble_5)

# From inspecting the data in the viewer, the variable elite has some blank values not encoded as missing values

glimpse(final_tibble_5)

# Creating a variable for the number of years each user has been an elite user: elite_years_count 

unique_values_elite <- final_tibble_5 %>%
  group_by(elite) %>%
  count(elite) %>%
  arrange(desc(n))

print(unique_values_elite)

final_tibble_6 <- final_tibble_5 %>%
  mutate(elite_count = ifelse(elite == "", 0, str_count(elite, ",") + 1))


unique_values_elite_count <- final_tibble_6 %>%
  group_by(elite_count) %>%
  count(elite_count) %>%
  arrange(desc(n))

print(unique_values_elite_count)

# The original variable can therefore be dropped

final_tibble_6 <- final_tibble_6 %>% 
  select(-elite)

glimpse(final_tibble_6)


# Creating a variable for the number of friends a user has on the platform: the friends_count variable 

final_tibble_7 <- final_tibble_6 %>%
  mutate(users_friend_count = ifelse(friends == "", 0, str_count(friends, ",") + 1))

unique_values_friend_count <- final_tibble_7 %>%
  group_by(users_friend_count) %>%
  count(users_friend_count) %>%
  arrange(desc(n))

print(n = 30, unique_values_friend_count)

# The original variable can therefore be dropped

final_tibble_7 <- final_tibble_7 %>% 
  select(-friends)

glimpse(final_tibble_7)

# Dropping the hours and attributes sub-dataframes, whilst retaining the useful columns

final_tibble_7 <- final_tibble_7 %>% 
  select(-hours)

glimpse(final_tibble_7)

final_tibble_7 <- final_tibble_7 %>%
  mutate(RestaurantsPriceRange2 = attributes$RestaurantsPriceRange2)

final_tibble_7 <- final_tibble_7 %>%
  mutate(RestaurantsPriceRange2 = factor(RestaurantsPriceRange2, levels = c("1", "2", "3", "4")))

glimpse(final_tibble_7)

final_tibble_7 <- final_tibble_7 %>% 
  select(-attributes)

glimpse(final_tibble_7)

# Conducting Sentiment Analysis

tidy_text <- final_tibble_7 %>%
  unnest_tokens(word, text)

bing_lexicon <- get_sentiments("bing")

sentiment_scores <- tidy_text %>%
  inner_join(bing_lexicon, by = c("word" = "word")) %>%
  group_by(review_id, word) %>%
  summarise(sentiment_score = sum(sentiment == "positive") - sum(sentiment == "negative"))

review_sentiment <- sentiment_scores %>%
  group_by(review_id) %>%
  summarise(sentiment_score = sum(sentiment_score))

final_tibble_8 <- final_tibble_7 %>%
  left_join(review_sentiment, by = c("review_id" = "review_id"))

summary(final_tibble_8$sentiment_score)
glimpse(final_tibble_8)

# Recoding RestaurantsPriceRange2 to make it compatible with linear modelling

final_tibble_7 <- cbind(final_tibble_7, model.matrix(~ RestaurantsPriceRange2 - 1, data = final_tibble_7))

# Investigating whether sentiment_score has any missing values

missings_in_columns <- colSums(is.na(final_tibble_8))
print(missings_in_columns)

final_tibble_8 <- final_tibble_8 %>%
  filter(!is.na(sentiment_score))

missings_in_columns <- colSums(is.na(final_tibble_8))
print(missings_in_columns)

glimpse(final_tibble_8)

# Dropping all remaining variables seen as irrelevant or unsuitable for linear modelling to get the final tibble for modelling

final_tibble_LASSO <- final_tibble_8 %>% 
  select(-RestaurantsPriceRange2, -postal_code, -state, -city, -address, -name.y, -name.x, -text, business_id, -user_id, -review_id, -business_id)

glimpse(final_tibble_LASSO)

str(final_tibble_LASSO)

dim(final_tibble_LASSO)

summary(final_tibble_LASSO$stars.x)

missings_in_columns <- colSums(is.na(final_tibble_LASSO))
print(missings_in_columns)


glimpse(final_tibble_LASSO)

# Distribution of Stars given per review

ggplot(final_tibble_LASSO, aes(x = as.factor(stars.x))) +
  geom_bar() +
  labs(title = "Distribution of Stars given per review", x = "stars.x", y = "Count")

# Correlation matrix for variables in the final tibble

numerical_vars <- sapply(final_tibble_LASSO, is.numeric)

cor_matrix <- cor(final_tibble_LASSO[, numerical_vars])

ggcorrplot(cor_matrix,
           type = "full", 
           lab = TRUE,  
           lab_size = 2, 
           method = "square",  
           colors = c("blue", "white", "red"), 
           title = "Correlation Matrix for the final tibble",
           ggtheme = ggplot2::theme_minimal(),
           hc.order = TRUE,    
           show.legend = TRUE)          

# LASSO model

# Test-train split

set.seed(1234)

train_index <- sample(1:nrow(final_tibble_LASSO), 0.9 * nrow(final_tibble_LASSO))

final_train <- final_tibble_LASSO[train_index, ]
final_x_train <- final_train %>% select(-stars.x)
final_y_train <- final_train$stars.x

final_test <- final_tibble_LASSO[-train_index, ]
final_x_test <- final_test %>% select(-stars.x)
final_y_test <- final_test$stars.x

x_train_matrix <- makeX(final_x_train)
y_train_matrix <- as.matrix(final_y_train)
x_test_matrix <- makeX(final_x_test)
y_test_matrix <- as.matrix(final_y_test)

# LASSO with Cross-Validation
cv.out <- cv.glmnet(x_train_matrix, y_train_matrix, alpha = 1, nfolds = 5)
plot(cv.out)
lambda_LASSO_cv <- cv.out$lambda.min 

LASSO.mod <- glmnet(x_train_matrix, y_train_matrix, alpha = 1, lambda = lambda_LASSO_cv, thresh = 1e-12)

print(coef(LASSO.mod))

# Fit on the Test Data
LASSO.pred <- predict(LASSO.mod, s = lambda_LASSO_cv, newx = x_test_matrix)
LASSO_MSE <- mean((LASSO.pred - y_test_matrix) ^ 2) 

cat("LASSO MSE on Test Data:", LASSO_MSE, "\n")

# Fit on  the Training Data
LASSO.pred_train <- predict(LASSO.mod, s = lambda_LASSO_cv, newx = x_train_matrix)
LASSO_MSE_train <- mean((LASSO.pred_train - y_train_matrix) ^ 2) 

cat("LASSO MSE on Training Data:", LASSO_MSE_train, "\n")

# LASSO coefficients
print(coef(LASSO.mod))

# LASSO Test Mean Squared Error
cat("LASSO MSE on Test Data:", LASSO_MSE, "\n")

# LASSO Training Mean Squared Error
cat("LASSO MSE on Training Data:", LASSO_MSE_train, "\n")


# Visualisation for Training and Test MSE

plot(log(cv.out$lambda), cv.out$cvm, type = 'b', col = 'blue', xlab = 'log(lambda)', ylab = 'Cross-validated MSE')
points(log(lambda_LASSO_cv), LASSO_MSE, col = 'red', pch = 19)  
points(log(lambda_LASSO_cv), LASSO_MSE_train, col = 'green', pch = 19)  
legend("topright", legend = c("Test MSE", "Training MSE"), col = c("red", "green"), pch = 19)

# Plotting Predicted vs Actual values for the Test and Training data

y_axis_limits <- c(-5, 10)

hex_train <- hexbin(y_train_matrix, LASSO.pred_train, xbins = 50)
plot(hex_train, main = 'Predicted vs. Actual Stars (Training)', xlab = 'Actual', ylab = 'Predicted', colramp = function(n) colorRampPalette(c("white", "blue"))(n))


hex_test <- hexbin(y_test_matrix, LASSO.pred, xbins = 50)
plot(hex_test, main = 'Predicted vs. Actual Stars (Test)', xlab = 'Actual', ylab = 'Predicted', colramp = function(n) colorRampPalette(c("white", "blue"))(n))

summary(final_tibble_LASSO$stars.x)

