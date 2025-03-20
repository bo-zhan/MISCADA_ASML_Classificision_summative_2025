
library(tidyverse)
library(caret)
library(pROC)
library(randomForest)
library(xgboost)








# Data Loading and Exploration
# Read data
hotel_data <- read.csv("hotels.csv")
# Check data structure
str(hotel_data)
# Basic statistical summary
summary(hotel_data)
# Convert categorical variables to factors
categorical_vars <- c("hotel", "arrival_date_month", "meal", "country", 
                     "market_segment", "distribution_channel", "reserved_room_type", 
                     "assigned_room_type", "deposit_type", "customer_type", 
                     "reservation_status", "is_canceled", "is_repeated_guest")
hotel_data[categorical_vars] <- lapply(hotel_data[categorical_vars], factor)
# Handle missing values
missing_values <- colSums(is.na(hotel_data))
print(missing_values)
# Handle NA values
hotel_data$children[is.na(hotel_data$children)] <- 0
hotel_data$agent[is.na(hotel_data$agent)] <- 0
hotel_data$company[is.na(hotel_data$company)] <- 0
# Check target variable distribution
canceled_dist <- table(hotel_data$is_canceled)
canceled_prop <- prop.table(canceled_dist)
# Create directories for storing plots and results
dir.create("plots", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)




# Feature Engineering
# Create total nights stayed feature
hotel_data$total_nights <- hotel_data$stays_in_weekend_nights + hotel_data$stays_in_week_nights
# Create lead time category
hotel_data$lead_time_category <- cut(hotel_data$lead_time, 
                                    breaks = c(-1, 7, 30, 90, 365, Inf),
                                    labels = c("LastMinute", "ShortTerm", "MediumTerm", "LongTerm", "VeryLongTerm"))
# Create total number of guests
hotel_data$total_guests <- hotel_data$adults + hotel_data$children + hotel_data$babies
# Create price-related features (total price = daily rate * number of nights)
hotel_data$total_price <- hotel_data$adr * hotel_data$total_nights
# Extract season information from months
hotel_data$season <- "Other"
hotel_data$season[hotel_data$arrival_date_month %in% c("December", "January", "February")] <- "Winter"
hotel_data$season[hotel_data$arrival_date_month %in% c("March", "April", "May")] <- "Spring"
hotel_data$season[hotel_data$arrival_date_month %in% c("June", "July", "August")] <- "Summer"
hotel_data$season[hotel_data$arrival_date_month %in% c("September", "October", "November")] <- "Fall"
hotel_data$season <- factor(hotel_data$season)





# Data Visualization
library(viridis)
require("corrplot")


# Target variable distribution visualization
cancel_dist_plot <- ggplot(hotel_data, aes(x = is_canceled, fill = is_canceled)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Distribution of hotel cancellations",
       x = "Cancel or not (0 = no, 1 = yes)", 
       y = "Numbers") +
  theme_minimal() +
  theme(legend.position = "none")
print(cancel_dist_plot)
ggsave("plots/cancel_distribution.png", cancel_dist_plot, width = 8, height = 6)

# Cancellation rates by hotel type
hotel_cancel_plot <- ggplot(hotel_data, aes(x = hotel, fill = is_canceled)) +
  geom_bar(position = "fill") +
  scale_fill_viridis(discrete = TRUE, 
                    name = "Cancel or not", 
                    labels = c("No", "Yes")) +
  labs(title = "Cancellation rates by hotel type",
       x = "Types of Hotel", 
       y = "ratio") +
  theme_minimal()
print(hotel_cancel_plot)
ggsave("plots/hotel_cancel_rate.png", hotel_cancel_plot, width = 8, height = 6)

# Effect of booking lead time on cancellation rate
leadtime_cancel_plot <- ggplot(hotel_data, aes(x = lead_time_category, fill = is_canceled)) +
  geom_bar(position = "fill") +
  scale_fill_viridis(discrete = TRUE, 
                    name = "Cancel or not", 
                    labels = c("No", "Yes")) +
  labs(title = "Cancellation rates by booking lead time category",
       x = "Booking lead time", 
       y = "ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(leadtime_cancel_plot)
ggsave("plots/leadtime_cancel_rate.png", leadtime_cancel_plot, width = 10, height = 6)

# Effect of season on cancellation rate
season_cancel_plot <- ggplot(hotel_data, aes(x = season, fill = is_canceled)) +
  geom_bar(position = "fill") +
  scale_fill_viridis(discrete = TRUE, 
                     name = "Cancel or not", 
                    labels = c("No", "Yes")) +
  labs(title = "Cancellation rates by season",
       x = "Season", 
       y = "ratio") +
  theme_minimal()
print(season_cancel_plot)
ggsave("plots/season_cancel_rate.png", season_cancel_plot, width = 8, height = 6)

# Relationship between market segment and cancellation rate
market_cancel_plot <- ggplot(hotel_data, aes(x = market_segment, fill = is_canceled)) +
  geom_bar(position = "fill") +
  scale_fill_viridis(discrete = TRUE, 
                     name = "Cancel or not", 
                    labels = c("No", "Yes")) +
  labs(title = "Cancellation rates by market segment",
       x = "Market", 
       y = "ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(market_cancel_plot)
ggsave("plots/market_segment_cancel_rate.png", market_cancel_plot, width = 10, height = 6)

# Relationship between deposit type and cancellation rate
deposit_cancel_plot <- ggplot(hotel_data, aes(x = deposit_type, fill = is_canceled)) +
  geom_bar(position = "fill") +
  scale_fill_viridis(discrete = TRUE, 
                     name = "Cancel or not", 
                    labels = c("No", "Yes")) +
  labs(title = "Cancellation rates for each deposit type",
       x = "Deposit type", 
       y = "Ratio") +
  theme_minimal()
print(deposit_cancel_plot)
ggsave("plots/deposit_cancel_rate.png", deposit_cancel_plot, width = 8, height = 6)

# Relationship between customer type and cancellation rate
customer_cancel_plot <- ggplot(hotel_data, aes(x = customer_type, fill = is_canceled)) +
  geom_bar(position = "fill") +
  scale_fill_viridis(discrete = TRUE, 
                     name = "Cancel or not", 
                    labels = c("No", "Yes")) +
  labs(title = "Cancellation rates by customer type",
       x = "Type of customers", 
       y = "Ratio") +
  theme_minimal()
print(customer_cancel_plot)
ggsave("plots/customer_cancel_rate.png", customer_cancel_plot, width = 8, height = 6)

# Box plot: Relationship between booking lead time and cancellation
leadtime_box_plot <- ggplot(hotel_data, aes(x = is_canceled, y = lead_time, fill = is_canceled)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "The relationship between reservation lead time and cancellation",
       x = "Cancel or not", 
       y = "Booking lead time (days)") +
  theme_minimal() +
  theme(legend.position = "none")
print(leadtime_box_plot)
ggsave("plots/leadtime_boxplot.png", leadtime_box_plot, width = 8, height = 6)

# Box plot: Relationship between total price and cancellation
price_box_plot <- ggplot(hotel_data, aes(x = is_canceled, y = total_price, fill = is_canceled)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "The relationship between total price and cancellation",
       x = "Cancel or not", 
       y = "Total prices") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 1000))
print(price_box_plot)
ggsave("plots/price_boxplot.png", price_box_plot, width = 8, height = 6)

# Correlation between numerical features
# Select main numerical features for correlation analysis
numeric_features <- hotel_data[, sapply(hotel_data, is.numeric)]
numeric_features <- numeric_features[, !names(numeric_features) %in% c("is_canceled")]

selected_numerics <- c("lead_time", "stays_in_weekend_nights", "stays_in_week_nights", 
                       "adults", "children", "previous_cancellations", 
                       "previous_bookings_not_canceled", "booking_changes",
                       "days_in_waiting_list", "adr", "total_nights", "total_guests", "total_price")
correlation_matrix <- cor(numeric_features[, names(numeric_features) %in% selected_numerics], 
                         use = "pairwise.complete.obs")

png("plots/correlation_matrix.png", width = 1000, height = 900)
corrplot(correlation_matrix, 
         method = "circle", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         title = "Numerical feature correlation matrix")
dev.off()





# Data preprocessing


# features  
selected_features <- c(
  "hotel", "lead_time", "arrival_date_month", "stays_in_weekend_nights", 
  "stays_in_week_nights", "adults", "children", "meal", "market_segment", 
  "distribution_channel", "is_repeated_guest", "previous_cancellations", 
  "previous_bookings_not_canceled", "reserved_room_type", "booking_changes", 
  "deposit_type", "days_in_waiting_list", "customer_type", "adr", 
  "required_car_parking_spaces", "total_of_special_requests",
  "total_nights", "lead_time_category", "total_guests", "total_price", "season"
)

# create the dataset of model
model_data <- hotel_data[, c(selected_features, "is_canceled")]


if ("country" %in% selected_features) {
  top_countries <- names(sort(table(model_data$country), decreasing = TRUE)[1:10])
  model_data$country <- as.character(model_data$country)
  model_data$country[!(model_data$country %in% top_countries)] <- "Other"
  model_data$country <- factor(model_data$country)
}

# Normalized numerical feature
numeric_cols <- names(model_data)[sapply(model_data, is.numeric)]
numeric_cols <- numeric_cols[!numeric_cols %in% c("is_canceled")]

# Data standardization
preprocess_params <- preProcess(model_data[, numeric_cols], method = c("center", "scale"))
model_data_scaled <- predict(preprocess_params, model_data)

# Divide the training set and test set
set.seed(42)
train_index <- createDataPartition(model_data_scaled$is_canceled, p = 0.7, list = FALSE)
train_data <- model_data_scaled[train_index, ]
test_data <- model_data_scaled[-train_index, ]

train_data$is_canceled <- factor(train_data$is_canceled, 
                               levels = c(0, 1), 
                               labels = c("Not_Canceled", "Canceled"))
test_data$is_canceled <- factor(test_data$is_canceled, 
                              levels = c(0, 1), 
                              labels = c("Not_Canceled", "Canceled"))

train_dist <- prop.table(table(train_data$is_canceled))
test_dist <- prop.table(table(test_data$is_canceled))

cat("\nTraining set class distribution: ", train_dist, "\n")
cat("Test set class distribution: ", test_dist, "\n")






# Cross validation Settings


# Simplified cross-validation of control parameters
cv_control <- trainControl(
  method = "cv",
  number = 3,
  sampling = "down",
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final",
  returnResamp = "final"
)

eval_metric <- "ROC"





# Model training and evaluation

require("rpart.plot")
# Because it takes too long to train with raw data, a smaller subset of the data is created to train the model
set.seed(123)
sample_idx <- createDataPartition(train_data$is_canceled, p = 0.7, list = FALSE)
train_data_small <- train_data[sample_idx, ]

# Examine and remove zero-variance and near-zero-variance predictors
nzv <- nearZeroVar(train_data_small)
if(length(nzv) > 0) {
  train_data_small <- train_data_small[, -nzv]
  test_data <- test_data[, colnames(train_data_small)]
}

# Logistic regression model - using the regularized version
logistic_model <- train(
  is_canceled ~ .,
  data = train_data_small,
  method = "glmnet",
  family = "binomial",
  trControl = cv_control,
  metric = eval_metric
)

# Decision tree
rpart_model <- train(
  is_canceled ~ .,
  data = train_data_small,
  method = "rpart",
  trControl = cv_control,
  tuneLength = 5,
  metric = eval_metric
)

# Random forest model
rf_model <- train(
  is_canceled ~ .,
  data = train_data_small,
  method = "rf",
  trControl = cv_control,
  tuneLength = 1,
  metric = eval_metric,
  importance = TRUE,
  ntree = 150
)

# Gradient lifting model
gbm_grid <- expand.grid(
  n.trees = 150,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 10
)

gbm_model <- train(
  is_canceled ~ .,
  data = train_data_small,
  method = "gbm",
  trControl = cv_control,
  tuneGrid = gbm_grid,
  metric = eval_metric,
  verbose = FALSE
)

# Extract the optimal complexity parameter
best_cp <- rpart_model$bestTune$cp

# Train the complete CART model using the optimal parameters
cart_final <- rpart(
  is_canceled ~ .,
  data = train_data_small,
  method = "class",
  control = rpart.control(cp = best_cp)
)

png("plots/decision_tree.png", width = 1000, height = 700)
rpart.plot(cart_final, 
           extra = 101,
           box.palette = "RdBu",
           shadow.col = "gray",
           main = "Hotel cancellation decision tree")
dev.off()

png("plots/rf_importance.png", width = 800, height = 600)
plot(varImp(rf_model), top = 20, main = "Random Forest")
dev.off()


print(logistic_model)
print(rf_model$results)



# Check data structure
cat("Original training data column names:\n")
print(colnames(train_data))
# Check if children variable exists in train_data_small
if("children" %in% colnames(train_data_small)) {
  cat("\n'children' variable exists in the small dataset\n")
} else {
  cat("\n'children' variable does not exist in the small dataset\n")
}
# Check variables in the model formula
if(exists("rf_model")) {
  cat("\nRandom forest model variables:\n")
  print(rf_model$finalModel$xNames)
}
# Solution 1: Ensure training data has the same columns as the small dataset
if(exists("train_data_small")) {
  # Get column names from the processed small dataset
  small_cols <- colnames(train_data_small)
  
  # Ensure complete training data has the same columns as the small dataset
  train_data_fixed <- train_data[, small_cols]
  
  cat("\nFixed training data size:", dim(train_data_fixed)[1], "rows",
      dim(train_data_fixed)[2], "columns\n")
  
  # Confirm children variable is not in the fixed dataset
  if("children" %in% colnames(train_data_fixed)) {
    cat("Warning: 'children' variable still exists in the fixed dataset\n")
  } else {
    cat("Confirmed: 'children' variable is not in the fixed dataset\n")
  }
  
  # Try training a model using the fixed dataset
  cat("\nTraining final model using the fixed dataset...\n")
  
  # Simplified controller to speed up execution
  cv_control_fixed <- trainControl(
    method = "cv",
    number = 3,
    sampling = "down",
    summaryFunction = twoClassSummary,
    classProbs = TRUE
  )
  
  # Ensure the model can run, set fewer trees to speed up testing
  tryCatch({
    test_model <- train(
      is_canceled ~ .,
      data = train_data_fixed,
      method = "rf",
      trControl = cv_control_fixed,
      tuneGrid = data.frame(mtry = rf_model$bestTune$mtry),
      metric = eval_metric,
      importance = TRUE,
      ntree = 10  # Use fewer trees for testing
    )
    cat("Test model training successful!\n")
    
    # Now train the complete model
    cat("\nStarting final model training...\n")
    final_model <- train(
      is_canceled ~ .,
      data = train_data_fixed,
      method = "rf",
      trControl = cv_control_fixed,
      tuneGrid = data.frame(mtry = rf_model$bestTune$mtry),
      metric = eval_metric,
      importance = TRUE,
      ntree = 100
    )
    
    cat("Final model training completed!\n")
    saveRDS(final_model, "results/final_model.rds")
    cat("Model saved to results/final_model.rds\n")
    
  }, error = function(e) {
    cat("Error: ", e$message, "\n")
    cat("Need to further check data structure or try other fix methods\n")
  })
} else {
  cat("Cannot find train_data_small object\n")
}





final_model <- readRDS("results/final_model.rds")

# Get the feature column names (simple direct method)
model_features <- colnames(final_model$trainingData)
model_features <- model_features[model_features != ".outcome"]

# Check if these features are available in the test data
available_features <- intersect(model_features, colnames(test_data))


test_data_ready <- test_data[, available_features]

predictions <- predict(final_model, newdata = test_data_ready)

try({
  pred_probs <- predict(final_model, newdata = test_data_ready, type = "prob")
})

if("is_canceled" %in% colnames(test_data)) {
  # Ensure target variable is factor type
  if(!is.factor(test_data$is_canceled)) {
    test_data$is_canceled <- as.factor(test_data$is_canceled)
  }
  
  # Calculate confusion matrix
  conf_matrix <- confusionMatrix(predictions, test_data$is_canceled)
  print(conf_matrix)
  
  # Calculate and print main metrics
  cat("\nMain performance metrics:\n")
  cat("Accuracy:", round(conf_matrix$overall["Accuracy"] * 100, 2), "%\n")
  cat("Sensitivity:", round(conf_matrix$byClass["Sensitivity"], 4), "\n")
  cat("Specificity:", round(conf_matrix$byClass["Specificity"], 4), "\n")
  
  # Plot simple confusion matrix
  png("results/confusion_matrix.png", width = 600, height = 500)
  fourfoldplot(conf_matrix$table, color = c("red", "green"), 
              main = "conf_matrix")
  dev.off()
  cat("Confusion matrix plot saved\n")
  
  # If probability predictions exist, calculate AUC
  if(exists("pred_probs") && ncol(pred_probs) == 2) {
    # Assume second column is positive class probability
    roc_curve <- roc(as.numeric(test_data$is_canceled) - 1, pred_probs[, 2])
    auc_value <- auc(roc_curve)
    cat("AUC:", round(auc_value, 4), "\n")
    
    # Plot ROC curve
    png("results/roc_curve.png", width = 600, height = 500)
    plot(roc_curve, main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))
    dev.off()
    cat("ROC curve saved\n")
  }
} else {
  cat("No target variable in test data, unable to evaluate performance\n")
}
# 7. If random forest, plot variable importance
if(final_model$method == "rf") {
  png("results/importance.png", width = 800, height = 600)
  plot(varImp(final_model), top = 15)
  dev.off()
  cat("Variable importance plot saved\n")
}
cat("Evaluation complete!\n")


