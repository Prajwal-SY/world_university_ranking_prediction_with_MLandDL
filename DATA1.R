Uni <- read.csv("C:/Users/ferra/Downloads/World University Rankings.csv", na.strings=c("", " ", NA,'n/a'), stringsAsFactors=TRUE,header = TRUE)
summary(Uni)
View(Uni)

record <- factor(Uni$record_type,
                 levels=c('master_account','private','public'),
                 labels=c(1,2,3))
Uni$record_type <- as.numeric(record)
View(Uni)

Uni$stats_number_students<- as.numeric(Uni$stats_number_students)


Uni$overall_score <- as.character(Uni$overall_score)
# Function to extract highest value from range
convert_range_to_max <- function(range_column) {
  # Check if each value is a range
  is_range <- grepl("–", range_column)
  
  # If a value is a range, replace it with the maximum value
  max_values <- ifelse(is_range, sapply(strsplit(range_column[is_range], "–"), function(x) max(as.numeric(x))), range_column)
  
  # Return the modified column
  return(max_values)
}
converted_data <- convert_range_to_max(Uni$overall_score)
converted_data<- as.numeric(converted_data)
Uni$overall_score <- converted_data

Uni$stats_pc_intl_students <- as.numeric(sub("%","",Uni$stats_pc_intl_students))
# Assuming 'Uni' is your data frame and it has a column named 'overall_score'
sorted_Uni <- Uni[order(-Uni$overall_score), ]
Uni <- sorted_Uni
View(Uni)



# Load the tidyr package
library(tidyr)
# Initialize vectors to store ratios
female_ratios <- numeric(nrow(Uni))
male_ratios <- numeric(nrow(Uni))

# Loop through each row
for (i in seq_len(nrow(Uni))) {
  # Extract female-to-male ratio string
  ratio_str <- as.character(Uni$stats_female_male_ratio[i])  # Ensure the ratio is a character
  
  # Check if the ratio is given in decimal form
  if (grepl("^0\\.", ratio_str)) {
    # Convert ratio from decimal to percentage
    ratio <- as.numeric(ratio_str) * 100
    # Append to female and male ratios
    female_ratios[i] <- ratio
    male_ratios[i] <- 100 - ratio
  } else {
    # Separate the ratio string into female and male ratios
    separated_ratios <- separate(data = data.frame(ratio_str), col = "ratio_str", into = c("female_ratio", "male_ratio"), sep = ":")
    # Convert ratios to numeric and append to vectors
    female_ratio <- as.numeric(separated_ratios$female_ratio)
    male_ratio <- as.numeric(separated_ratios$male_ratio)
    # If both are NA add 50 to both or Perform mean imputation if either female or male ratio is NA
    if (is.na(female_ratio) && is.na(male_ratio)) {
      female_ratio <- 50
      male_ratio <- 50
    } else if (is.na(female_ratio) || is.na(male_ratio)) {
      female_ratio <- ifelse(is.na(female_ratio), mean(female_ratios, na.rm = TRUE), female_ratio)
      male_ratio <- ifelse(is.na(male_ratio), mean(male_ratios, na.rm = TRUE), male_ratio)
    }
    female_ratios[i] <- female_ratio
    male_ratios[i] <- male_ratio
  }
}

# Append female and male ratios to Uni dataframe
Uni$female_ratio <- female_ratios
Uni$male_ratio <- male_ratios

# Convert 'location' to a factor to ensure consistent levels
Uni$location <- as.factor(Uni$location)

# Perform label encoding for the 'location' variable
Uni$encoded_location <- as.integer(Uni$location)

# View the updated dataset
head(Uni)


# Display Uni dataset with new columns
print(Uni)
View(Uni)

#missing data analysis
library(naniar)
vis_miss(Uni)
View(Uni)
#mcar_test(Uni)

# Perform Little's MCAR test
library(VIM)
little_mcar <- aggr(Uni, numbers=TRUE, sortVars=TRUE, labels=names(Uni), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"), prop=T)
# Print the result
print(little_mcar)
View(Uni)



# Load the `VIM` Package
library(VIM)
# Perform KNN Imputation and Replace Missing Values
imputed_data <- kNN(Uni, k = 5)
# Replace Missing Values
for (variable in colnames(Uni)) {
  Uni[[variable]][is.na(Uni[[variable]])] <- imputed_data[[variable]][is.na(Uni[[variable]])]
}

# Check if missing values are replaced
summary(Uni)


# feature engineering

#check for duplicates
noOfDuplicates <- sum(duplicated(Uni$name))
print(noOfDuplicates)

# Filter out unaccredited universities and remove unnecessary columns
Uni <- subset(Uni, unaccredited == FALSE, select = -c(closed, unaccredited, stats_female_male_ratio, location, subjects_offered))

# Move overall_score column to the end
overall_score <- Uni$overall_score
Uni <- subset(Uni, select = -overall_score)
Uni <- cbind(Uni, overall_score)




# View the resulting dataset and summary
View(Uni)
summary(Uni)




#standardization
rs_function <- function(x){(x-min(x))/(max(x)-min(x))}

Uni$scores_teaching <- rs_function(Uni$scores_teaching)
Uni$scores_research <- rs_function(Uni$scores_research)
Uni$scores_citations <- rs_function(Uni$scores_citations)
Uni$scores_industry_income <- rs_function(Uni$scores_industry_income)
Uni$scores_international_outlook <- rs_function(Uni$scores_international_outlook)
Uni$member_level <- rs_function(Uni$member_level)
Uni$stats_number_students <- rs_function(Uni$stats_number_students)
Uni$stats_student_staff_ratio <- rs_function(Uni$stats_student_staff_ratio)
Uni$stats_pc_intl_students <- rs_function(Uni$stats_pc_intl_students)
Uni$female_ratio <- rs_function(Uni$female_ratio)
Uni$male_ratio <- rs_function(Uni$male_ratio)
Uni$encoded_location <- rs_function(Uni$encoded_location)
Uni$record_type <- rs_function(Uni$record_type)


summary(Uni)
View(Uni)



#Analysis


library(corrplot)
correlation_matrix <- cor(Uni[,-1])
corrplot(correlation_matrix,method='color',number.cex = 0.5,addCoef.col = "black")



# model building

set.seed(42)
Uni[,"train"] <- ifelse(runif(nrow(Uni))<0.8,1,0)
View(Uni)
trainset <- Uni[Uni$train == "1",]
testset <- Uni[Uni$train == "0",]

trainset <- trainset[-16]
testset <- testset[-16]
View(trainset)
View(testset)


#MLR

overallScoreMulti <- lm(overall_score ~ scores_teaching + scores_research + scores_citations+ scores_industry_income
                        + scores_international_outlook+ record_type + member_level+stats_number_students
                        +stats_student_staff_ratio+stats_pc_intl_students+female_ratio 
                        + male_ratio +encoded_location, data = trainset)
print(overallScoreMulti)
summary(overallScoreMulti)

score_pred_multi <- predict(overallScoreMulti, testset)
cor(testset$overall_score, score_pred_multi, method = "pearson")

#RMSE
library(Metrics)
std_dev <- sd(Uni$overall_score)
std_dev1<- sd(testset$overall_score)
print(std_dev)
print(std_dev1)
rmse(testset$overall_score, score_pred_multi)

# Calculate MAE
mae(testset$overall_score, score_pred_multi)

#GLM
library(mlbench)
library(glmnet)
train_matrix <- model.matrix(overall_score~., trainset[-1])
print(train_matrix)
View(train_matrix)

response <- trainset$overall_score
View(response)
hist(response)
ScorePred_glm <- cv.glmnet(train_matrix, response, family ="gaussian", alpha = 1, nfolds = 10, type.measure = "mse")
summary(ScorePred_glm)
# Calculate adjusted R-squared
n <- nrow(trainset)
p <- length(coef(ScorePred_glm, s=lambda_min)) - 1
r_squared <- 1 - (1 - cor(response, predict(ScorePred_glm, newx = train_matrix, s = lambda_min))^2) * (n-1)/(n-p-1)
cat("Adjusted R-squared:", r_squared)


#configure Lasso
lambda_min <- ScorePred_glm$lambda.min

coef(ScorePred_glm,s=lambda_min)

#test performance of glm
test_matrix <- model.matrix(overall_score~., testset[-1])


#predict function to predict the values
Score_pred_glm <- predict(ScorePred_glm, newx = test_matrix, s =lambda_min)

cor(testset$overall_score, Score_pred_glm, method = "pearson")

#RMSE
library(Metrics)
std_dev <- sd(Uni$overall_score)
print(std_dev)
rmse(testset$overall_score, Score_pred_glm)
# Calculate MAE
mae(testset$overall_score, Score_pred_glm)


#PCA
library(factoextra)
active_data <- Uni[c(2:14)]
View(active_data)
pca_Uni <- princomp(active_data, cor = TRUE, scores =TRUE)

fviz_eig(pca_Uni)
fviz_pca_var(pca_Uni, col.var = "contrib", gradient.cols=c( "#E7B800", "#FC4E07"), repel=TRUE)
fviz_pca_var(pca_Uni, col.var = "contrib", repel = TRUE)


# Load the factoextra library
library(factoextra)

# Select columns 2 through 14 from the Uni dataset
active_data <- Uni[c(2:14)]

# Perform PCA on the selected data, with correlation and scores options set to TRUE
pca_Uni <- princomp(active_data, cor = TRUE, scores =TRUE)

# Visualize the eigenvalues of the PCA result
fviz_eig(pca_Uni)

# Visualize the variables of the PCA result, colored by their contribution to the principal components
# The colors will range from red ("#FF0000") to green ("#00FF00") to blue ("#0000FF"), and labels will be repelled for better visibility
fviz_pca_var(pca_Uni, col.var="contrib", gradient.cols=c("#FF0000", "#00FF00", "#0000FF"), repel=TRUE)


#randomn Forest 
# Load required library
library(randomForest)

# Train the Random Forest model
rf_model <- randomForest(overall_score ~ ., data = trainset[-1], ntree = 500, mtry = 4)
summary(rf_model)
# Calculate adjusted R-squared
n <- nrow(trainset)
p <- length(rf_model$importance) - 1
r_squared <- 1 - (1 - cor(trainset$overall_score, predict(rf_model, newdata = trainset[-1]))^2) * (n-1)/(n-p-1)
cat("Adjusted R-squared:", r_squared)


# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = testset[-1])
cor(testset$overall_score, rf_predictions, method = "pearson")

#RMSE
library(Metrics)
std_dev <- sd(Uni$overall_score)
print(std_dev)
rmse(testset$overall_score, rf_predictions)
# Calculate MAE
mae(testset$overall_score, rf_predictions)



# Load required library
library(gbm)

# Train the Gradient Boosting model
gbm_model <- gbm(overall_score ~ ., data = trainset[-1], distribution = "gaussian", n.trees = 500, interaction.depth = 4)
summary(gbm_model)
# Calculate adjusted R-squared
n <- nrow(trainset)
p <- length(gbm_model$var.rel.inf) - 1
r_squared <- 1 - (1 - cor(trainset$overall_score, predict(gbm_model, newdata = trainset[-1], n.trees = 500))^2) * (n-1)/(n-p-1)
cat("Adjusted R-squared:", r_squared)


# Make predictions on the test set
gbm_predictions <- predict(gbm_model, newdata = testset[-1], n.trees = 500)
cor(testset$overall_score, gbm_predictions, method = "pearson")
#RMSE
library(Metrics)
std_dev <- sd(Uni$overall_score)
print(std_dev)
rmse(testset$overall_score, gbm_predictions)
# Calculate MAE
mae(testset$overall_score, gbm_predictions)


#XGBM
library(xgboost)
library(caret)
xgb_model <- xgboost(data = train_matrix,response,
                     max_depth = 3, eta = 0.1, nthread = 4,
                     nrounds = 500, objective = "reg:squarederror")
summary(xgb_model)
# Calculate adjusted R-squared
n <- nrow(trainset)
p <- length(xgb_model$feature_names) 
r_squared <- 1 - (1 - cor(trainset$overall_score, predict(xgb_model, newdata = train_matrix))^2) * (n-1)/(n-p-1)
cat("Adjusted R-squared:", r_squared)


#predictions
xgm_predictions <- predict(xgb_model,newdata=test_matrix)
cor(testset$overall_score, xgm_predictions, method = "pearson")


#RMSE
library(Metrics)
std_dev <- sd(Uni$overall_score)
print(std_dev)
rmse(testset$overall_score, xgm_predictions)
# Calculate MAE
mae(testset$overall_score, xgm_predictions)

# Assuming testset$overall_score and xgm_predictions are vectors of the same length
combined_values <- cbind(testset$overall_score, xgm_predictions)

# Print the combined values
print(combined_values)


# Define the control function for training
ctrl <- trainControl(method = "cv", number = 10)

# Define the grid of hyperparameters to search over
grid <- expand.grid(
  nrounds = c(100, 200, 300),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1, 5),
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.6, 0.8, 1)
)

# Train the model with early stopping
xgb_model <- train(
  response ~ ., 
  data = train_matrix, 
  method = "xgbTree", 
  trControl = ctrl, 
  tuneGrid = grid, 
  nthread = 4,
  early_stopping_rounds = 10
)
