# Logistic-Regression-in-R
My Process for Random Forest & Logistic Regression model in R to find the predictors of attrition

# STEP 1: Install & load packages (only run install lines once)
install.packages("tidyverse")
install.packages("randomForest")

library(tidyverse)
library(randomForest)

# STEP 2: Simulate a small dummy dataset
set.seed(123)
df <- tibble(
  Attrition = sample(c(1, 0), 100, replace = TRUE),
  TrainingHours = rnorm(100, mean = 40, sd = 10),
  YearsSinceLastPromotion = rpois(100, lambda = 3),
  AvgHoursWorked = rnorm(100, mean = 45, sd = 5),
  Absenteeism = rnorm(100, mean = 3, sd = 1),
  PerformanceRating = sample(1:5, 100, replace = TRUE)
)

# STEP 3: Run logistic regression
log_model <- glm(Attrition ~ TrainingHours + YearsSinceLastPromotion + AvgHoursWorked + Absenteeism + PerformanceRating,
                 data = df, family = binomial)

summary(log_model)  # View logistic regression output
exp(coef(log_model))  # Get odds ratios

# STEP 4: Run Random Forest
rf_model <- randomForest(as.factor(Attrition) ~ TrainingHours + YearsSinceLastPromotion + AvgHoursWorked + Absenteeism + PerformanceRating,
                         data = df,
                         importance = TRUE,
                         ntree = 500)

print(rf_model)  # View RF model info
varImpPlot(rf_model)  # Visualize variable importance

# STEP 5: Interpreting Logistic Regression
# Look under the Estimate column: Positive = increases chance of attrition, Negative = decreases chance of attrition, Check p-values (Pr(>|z|)):< 0.05 means
# statistically significant 
summary(log_model)

# STEP 6: Get odds Ratio 
exp(coef(log_model))

# STEP 7: Run Random Forest Model again to visualize
varImpPlot(rf_model)

# STEP 8: Install Random Forest Accuracy (mtry & ntree package), ntree: Number of trees in the forest. More = potentially better accuracy, but slower. mtry: Number of variables randomly chosen at each split. A key hyperparameter to tune.
# Install caret if not done yet
install.packages("caret")
library(caret)

# Set up control for cross-validation
control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Train with tuning grid
tuned_rf <- train(
  as.factor(Attrition) ~ TrainingHours + YearsSinceLastPromotion + AvgHoursWorked + Absenteeism + PerformanceRating,
  data = df,
  method = "rf",
  trControl = control,
  tuneGrid = expand.grid(mtry = 1:5),  # Try different mtry values
  ntree = 500
)

# View best parameters and accuracy
print(tuned_rf)
plot(tuned_rf)

# STEP 9: Visualize with ggplot2 (feature importance from Random Forest) 

# Get importance values
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

# Plot
library(ggplot2)

ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)", x = "Feature", y = "Mean Decrease in Gini") +
  theme_minimal()

# STEP 10: If you want to export to Excel for Dashboards: 

# Install if needed
install.packages("writexl")
library(writexl)

# Export Random Forest importance
write_xlsx(importance_df, "feature_importance.xlsx")

# Export model predictions (optional)
df$predicted <- predict(rf_model, df, type = "prob")[,2]
write_xlsx(df, "predicted_attrition_scores.xlsx")

# STEP 11: Export to Dataframe to Excel
install.packages("writexl")  # Only once
library(writexl)

importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

write_xlsx(importance_df, "feature_importance.xlsx")

df$Predicted_Prob <- predict(rf_model, df, type = "prob")[,2]

write_xlsx(df, "predicted_attrition_scores.xlsx")

# STEP 12: Where is the file saved? By default, it saves to your working directory. To check or change it:
getwd()  # Shows current working directory

setwd("C:/Users/Ivana/Documents/HR_Project")  # Change location if needed

# STEP 13: Export LogModel to Excel
install.packages("broom")      # For tidying model output
install.packages("writexl")    # For Excel export

library(broom)
library(writexl)

log_summary <- tidy(log_model)

# View it
print(log_summary)

# Add Odd Ratios (opotional)
log_summary$odds_ratio <- exp(log_summary$estimate)

# Export to Excel
write_xlsx(log_summary, "logistic_regression_summary.xlsx")

# STEP 14: Save Logistic Regression Summary to Desktop
# Load libraries
library(broom)
library(writexl)

# Tidy the model
log_summary <- tidy(log_model)
log_summary$odds_ratio <- exp(log_summary$estimate)

# Set File Path to Desktop
file_path <- "C:/Users/Ivana/Desktop/logistic_regression_summary.xlsx"
write_xlsx(log_summary, file_path)

file.exists(file_path) # to double check in R that it worked

# STEP 15: Visualize Logistic Regression Coefficients 
# Load libraries
library(broom)
library(ggplot2)

# Tidy model + odds ratio
log_summary <- tidy(log_model)
log_summary$odds_ratio <- exp(log_summary$estimate)

# Optional: Remove intercept to focus only on predictors
log_summary <- log_summary[log_summary$term != "(Intercept)", ]

ggplot(log_summary, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = ifelse(log_summary$estimate > 0, "#D95F02", "#1B9E77")) +
  coord_flip() +
  labs(
    title = "Impact of Predictors on Attrition (Logistic Regression)",
    x = "Predictor",
    y = "Log-Odds Coefficient"
  ) +
  theme_minimal()

# STEP 16: Add confidence intervals (if available) for 95% confidence 
ggplot(log_summary, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "#1f78b4") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +
  labs(
    title = "Predictor Impact on Attrition (w/ 95% CI)",
    x = "Predictor",
    y = "Coefficient (Log-Odds)"
  ) +
  theme_minimal()

# This plot will clearly show: Positive predictors pushing attrition risk up, Negative predictors reducing attrition likelihood, Relative strength of each



