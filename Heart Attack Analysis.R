# Set working directory
#setwd("/Users/nustanishant/Desktop/Temple University/Semester 2/Advance Business Analytics/Group Work/Mini Group Projet - Week 14")
setwd("D:/Gauri/Gauri SOPs & LORs/Temple University/Courses/Spring 2024/STAT-5607 Advanced Business Analytics/Week 14/Group Project")

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(caret)      # For machine learning functions
library(xgboost)    # For XGBoost algorithm
library(pROC)       # For ROC curve analysis
library(randomForest)  # For random forest algorithm
library(ggplot2)    # For advanced plotting
library(psych)      # For PCA and Bartlett's test
library(MASS)       # For confidence intervals
library(car)        # For Variance Inflation Factor (VIF)


# Read data
heart_disease_data <- read.csv("Heart Attack 1.csv")
head(heart_disease_data)

# Summary statistics
summary_statistics <- summary(heart_disease_data)
print(summary_statistics)

# Plotting histograms
par(mfrow=c(3,2))
columns_to_plot <- c('age', 'resting.bp.s', 'cholesterol', 'max.heart.rate', 'oldpeak')
for(column_name in columns_to_plot) {
  hist(heart_disease_data[[column_name]], main = paste("Distribution of", column_name), xlab = column_name, ylab = "Frequency", col = "lightblue", border = "black")
}
par(mfrow=c(1,1))

# Plotting boxplots
par(mfrow=c(3,2))
for(column_name in columns_to_plot) {
  boxplot(heart_disease_data[[column_name]], main = bquote(bold(.(paste("Boxplot of", column_name)))), ylab = column_name, col = "lightblue")
}
par(mfrow=c(1,1))

# QQ plots
par(mfrow=c(3,2))
for(column_name in columns_to_plot) {
  qqnorm(heart_disease_data[[column_name]], main = bquote(bold(.(paste("QQ Plot of", column_name)))), ylab = "Quantiles", xlab = "Theoretical Quantiles")
  qqline(heart_disease_data[[column_name]], col = "red")
}
par(mfrow=c(1,1))




# Preprocessing: Check for zero values
heart_disease_data$resting.bp.s[heart_disease_data$resting.bp.s == 0] <- median(heart_disease_data$resting.bp.s[heart_disease_data$resting.bp.s != 0])
heart_disease_data$cholesterol[heart_disease_data$cholesterol == 0] <- median(heart_disease_data$cholesterol[heart_disease_data$cholesterol != 0])

# Plotting histograms after preprocessing
par(mfrow=c(3,2))
columns_to_plot <- c('age', 'resting.bp.s', 'cholesterol', 'max.heart.rate', 'oldpeak')
for(column_name in columns_to_plot) {
  hist(heart_disease_data[[column_name]], main = paste("Distribution of", column_name), xlab = column_name, ylab = "Frequency", col = "lightblue", border = "black")
}
par(mfrow=c(1,1))

# Plotting boxplots after preprocessing
par(mfrow=c(3,2))
for(column_name in columns_to_plot) {
  boxplot(heart_disease_data[[column_name]], main = bquote(bold(.(paste("Boxplot of", column_name)))), ylab = column_name, col = "lightblue")
}
par(mfrow=c(1,1))

# QQ plots after preprocessing
par(mfrow=c(3,2))
for(column_name in columns_to_plot) {
  qqnorm(heart_disease_data[[column_name]], main = bquote(bold(.(paste("QQ Plot of", column_name)))), ylab = "Quantiles", xlab = "Theoretical Quantiles")
  qqline(heart_disease_data[[column_name]], col = "red")
}
par(mfrow=c(1,1))



# Normality Test
shapiro_results <- lapply(heart_disease_data[columns_to_plot], shapiro.test)
print(shapiro_results)

# Correlation matrix
correlation_matrix <- cor(heart_disease_data[columns_to_plot])
print(round(correlation_matrix, 2))

# Multicollinearity using Variance Inflation Factor (VIF)
heart_disease_data$target <- as.numeric(heart_disease_data$target)
vif_results <- vif(lm(target ~ ., data = heart_disease_data))
print(vif_results)

# Bartlett's test of sphericity
bartlett_results <- cortest.bartlett(heart_disease_data[columns_to_plot])
print(bartlett_results)



# Convert 'target' column to a factor for classification
heart_disease_data$target <- as.factor(heart_disease_data$target)

# Split the data into training and test sets
set.seed(42) # for reproducibility
splitIndex <- createDataPartition(heart_disease_data$target, p = 0.8, list = FALSE)
train_data <- heart_disease_data[splitIndex, ]
test_data <- heart_disease_data[-splitIndex, ]

# Preparing data for modeling
X_train <- as.matrix(train_data[, -ncol(train_data)])
y_train <- train_data$target
X_test <- as.matrix(test_data[, -ncol(test_data)])
y_test <- test_data$target

# Logistic Regression
log_model <- glm(target ~ ., data = train_data, family = "binomial")
summary(log_model)
log_preds <- predict(log_model, newdata = test_data, type = "response")
log_preds <- ifelse(log_preds > 0.5, 1, 0)
log_conf_matrix <- confusionMatrix(as.factor(log_preds), as.factor(y_test))
print(log_conf_matrix)


# ANOVA
# Fit the ANOVA model
#anova_model <- aov(target ~ chest.pain.type, data = heart_disease_data)

# Perform ANOVA
#anova_results <- summary(anova_model)
#print(anova_results)

# ANCOVA
# Fit the ANCOVA model
#ancova_model <- lm(target ~ chest.pain.type + age, data = heart_disease_data)

# Perform ANCOVA
#ancova_results <- anova(ancova_model)
#print(ancova_results)

# ANOVA
# Fit the ANOVA model
#anova_model <- aov(target ~ chest.pain.type, data = heart_disease_data)

# Perform ANOVA
#anova_results <- summary(anova_model)
#print(anova_results)

# ANOVA
# Fit the ANOVA model
#anova_model <- aov(target ~ chest.pain.type, data = heart_disease_data)

# Check ANOVA results
#print(summary(anova_model))



# ANCOVA
# Convert 'target' to numeric if it's not already numeric
heart_disease_data$target <- as.numeric(as.character(heart_disease_data$target))

# Fit the ANCOVA model
ancova_model <- lm(target ~ chest.pain.type + age, data = heart_disease_data)

# Perform ANCOVA
ancova_results <- anova(ancova_model)
print(ancova_results)





# ANOVA for 'chest.pain.type'
anova_chest_pain <- aov(target ~ chest.pain.type, data = heart_disease_data)
summary(anova_chest_pain)

# ANOVA for 'resting.bp.s'
anova_resting_bp <- aov(target ~ resting.bp.s, data = heart_disease_data)
summary(anova_resting_bp)

# ANOVA for 'cholesterol'
anova_cholesterol <- aov(target ~ cholesterol, data = heart_disease_data)
summary(anova_cholesterol)

# ANOVA for 'max.heart.rate'
anova_max_hr <- aov(target ~ max.heart.rate, data = heart_disease_data)
summary(anova_max_hr)

# ANOVA for 'oldpeak'
anova_oldpeak <- aov(target ~ oldpeak, data = heart_disease_data)
summary(anova_oldpeak)

# ANOVA for 'exercise.angina'
anova_exercise_angina <- aov(target ~ exercise.angina, data = heart_disease_data)
summary(anova_exercise_angina)

# ANOVA for 'ST.slope'
anova_st_slope <- aov(target ~ ST.slope, data = heart_disease_data)
summary(anova_st_slope)



# ANCOVA for 'chest.pain.type' controlling for 'age'
ancova_chest_pain <- lm(target ~ chest.pain.type + age, data = heart_disease_data)
anova(ancova_chest_pain)

# ANCOVA for 'resting.bp.s' controlling for 'age'
ancova_resting_bp <- lm(target ~ resting.bp.s + age, data = heart_disease_data)
anova(ancova_resting_bp)

# ANCOVA for 'cholesterol' controlling for 'age'
ancova_cholesterol <- lm(target ~ cholesterol + age, data = heart_disease_data)
anova(ancova_cholesterol)

# ANCOVA for 'max.heart.rate' controlling for 'age'
ancova_max_hr <- lm(target ~ max.heart.rate + age, data = heart_disease_data)
anova(ancova_max_hr)

# ANCOVA for 'oldpeak' controlling for 'age'
ancova_oldpeak <- lm(target ~ oldpeak + age, data = heart_disease_data)
anova(ancova_oldpeak)

# ANCOVA for 'exercise.angina' controlling for 'age'
ancova_exercise_angina <- lm(target ~ exercise.angina + age, data = heart_disease_data)
anova(ancova_exercise_angina)

# ANCOVA for 'ST.slope' controlling for 'age'
ancova_st_slope <- lm(target ~ ST.slope + age, data = heart_disease_data)
anova(ancova_st_slope)



# Fit logistic regression models and extract odds ratios and confidence intervals for each predictor variable

# Chest Pain Type
logistic_model_chest_pain <- glm(target ~ chest.pain.type, data = heart_disease_data, family = "binomial")
odds_ratios_chest_pain <- exp(coef(logistic_model_chest_pain))
ci_chest_pain <- exp(confint(logistic_model_chest_pain))

# Resting Blood Pressure
logistic_model_resting_bp <- glm(target ~ resting.bp.s, data = heart_disease_data, family = "binomial")
odds_ratios_resting_bp <- exp(coef(logistic_model_resting_bp))
ci_resting_bp <- exp(confint(logistic_model_resting_bp))

# Cholesterol
logistic_model_cholesterol <- glm(target ~ cholesterol, data = heart_disease_data, family = "binomial")
odds_ratios_cholesterol <- exp(coef(logistic_model_cholesterol))
ci_cholesterol <- exp(confint(logistic_model_cholesterol))

# Max Heart Rate
logistic_model_max_hr <- glm(target ~ max.heart.rate, data = heart_disease_data, family = "binomial")
odds_ratios_max_hr <- exp(coef(logistic_model_max_hr))
ci_max_hr <- exp(confint(logistic_model_max_hr))

# Oldpeak
logistic_model_oldpeak <- glm(target ~ oldpeak, data = heart_disease_data, family = "binomial")
odds_ratios_oldpeak <- exp(coef(logistic_model_oldpeak))
ci_oldpeak <- exp(confint(logistic_model_oldpeak))

# Exercise Angina
logistic_model_exercise_angina <- glm(target ~ exercise.angina, data = heart_disease_data, family = "binomial")
odds_ratios_exercise_angina <- exp(coef(logistic_model_exercise_angina))
ci_exercise_angina <- exp(confint(logistic_model_exercise_angina))

# ST Slope
logistic_model_st_slope <- glm(target ~ ST.slope, data = heart_disease_data, family = "binomial")
odds_ratios_st_slope <- exp(coef(logistic_model_st_slope))
ci_st_slope <- exp(confint(logistic_model_st_slope))

# Print results for each predictor variable
cat("Chest Pain Type:\n")
cat("Odds Ratio:", odds_ratios_chest_pain, "\n")
cat("95% CI:", ci_chest_pain, "\n\n")

cat("Resting Blood Pressure:\n")
cat("Odds Ratio:", odds_ratios_resting_bp, "\n")
cat("95% CI:", ci_resting_bp, "\n\n")

cat("Cholesterol:\n")
cat("Odds Ratio:", odds_ratios_cholesterol, "\n")
cat("95% CI:", ci_cholesterol, "\n\n")

cat("Max Heart Rate:\n")
cat("Odds Ratio:", odds_ratios_max_hr, "\n")
cat("95% CI:", ci_max_hr, "\n\n")

cat("Oldpeak:\n")
cat("Odds Ratio:", odds_ratios_oldpeak, "\n")
cat("95% CI:", ci_oldpeak, "\n\n")

cat("Exercise Angina:\n")
cat("Odds Ratio:", odds_ratios_exercise_angina, "\n")
cat("95% CI:", ci_exercise_angina, "\n\n")

cat("ST Slope:\n")
cat("Odds Ratio:", odds_ratios_st_slope, "\n")
cat("95% CI:", ci_st_slope, "\n\n")

# Create data frames for odds ratios and confidence intervals
predictors <- c("Chest Pain Type", "Resting Blood Pressure", "Cholesterol", "Max Heart Rate", "Oldpeak", "Exercise Angina", "ST Slope")

odds_ratios <- c(odds_ratios_chest_pain, odds_ratios_resting_bp, odds_ratios_cholesterol, odds_ratios_max_hr, odds_ratios_oldpeak, odds_ratios_exercise_angina, odds_ratios_st_slope)

ci_lower <- c(ci_chest_pain[1], ci_resting_bp[1], ci_cholesterol[1], ci_max_hr[1], ci_oldpeak[1], ci_exercise_angina[1], ci_st_slope[1])
ci_upper <- c(ci_chest_pain[2], ci_resting_bp[2], ci_cholesterol[2], ci_max_hr[2], ci_oldpeak[2], ci_exercise_angina[2], ci_st_slope[2])

odds_ci_table <- data.frame(
  Predictor = predictors,
  Odds_Ratio = odds_ratios,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# Print the combined table
print(odds_ci_table)

