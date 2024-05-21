## Exercise 01 :  --------------------------------------------------------------
## A Department of Education analyst is interested in investigating the pay 
## structure of secondary school teachers. He believes there are 3 factors that 
## affect the salaries of teachers: years of experience; a rating of teaching 
## effectiveness given by a department inspector; and the numbers of different 
## subjects taught. A sample of 20 teachers produced the results given in the 
## salary dataset. 

# Load necessary libraries
library(readr)
library(psych)
library(car)
library(e1071)
library(faraway)

# Read the dataset
salary_data <- read.csv("salary.csv", na.strings = "")
salary <- as.data.frame(salary_data)
names(salary)[names(salary) == "Number.of.Subjects"] <- "Number_of_Subjects"

# Display the structure and first few rows of the dataset
str(salary)
head(salary)

# Drop the column 'CaseNum'
salary <- salary[, c(2, 3, 4, 5)]
str(salary)

# Using the pairs function to examine linearity
windows(20, 10)
pairs(salary)

# Generate pairs plot for all variables using pairs.panels from the psych package
pairs.panels(salary,
             smooth = FALSE, 
             scale = FALSE,   
             density = TRUE, 
             ellipses = FALSE, 
             method = "spearman",
             pch = 21,           
             lm = FALSE,         
             cor = TRUE,         
             jiggle = FALSE,     
             factor = 2,         
             hist.col = 4,       
             stars = TRUE,       
             ci = TRUE)          

# Scatter plots with smooth lines
windows(20, 12)
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid

scatter.smooth(x = salary$Years,
               y = salary$Salary,
               main = "Correlation of Salary - Years",
               xlab = "Years",
               ylab = "Salary $")

scatter.smooth(x = salary$Rating,
               y = salary$Salary,
               main = "Correlation of Salary - Rating",
               xlab = "Rating",
               ylab = "Salary $")

scatter.smooth(x = salary$Number_of_Subjects,
               y = salary$Salary,
               main = "Correlation of Salary - Number of Subjects",
               xlab = "Number of Subjects",
               ylab = "Salary $")

# Calculate and print the correlation matrix
cor_matrix <- cor(salary[, c("Salary", "Years", "Rating", "Number_of_Subjects")])
print(cor_matrix)

# Based on the correlation matrix, we can observe the following relationships:
# Salary is highly correlated with Years (0.868) and Number_of_Subjects (0.952).
# Years is moderately correlated with Number_of_Subjects (0.804).
# Rating has moderate correlations with Salary (0.547) and Number_of_Subjects 
# (0.563), but a low correlation with Years (0.187).

# Check for outliers using boxplots
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
boxplot(salary$Salary, main = "Boxplot of Salary")
boxplot(salary$Years, main = "Boxplot of Years")
boxplot(salary$Rating, main = "Boxplot of Rating")
boxplot(salary$Number_of_Subjects, main = "Boxplot of Number_of_Subjects")

# Identify outliers for each variable
outliers_salary <- boxplot.stats(salary$Salary)$out
outliers_years <- boxplot.stats(salary$Years)$out
outliers_rating <- boxplot.stats(salary$Rating)$out
outliers_subjects <- boxplot.stats(salary$Number_of_Subjects)$out

print(outliers_salary)
print(outliers_years)
print(outliers_rating)
print(outliers_subjects)

# Optionally, remove outliers
# salary_no_outliers <- salary[!(salary$Salary %in% outliers_salary |
#                               salary$Years %in% outliers_years |
#                               salary$Rating %in% outliers_rating |
#                               salary$Number_of_Subjects %in% outliers_subjects), ]

# Check for skewness and kurtosis
skewness_values <- apply(salary, 2, skewness)
kurtosis_values <- apply(salary, 2, kurtosis)
print(skewness_values)
print(kurtosis_values)

# Create Q-Q plots for visual inspection of normality
windows(20, 12)
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
qqnorm(salary$Salary, main = "Q-Q Plot of Salary")
qqline(salary$Salary)
qqnorm(salary$Years, main = "Q-Q Plot of Years")
qqline(salary$Years)
qqnorm(salary$Rating, main = "Q-Q Plot of Rating")
qqline(salary$Rating)
qqnorm(salary$Number_of_Subjects, main = "Q-Q Plot of Number_of_Subjects")
qqline(salary$Number_of_Subjects)

# Decide whether to include all variables in the model based on normality
# If any variable shows significant skewness, consider transformations

# Fit the multiple linear regression model
model <- lm(Salary ~ Years + Rating + Number_of_Subjects, data = salary)

# Display the summary of the model
summary(model)

# Check for multicollinearity using VIF
vif_values <- vif(model)
print(vif_values)

# Interpret VIF values
if(any(vif_values > 5)){
  print("Warning: High multicollinearity detected")
  print(vif_values[vif_values > 5])
} else {
  print("No multicollinearity issues detected")
}

# Attach the data frame to access variables directly
attach(salary)

# Fit a new model without 'Number_of_Subjects'
model_reduced <- lm(Salary ~ Years + Rating, data = salary)

# Display the summary of the reduced model
summary(model_reduced)

# Check for multicollinearity using VIF in the reduced model
vif_values_reduced <- vif(model_reduced)
print(vif_values_reduced)

# Interpret VIF values for the reduced model
if(any(vif_values_reduced > 5)){
  print("Warning: High multicollinearity detected in the reduced model")
  print(vif_values_reduced[vif_values_reduced > 5])
} else {
  print("No multicollinearity issues detected in the reduced model")
}

# Attach the data frame to access variables directly
attach(salary)

