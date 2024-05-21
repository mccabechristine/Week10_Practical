## Exercise 01 :  --------------------------------------------------------------
## A Department of Education analyst is interested in investigating the pay 
## structure of secondary school teachers. He believes there are 3 factors that 
## affect the salaries of teachers: years of experience; a rating of teaching 
## effectiveness given by a department inspector; and the numbers of different 
## subjects taught. A sample of 20 teachers produced the results given in the 
## salary dataset. 

# Load necessary libraries
install.packages("psych")
install.packages("e1071")
library(psych)
library(e1071)

# Read the dataset
salary <- read.csv("Salary.csv")

# Display the structure and first few rows of the dataset
str(salary)
head(salary)

# Drop the column 'CaseNum'
salary <- salary[, -which(names(salary) == "CaseNum")]
str(salary)
head(salary)

# Rename variable 'Number of Subjects' as 'Number_of_Subjects'
names(salary)[names(salary) == "Number.of.Subjects"] <- "Number_of_Subjects"
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

## Summary of Correlation Matrix
## Salary and Years: 0.868
## Salary and Rating: 0.547
## Salary and Number of Subjects: 0.952
## Years and Rating: 0.187
## Years and Number of Subjects: 0.804
## Rating and Number of Subjects: 0.563

## These correlations indicate that there is a strong positive relationship 
## between salary and the number of subjects taught, and a strong positive 
## relationship between salary and years of experience.

# Check for outliers using boxplots. Arrange plots in a 2x2 grid
par(mfrow = c(2, 2))
boxplot(salary$Salary, main="Salary", 
        sub=paste("Outliers:", boxplot.stats(salary$Salary)$out))
boxplot(salary$Years, main="Years of Experience", 
        sub=paste("Outliers:", boxplot.stats(salary$Years)$out))
boxplot(salary$Rating, main="Teaching Effectiveness Rating", 
        sub=paste("Outliers:", boxplot.stats(salary$Rating)$out))
boxplot(salary$Number_of_Subjects, main="Number of Subjects", 
        sub=paste("Outliers:", boxplot.stats(salary$Number_of_Subjects)$out))

## The boxplots indicate that there are no apparent outliers in the data, 
## based on the whiskers extending to the minimum and maximum values without 
## any points beyond them.

# Identify and remove outliers
outliers <- function(x) {
  return(x[!x %in% boxplot.stats(x)$out])
}
salary <- salary[salary$Salary %in% outliers(salary$Salary),]
salary <- salary[salary$Years %in% outliers(salary$Years),]
salary <- salary[salary$Rating %in% outliers(salary$Rating),]
salary <- salary[salary$Number_of_Subjects %in% outliers(salary$Number_of_Subjects),]

# Histograms for visual inspection of normality
par(mfrow = c(2, 2))
hist(salary$Salary, main="Salary", 
     xlab="Salary", breaks=10, col="blue")
hist(salary$Years, main="Years of Experience", 
     xlab="Years of Experience", breaks=10, col="blue")
hist(salary$Rating, main="Teaching Effectiveness Rating", 
     xlab="Teaching Effectiveness Rating", breaks=10, col="blue")
hist(salary$Number_of_Subjects, main="Number of Subjects", 
     xlab="Number of Subjects", breaks=10, col="blue")

# Check for normality using the e1071 library and qqnorm() function
normality_check <- function(x) {
  print(skewness(x))
  print(kurtosis(x))
  qqnorm(x)
  qqline(x)
}

normality_check(salary$Salary)
normality_check(salary$Years)
normality_check(salary$Rating)
normality_check(salary$Number_of_Subjects)

## The skewness and kurtosis values for the variables are as follows:
# Salary: Skewness: 0.235
#         Kurtosis: -1.177
# Years:  Skewness: 0.583
#         Kurtosis: -0.724
# Rating: Skewness: -0.343
#         Kurtosis: -1.161
# Number of Subjects: Skewness: 0.024
#                     Kurtosis: -1.369
# Interpretation:  Skewness close to 0 indicates that the data is 
# fairly symmetrical.
# Kurtosis close to 0 indicates a normal distribution. Negative kurtosis 
# indicates a flatter distribution than normal. None of the skewness values 
# are extreme, indicating that the distributions of these variables are fairly 
# symmetrical. The kurtosis values are negative, indicating that the 
# distributions are somewhat flatter than a normal distribution.

# Shapiro-Wilk test for normality
shapiro_test <- function(x) {
  return(shapiro.test(x))
}
shapiro_test(salary$Salary)
shapiro_test(salary$Years)
shapiro_test(salary$Rating)
shapiro_test(salary$Number_of_Subjects)

## The Shapiro-Wilk test results for each variable are as follows:
# Salary:
# W = 0.96011, p-value = 0.546
# Years:
# W = 0.93624, p-value = 0.2034
# Rating:
# W = 0.95076, p-value = 0.3788
# Number of Subjects:
# W = 0.93768, p-value = 0.2166
# Interpretation:  Shapiro-Wilk Test: The null hypothesis for the Shapiro-Wilk 
# test is that the data is normally distributed. 
# A p-value greater than 0.05 means we fail to reject the null hypothesis, 
# suggesting that the data is normally distributed.

# In this case, all p-values are greater than 0.05, indicating that we do not 
# have enough evidence to reject the null hypothesis for normality. 
# Thus, we can conclude that the variables do not significantly deviate 
# from a normal distribution.

# Compare models using AIC and BIC
model1 <- lm(Salary ~ Years + Rating + Number_of_Subjects, data = salary)
model2 <- lm(Salary ~ Years + Rating, data = salary)
model3 <- lm(Salary ~ Years, data = salary)

# Print AIC and BIC for each model
cat("Model 1: AIC =", AIC(model1), "BIC =", BIC(model1), "\n")
cat("Model 2: AIC =", AIC(model2), "BIC =", BIC(model2), "\n")
cat("Model 3: AIC =", AIC(model3), "BIC =", BIC(model3), "\n")


## Model Comparison Results
## Model 1 (Salary ~ Years + Rating + Number_of_Subjects):
## AIC = 84.36512
## BIC = 89.34379

## Model 2 (Salary ~ Years + Rating):
## AIC = 95.51505
## BIC = 99.49797

## Model 3 (Salary ~ Years):
## AIC = 112.9439
## BIC = 115.9311

# Interpretation: AIC (Akaike Information Criterion) and BIC (Bayesian 
# Information Criterion) are both used for model selection. 
# Lower values indicate a better-fitting model, taking into account the number 
# of parameters to avoid overfitting.

# Among the three models:
# Model 1 has the lowest AIC (84.36512) and BIC (89.34379) values, 
# indicating it is the best model among the three for predicting salary.

# Model 2 is the next best, but it has higher AIC and BIC values compared 
# to Model 1.
# Model 3 has the highest AIC and BIC values, suggesting it fits the data less 
# well than the other two models.

# Conclusion: Based on the AIC and BIC values, 
# Model 1 (Salary ~ Years + Rating + Number_of_Subjects) is the best model 
# for predicting salary. It provides the best balance between model fit 
# and complexity.

# Explore residuals for model 1
# Fit the selected model
model1 <- lm(Salary ~ Years + Rating + Number_of_Subjects, data = salary)

# Plot residuals vs. fitted values
plot(model1$fitted.values, residuals(model1), 
     main = "Residuals vs Fitted",
     xlab = "Fitted values", 
     ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q plot of residuals to check for normality
qqnorm(residuals(model1))
qqline(residuals(model1), col = "red")

# Histogram of residuals to check for normality
hist(residuals(model1), main = "Histogram of Residuals", 
     xlab = "Residuals", breaks = 10, col = "blue")

# Scale-Location plot (sqrt standardized residuals vs fitted values)
plot(model1$fitted.values, sqrt(abs(scale(residuals(model1)))), 
     main = "Scale-Location",
     xlab = "Fitted values", 
     ylab = "Sqrt |Standardized residuals|")
abline(h = 0, col = "red")

# Residuals vs Leverage plot to identify influential points
plot(model1, which = 5)

# Check for heteroscedasticity using Breusch-Pagan test
install.packages("lmtest")
library(lmtest)
bptest(model1)

## Residual Analysis Results
## Based on the residual analysis plots and the Breusch-Pagan test results:
  
## Residuals vs. Fitted Values Plot:
## The residuals appear randomly scattered around the horizontal line at 0. 
## This indicates that the assumption of linearity and homoscedasticity 
## (constant variance) is likely met.

## Normal Q-Q Plot:
## The points in the Q-Q plot lie approximately along the line, indicating 
## that the residuals are approximately normally distributed.


## Histogram of Residuals:
## The histogram shows a roughly bell-shaped distribution, further supporting 
## the normality of residuals.

## Scale-Location Plot:
## The spread of the standardized residuals appears relatively constant across 
## the range of fitted values, suggesting homoscedasticity.

## Residuals vs. Leverage Plot:
## There are no points with high leverage and large residuals, indicating no 
##significant influential points that could disproportionately affect the model.

## Breusch-Pagan Test for Heteroscedasticity:
## The test result (BP = 3.2835, p-value = 0.35) indicates that we fail 
## to reject the null hypothesis of homoscedasticity (constant variance). Therefore, there is no evidence of heteroscedasticity in the model.

## Conclusion
## The residual analysis and diagnostic tests suggest that the model assumptions
## are reasonably met:
## The residuals are approximately normally distributed.
## The variance of the residuals is constant (homoscedasticity).
## There are no significant outliers or influential points.
## The linear relationship between the predictors and the response variable 
## seems valid.

# Create a new data frame with predictor values for making predictions
new_data <- data.frame(
  Years = c(5, 10, 15),
  Rating = c(40, 60, 80),
  Number_of_Subjects = c(2, 3, 4)
)

# Make predictions using the model
predictions <- predict(model1, newdata = new_data, interval = "confidence")

# Print the predictions
print(predictions)

