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
salary_data <- salary[, -which(names(salary) == "CaseNum")]

# Rename variable 'Number of Subjects' as 'Number_of_Subjects'
names(salary)[names(salary) == "Number.of.Subjects"] <- "Number_of_Subjects"

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

# Check for outliers using boxplots. Arrange plots in a 2x2 grid
par(mfrow = c(2, 2))
boxplot(salary_data$Salary, main="Salary", sub=paste("Outliers:", boxplot.stats(salary_data$Salary)$out))
boxplot(salary_data$Years, main="Years of Experience", sub=paste("Outliers:", boxplot.stats(salary_data$Years)$out))
boxplot(salary_data$Rating, main="Teaching Effectiveness Rating", sub=paste("Outliers:", boxplot.stats(salary_data$Rating)$out))
boxplot(salary_data$Number_of_Subjects, main="Number of Subjects", sub=paste("Outliers:", boxplot.stats(salary_data$Number_of_Subjects)$out))

# Identify and remove outliers
outliers <- function(x) {
  return(x[!x %in% boxplot.stats(x)$out])
}
salary_data <- salary_data[salary_data$Salary %in% outliers(salary_data$Salary),]
salary_data <- salary_data[salary_data$Years %in% outliers(salary_data$Years),]
salary_data <- salary_data[salary_data$Rating %in% outliers(salary_data$Rating),]
salary_data <- salary_data[salary_data$Number_of_Subjects %in% outliers(salary_data$Number_of_Subjects),]

# Histograms for visual inspection of normality
par(mfrow = c(2, 2))
hist(salary_data$Salary, main="Salary", xlab="Salary", breaks=10, col="blue")
hist(salary_data$Years, main="Years of Experience", xlab="Years of Experience", breaks=10, col="blue")
hist(salary_data$Rating, main="Teaching Effectiveness Rating", xlab="Teaching Effectiveness Rating", breaks=10, col="blue")
hist(salary_data$Number_of_Subjects, main="Number of Subjects", xlab="Number of Subjects", breaks=10, col="blue")

# Check for normality using the e1071 library and qqnorm() function
normality_check <- function(x) {
  print(skewness(x))
  print(kurtosis(x))
  qqnorm(x)
  qqline(x)
}

normality_check(salary_data$Salary)
normality_check(salary_data$Years)
normality_check(salary_data$Rating)
normality_check(salary_data$Number_of_Subjects)

# Shapiro-Wilk test for normality
shapiro_test <- function(x) {
  return(shapiro.test(x))
}
shapiro_test(salary_data$Salary)
shapiro_test(salary_data$Years)
shapiro_test(salary_data$Rating)
shapiro_test(salary_data$Number_of_Subjects)

# Compare models using AIC and BIC
model1 <- lm(Salary ~ Years + Rating + Number_of_Subjects, data = salary_data)
model2 <- lm(Salary ~ Years + Rating, data = salary_data)
model3 <- lm(Salary ~ Years, data = salary_data)

# Print AIC and BIC for each model
cat("Model 1: AIC =", AIC(model1), "BIC =", BIC(model1), "\n")
cat("Model 2: AIC =", AIC(model2), "BIC =", BIC(model2), "\n")
cat("Model 3: AIC =", AIC(model3), "BIC =", BIC(model3), "\n")


