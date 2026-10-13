# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2"),  pkgTest)

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check working directory
getwd()

# Agenda
# (a.) Correlation
# (b.) Bivariate regression 

# -------------------------------#
# a. Correlation
# -------------------------------#

df <- read.csv("../../datasets/fictional_data.csv")
View(df)

# Create a scatter plot 
ggplot(data = df, aes(x = income, y = edu)) +
  geom_point()

plot(df$edu, df$income)
# Calculate correlation
cor(df$income, df$edu)
# Add correlation to scatter plot
text(1.5, 2600, sprintf("Correlation=%s", round(cor(df$edu, df$income), 4)))
# Improve visualization with detailed axis and main labels, save as PDF

# Run a t-test for the correlation coefficient
cor.test(df$edu, df$income)
# Check p-value
sprintf("%.20f", 7.52e-07)
# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# -------------------------------#
# b. Bivariate regression
# -------------------------------#

# Question: Is there a relationship between education and income?
# Model: income = b0 + b1 * education
# Save model as object
summary(lm(income ~ edu, data = df))
model <-(lm(income~edu, data=df))
# Output interpretation:
#  - Intercept (b0)
#  - Slope (b1)

# Conduct t-test for the slope of a regression line
summary(model)
250.64/33.06
# Check p-value
sprintf("%.20f", 2.17e-06)
# Create a residual plot
model.res <- resid(model)
model.res
plot(df$edu, model.res,
     ylab = "Residuals",
     xlab = "University education (in years)")
abline(0,0)
# no pattern as x increases - homoscedastic situation - we havent failed to capture a pattern in our model

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Calculate 95% and 99% confidence intervals around your estimated coefficients
confint(model, level=0.95)
confint(model, level=0.99)
# Create a scatter plot w/ educate and income, add regression line
plot(x=df$edu, y=df$income)
abline(model)

# What is the prediction equation?
summary(model)
#predicted values should be called hat 
# Make predictions for first observation in df
head(df)
976.16 + 250.64 * 1
model$fitted.values
# Make predictions for a range of x values (specifically min to max of education)
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)))
# Add standard errors for your predicted values
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), se.fit=TRUE)
# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
predict(model, newdata=data.frame(edu = 15), se.fit=TRUE) # se gets very big here, meaning we are not very precise in estimating the population value, our data frame has only data from 1-8 so our predictions start getting more unreliable at a point after that
# Rembmer: more variability in individual responses --> wider intervals

# Make predictions for x values not in data
# (1) Mean education
# (2) Only unique values of x

# Now, plot those predictions in base R

# Plot, regression line with confidence intervals
# First, save confidence intervals
# Second, create scatter plot
# Then, add regression line, lower bound, and upper bound

# And now try with ggplot2