#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

# Aim: We want to figure out whether professionals have more prestigious jobs than blue or white collar workers. 

# a) create a new variable "professional" by recoding the variable "type" so that professionals are coded 1, and blue and white collar workers are coded 0.

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# b) Run linear model with outcome=prestige, predictors=income, professional (continuous x dummy interaction)

model1 <- lm(Prestige$prestige ~ Prestige$income + Prestige$professional + Prestige$income:Prestige$professional)
summary(model1)

stargazer(model1)

# c) Write prediction equation

# y_hat_i = alpha + ß1 * income + ß2 * professional + ß3(income * professional)
# prestige = 21.142 + 0.0031* income + 37.7812*professional - 0.0023*income*professional
# Prediction equation for professionals: prestige = 21.142 + 0.0031* income + 37.7812*1 - 0.0023*income*1
# Prediction equation for non-professionals: prestige = 21.142 + 0.0031* income

# d) interpret coefficient for income
# For non-professionals, a one unit increase in income is associated with an increase of 0.0031 in prestige of their job, on average.

# e) interpret coefficient for professional
# At zero income, professionals have, on average, 37.78 more prestigious jobs than non-professionals. The interaction effect of professional status on income decreases the prestige by -0.0023.

# f) marginal effect of income - change in estimate of y (prestige for prof) with a 1,000$ increase in income
# prestige = 21.142 + 0.0031* 1000 + 37.7812*1 - 0.0023*income*professional

# for 0$
21.142 + 0.0031 * 0 + 37.7812 * 1 - 0.0023*0*1
#result = 58.9232
# for 1,000$
21.142 + 0.0031* 1000 + 37.7812*1 - 0.0023*1000*1
# result = 59.72

# Marginal change
59.7232 - 58.9232
#result = 0.8

# Check - marginal change of X1 on Y: ß1 + ß3 * X2
0.0031 + (-0.0023) * 1
# result = 0.00008
# for 1,000$: 0.00008 * 1000 = 0.8

#Intepretation: The marginal change in prestige for a 1,000$ increase in income for a professional is 0.8.

# g) marginal effect of income between professional and non-professional of 6,000$.
# prestige = 21.142 + 0.0031* 6000 + 37.7812*0 - 0.0032*income*0
# prestige = 21.142 + 0.0031* 6000 + 37.7812*1 - 0.0032*income*1

# Non-professionals:
21.142 + 0.0031* 6000 + 37.7812*0 - 0.00232*6000*0
# result = 39.742
# Professionals:
21.142 + 0.0031* 6000 + 37.7812*1 - 0.00232*6000*1
# result = 63.6032

63.6032 - 39.742
# result = 23.8612

# Check: Y_hat = ßo + ß2M + (ß1 + ß3M)X
# non-professional: 
21.142 + 37.7812 * 0 + (0.0031 - 0.0023 * 0) * 6000
21.142 + 0.0031 * 6000
# result = 39.742

# professional:
21.142 + 37.7812* 1 + (0.0031 - 0.0023 * 1) * 6000
21.142 + 37.7812 + (0.0031 - 0.0023) * 6000
# result: 63.7232

# Interpretation: A change from non-professional to professional status at an income of 6,000$ is associated with an increase in prestige of 23.8612.


# Question 2
# a) Hypothesis test
# H0: slope = 0, H1: slope != 0 

# t = beta1_hat / standard error
tstat1 <- 0.042 / 0.016
tstat1
result: 2.625

# degrees of freedom = n - k - 1
df <- 131 - 2 - 1
df

2*pt(tstat1, df, lower.tail=FALSE)

result: p-value: 0.00972002

# Interpretation: We can reject the null hypothesis that the presence of yard signs in a precinct does not affect vote share at a significant level of 0.05.

# b) 
# H0 = slope = 0, H1: slope != 0
tstat2 <- 0.042 / 0.013
tstat2
result: 3.230769

# degrees of freedom = n - k - 1
df <- 131 - 2 - 1
df

2*pt(tstat2, df, lower.tail=FALSE)

pvalue: 0.00156946

# Interpretation: We can reject the null hypothesis that being next to precincts with yard signs has no effect on vote share at a significance level of 0.05.

# c) interpret the constant:
# In precincts without proximity to lawn signs, the vote share of Ken Cuccinelli is 30.2% on average.

# d) Model fit
# The R2 value indicates how much variation in the dependent variable is explained by our model, the score of which fall between 0 to 1. The closer the score is to one, the higher the variation is explained by the model. Our R2 value of 0.094 indicates a low model fit, indicating that the effect of lawn signs is likely low compared to other factors in explaining the difference in vote share between Cuccinelli and McAuliff.
