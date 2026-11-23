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

# c) Write prediction equation

# prestige = 21.142 + 0.0031* income + 37.7812*professional - 0.0032*income*professional

# d) interpret coefficient for income
# Controlling job type, a one unit increase in income is associated with an increase of 0.0031 in prestige of their job.

# e) interpret coefficient for professional
# Controlling for income, professionals have, on average, 37.78 more prestigious jobs than non-professionals.

# f) marginal effect of income - change in estimate of y (prestige for prof) with a 1,000$ increase in income
# prestige = 21.142 + 0.0031* 1000 + 37.7812*1 - 0.0032*income*professional

21.142 + 0.0031* 1000 + 37.7812*1 - 0.00232*1000*1
# result = 59.70

#Intepretation:

# g) marginal effect of income between professional and non-professional of 6,000$.
# prestige = 21.142 + 0.0031* 6000 + 37.7812*0 - 0.0032*income*professional
# prestige = 21.142 + 0.0031* 6000 + 37.7812*1 - 0.0032*income*professional

21.142 + 0.0031* 6000 + 37.7812*0 - 0.00232*6000*0
# result = 39.742
21.142 + 0.0031* 6000 + 37.7812*1 - 0.00232*6000*1
# result = 63.6032

63.6032 - 39.742
# result = 23.8612

# Interpretation:


# Question 2
# a) Hypothesis test
# H0 = slope = 0, H1 = slope unequal 0 
tstat1 <- 0.042 / 0.016
result: 2.625

df <- 131 - 1

2*pt(tstat1, df, lower.tail=FALSE)

result: p-value: 0.009703405

# Interpretation: We can reject the null hypothesis ......

# b) 
tstat2 <- 0.042 / 0.013
tstat2
result: 3.230769

df <- 131 - 1

2*pt(tstat2, df, lower.tail=FALSE)

pvalue: 0.001563955

# Interpretation: 

# c) interpret the constant:
# In precincts without proximity to lawn signs, the vote share of Ken Cuccinelli is 30.2% on average.

# d) Model fit
# The R2 value of 0.094 indicates a low model fit, indicating that the effect of lawn signs is likely low compared to other factors in explaining the difference in vote share between Cuccinelli and McAuliff.
