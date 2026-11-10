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
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

# Question 1
# 1.1.

model_1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model_1)

# 1.2.

plot(x = inc.sub$difflog, y = inc.sub$voteshare,
     main = "Influence of Campaign Spending on Vote Share",
     xlab = "Difference in Campaign Spending",
     ylab = "Vote Share of the Incumbent") +
abline(model_1, col = "red")

# 1.3.

model_1_res <- residuals(model_1)
model_1_res

# 1.4.
# Prediction Equation: yhat = alphahat0 + betahat1x = 0.58 + 0.04x ? 

# Question 2
# 2.1.

model_2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model_2)

# 2.2.

plot(x = inc.sub$difflog, y = inc.sub$presvote,
     main = "Influence of Campaign Spending on Pres. Candidate Vote Share",
     xlab = "Difference in Campaign Spending",
     ylab = "Vote Share of Inc. Pres. Candidate") +
  abline(model_2, col = "red")

# 2.3. 

model_2_res <- residuals(model_2)
model_2_res

# 2.4.
#Prediction Equation Y = 0.51 + 0.02x ?

# Question 3
# 3.1.

model_3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model_3)

#3.2.

plot(x = inc.sub$presvote, y = inc.sub$voteshare,
     main = "Association between Pres. Candidate Vote Share and Incumbent Electoral Success",
     xlab = "Incumbent Presidential Candidate Vote Share",
     ylab = "Incumbent Party Vote Share") +
  abline(model_3, col = "red")

# 3.3. Prediction equation Y = 0.44 + 0.39x

# Question 4
# 4.1.
model_res <- lm(model_1_res ~ model_2_res)
summary(model_res)

# 4.2.

plot(x = model_2_res, y = model_1_res,
     main = "Variation in Vote Share Unexplained by Spending",
     xlab = "Incumbent Presidential Candidate Vote Share",
     ylab = "Incumbent Party Vote Share") +
  abline(model_res, col = "red")

# 4.3.
# Prediction Equation: Y = -5.934e-18 + 2-569e-01x ????

# Question 5
# 5.1.

model_5 <- lm(inc.sub$voteshare ~ inc.sub$difflog + inc.sub$presvote)
summary(model_5)

# 5.2.
# Prediction Equation: Y = 0.45 + 0.04x1 + 0.26x2

#5.3.
# The coefficients for the incumbent presidential candidate's vote share are identical in the models of Question 4 and Question 5. 
# This is because the regression model in Q4 controls for the effect of difference in campaign spending by regressing the residuals of the models of Q1 and Q2.
# As the effect of difference in campaign spending has been removed from the model in Q4, we can compute the effect of the incumbent presidential candidate's vote share on the vote share of the party.
# Hence, the estimated beta2 coefficient for the incumbent presidential candidate's vote share that is displayed in the output of the multivariate regression of Q5 is identical to the estimated beta1 of the model of Q4 that presents the effect of presidential candidate vote share by controlling for difference in campaign spending.
