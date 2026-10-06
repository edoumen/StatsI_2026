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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
y
mean(y)
sd(y)
se <- (sd(y) / sqrt(25))
se
n <- 25
n
margin <- (qt(0.95, df=n-1) * se)
margin
low <- mean(y) - margin
low
high <- mean(y) + margin
high

# Question 2

print("H0: the sample mean is not higher than the population mean of 100")
print("H1: The sample mean is higher than the population mean of 100")
t <- (mean(y) - 100) / se
t
df = 24
df
p <- pt(t, df, lower.tail = FALSE)
p
print("The p-value is greater than alpha-level 0.05, therefore we cannot reject the Null hypothesis. We do not find evidence that the average student IQ at the teacher's school is not higher than the national school average.")
#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
show(expenditure)

ggplot(data = expenditure, aes(x = X1,
                          y = Y, size = X2, 
                          color = X3)) +
  geom_point() +
  scale_size(range = c(1, 5), name = "Urban residence per 1000") +
  labs(title = "State Expenditures and Income",
       x = "Per Capita Personal Income",
       y = "Per Capita Housing Assistance Expenditure",
       color = "Financial security per 100,000") +
  theme_bw()

print("Housing assitance expenditure is higher in states with higher personal income. These states also show higher financial security. Lower income states show a higher number of urban residents per 1000.")

ggplot(data = expenditure, aes(x = Region,
                               y = Y)) +
  geom_point() +
  labs(title = "Housing Assistance Expenditure by Region",
       x = "Region",
       y = "Per capita housing assistance expenditure") +
  theme_bw()

print("On average, the Western region has the highest per capita housing assistance expenditure.")


ggplot(data = expenditure, aes(x = X1,
                              y = Y)) +
  geom_point() +
  labs(title = "Per Capita Housing Assistance Expenditure by Personal Income",
       x = "Per Capita Personal Income",
       y = "Per Capita Housing Assistance Expenditure") +
  theme_bw()

print("Housing assistance expenditure is highest on average in states with a personal income around the $2000 mark.")

ggplot(data = expenditure, aes(x = X1,
                               y = Y,
                               color = Region,
                               shape = as.factor(Region))) +
  geom_point() +
  labs(title = "Per Capita Housing Assistance Expenditure by Personal Income",
       x = "Per Capita Personal Income",
       y = "Per Capita Housing Assistance Expenditure",
       color = "Region",
       shape = "Region") +
  theme_bw()

print("Housing assistance expenditure is highest on average in states with a personal income around the $2000 mark and in the Western region.")
