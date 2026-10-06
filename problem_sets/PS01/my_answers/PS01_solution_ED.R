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

# Question 1

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

class(y)

print("H0: the sample mean is not higher than the population mean of 100")
print("H1: The sample mean is higher than the population mean of 100")
t <- (mean(y) - 100) / se
t
df = 24
df
p <- pt(t, df, lower.tail = FALSE)
p
print("The p-value is greater than the alpha-level 0.05, therefore we fail to reject the Null hypothesis that the average student IQ at the teacher's school is not higher than the national school average.")
#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
show(expenditure)

# Question 1

# Y, X1, X2, X3 in one plot
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

# Plot 1: Y + X1
ggplot(data = expenditure, aes(x = X1,
                               y = Y)) +
  geom_point() +
  labs(title = "Per Capita Housing Assistance Expenditure by Personal Income",
       x = "Per Capita Personal Income",
       y = "Per Capita Housing Assistance Expenditure") +
theme_bw()
print("The scatterplot implies a positive linear association between per capita housing assistance expenditure and per capita personal income in the states.")

# Plot 2: Y + X2
ggplot(data = expenditure, aes(x = X2,
                               y = Y)) +
  geom_point() +
  labs(title = "Per Capita Housing Assistance Expenditure and Financial Security",
       x = "Financially Secure Residents per 100,000",
       y = "Per Capita Housing Assistance Expenditure") +
  theme_bw()

print("The scatterplot shows a slightly U-shaped distribution, showing more housing assistance expenditure in areas with financially stable residents <200 and >400 per 100,000.")

# Plot 3: Y + X3
ggplot(data = expenditure, aes(x = X3,
                               y = Y)) +
  geom_point() +
  labs(title = "Per Capita Housing Assistance Expenditure and Urban Residence",
       x = "Number of residents in urban areas per 1,000",
       y = "Per Capita Housing Assistance Expenditure") +
  theme_bw()

print("The scatterplot shows a positive linear distribution of the number of urban area residents and the per capita ousing assistance expenditure.")

# Plot 4: X1 + X2
ggplot(data = expenditure, aes(x = X1,
                               y = X2)) +
  geom_point() +
  labs(title = "Personal Income and Financial Security",
       x = "Per Capita Personal Income",
       y = "Financially secure residents per 100,000") +
  theme_bw()

print("The scatterplot shows no association between per capita personal income and financially secure residents per 100,000.")

# Plot 5: X1 + X3
ggplot(data = expenditure, aes(x = X1,
                               y = X3)) +
  geom_point() +
  labs(title = "Personal Income and Urban residency",
       x = "Per Capita Personal Income",
       y = "Number of residents in urban areas per 1,000") +
  theme_bw()

print("The scatterplot shows a positive linear association between per capita personal income and the number of residents in urban areas per 1,000.")

# Plot 6:
ggplot(data = expenditure, aes(x = X3,
                               y = X2)) +
  geom_point() +
  labs(title = "Urban Area Residence and Financial Security",
       x = "Number of residents in urban areas per 1,000",
       y = "Financially stable residents per 100,000") +
  theme_bw()

print("The scatterplot shows no association between the number of urban area residents per 1,000 and the number of financially stable residents per 100,000.")

# Question 2

levels(expenditure$Region)
expenditure$Region <- as.factor(expenditure$Region)
levels(expenditure$Region) <- c("Northeast", "North Center", "South", "West")

ggplot(data = expenditure, aes(x = Region,
                               y = Y)) +
  geom_point() +
  labs(title = "Housing Assistance Expenditure by Region",
       x = "Region",
       y = "Per capita housing assistance expenditure") +
  theme_bw()

print("On average, the Western region has the highest per capita housing assistance expenditure.")

# Question 3

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
                               color = (Region),
                               shape = (Region))) +
  geom_point() +
  labs(title = "Per Capita Housing Assistance Expenditure by Personal Income",
       x = "Per Capita Personal Income",
       y = "Per Capita Housing Assistance Expenditure",
       color = "Region",
       shape = "Region") +
  theme_bw()

print("Housing assistance expenditure is highest on average in states with a personal income around the $2000 mark and in the Western region.")
