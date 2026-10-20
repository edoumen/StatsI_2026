# Question No. 1

# Question 1 a)

# First I calculate the expected frequencies

fe_upperc_not_stopped <- (27*21)/42
fe_upperc_bribe <- (27 * 13)/42
fe_upperc_warning <- (27 * 8)/42
fe_lowerc_not_stopped <- (15 * 21)/42
fe_lowerc_bribe <- (15 * 13)/42
fe_lowerc_warning <- (15 * 8)/42

# Here I enter the observed frequencies

fo_upperc_not_stopped <- 14
fo_upperc_bribe <- 6
fo_upperc_warning <- 7
fo_lowerc_not_stopped <- 7
fo_lowerc_bribe <- 7
fo_lowerc_warning <- 1

# Next I calculate each part of the chi squared function to be summed up, so the full equation will look neater

UC1 <- (fo_upperc_not_stopped - fe_upperc_not_stopped)^2/fe_upperc_not_stopped
UC2 <- (fo_upperc_bribe - fe_upperc_bribe)^2/fe_upperc_bribe
UC3 <- (fo_upperc_warning - fe_upperc_warning)^2/fe_upperc_warning
LC1 <- (fo_lowerc_not_stopped - fe_lowerc_not_stopped)^2/fe_lowerc_not_stopped
LC2 <- (fo_lowerc_bribe - fe_lowerc_bribe)^2/fe_lowerc_bribe
LC3 <- (fo_lowerc_warning - fe_lowerc_warning)^2/fe_lowerc_warning

# Finally, I sum up the above values to get the chi squared test statistic result

drivers_chi_squared <- sum(UC1, UC2, UC3, LC1, LC2, LC3)

# Question 1 b)

# I use an R command to calculate the p-value after deriving the degrees of freedom
df <- (2 -1) * (3 - 1)
pchisq(q = drivers_chi_squared, df = df , lower.tail = FALSE)

# I conclude as follows:

print("Because the p-value 0.15 is larger than the alpha level 0.1, we fail to reject the hypothesis that class is independent of police treatment in traffic.")

# Question 1 c)

# First, I calculate the row and column proportions

UC_row_prop <- 27/42
LC_row_prop <- 15/42
Col_prop_not_stopped <- 21/42
Col_prop_bribe <- 13/42
Col_prop_warning <- 8/42

#Then I calculate the residuals for each cell as follows:

UC1_res <- (fo_upperc_not_stopped - fe_upperc_not_stopped) / sqrt(fe_upperc_not_stopped * (1 - UC_row_prop) * (1 - Col_prop_not_stopped))
UC2_res <- (fo_upperc_bribe - fe_upperc_bribe) / sqrt(fe_upperc_bribe * (1-UC_row_prop) * (1-Col_prop_bribe))
UC3_res <- (fo_upperc_warning - fe_upperc_warning) / sqrt(fe_upperc_warning * (1-UC_row_prop) * (1-Col_prop_warning))
LC1_res <- (fo_lowerc_not_stopped - fe_lowerc_not_stopped) / sqrt(fe_lowerc_not_stopped * (1-LC_row_prop) * (1-Col_prop_not_stopped))
LC2_res <- (fo_lowerc_bribe - fe_lowerc_bribe) / sqrt(fe_lowerc_bribe * (1-LC_row_prop) * (1-Col_prop_bribe))
LC3_res <- (fo_lowerc_warning - fe_lowerc_warning) / sqrt(fe_lowerc_warning * (1-LC_row_prop) * (1-Col_prop_warning))

#Question 1 d)

print("The standardized residuals for the not-stopped-column are 0.3/-0.3, indicating support for the null hypothesis of independence. The bribe-column standardized residuals are -1.6/1.6, and for the warning-column 1.5/-1.5, indicating a possible mild association.")

#Question 2

#First I import the dataset from the online csv file

wpolicy <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
summary(wpolicy)
head(wpolicy)
tail(wpolicy)

# Question 2 a) Hypotheses

print("H0: The reservation policy's effect on number of new or repaired drinking water facilities equals zero.")
print("H1: The reservation policy's effect on the number of new or repaired drinking water facilities does not equal zero.")

# Question 2 b) Bivariate Regression

plot(x=wpolicy$reserved, y=wpolicy$water)
cor.test(wpolicy$reserved, wpolicy$water)
model <- lm(water~reserved, data = wpolicy)
model
abline(model)

summary(model)
9.252/3.948

print("p-value < 0.05, we estimate the slope coefficient is statistically significant, providing evidence against the null hypothesis that the effect of reservation policy on new or repaired drinking water facilities equals zero.")

# Question 2 c)

print("The estimated coefficients imply that, in the absence of reservation policy, on average, 14.738 new or repaired water facilities are present. In the presence of reservation policy 9.252 additional new or repaired water facilities are available.")


