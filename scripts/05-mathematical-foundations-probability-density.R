##################################################
### Load libraries
##################################################

library(broom)
library(tidyverse)



##################################################
### Probability density
##################################################

# Compute the probability density of x=65 in N(50,10)
(1 / (10 * sqrt(2 * pi))) * exp(-(225) / 200)


# Compute the probability density of x=65 in N(50,10)
dnorm(x = 65, mean = 50, sd = 10)


# Compute the probability density of x=65 in N(30,20)
dnorm(x = 65, mean = 30, sd = 20)



##################################################
### pnorm(): Find area under curve
##################################################

# Compute the p-value based on z=2.5
2 * pnorm(q = -2.5, mean = 0, sd = 1)


# Compute the p-value based on z=2.5
2 * pnorm(q = -2.5, mean = 0, sd = 1)



##################################################
### qnorm(): Compute quantiles
##################################################

# Find the quantile that has a cumulative density of 0.5 in the N(50, 10) distribution
qnorm(p = 0.5, mean = 50, sd = 10)



##################################################
### rnorm(): Generate random observations
##################################################

# Generate 15 observations from N(50,10)
set.seed(100)
rnorm(n = 15, mean = 50, sd = 10)



##################################################
### Student's t-distribution
##################################################

dt(x = 2.5, df = 30)
pt(q = -2.5, df = 30)
qt(p = .025, df = 30)
rt(n = 10, df = 30)



##################################################
### F-distribution
##################################################

df(x = 2.5, df1 = 5, df2 = 25)
1 - pf(q = 2, df1 = 5, df2 = 25) # Compute the p-value based on F(5,25)=2
qf(p = .975, df1 = 5, df2 = 25)
rf(n = 10, df1 = 5, df2 = 25)



##################################################
### Creating a PDF and Adding Shading in a ggplot
##################################################

# Create F-value and compute probability densities
fig_09 = data.frame(
  X = seq(from = 0, to = 10, by = 0.01)
) %>% 
  mutate(
    Y = df(x = X, df1 = 5, df2 = 25)
  )


# View data
head(fig_09)


ggplot(data = fig_09, aes(x = X, y = Y)) +
  geom_line() +
  xlab("F") +
  ylab("Probability density") +
  theme_light()


# Filter data included in the shaded region
shade_09 = fig_09 %>%
  filter(X >= 2)


# View data
head(shade_09)


# Create plot
ggplot(data = fig_09, aes(x = X, y = Y))  +
  geom_ribbon(data = shade_09, ymin = 0, aes(x = X, ymax = Y), 
              color = "#bbbbbb", alpha = 0.4) +
  geom_line() +
  xlab("F") +
  ylab("Probability density") +
  theme_light()



##################################################
### Import data
##################################################

city = read_csv(file = "https://raw.githubusercontent.com/zief0002/fluffy-ants/main/data/riverview.csv")


# View data
head(city)


# Count number of rows (n)
nrow(city)



##################################################
### Fit regression model
##################################################

lm.1 = lm(income ~ 1 + education + seniority, data = city)



##################################################
### Examine regression output
##################################################

# Model-level output
glance(lm.1)


# Partition the variation
anova(lm.1)


# Coefficient-level output
tidy(lm.1, conf.int = TRUE)



##################################################
### Model-level inference
##################################################

# Partition SS
4147.3 + 722.9  #SS_model
1695.3          #SS_residual
4870.2 + 1695.3 #SS_Total


# Compute R^2
48070.2 / 6565.5


# Convert R^2 to F
0.742 / (1 - 0.742) * 29 / 2



##################################################
### Compute F from mean squares
##################################################

# Compute MS_model
(4147330492 + 722883649) / (1 + 1)


# Compute MS_error
1695313285 / 29


# Compute F
2435107070 / 58459079



##################################################
### Test model-level null hypothesis
##################################################

# Compute p-value for F(2,29) = 41.7
1 - pf(41.7, df1 = 2, df2 = 29)


# p-value for F(2,29)=41.7
pf(41.7, df1 = 2, df2 = 29, lower.tail = FALSE)


##################################################
### Residual standard error / Root mean square error
##################################################

# Compute RSE/RMSE
sqrt(58.5)



##################################################
### Coefficient-level inference
##################################################

# Compute t-statistic for effect of education
2252 / 335


# Compute p-value for t(29) = 6.72
2 * pt(q = -6.72, df = 29)


# alternatively
2 * pt(q = 6.72, df = 29, lower.tail = FALSE)



##################################################
### Confidence intervals
##################################################

# Find 0.975th quantile
qt(p = 0.975, df = 29)

2.25 - 2.04523 * 0.335 #Lower limit
2.25 + 2.04523 * 0.335 #Upper limit









