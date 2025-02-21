##################################################
### Load libraries
##################################################

library(broom)
library(educate)
library(patchwork)
library(tidyverse)
library(tidyr)



##################################################
### Import data
##################################################

# Import data
pew = read_csv(file = "https://raw.githubusercontent.com/zief0002/fluffy-ants/main/data/pew.csv")




##################################################
### Model 1: Classical Framework of Evidence
##################################################

# Fit Model 1
lm.1 = lm(knowledge ~ 1 + news, data = pew)

# Coefficient-level output
tidy(lm.1)



##################################################
### Joint Probability Density
##################################################

# Compute joint density
dnorm(x = 60, mean = 50, sd = 10) * 
  dnorm(x = 65, mean = 50, sd = 10) * 
  dnorm(x = 67, mean = 50, sd = 10)

# Compute joint density: Shortcut
prod(dnorm(x = c(60, 65, 67), mean = 50, sd = 10))

# N(60, 10)
prod(dnorm(x = c(60, 65, 67), mean = 60, sd = 10))

#N(5, 2)
L1 = prod(dnorm(x = c(3, 8, 17), mean = 5, sd = 2))

#N(6, 6)
L2 = prod(dnorm(x = c(3, 8, 17), mean = 6, sd = 3))

L1
L2

prod(dnorm(x = c(3, 8, 17), mean = 10, sd = 4))




##################################################
### Computing and evaluating likelihood
##################################################

# L(mu=20, sigma = 4 | x and ~N)
prod(dnorm(x = c(30, 20, 24, 27), mean = 20, sd = 4))

# L(mu=25, sigma = 4 | x and ~N)
prod(dnorm(x = c(30, 20, 24, 27), mean = 25, sd = 4))

# Likelihood ratio
0.00001774012 / 0.0000005702554



##################################################
### Back to regression example
##################################################

# Get RSE for use in likelihood
glance(lm.1)

# Compute likelihood for lm.1
prod(dnorm(x = resid(lm.1), mean = 0, sd = 20.3))



##################################################
### Baseline model (intercept-only)
##################################################

# Fit Model 0
lm.0 = lm(knowledge ~ 1,                                     data = pew)
lm.1 = lm(knowledge ~ 1 +        education,                  data = pew)
lm.2 = lm(knowledge ~ 1 + male + education,                  data = pew)
lm.3 = lm(knowledge ~ 1 + male,                              data = pew)
lm.4 = lm(knowledge ~ 1 + male + education + male:education, data = pew)


# lm.0 vs lm.1 --- evaluates the uncontrolled effect of education
# lm.1 vs lm.2 --- evalues the partial effect of sex (male)
# lm.2 vs lm.3 --- evalues the partial effect of education


# Get RSE for use in likelihood
glance(lm.0)
glance(lm.1)

# Compute likelihood for lm.0
prod(dnorm(x = resid(lm.0), mean = 0, sd = 21.5))
prod(dnorm(x = resid(lm.1), mean = 0, sd = 20.3))

# Compute likelihood ratio
1.382925e-192 / 2.489266e-195



##################################################
### Log-likelihood
##################################################

# Log-likelihood for Model 0
log(2.489266e-195)

# Log-likelihood for Model 1
log(1.382925e-192)


# Compute log-likelihood for Model 0
logLik(lm.0)

# Compute likelihood for Model 0
exp(logLik(lm.0)[1])

# Compute log-likelihood for Model 1
logLik(lm.1)

exp(logLik(lm.1)[1])

# 
# # Difference in log-likelihoods
# log(1.680643e-20) - log(5.452323e-26)
# 
# # Equivalent to ln(LR)
# log(1.680643e-20 / 5.452323e-26)
# 
# # Exponentiate the difference in log-likelihoods to get LR
# exp(12.63865)



##################################################
### Shortcut: logLik()
##################################################

# Compute log-likelihood for Model 0
logLik(lm.0)

# Compute likelihood for Model 2
exp(logLik(lm.0)[1])

# Compute difference in log-likelihoods
logLik(lm.1)[1] - logLik(lm.0)[1]

# Compute LR
exp( logLik(lm.1)[1] - logLik(lm.0)[1] )



##################################################
### Likelihood Ratio Test for Nested Models (Model 0 vs. Model 1)
##################################################

# Compute chi-squared
-2 * (logLik(lm.0)[1] - logLik(lm.1)[1])

# Compute the deviance for Model 0
-2 * logLik(lm.0)[1]

# Compute the deviance for Model 1
-2 * logLik(lm.1)[1]

# Compute difference in deviances
896.2 - 883.5

# Compute p-value for X^2(1) = 12.66
1 - pchisq(q = 12.66, df = 1)

# Alternative method
pchisq(q = 12.66, df = 1, lower.tail = FALSE)



##################################################
### Using lrtest()
##################################################

# Load library
library(lmtest)

# LRT to compare Model 0 and Model 1
lrtest(lm.0, lm.1, lm.2)



##################################################
### Evaluating partial effect of news (after controlling fo education, male, and engagement)
##################################################

# Simple model
lm.2 = lm(knowledge ~ 1 + education + male + engagement, data = pew)

# Complex model
lm.3 = lm(knowledge ~ 1 + education + male + engagement + news, data = pew)

# Compute the difference in deviances between Model 1 and Model 2
-2 * logLik(lm.2)[1] - (-2 * logLik(lm.3)[1])


# Compute the difference in model complexity
6 - 5

# Compute p-value for X^2(1) = 8.924345
pchisq(q = 8.924345, df = 1, lower.tail = FALSE)


# LRT to compare Model 2 and Model 3
lrtest(lm.2, lm.3)



##################################################
### Evaluating interaction effect
##################################################

# Fit interaction model
lm.4 = lm(knowledge ~ 1 + education + male + engagement + news + news:education, data = pew)

# LRT to compare Model 3 and Model 4
lrtest(lm.3, lm.4)


##################################################
### Evaluate assumptions for Model 4
##################################################

# Create residual plots
residual_plots(lm.4)



##################################################
### Evaluate individual predictors (effect of education)
##################################################

# Fit full model 4
lm.4 = lm(knowledge ~ 1 + education + male + engagement + news + news:education, data = pew)

# Fit model without education
lm.4_education = lm(knowledge ~ 1 + male + engagement + news + news:education, data = pew)

# Carry out LRT
lrtest(lm.4_education, lm.4)


# Effect of male
lm.4 = lm(knowledge ~ 1 + education + male + engagement + news + news:education, data = pew)
lm.4_male = lm(knowledge ~ 1 + education + engagement + news + news:education, data = pew)
lrtest(lm.4_male, lm.4) # Carry out LRT


# Effect of engagement
lm.4 = lm(knowledge ~ 1 + education + male + engagement + news + news:education, data = pew)
lm.4_engage = lm(knowledge ~ 1 + education + male + news + news:education, data = pew)
lrtest(lm.4_engage, lm.4) # Carry out LRT


# Effect of new exposure (main-effect)
lm.4 = lm(knowledge ~ 1 + education + male + engagement + news + news:education, data = pew)
lm.4_news = lm(knowledge ~ 1 + education + male + engagement + news:education, data = pew)
lrtest(lm.4_news, lm.4) # Carry out LRT


# Effect of interaction
lm.4 = lm(knowledge ~ 1 + education + male + engagement + news + news:education, data = pew)
lm.4_interaction = lm(knowledge ~ 1 + education + male + engagement + news, data = pew)
lrtest(lm.4_interaction, lm.4) # Carry out LRT


# Intercept
lm.4 = lm(knowledge ~ 1 + education + male + engagement + news + news:education, data = pew)
lm.4_intercept = lm(knowledge~ 0 + education + male + engagement + news + news:education, data = pew)
lrtest(lm.4_intercept, lm.4) # Carry out LRT


tidy(lm.4) |>
  mutate(
    statistic = c(11.38, 14.62, 12.80, 17.94, 7.45, 5.15),
    p.value = c(0.0007417, 0.0001314, 0.0003466, 0.0000228, 0.006351, 0.02324)
  ) 


##################################################
### Table of regression results
##################################################

# Load library
library(texreg)

# Create the table
htmlreg(
  l = list(lm.0, lm.1, lm.2, lm.3),
  stars = numeric(0),    #No p-value stars
  digits = 3,
  padding = 20,          #Add space around columns (you may need to adjust this via trial-and-error)
  include.adjrs = FALSE, #Omit Adjusted R^2
  include.nobs = FALSE,  #Omit sample size
  include.rmse = TRUE,   #Include RMSE
  custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 4"),
  custom.coef.names = c("Intercept", "Total Enrollment", "Full-time Students",
                        "Full-time Faculty", "Nonresident Tuition",
                        "Student/Faculty Ratio", "Percent Ph.D. Students", "Undergraduate GPA", 
                        "GRE Score", "Ph.D. Acceptance Rate", "Total Publications", "Total Research"),
  #custom.note = "Note. (L) = Linear effect. (Q) = Quadratic effect.",
  reorder.coef = c(2:12, 1), #Put intercept at bottom of table
  custom.gof.rows = list(
    `$k$` = c(2, 6, 11, 13), # Add parameters
    `$\\chi^2$` = c(NA, 25.43, 45.10, 71.12),  # Add X^2 values
    `$p$` = c("", "<.001", "<.001", "<.001")
  ),
  reorder.gof = c(3, 4, 5, 1, 2),
  caption.above = TRUE, #Move caption above table
  inner.rules = 1, #Include line rule before model-level output
  outer.rules = 1,  #Include line rules around table
  caption = "Table 2: Coefficients (and standard errors) for four models evaluating predictors of institutional prestige. The $\\chi^2$-values, number of parameters (*k*), and associated *p*-values are also reported from the likelihood ratio tests."
)


##################################################
### Testing individual predictors From a model
##################################################

# Evaluate total enrollment predictor
# Fit Model 1a
lm.1a = lm(peer_rating ~ 1 +          ft_students + ft_fac + nonres_tuition, data = usnwr_complete)
#lm.1 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition, data = usnwr_complete)

lrtest(lm.1a, lm.1) # Carry out LRT


# Evaluate effect of number of full-time students
lm.1b = lm(peer_rating ~ 1 + enroll +               ft_fac + nonres_tuition, data = usnwr_complete)
#lm.1 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition, data = usnwr_complete)

lrtest(lm.1b, lm.1)


# Evaluate effect of number of full-time faculty
lm.1c = lm(peer_rating ~ 1 + enroll + ft_students +          nonres_tuition, data = usnwr_complete)
#lm.1 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition, data = usnwr_complete)

lrtest(lm.1c, lm.1)


# Evaluate effect of tuition
lm.1d = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac                 , data = usnwr_complete)
#lm.1 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition, data = usnwr_complete)
lrtest(lm.1d, lm.1)




