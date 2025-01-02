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
usnwr = read_csv(file = "https://raw.githubusercontent.com/zief0002/benevolent-anteater/main/data/usnwr-2024.csv")

# View data
usnwr

usnwr_complete = usnwr |>
  drop_na()



##################################################
### Model 1: Classical Framework of Evidence
##################################################

# Fit Model 1
lm.1 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition, data = usnwr_complete)

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

# Compute likelihood for Model 1
prod(dnorm(x = resid(lm.1), mean = 0, sd = 0.432))

# Fit Model 0
lm.0 = lm(peer_rating ~ 1, data = usnwr_complete)

# Get RSE for use in likelihood
glance(lm.0)

# Compute likelihood for lm.2
prod(dnorm(x = resid(lm.0), mean = 0, sd = 0.491))

# Compute likelihood ratio
1.680643e-20 / 5.452323e-26



##################################################
### Log-likelihood
##################################################

# Log-likelihood for Model 0
log(5.452323e-26)

# Log-likelihood for Model 1
log(1.680643e-20)

# Difference in log-likelihoods
log(1.680643e-20) - log(5.452323e-26)

# Equivalent to ln(LR)
log(1.680643e-20 / 5.452323e-26)

# Exponentiate the difference in log-likelihoods to get LR
exp(12.63865)



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
116.3347 - 90.89834

# Compute p-value for X^2 = 25.44
1 - pchisq(q = 25.44, df = 4)

# Alternative method
pchisq(q = 25.44, df = 4, lower.tail = FALSE)



##################################################
### Using lrtest()
##################################################

# Load library
library(lmtest)

# LRT to compare Model 0 and Model 1
lrtest(lm.0, lm.1, lm.2)



##################################################
### Evaluating predictors in Block 2
##################################################

# Fit Model 2
lm.2 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition + 
            sf_ratio + pct_doc + ug_gpa + gre + doc_acc, data = usnwr_complete)

# Coefficient-level output
glance(lm.2)


# Compute the difference in deviances between Model 1 and Model 2
-2 * logLik(lm.1)[1] - (-2 * logLik(lm.2)[1])

# Compute the difference in model complexity
11 - 6


# Compute p-value for X^2(5) = 45.0956
pchisq(q = 45.0956, df = 5, lower.tail = FALSE)


# LRT to compare Model 1 and Model 2
lrtest(lm.1, lm.2)



##################################################
### Evaluating predictors in Block 3
##################################################

# Fit Model 3
lm.3 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition + 
            sf_ratio + pct_doc + ug_gpa + gre + doc_acc + 
            tot_pubs + tot_res, data = usnwr_complete)

# Coefficient-level output
glance(lm.3)

# LRT to compare Model 1 and Model 2
lrtest(lm.2, lm.3)



##################################################
### Evaluate assumptions for Model 3
##################################################

# Create residual plots
residual_plots(lm.3)

# Identify outlying institution
augment(lm.3) |>
  mutate(
    school = usnwr_complete$school
  ) |>
  filter(.std.resid > 3) |>
  print(width = Inf)



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




