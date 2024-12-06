##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(educate)
library(patchwork)
library(tidyverse)



##################################################
### Import data
##################################################

mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/benevolent-anteater/main/data/mn-schools.csv")
mn



##################################################
### Relationship between graduation rates and median SAT scores
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")


lm.1 = lm(grad ~ 1 + sat, data = mn)
residual_plots(lm.1)






##################################################
### Fit base-2 log regression model
##################################################

lm.log2 = lm(grad ~ 1 + log(sat, base = 2), data = mn)



##################################################
### Evaluate residuals for avg. residual = 0 (linearity)
##################################################

# Obtain residuals for log-transformed SAT
residual_plots(lm.log2)



##################################################
### Interpret regression output
##################################################

# Raw SAT
glance(lm.log2) |>
  print(width = Inf) # Model-level output

tidy(lm.log2)        # Coefficient-level output



##################################################
### Understanding slope
##################################################

#Augsburg
#Raw SAT = 10.3
#L2SAT = log2(10.3) = 3.36

# Compute predicted grad rate
-306.7 + 106.4 * 3.36


# Compute predicted grad rate for 1-unit change in L2SAT
-306.7 + 106.4 * 4.36



##################################################
### Base-10 log SAT
##################################################

lm.log10 = lm(grad ~ 1 + log(sat, base = 10), data = mn)


# Model-level output
glance(lm.log10) |>
  print(width = Inf)


# Coefficient-level output
tidy(lm.log10)


#Augsburg
#Raw SAT = 10.3
#L10SAT = log10(10.3) = 1.01

# Compute predicted grad rate
-306.7 + 353.58 * 1.01


# Compute predicted grad rate for 1-unit change in L10SAT
-306.7 + 353.58 * 2.01



##################################################
### Compare residuals
##################################################

# Residual plots for base-2 model
p1 = residual_plots(lm.log2)

# Residual plots for base-10 model
p2 = residual_plots(lm.log10)

# Layout
p1 / p2


# Why? Augsburg
# Base-2 fitted value
-306.7 + 106.4392 * log(10.3, base = 2)

# Base-10 fitted value
-306.7 + 353.5833 * log(10.3, base = 10)

# Check augment() for both models, the fitted and residual values are identical



##################################################
### Natural logarithm (Base-e) of SAT
##################################################

# Fit regression model
lm.ln = lm(grad ~ 1 + log(sat), data = mn)


# Model-level output
glance(lm.ln) |>
  print(width = Inf)


# Coefficient-level output
tidy(lm.ln)


# Obtain residuals for base-2 log-transformed SAT
residual_plots(lm.ln)



##################################################
### Interpreting slope from natural log-transformed model
##################################################

# Compute predicted values for three SAT scores that are 1% different
-306.7055 + 153.5593 * log(c(10, 10.1, 10.201))


# Difference between predicted value for 10.201 and 10.1
49.9338 - 48.4058


# Difference between predicted value for 10.201 and 10.1
49.9338 - 48.4058


# Compute exact effect
153.6 * log(1.01)



##################################################
### Plot the fitted curves
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  geom_function(
    fun = function(x) {-306.7 + 153.6*log(x)}
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Select functional form
##################################################

# Fit models
lm.quad = lm(grad ~ 1 + sat + I(sat ^ 2), data = mn)
lm.log = lm(grad ~ 1 + log(sat), data = mn)


# Table of model evidence
aictab(
  cand.set = list(lm.quad, lm.log),
  modnames = c("Quadratic polynomial", "Log-transformed SAT")
)


# Evaluate residuals
p1 = residual_plots(lm.log)
p2 = residual_plots(lm.quad)

# Layout
p1 / p2



##################################################
### Add covariate to model
##################################################

# Fit model
lm.2 = lm(grad ~ 1 + public + log(sat), data = mn)


# Model-level output
glance(lm.2) |>
  print(width = Inf)


# Coefficient-level output
tidy(lm.2)   



##################################################
### Plot the fitted curves
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  geom_function( # Private schools
    fun = function(x) {-286.1 + 146.0*log(x)},
    color = "#6d92ee",
    linetype = "solid"
  ) +
  geom_function( # Public schools
    fun = function(x) {-294.6 + 146.0*log(x)},
    color = "#fe932d",
    linetype = "dashed"
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Information criteria
##################################################

# Fit models (use natural log of SAT)
lm.1 = lm(grad ~ 1, data = mn)
lm.2 = lm(grad ~ 1 + log(sat), data = mn)
lm.3 = lm(grad ~ 1 + log(sat) + public, data = mn)


# Table of model evidence
aictab(
  cand.set = list(lm.1, lm.2, lm.3),
  modnames = c("Intercept-only", "Ln(SAT)", "Ln(SAT) and Sector")
)







