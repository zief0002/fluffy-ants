##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(corrr)
library(patchwork)
library(performance)
library(tidyverse)



##################################################
### Read in data
##################################################

# Read in data and create dummy variable for outcome
grad = read_csv(file = "https://raw.githubusercontent.com/zief0002/fluffy-ants/main/data/graduation.csv") |>
  mutate(
    got_degree = if_else(degree == "Yes", 1, 0)
  )


# View data
grad



##################################################
### Proportion who graduate by ACT score
##################################################

# Obtain the proportion who obtain a degree for each ACT score
prop_grad = grad |> 
  group_by(act, degree) |> 
  summarize(N = n()) |> 
  mutate(
    Prop = N / sum (N)
  ) |>
  ungroup() |>
  filter(degree == "Yes")


#View
prop_grad


# Scatterplot
ggplot(data = prop_grad, aes(x = act, y = Prop)) +
  geom_point(aes(size = N)) +
  geom_smooth(aes(weight = N), method = "loess", se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Proportion who obtained a degree")



##################################################
### Logistic transformation example
##################################################

# Create w values and transformed values
example = tibble(
  x = seq(from = -4, to = 4, by = 0.01)  # Set up values
) |>
  mutate(
    Lambda = 1 / (1 + exp(-x))  # Transform using logistic function
  )


# View data
example


# Plot the results
ggplot(data = example, aes(x = x, y = Lambda)) +
  geom_line() +
  theme_light()



##################################################
### Odds
##################################################

# Odds of getting an A
0.7 / 0.3

# Log-odds of getting an A
log(0.7 / 0.3)


# Odds of MN Wild winning Stanley Cup
0.02 / 0.98

# Log-odds of MN Wild winning Stanley Cup
log(0.02 / 0.98)



##################################################
### Relationship between x and other transformations
##################################################

# Create odds and logit values
example = example |>
  mutate(
    Odds = Lambda / (1 - Lambda),  # Transform to odds
    Logits = log(Odds)
  )


# View data
example


# S-shaped curve (probabilities)
p1 = ggplot(data = example, aes(x = x, y = Lambda)) +
  geom_line() +
  theme_light() +
  ylab("Probabilities")

# Exponential growth curve (odds)
p2 = ggplot(data = example, aes(x = x, y = Odds)) +
  geom_line() +
  theme_light()

# Linear (log-odds)
p3 = ggplot(data = example, aes(x = x, y = Logits)) +
  geom_line() +
  theme_light()

p1 | p2 | p3



##################################################
### Fit binomial logistic model
##################################################

# Fit the model
glm.1 = glm(got_degree ~ 1 + act, data = grad, family = binomial(link = "logit"))


# Coefficients
coef(glm.1)



##################################################
### Back-transform coefficients to odds interpretation
##################################################

exp(coef(glm.1))



##################################################
### Plot of the fitted model - Odds
##################################################

# Plot the fitted equation
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {exp(-1.611 + 0.108*x)}
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted odds of obtaining a degree")



##################################################
### Back-transform intercept value to probability of obtaining degree
##################################################

# Predicted probability for students with ACT=0
exp(-1.61) / (1 + exp(-1.61))



##################################################
### Plot of the fitted model - Probability
##################################################

# Plot the fitted equation
ggplot(data = grad, aes(x = act, y = got_degree)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {exp(-1.611 + 0.108*x) / (1 + exp(-1.611 + 0.108*x))}
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted probability of obtaining a degree") +
  ylim(0, 1)



##################################################
### Model-level summaries
##################################################

glance(glm.1)


# Understanding null deviance
# Fit intercept-only model
glm.0 = glm(got_degree ~ 1, data = grad, family = binomial(link = "logit"))

# Compute deviance
-2 * logLik(glm.0)[[1]]



##################################################
### Model evidence
##################################################

aictab(
  cand.set = list(glm.0, glm.1),
  modnames = c("Intercept-Only", "Effect of ACT")
)




##################################################
### Pseudo R^2
##################################################

# Baseline residual deviance: 2722.6
# Model residual deviance: 2633.2

# Compute pseudo R-squared
(2722.6 - 2633.2) / 2722.6



##################################################
### Evaluate residuals
##################################################

# Obtain average fitted values and average residuals
out.1 = binned_residuals(glm.1)


# View binned residuals
out.1


# Residual plot
plot(out.1)







