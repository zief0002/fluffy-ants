##################################################
### Load libraries
##################################################

library(tidyverse)    #Plotting, wrangling, basically everything (Loads dplyr, ggplot2, readr, and others)
library(broom)        #Fitted regression results, creating residuals
library(corrr)        #Correlations
library(educate)      #Evaluate residual plots; NEED VERSION 0.3.01 (or higher)
library(patchwork)    #For layout with more than one plot
library(skimr)        #To plot/describe many variables simultaneously




##################################################
### Import data
##################################################

pew = read_csv(file = "https://raw.githubusercontent.com/zief0002/benevolent-anteater/main/data/pew.csv")


# View data
pew


##################################################
### Explore Outcome
##################################################

# Density plot of outcome
ggplot(data = pew, aes(x = knowledge)) +
  geom_density() +
  theme_light() +
  xlab("Political knowledge") +
  ylab("Probability density")

# Numerical Summaries

pew |> 
  summarize(
    M = mean(knowledge),
    SD = sd(knowledge)
  )


##################################################
### Explore Focal Predictor
##################################################

# Density plot
ggplot(data = pew, aes(x = news)) +
  geom_density() +
  theme_light() +
  xlab("News exposure") +
  ylab("Probability density")

# Numerical Summaries
pew |> 
  summarize(
    M = mean(news),
    SD = sd(news)
  )


##################################################
### Explore Covariates
##################################################

# Dummy code party attribute
pew = pew |>
  mutate(
    democrat = if_else(party == "Democrat", 1, 0),
    republican = if_else(party == "Republican", 1, 0)
  )

pew

# Compute quick summaries
pew |>
  select(age:republican, -party) |>
  skim()


##################################################
### Explore Relationships
##################################################

# Scatterplot
ggplot(data = pew, aes(x = news, y = knowledge)) +
  geom_point(size = 4, shape = 21, color = "black", fill = "skyblue") +
  theme_light() +
  xlab("News exposure") +
  ylab("Political knowledge")

# Correlation
pew |> 
  select(knowledge, news) |>
  correlate()


# Scatterplots between all Numeric attributes
# Scatterplot matrix
pew |>
  select(-id, -party) |>
  pairs()

# Correlations between all Numeric attributes
pew |> 
  select(-id, -party) |>
  correlate()



##################################################
### Evaluate RQ 1
##################################################

# Fit model
lm.1 = lm(knowledge ~ 1 + news, data = pew)

# model-level output
glance(lm.1)

# Coefficient-level output
tidy(lm.1)

# Assumptions
# Need educate and patchwork libraries loaded
residual_plots(lm.1)



##################################################
### Evaluate RQ 2
##################################################

lm.2 = lm(knowledge ~ 1 + age + educ + male + engagement + ideology + democrat + republican + state_blue + news, data = pew)

# model-level output
glance(lm.2)

# Coefficient-level output
tidy(lm.2)

# Assumptions
residual_plots(lm.2)



##################################################
### Evaluate RQ 3
##################################################

lm.3 = lm(knowledge ~ 1 + age + educ + male + engagement + ideology + democrat + republican + 
            state_blue + news + news:educ, data = pew)

# model-level output
glance(lm.3)

# Coefficient-level output
tidy(lm.3)

# Assumptions
residual_plots(lm.3)



##################################################
### Plot interaction effect
##################################################

ggplot(data = pew, aes(x = news, y = knowledge)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 32.28, slope = 0.17, color = "#2dd7f8", linewidth = 1.5) + #HS education
  geom_abline(intercept = 49.12, slope = 0.09, color = "#6d92ee", linewidth = 1.5) + #UG education
  theme_light() +
  xlab("News exposure") +
  ylab("Political knowledge")




