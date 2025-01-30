##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(educate)
library(patchwork)
library(tidyverse)
library(tidyr)


##################################################
### Import data
##################################################

# Import data
mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/fluffy-ants/main/data/mn-schools.csv")

# View data
mn



##################################################
### Fit candidate models
##################################################

lm.0 = lm(grad ~ 1, data = mn)
lm.1 = lm(grad ~ 1 + sat, data = mn)
lm.2 = lm(grad ~ 1 + sat + I(sat^2), data = mn)



##################################################
### Akiake's Information Criteria (AIC)
##################################################

# Compute AIC for model associated with Hypothesis 1
# logLik(lm.1)
-2*-113.5472 + 2*3



##################################################
### Use AIC() and glance() functions
##################################################

AIC(lm.0) #Model 0
AIC(lm.1) #Model 1
AIC(lm.2) #Model 2


# AIC available in glance() output
glance(lm.2)



##################################################
### AIC Second-Order Corrected (Corrected AIC)
##################################################

n = 33
k = 4

# Compute AICc for Model 3
-2 * logLik(lm.2)[[1]] + 2 * k * n / (n - k - 1)


# Shortcut with function
AICc(lm.0) #Model 0
AICc(lm.1) #Model 1
AICc(lm.2) #Model 2



##################################################
### Delta-AICc values
##################################################

AICc(lm.0) - AICc(lm.2)  #Model 0
AICc(lm.1) - AICc(lm.2)  #Model 1
AICc(lm.2) - AICc(lm.2)  #Model 2



##################################################
### Relative likelihood
##################################################

exp(-1/2 * 54.45462) # Model 0
exp(-1/2 * 5.37686)  # Model 1
exp(-1/2 * 0)        # Model 2



##################################################
### Evidence ratios
##################################################

1 / 1.497373e-12  #Model 2 vs Model 0
1 / 6.798761e-02  #Model 2 vs Model 1



##################################################
### Model probabilities (Akaike Weight)
##################################################

# Compute sum of relative likelihoods
sum_rel = 1.497373e-12 + 6.798761e-02 + 1


1.497373e-12 / sum_rel  # Model 0
6.798761e-02 / sum_rel  # Model 1
1 / sum_rel             # Model 2



##################################################
### Table of model evidence
##################################################

#Create table of model evidence
model_evidence = aictab(
  cand.set = list(lm.0, lm.1, lm.2), 
  modnames = c("Model 0", "Model 1", "Model 2")
)


# View output
model_evidence



##################################################
### RQ 3
##################################################

# Fit models
lm.3 = lm(grad ~ 1 + public, data = mn)
lm.4 = lm(grad ~ 1 + public + sat, data = mn)
lm.5 = lm(grad ~ 1 + public + sat + I(sat^2), data = mn)


#Create table of model evidence
model_evidence = aictab(
  cand.set = list(lm.3, lm.4, lm.5), 
  modnames = c("Model 3", "Model 4", "Model 5")
)


# View output
model_evidence



##################################################
### RQ 4
##################################################

# Fit models
lm.6 = lm(grad ~ 1 + public + sat + public:sat, data = mn)
lm.7 = lm(grad ~ 1 + public + sat + I(sat^2) + public:sat, data = mn)
lm.8 = lm(grad ~ 1 + public + sat + I(sat^2) + public:sat + public:I(sat^2), data = mn)


#Create table of model evidence
model_evidence = aictab(
  cand.set = list(lm.4, lm.5, lm.6, lm.7, lm.8), 
  modnames = c("Model 4", "Model 5", "Model 6", "Model 7", "Model 8")
)


# View output
model_evidence



##################################################
### Table of model evidence --- All models
##################################################

#Create table of model evidence
model_evidence = aictab(
  cand.set = list(lm.0, lm.1, lm.2, lm.3, lm.4, lm.5, lm.6, lm.7, lm.8),
  modnames = c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4", 
               "Model 5", "Model 6", "Model 7", "Model 8")
)


# View output
model_evidence



##################################################
### Pretty printing tables of model evidence
##################################################

# Create data frame to format into table
tab_01 = model_evidence |>
  data.frame() |>
  select(-LL, -Cum.Wt)


# View table
tab_01


# Create table with gt()
tab_01 |>
  gt() |>
  cols_align(
    columns = c(Modnames),
    align = "left"
  ) |>
  cols_align(
    columns = c(K, AICc, Delta_AICc, ModelLik, AICcWt),
    align = "center"
  ) |>
  cols_label(
    Modnames = md("*Model*"),
    K = md("*k*"),
    AICc = md("*AICc*"),
    Delta_AICc = html("&#916;AICc"),
    ModelLik = html("Rel(&#8466;)"),
    AICcWt = md("*AICc Wt.*")
  ) |>
  tab_options(
    table.width = pct(70)
  ) |>
  tab_footnote(
    footnote = html("Rel(&#8466;) = Relative likelihood. Values are displayed using scientific notation."),
    locations = cells_column_labels(columns = ModelLik)
  )






