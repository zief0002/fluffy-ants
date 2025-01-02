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
### Read in and prepare data
##################################################

# Import data
usnwr = read_csv(file = "https://raw.githubusercontent.com/zief0002/benevolent-anteater/main/data/usnwr-2024.csv") |>
  drop_na()

# View data
usnwr



##################################################
### Fit candidate models
##################################################

lm.0 = lm(peer_rating ~ 1, data = usnwr)
lm.1 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition, data = usnwr)
lm.2 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition + 
            sf_ratio + pct_doc + ug_gpa + gre + doc_acc, data = usnwr)
lm.3 = lm(peer_rating ~ 1 + enroll + ft_students + ft_fac + nonres_tuition + 
            sf_ratio + pct_doc + ug_gpa + gre + doc_acc + 
            tot_pubs + tot_res, data = usnwr)



##################################################
### Akiake's Information Criteria (AIC)
##################################################

# Compute AIC for model associated with Hypothesis 1
# logLik(lm.1)
-2*-45.44917 + 2*6



##################################################
### Use AIC() and glance() functions
##################################################

AIC(lm.0) #Model 0
AIC(lm.1) #Model 1
AIC(lm.2) #Model 2
AIC(lm.3) #Model 3


# AIC available in glance() output
glance(lm.3)



##################################################
### AIC Second-Order Corrected (Corrected AIC)
##################################################

n = 83
k = 13

# Compute AICc for Model 3
-2 * logLik(lm.3)[[1]] + 2 * k * n / (n - k - 1)


# Shortcut with function
AICc(lm.0) #Model 0
AICc(lm.1) #Model 1
AICc(lm.2) #Model 2
AICc(lm.3) #Model 3



##################################################
### Delta-AICc values
##################################################

AICc(lm.0) - AICc(lm.3)  #Model 1
AICc(lm.1) - AICc(lm.3)  #Model 1
AICc(lm.2) - AICc(lm.3)  #Model 2
AICc(lm.3) - AICc(lm.3)  #Model 3



##################################################
### Relative likelihood
##################################################

exp(-1/2 * 114.5283) #Model 0
exp(-1/2 * 98.04722) #Model 1
exp(-1/2 * 65.56467) #Model 2
exp(-1/2 * 0).       #Model 3



##################################################
### Evidence ratios
##################################################

1 / 1.350503e-25           #Model 3 vs Model 0
5.79179e-15 / 5.120551e-22 #Model 2 vs Model 1



##################################################
### Model probabilities (Akaike Weight)
##################################################

# Compute sum of relative likelihoods
sum_rel = 1.350503e-25 + 5.120551e-22 + 5.79179e-15 + 1


# Compute model probability for each model
1.350503e-25 / sum_rel #Model 0
5.120551e-22 / sum_rel #Model 1
5.79179e-15 / sum_rel  #Model 2
1 / sum_rel            #Model 3



##################################################
### Table of model evidence
##################################################

#Create table of model evidence
model_evidence = aictab(
  cand.set = list(lm.0, lm.1, lm.2, lm.3), 
  modnames = c("Model 0", "Model 1", "Model 2", "Model 3")
)


# View output
model_evidence



##################################################
### Pretty printing tables of model evidence
##################################################

# Create data frame to format into table
tab_01 = model_evidence %>%
  data.frame() %>%
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
    table.width = pct(50)
  ) |>
  tab_footnote(
    footnote = html("Rel(&#8466;) = Relative likelihood"),
    locations = cells_column_labels(columns = ModelLik)
  )






