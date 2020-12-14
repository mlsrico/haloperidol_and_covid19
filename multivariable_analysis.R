
# Multivariable analysis # 

library(tidyverse)
library(survival)
library(coxphw)
library(car)

# Data ----

dat <- readRDS("XXX/haloperidol_sample.rds")

# Variables included in dat ----

## gr_var (logical) = TRUE if exposed to haloperidol in the first 48h of hospitalization
## wait_var (numeric) = time of follow-up
## status_var (logical) = TRUE if event present at the end of the follow-up
## other variables addressed (Table 1)

# Variables' names vector ----

vars_for_multi <- dat %>% colnames; vars_for_multi


# Frequency table --- 

tbcontrol <- compareGroups(gr_var ~ ., data = dat %>% select(all_of(vars_for_multi)))
restable <- createTable(tbcontrol, show.ratio = F, hide.no = "no")
restable


# SMD to compare exposed vs non-exposed ---- 

compres <- CreateTableOne(vars = vars, strata = "gr_var", data = dat %>% select(all_of(vars_for_multi)), test = FALSE)
print(compres, smd = TRUE)


# Test proportional hazards (PH) assumtion ---- 

res_ph <- coxph(Surv(wait_var, status_var)~., 
                data = dat %>% select(all_of(vars_for_multi)))
cox.zph(res_ph) ## PH test


# Univariated Cox regression if PH assumption met ----

res <- coxph(Surv(wait_var, status_var)~gr_var, data = dat) ## model
summary(res) ## summary


# Weighted univariated Cox regression if PH assumption not met ----

resw <- coxphw(Surv(wait_var, status_var)~., template = "AHR", 
               data = dat %>% select(all_of(vars_for_multi))) ## model
summary(resw) ## summary

# VIF ----
vif(res_multi)


# Check for outliers ---- 

univ_dat <- dat %>% mutate(residuals = residuals(res, "deviance")) # deviance residuals
summary(residuals(res, "deviance")) # exploring residuals

no_out <- univ_dat %>% filter(residuals <= 2.5, residuals >=-2.5) # sample with residuals < -2.5 & < 2.5

