

# Crude analysis #

library(tidyverse)
library(survival)
library(coxphw)
library(survminer)
library(optmatch)
library(compareGroups)

# Data ----

dat <- readRDS("XXX/haloperidol_sample.rds")

# Variables included in dat ----

## gr_var (logical) = TRUE if exposed to haloperidol in the first 48h of hospitalization
## wait_var (numeric) = time of follow-up
## status_var (logical) = TRUE if event present at the end of the follow-up
## other variables addressed (Table 1)


# Variables' names vector ----

vars_for_match <- dat %>% select(-status_var, -wait_var) %>% colnames; vars_for_match


# Matching algorithm ---- 

datm <- dat %>% mutate(gr_var = ifelse(gr_var=="TRUE", 1, 0)) ## pairmatch() requires numeric 
pm <- pairmatch(gr_var ~., controls = 4, data = datm %>% select(all_of(vars_for_match)))

match_dat <- dat
match_dat$mgroup <- pm[match(rownames(dat), names(pm))]
match_dat <- match_dat %>% filter(!is.na(mgroup)) ## sample with matched data


# Frequency table --- 

tbcontrol <- compareGroups(gr_var ~ ., data = match_dat %>% select(all_of(vars_for_match)))
restable <- createTable(tbcontrol, show.ratio = F, hide.no = "no")
restable


# SMD to check balance ---- 

matchres <- CreateTableOne(vars = vars, strata = "gr_var", data = match_dat, test = FALSE)
print(matchres, smd = TRUE)


# Univariated Cox regression if PH assumption met ----

res <- coxph(Surv(wait_var, status_var)~gr_var, data = match_dat) ## model
summary(res) ## summary



# Weighted univariated Cox regression if PH assumption not met ----

resw <- coxphw(Surv(wait_var, status_var) ~ gr_var, data = match_dat, template = "AHR") ## model
summary(resw) ## summary



# Check for outliers ---- 

univ_dat <- dat %>% mutate(residuals = residuals(res, "deviance")) # deviance residuals
summary(residuals(res, "deviance")) # exploring residuals

no_out <- univ_dat %>% filter(residuals <= 2.5, residuals >=-2.5) # sample with residuals < -2.5 & < 2.5


# KM-curves ---- 

fontsz <- 30

match_plot <- match_dat %>% mutate(gr_var = factor(as.character(gr_var), levels = c("TRUE", "FALSE")))

fit <- survfit(Surv(wait_var, status_var)~gr_var, data = match_plot)

resplot2 <- ggsurvplot(fit, data = match_plot, 
                       risk.table = TRUE, break.time.by = 14, 
                       risk.table.y.text = FALSE, risk.table.fontsize = 7,
                       legend.title = "",legend = c("top"), 
                       legend.labs=c("Haloperidol","Non-exposed matched group"), 
                       censor = T,
                       xlab = "Days", ylab = "Probability of being event-free", 
                       conf.int = TRUE, conf.int.alpha = 0.07, #confidence intervals
                       palette = c("gray2", "gray"), 
                       ggtheme = theme_bw())

resplot2$plot <- resplot2$plot + theme(axis.text = element_text(size = fontsz-2),                                      
                                       axis.title.y = element_text(size = fontsz-1),
                                       axis.title.x = element_blank(),
                                       legend.text = element_text(size = fontsz))

resplot2$table <- resplot2$table + theme(panel.background = element_blank(),  
                                         axis.title.x = element_text(size = fontsz+1),
                                         axis.text.x = element_text(size = fontsz-2), 
                                         title = element_text(size = fontsz-5))

resplot2

