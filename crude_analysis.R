
# Crude analysis #

library(tidyverse)
library(survival)
library(coxphw)
library(survminer)

# Data ----

dat <- readRDS("XXX/haloperidol_sample.rds")

# Variables included in dat ----

## gr_var (logical) = TRUE if exposed to haloperidol in the first 48h of hospitalization
## wait_var (numeric) = time of follow-up
## status_var (logical) = TRUE if event present at the end of the follow-up


# Test proportional hazards (PH) assumtion ---- 

res_ph <- coxph(Surv(wait_var, status_var)~gr_var, data = dat)
cox.zph(res_ph) ## PH test


# Univariated Cox regression if PH assumption met ----

res <- coxph(Surv(wait_var, status_var)~gr_var, data = dat) ## model
summary(res) ## summary


# Weighted univariated Cox regression if PH assumption not met ----

resw <- coxphw(Surv(wait_var, status_var)~gr_var, data = datl, template = "AHR") ## model
summary(resw) ## summary


# K-M curves ----

fontsz <- 30

plot <- dat %>% mutate(gr_var = factor(as.character(gr_var), levels = c("TRUE", "FALSE")))

fit <- survfit(Surv(wait_var, status_var)~gr_var, data = plot)

resplot0 <- ggsurvplot(fit, data = plot,   
                       risk.table = TRUE, break.time.by = 14, 
                       risk.table.y.text = FALSE, risk.table.fontsize = 7,
                       legend.title = "",legend = c("top"), 
                       legend.labs=c("Haloperidol","Non-exposed group"), 
                       censor = T,
                       xlab = "Days", ylab = "Probability of being event-free", 
                       conf.int = TRUE, conf.int.alpha = 0.07, #confidence intervals
                       palette = c("gray2", "gray"), 
                       ggtheme = theme_bw())

resplot0$plot <- resplot0$plot + theme(axis.text = element_text(size = fontsz-2),                                      
                                       axis.title.y = element_text(size = fontsz-1),
                                       axis.title.x = element_blank(),
                                       legend.text = element_text(size = fontsz))

resplot0$table <- resplot0$table + theme(panel.background = element_blank(),  
                                         axis.title.x = element_text(size = fontsz+1),
                                         axis.text.x = element_text(size = fontsz-2), 
                                         title = element_text(size = fontsz-5)
)

resplot0


# Check for outliers ---- 

univ_dat <- dat %>% mutate(residuals = residuals(res, "deviance")) # deviance residuals
summary(residuals(res, "deviance")) # exploring residuals

no_out <- univ_dat %>% filter(residuals <= 2.5, residuals >=-2.5) # sample with residuals < -2.5 & < 2.5

