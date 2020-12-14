
# Propensity score analysis with inverse probability weighting # 

library(tidyverse)
library(survival)
library(coxphw)
library(survminer)
library(survey)
library(tableone)

# Data ----

dat <- readRDS("XXX/haloperidol_sample.rds")

# Variables included in dat ----

## gr_var (logical) = TRUE if exposed to haloperidol in the first 48h of hospitalization
## wait_var (numeric) = time of follow-up
## status_var (logical) = TRUE if event present at the end of the follow-up
## other variables addressed (Table 1)

# Variables' names vector ----

vars_for_ps <- dat %>% select(-status_var, -wait_var) %>% colnames; vars_for_ps


# PS and weights estimation ---- 

ps <- glm(gr_var ~.,data = dat %>% select(all_of(vars_for_ps)), family = binomial())

ps_dat <- dat %>%
  mutate(psvalue = predict(ps, type = "response")) %>%
  mutate(weights_treat_inv = ifelse(test = gr_var==1, 
                                           yes = 1/psvalue, 
                                           no = 1/(1-psvalue))) %>%
  mutate(weights_treat = ifelse(test = gr_var==1, 
                                       yes = psvalue, 
                                       no = (1-psvalue))) %>% 
  mutate(weights_treat_inv_log = log(weights_treat_inv)) %>%
  mutate(weights_treat_log = log(weights_treat))


# Univariable cox regression ---- 

res_ps <- coxph(Surv(wait_var, status_var)~gr_var, data = ps_dat, weights = weights_treat_inv_log)
cox.zph(uni_deathmech)
summary(res_ps)


# K-M curves ----

fontsz <- 30

ps_plot <- ps_dat %>% mutate(gr_var = factor(as.character(gr_var), levels = c("TRUE", "FALSE")))

fit <- survfit(Surv(wait_var, status_var)~gr_var, data = ps_plot, weights = weights_treat_inv_log)
resplot <- ggsurvplot(fit, data = ps_plot,   
                      risk.table = T, break.time.by = 14, 
                      risk.table.y.text = FALSE, risk.table.fontsize = 7,
                      legend.title = "",legend = c("top"), 
                      legend.labs=c("Haloperidol","Non-exposed group"), 
                      censor = T,
                      xlab = "Days", ylab = "Probability of being event-free", 
                      conf.int = TRUE, conf.int.alpha = 0.05, #confidence intervals
                      palette = c("gray2", "gray"), 
                      ggtheme = theme_bw())

resplot$plot <- resplot$plot + theme(axis.text = element_text(size = fontsz-2),                                      
                                     axis.title.y = element_text(size = fontsz-1),
                                     axis.title.x = element_blank(),
                                     legend.text = element_text(size = fontsz))

resplot$table <- resplot$table + theme(panel.background = element_blank(),  
                                       axis.title.x = element_text(size = fontsz+1),
                                       axis.text.x = element_text(size = fontsz-2), 
                                       title = element_text(size = fontsz-5))
resplot


# SMD to check balance ----

psw <- svydesign(ids = ~ 1, data = ps_dat, weights = ~ weights_treat_inv_log) 

psres <- svyCreateTableOne(vars = vars, strata = "gr_var", data = psw, test = FALSE)
print(psres, smd = TRUE)


