library(survival)
res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
sumReg(res.cox)
