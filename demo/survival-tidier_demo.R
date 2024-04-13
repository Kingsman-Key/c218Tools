

data(cancer, package="survival")
res.cox <- survival::coxph(survival::Surv(time, status) ~ sex, data = lung)
sumReg(res.cox, pType = "value")


