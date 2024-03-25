library(tidyverse)
library(geepack)
data(package = "geepack", dietox)
dietox$Cu     <- as.factor(dietox$Cu)
mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
gee1 <- geepack::geeglm(mf, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
res <- sumReg(gee1)

