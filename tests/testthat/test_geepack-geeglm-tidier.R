


## test warnings

testthat::test_that("basic test on warning", {
  ## test on n2 is empty
  testthat::expect_warning({
    data(package = "geepack", dietox)
    dietox$Cu     <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3))) %>% as.formula
    gee1 <- geepack::geeglm(mf, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
    gee1 %>%
      c218Tools::sumReg(model = ., n1 = 1, toClip = F)
  })

  ## test on n1 is empty
  testthat::expect_warning({
    lm("Sepal.Length~ Sepal.Width", data = iris) %>%
      c218Tools::sumLM(model = ., n2 = 1, toClip = F)
  })
})




