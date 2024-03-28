


## test warnings

testthat::test_that("basic test on warning", {
  ## test on n2 is empty
  # test on geeglm package
  testthat::expect_warning({
    library(geepack)
    URL <- "http://static.lib.virginia.edu/statlab/materials/data/depression.csv"
    dat <- read.csv(URL, stringsAsFactors = TRUE)
    dat$id <- factor(dat$id)
    dat$drug <- relevel(dat$drug, ref = "standard")
    with(dat, tapply(depression, list(diagnose, drug, time), mean)) %>%
      ftable() %>%
      round(2)
    gee1 <- geeglm(depression ~ diagnose + drug*time,
                           data = dat,
                           id = id,
                           family = binomial,
                           corstr = "independence")
    res <- gee1 %>%
      sumReg(model = ., n1 = 1, toClip = F)
  })
})








