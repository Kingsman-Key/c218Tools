

# geepack tidier test -------------------------


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
      sumReg(model = ., n2 = 3, toClip = F)
  })

})


# stats lm tidier test ------------------------


testthat::test_that("basic test on warning", {
  ## test on n2 is empty
  # test on geeglm package
  testthat::expect_warning({
    model <- lm("Sepal.Length~ Sepal.Width", data = iris)
    regTab <- c218Tools::sumReg(model = model, n1 = 1)
  })

  testthat::expect_warning({
    model <- lm("Sepal.Length~ Sepal.Width", data = iris)
    regTab <- c218Tools::sumReg(model = model, n2 = 3)
  })
})

testthat::test_that("basic test on line number", {
  ## test on n2 is empty
  # test on geeglm package
  testthat::expect_equal({
    model <- lm("Sepal.Length~ Sepal.Width", data = iris)
    res <- c218Tools::sumReg(model = model)
    nrow(res)
  }, expected = 1L)

  testthat::expect_equal({
    model <- lm("Sepal.Length~ Sepal.Width +Petal.Length+Petal.Width", data = iris)
    res <- c218Tools::sumReg(model = model, n1 = 1, n2 = 2)
    nrow(res)
  }, expected = 2L)

  testthat::expect_equal({
    model <- lm("Sepal.Length~ Sepal.Width +Petal.Length+Petal.Width", data = iris)
    res <- c218Tools::sumReg(model = model)
    nrow(res)
  }, expected = 1L)
  testthat::expect_equal({
    df <- mtcars
    df$carb <- as.factor(df$carb)
    model <- lm("disp~ carb", data = df)
    res <- c218Tools::sumReg(model = model)
    nrow(res)
  }, expected = length(levels(df$carb)))
})
