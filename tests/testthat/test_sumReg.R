# =========================================================
# sumReg.lm tests
# =========================================================

testthat::test_that("sumReg.lm: warning when only n1 or n2 is set", {
  testthat::expect_warning({
    model <- lm("Sepal.Length ~ Sepal.Width", data = iris)
    c218Tools::sumReg(model = model, n1 = 1)
  })
  testthat::expect_warning({
    model <- lm("Sepal.Length ~ Sepal.Width", data = iris)
    c218Tools::sumReg(model = model, n2 = 3)
  })
})

testthat::test_that("sumReg.lm: default returns 1 row for numeric exposure", {
  model <- lm("Sepal.Length ~ Sepal.Width", data = iris)
  res <- c218Tools::sumReg(model = model)
  testthat::expect_equal(nrow(res), 1L)
})

testthat::test_that("sumReg.lm: n1/n2 both set returns correct number of rows", {
  model <- lm("Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width", data = iris)
  res <- c218Tools::sumReg(model = model, n1 = 1, n2 = 2)
  testthat::expect_equal(nrow(res), 2L)
})

testthat::test_that("sumReg.lm: factor exposure returns level-count rows", {
  df <- mtcars
  df$carb <- as.factor(df$carb)
  model <- lm("disp ~ carb", data = df)
  res <- c218Tools::sumReg(model = model)
  testthat::expect_equal(nrow(res), length(levels(df$carb)))
})

testthat::test_that("sumReg.lm: no error with formula as string", {
  testthat::expect_no_error({
    model <- lm("Sepal.Length ~ Sepal.Width", data = iris)
    c218Tools::sumReg(model = model)
  })
})

# Bug 2 fix: regressionTableOnly = FALSE should not error
testthat::test_that("sumReg.lm: regressionTableOnly = FALSE returns named list without error", {
  testthat::expect_no_error({
    model <- lm("Sepal.Length ~ Sepal.Width", data = iris)
    res <- c218Tools::sumReg(model = model, regressionTableOnly = FALSE)
    testthat::expect_type(res, "list")
    testthat::expect_true("regressionTable" %in% names(res))
    testthat::expect_true("outcomeCategory" %in% names(res))
    testthat::expect_true("tableName" %in% names(res))
  })
})

# Bug 1 fix: n1 and n2 both set should not cause "object not found" error
testthat::test_that("sumReg.lm: no error when n1 and n2 are both manually set", {
  testthat::expect_no_error({
    model <- lm("Sepal.Length ~ Sepal.Width + Petal.Length", data = iris)
    c218Tools::sumReg(model = model, n1 = 2, n2 = 3)
  })
})

testthat::test_that("sumReg.lm: output columns correct for latex mark mode", {
  model <- lm("Sepal.Length ~ Sepal.Width", data = iris)
  res <- c218Tools::sumReg(model = model, latex = TRUE, pType = "mark")
  testthat::expect_equal(ncol(res), 2L)
})

testthat::test_that("sumReg.lm: output columns correct for value mode", {
  model <- lm("Sepal.Length ~ Sepal.Width", data = iris)
  res <- c218Tools::sumReg(model = model, pType = "value")
  testthat::expect_equal(ncol(res), 3L)
})

testthat::test_that("sumReg.lm: output columns correct for excel mark mode", {
  model <- lm("Sepal.Length ~ Sepal.Width", data = iris)
  res <- c218Tools::sumReg(model = model, latex = FALSE, pType = "mark")
  testthat::expect_equal(ncol(res), 2L)
})


# =========================================================
# sumReg.glm tests
# =========================================================

testthat::test_that("sumReg.glm: no error on binary outcome", {
  testthat::expect_no_error({
    mydata <- read.csv(system.file("extdata", "binary.csv", package = "c218Tools"))
    mydata$rank <- factor(mydata$rank)
    mylogit <- glm("admit ~ gre + gpa + rank", data = mydata, family = "binomial")
    c218Tools::sumReg(model = mylogit)
  })
})

testthat::test_that("sumReg.glm: default returns 1 row for numeric exposure", {
  mydata <- read.csv(system.file("extdata", "binary.csv", package = "c218Tools"))
  mydata$rank <- factor(mydata$rank)
  mylogit <- glm("admit ~ gre + gpa + rank", data = mydata, family = "binomial")
  res <- c218Tools::sumReg(model = mylogit)
  testthat::expect_equal(nrow(res), 1L)
})

# Bug 1 fix: manually setting n1 and n2 should not cause "object not found" error
testthat::test_that("sumReg.glm: no error when n1 and n2 are both manually set", {
  testthat::expect_no_error({
    mydata <- read.csv(system.file("extdata", "binary.csv", package = "c218Tools"))
    mylogit <- glm("admit ~ gre + gpa", data = mydata, family = "binomial")
    c218Tools::sumReg(model = mylogit, n1 = 2, n2 = 3)
  })
})

testthat::test_that("sumReg.glm: factor exposure returns correct rows", {
  mydata <- read.csv(system.file("extdata", "binary.csv", package = "c218Tools"))
  mydata$rank <- factor(mydata$rank)
  mylogit <- glm("admit ~ rank + gre", data = mydata, family = "binomial")
  res <- c218Tools::sumReg(model = mylogit)
  testthat::expect_equal(nrow(res), length(levels(mydata$rank)))
})

testthat::test_that("sumReg.glm: regressionTableOnly = FALSE returns named list", {
  testthat::expect_no_error({
    mydata <- read.csv(system.file("extdata", "binary.csv", package = "c218Tools"))
    mylogit <- glm("admit ~ gre", data = mydata, family = "binomial")
    res <- c218Tools::sumReg(model = mylogit, regressionTableOnly = FALSE)
    testthat::expect_type(res, "list")
    testthat::expect_true("regressionTable" %in% names(res))
  })
})


# =========================================================
# sumReg.geeglm tests
# =========================================================

testthat::test_that("sumReg.geeglm: warning when only n1 or n2 is set", {
  testthat::expect_warning({
    library(geepack)
    URL <- system.file("extdata", "depression.csv", package = "c218Tools")
    dat <- read.csv(URL, stringsAsFactors = TRUE)
    dat$id <- factor(dat$id)
    dat$drug <- relevel(dat$drug, ref = "standard")
    gee1 <- geeglm(depression ~ diagnose + drug * time,
                   data = dat, id = id,
                   family = binomial, corstr = "independence")
    c218Tools::sumReg(model = gee1, n1 = 1, toClip = FALSE)
  })
  testthat::expect_warning({
    library(geepack)
    URL <- system.file("extdata", "depression.csv", package = "c218Tools")
    dat <- read.csv(URL, stringsAsFactors = TRUE)
    dat$id <- factor(dat$id)
    dat$drug <- relevel(dat$drug, ref = "standard")
    gee1 <- geeglm(depression ~ diagnose + drug * time,
                   data = dat, id = id,
                   family = binomial, corstr = "independence")
    c218Tools::sumReg(model = gee1, n2 = 3, toClip = FALSE)
  })
})

testthat::test_that("sumReg.geeglm: default runs without error", {
  testthat::expect_no_error({
    library(geepack)
    URL <- system.file("extdata", "depression.csv", package = "c218Tools")
    dat <- read.csv(URL, stringsAsFactors = TRUE)
    dat$id <- factor(dat$id)
    dat$drug <- relevel(dat$drug, ref = "standard")
    gee1 <- geeglm(depression ~ diagnose + drug * time,
                   data = dat, id = id,
                   family = binomial, corstr = "independence")
    c218Tools::sumReg(model = gee1, toClip = FALSE)
  })
})

# Bug 1 fix: manually setting n1 and n2 should not error
testthat::test_that("sumReg.geeglm: no error when n1 and n2 are both manually set", {
  testthat::expect_no_error({
    library(geepack)
    URL <- system.file("extdata", "depression.csv", package = "c218Tools")
    dat <- read.csv(URL, stringsAsFactors = TRUE)
    dat$id <- factor(dat$id)
    dat$drug <- relevel(dat$drug, ref = "standard")
    gee1 <- geeglm(depression ~ diagnose + drug * time,
                   data = dat, id = id,
                   family = binomial, corstr = "independence")
    c218Tools::sumReg(model = gee1, n1 = 1, n2 = 2, toClip = FALSE)
  })
})


# =========================================================
# genForm tests
# =========================================================

testthat::test_that("genForm: single covariate vector generates 2 formulas", {
  forms <- c218Tools::genForm(
    outcome = "Y", exposure = "X",
    covariate = c("a", "b")
  )
  testthat::expect_equal(length(forms), 2L)
  testthat::expect_equal(forms[1], "Y~X")
  testthat::expect_true(grepl("a", forms[2]))
  testthat::expect_true(grepl("b", forms[2]))
})

testthat::test_that("genForm: list covariate generates length+1 formulas", {
  forms <- c218Tools::genForm(
    outcome = "Y", exposure = "X",
    covariate = list(c("a", "b"), c("c", "d"))
  )
  testthat::expect_equal(length(forms), 3L)
  testthat::expect_equal(forms[1], "Y~X")
  testthat::expect_true(grepl("a", forms[2]) & grepl("b", forms[2]))
  testthat::expect_true(grepl("c", forms[3]) & grepl("d", forms[3]))
})

testthat::test_that("genForm: exposure appears in every generated formula", {
  forms <- c218Tools::genForm(
    outcome = "Y", exposure = "myexp",
    covariate = list(c("a"), c("b"))
  )
  testthat::expect_true(all(grepl("myexp", forms)))
})


# =========================================================
# regDisplay tests
# =========================================================

testthat::test_that("regDisplay: lm single exposure runs without error", {
  testthat::expect_no_error({
    c218Tools::regDisplay(
      outcome   = "Sepal.Length",
      exposure  = "Sepal.Width",
      covariate = list(c("Petal.Length"), c("Petal.Width")),
      data      = iris,
      regType   = "lm"
    )
  })
})

testthat::test_that("regDisplay: lm multiple exposures output has correct row count", {
  df <- iris
  df <- within(df, {
    SW2f <- NA_character_
    SW2f[Sepal.Width < median(Sepal.Width)] <- "0"
    SW2f[Sepal.Width >= median(Sepal.Width)] <- "1"
  })
  df$SW2f <- factor(df$SW2f, levels = c("0", "1"))
  res <- c218Tools::regDisplay(
    outcome   = "Sepal.Length",
    exposure  = c("Sepal.Width", "SW2f"),
    covariate = list(c("Petal.Length", "Petal.Width"), c("Species")),
    data      = df,
    regType   = "lm"
  )
  # Sepal.Width is numeric → 1 row; SW2f is 2-level factor → 2 rows; total = 3
  testthat::expect_equal(nrow(res), 3L)
})

testthat::test_that("regDisplay: lm output column count equals (covariate groups + 1) * cols_per_model", {
  res <- c218Tools::regDisplay(
    outcome   = "Sepal.Length",
    exposure  = "Sepal.Width",
    covariate = list(c("Petal.Length"), c("Petal.Width")),
    data      = iris,
    regType   = "lm"
  )
  # 3 models (crude + 2 adjusted), each returns 2 cols → 6 cols total
  testthat::expect_equal(ncol(res), 6L)
})

# Bug 4 fix: invalid regType should error clearly via match.arg
testthat::test_that("regDisplay: invalid regType throws error", {
  testthat::expect_error({
    c218Tools::regDisplay(
      outcome   = "Sepal.Length",
      exposure  = "Sepal.Width",
      covariate = list(c("Petal.Length")),
      data      = iris,
      regType   = "invalid_type"
    )
  })
})

testthat::test_that("regDisplay: glm binary outcome runs without error", {
  testthat::expect_no_error({
    mydata <- read.csv(system.file("extdata", "binary.csv", package = "c218Tools"))
    c218Tools::regDisplay(
      outcome   = "admit",
      exposure  = "gre",
      covariate = list(c("gpa"), c("rank")),
      data      = mydata,
      regType   = "glm"
    )
  })
})


# =========================================================
# cbnRegLst tests
# =========================================================

testthat::test_that("cbnRegLst: single exposure 2 models produces correct shape", {
  m1 <- data.frame(term = "X", v1 = "1.0 (0.5)")
  m2 <- data.frame(term = "X", v2 = "1.2 (0.6)")
  res <- c218Tools::cbnRegLst(exposureLength = 1, modelNum = 2, regList = list(m1, m2))
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_equal(ncol(res), 4L)
})

testthat::test_that("cbnRegLst: two exposures 2 models produces correct shape", {
  m1 <- data.frame(term = "X1", v1 = "1.0")
  m2 <- data.frame(term = "X1", v2 = "1.1")
  m3 <- data.frame(term = "X2", v1 = "2.0")
  m4 <- data.frame(term = "X2", v2 = "2.1")
  res <- c218Tools::cbnRegLst(exposureLength = 2, modelNum = 2, regList = list(m1, m2, m3, m4))
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_equal(ncol(res), 4L)
})


# =========================================================
# detectTargetLevels / detectOutcomeLevels tests
# =========================================================

testthat::test_that("detectTargetLevels: numeric returns 2", {
  testthat::expect_equal(
    c218Tools::detectTargetLevels("Sepal.Length", iris), 2L
  )
})

testthat::test_that("detectTargetLevels: factor returns number of levels", {
  df <- mtcars
  df$carb <- factor(df$carb)
  testthat::expect_equal(
    c218Tools::detectTargetLevels("carb", df),
    length(levels(df$carb))
  )
})

testthat::test_that("detectTargetLevels: character returns number of unique values", {
  df <- data.frame(x = c("a", "b", "c", "a"))
  testthat::expect_equal(c218Tools::detectTargetLevels("x", df), 3L)
})

testthat::test_that("detectOutcomeLevels: numeric returns 'continuous'", {
  testthat::expect_equal(
    c218Tools::detectOutcomeLevels("Sepal.Length", iris), "continuous"
  )
})

testthat::test_that("detectOutcomeLevels: factor returns 'categorical'", {
  testthat::expect_equal(
    c218Tools::detectOutcomeLevels("Species", iris), "categorical"
  )
})
