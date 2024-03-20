



## test warnings

testthat::test_that("basic", {
  testthat::expect_warning({
    lm("Sepal.Length~ Sepal.Width", data = iris) %>%
      c218Tools::sumLM(form = "Sepal.Length~ Sepal.Width", n1 = 1, data = iris, toClip = F)
  })

})


## test row number

