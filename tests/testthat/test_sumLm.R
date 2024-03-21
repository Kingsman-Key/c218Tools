



## test warnings

testthat::test_that("basic test on warning", {
  ## test on n2 is empty
  testthat::expect_warning({
    lm("Sepal.Length~ Sepal.Width", data = iris) %>%
      c218Tools::sumLM(model = ., n1 = 1, toClip = F)
  })

  ## test on n1 is empty
  testthat::expect_warning({
    lm("Sepal.Length~ Sepal.Width", data = iris) %>%
      c218Tools::sumLM(model = ., n2 = 1, toClip = F)
  })
})


## test row number

testthat::test_that("basic test on row number", {
  testthat::expect_equal({
    lm("Sepal.Length~ Sepal.Width", data = iris) %>%
      c218Tools::sumLM(model = .) %>%
      nrow()
  }, expected = 1)

})

