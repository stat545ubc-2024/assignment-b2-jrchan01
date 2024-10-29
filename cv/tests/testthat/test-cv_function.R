library(gapminder)
library(cv)
library(testthat)

test_that("The cv function is producing the correct value", {
  expect_equal(cv_function(gapminder$pop), (sd(gapminder$pop) / mean(gapminder$pop)) * 100)
})

test_that("Non numeric values will not produce a cv", {
  expect_error(cv_function(gapminder$continent), "Input must be numeric")
})

test_that("Non numeric values will not produce a cv", {
  expect_error(cv_function(gapminder$continent), "Input must be numeric")
})
