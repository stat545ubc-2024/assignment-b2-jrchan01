library(gapminder)
library(cv)
library(testthat)

test_that("The cv function is producing the correct value", {
  expect_equal(cv_function(gapminder$pop), (sd(gapminder$pop) / mean(gapminder$pop)) * 100)
})

test_that("Non numeric values will not produce a cv", {
  expect_error(cv_function(gapminder$continent), "Input must be numeric")
})

empty_vector <- numeric(0)
test_that("Coefficient of variation is positive", {
  expect_true(cv_function(gapminder$lifeExp) > 0)
})

rm(empty_vector)
