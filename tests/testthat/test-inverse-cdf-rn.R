library(testthat)
library(aksStat)

test_that("inverse_cdf_rn generates samples correctly", {
  inv_exp <- function(u) { -log(1 - u) / 4 }
  x <- inverse.cdf.rn(inv_exp, 1000)
  expect_length(x, 1000)
  expect_true(all(x >= 0))
})

test_that("inverse_cdf_rn throws error on invalid input", {
  expect_error(inverse.cdf.rn("not_a_function", 10))
  expect_error(inverse.cdf.rn(function(u) u, -5))
})
