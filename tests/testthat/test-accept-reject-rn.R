library(testthat)
library(aksStat)

test_that("accept_reject_rn handles invalid inputs", {
  # Should throw an error if first argument is not a function
  expect_error(accept_reject_rn("not_a_function", c(0, 1), 10))
})

test_that("accept_reject_rn throws error on invalid input", {
  expect_error(accept_reject_rn("not_a_function", c(0, 1), 10))
  expect_error(accept_reject_rn(function(x) x, c(0, 1), -5))
})
