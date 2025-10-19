library(testthat)
library(aksStat)

test_that("gelman_diag works with valid chains", {
  set.seed(123)
  ch1 <- rnorm(1000)
  ch2 <- rnorm(1000, mean = 0.1)

  res <- gelman_diag(list(ch1, ch2))

  expect_s3_class(res, "data.frame")
  expect_true(all(c("W", "B", "sigma_squared", "R", "n_eff") %in% names(res)))

  # R_hat should be > 0
  expect_true(res$R > 0)
  # n_eff should be > 0
  expect_true(res$n_eff > 0)
})

test_that("gelman_diag fails with invalid input", {
  ch1 <- rnorm(100)

  # only one chain
  expect_error(gelman_diag(list(ch1)), "chains must be a list of at least two numeric vectors")

  # chains of different lengths
  ch2 <- rnorm(50)
  expect_error(gelman_diag(list(ch1, ch2)), "All chains must have the same length")

  # not a list
  expect_error(gelman_diag(ch1), "chains must be a list of at least two numeric vectors")
})

test_that("gelman_diag handles identical chains correctly", {
  ch <- rnorm(500)
  res <- gelman_diag(list(ch, ch))

  # If chains are identical, W = 0 variance between chains
  expect_equal(res$B >= 0, TRUE)
  expect_equal(res$W >= 0, TRUE)
  expect_true(res$R >= 0)
})
