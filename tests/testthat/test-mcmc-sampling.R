library(testthat)
library(aksStat)

test_that("mcmc_sampling returns correct number of samples and reasonable values", {
  # Define target density: standard normal
  target_pdf <- function(x) dnorm(x, mean = 0, sd = 1)

  # Proposal function: normal random walk
  proposal <- function(x) rnorm(1, mean = x, sd = 0.5)

  set.seed(123)
  n_samples <- 500
  start_val <- 0

  chain <- mcmc_sampling(pdf = target_pdf, proposal = proposal, start = start_val, n = n_samples)

  # Check length
  expect_equal(length(chain), n_samples)

  # Check numeric
  expect_true(is.numeric(chain))

  # Check basic statistical properties (rough sanity)
  expect_true(abs(mean(chain)) < 0.2)     # mean close to 0
  expect_true(abs(sd(chain) - 1) < 0.2)   # sd close to 1
})
