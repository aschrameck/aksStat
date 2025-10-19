library(testthat)
library(aksStat)
data(iris)

test_that("tune.k returns valid K", {
  best_k <- tune.k(iris, y = "Species", vars = c("Sepal.Length","Sepal.Width"), trials = 3, split = 0.7, k.grid = 1:5)
  expect_true(best_k %in% 1:5)
  expect_type(best_k, "integer")
})

test_that("tune.k throws error on invalid k.grid", {
  expect_error(tune.k(iris, y = "Species", vars = c("Sepal.Length"), trials = 3, split = 0.7, k.grid = character(0)))
})
