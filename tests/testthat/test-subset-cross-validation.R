library(testthat)
library(aksStat)
data(iris)

test_that("subset.cross.validation works with GLM, LDA, QDA, KNN", {
  # GLM
  suppressWarnings({
  res_glm <- subset.cross.validation(iris, y = "Species", vars = c("Sepal.Length","Sepal.Width"), trials = 3, split = 0.7, model = "GLM")
  expect_length(res_glm, 3) })

  # LDA
  res_lda <- subset.cross.validation(iris, y = "Species", vars = c("Sepal.Length","Sepal.Width"), trials = 3, split = 0.7, model = "LDA")
  expect_length(res_lda, 3)

  # QDA
  res_qda <- subset.cross.validation(iris, y = "Species", vars = c("Sepal.Length","Sepal.Width"), trials = 3, split = 0.7, model = "QDA")
  expect_length(res_qda, 3)

  # KNN
  res_knn <- subset.cross.validation(iris, y = "Species", vars = c("Sepal.Length","Sepal.Width"), trials = 3, split = 0.7, model = "KNN", K = 3, kernal = "rectangular")
  expect_length(res_knn, 3)
})

test_that("subset.cross.validation throws errors for bad input", {
  expect_error(subset.cross.validation(iris, y = "Species", vars = c("Sepal.Length"), trials = -1, split = 0.7, model = "LDA"))
  expect_error(subset.cross.validation(iris, y = "Species", vars = c("Sepal.Length"), trials = 3, split = 1.5, model = "LDA"))
  expect_error(subset.cross.validation(iris, y = "Species", vars = c("Sepal.Length"), trials = 3, split = 0.7, model = "KNN"))
})
