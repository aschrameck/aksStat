#' Subset (random) cross validation for classification methods
#'
#' A wrapper to repeatedly (randomly) split data into training/test sets and compute
#' classification accuracy for LDA, QDA, GLM (logistic) and KNN (via kknn).
#'
#' @param data A data.frame containing predictors and a response.
#' @param y Character scalar; name of the response column in `data` (factor for classification).
#' @param vars Character vector; names of predictor columns to use.
#' @param trials Integer; number of random splits to perform.
#' @param split Numeric in (0,1); proportion of observations used for training.
#' @param model Character; one of "LDA", "QDA", "GLM", or "KNN".
#' @param K Integer; required only for KNN; nearest neighbors to use.
#' @param kernal Character; kernel to use for KNN (e.g., "rectangular", "triangular").
#' @return A numeric vector of length `trials` containing accuracy on the test set for each trial.
#' @examples
#' \dontrun{
#' subset.cross.validation(
#'   iris, y = "Species",
#'   vars = c("Sepal.Length", "Sepal.Width"),
#'   trials = 10, split = 0.7, model = "GLM"
#' )}
#' @importFrom MASS lda qda
#' @importFrom kknn kknn
#' @export
subset.cross.validation <- function(data, y, vars, trials, split, model, K = NULL, kernal = NULL) {
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!is.character(y) || length(y) != 1) stop("y must be a character column name")
  if (!all(vars %in% names(data))) stop("Some vars not found in data")
  if (!is.numeric(trials) || trials <= 0) stop("trials must be positive integer")
  if (!is.numeric(split) || split <= 0 || split >= 1) stop("split must be in (0,1)")


  n.total <- nrow(data)
  Accuracies <- numeric(trials)
  form <- as.formula(paste(y, "~", paste(vars, collapse = "+")))


  for (i in seq_len(trials)) {
    index <- sample(seq_len(n.total), size = floor(n.total * split))
    Train <- data[index, , drop = FALSE]
    Test <- data[-index, , drop = FALSE]


    if (model == "LDA") {
      fit <- MASS::lda(form, data = Train)
      preds <- predict(fit, Test)
      # try to preserve factor levels: if response is factor with two levels, posterior[,2]
      pred.class <- preds$class
      true.class <- Test[[y]]
      Accuracies[i] <- mean(as.character(pred.class) == as.character(true.class), na.rm = TRUE)
    }

    else if (model == "QDA") {
      fit <- MASS::qda(form, data = Train)
      preds <- predict(fit, Test)
      pred.class <- preds$class
      true.class <- Test[[y]]
      Accuracies[i] <- mean(as.character(pred.class) == as.character(true.class), na.rm = TRUE)
    }

    else if (model == "GLM") {
      fit <- glm(form, data = Train, family = binomial())
      probs <- predict(fit, Test, type = "response")
      pred.class <- ifelse(probs > 0.5, 1, 0)
      true.class <- Test[[y]]
      Accuracies[i] <- mean(as.character(pred.class) == as.character(true.class), na.rm = TRUE)
    }

    else if (model == "KNN") {
      if (is.null(K)) stop("K must be provided for KNN")
      if (is.null(kernal)) stop("Kernal must be provided for KNN")
      # kknn returns fitted.values as predicted classes
      kfit <- kknn::kknn(formula = form, train = Train, test = Test, k = K, kernel = kernal)
      pred.class <- kfit$fitted.values
      true.class <- Test[[y]]
      Accuracies[i] <- mean(as.character(pred.class) == as.character(true.class), na.rm = TRUE)
    }

    else {
      stop("Unsupported model. Choose one of 'LDA', 'QDA', 'GLM', 'KNN'.")
    }
  }
  return(Accuracies)
}



#' Tune K for KNN using subset cross validation
#'
#' @param data A data.frame with predictors and response.
#' @param y Character; response name.
#' @param vars Character vector; predictor names.
#' @param trials Integer; number of random splits per K.
#' @param split Numeric; training proportion.
#' @param k.grid Integer vector; values of K to evaluate.
#' @return The K (from k.grid) that achieved highest mean accuracy.
#' @examples
#' \dontrun{
#' tune.k(
#'   iris,
#'   y = "Species",
#'   vars = c("Sepal.Length", "Sepal.Width"),
#'   trials = 20,
#'   split = 0.7,
#'   k.grid = 1:10
#' )
#' }
#' @export
tune.k <- function(data, y, vars, trials, split, k.grid) {
  if (!is.numeric(k.grid) || length(k.grid) < 1) stop("k.grid must be a numeric vector of candidate K values")

  mean_accuracies <- numeric(length(k.grid))

  for (i in seq_along(k.grid)) {
    acc <- subset.cross.validation(data = data,
                                   y = y,
                                   vars = vars,
                                   trials = trials,
                                   split = split,
                                   model = "KNN",
                                   K = k.grid[i],
                                   kernal = "rectangular")
    mean_accuracies[i] <- mean(acc, na.rm = TRUE)
  }

  best.idx <- which.max(mean_accuracies)
  best.k <- k.grid[best.idx]

  return(best.k)
}
