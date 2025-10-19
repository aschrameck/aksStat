#' Gelman diagnostic for multiple MCMC chains (basic version)
#' Computes W, B, sigma^2_hat, R_hat and a simple n_eff estimate for a list
#' of equal-length numeric MCMC chains.
#'
#' @param chains A list of numeric vectors (each vector is a single chain). All chains
#' must have equal length >= 2.
#' @return A data.frame with columns: W, B, sigma_squared, R, n_eff.
#' @examples
#' ch1 <- rnorm(1000)
#' ch2 <- rnorm(1000, mean = 0.1)
#' gelman_diag(list(ch1, ch2))
#' @export
gelman_diag <- function(chains) {
  if (!is.list(chains) || length(chains) < 2) stop("chains must be a list of at least two numeric vectors")
  m <- length(chains)
  n <- length(chains[[1]])

  if (any(sapply(chains, length) != n)) stop("All chains must have the same length")


  chains_mat <- do.call(cbind, chains)
  chain_means <- colMeans(chains_mat)
  overall_mean <- mean(chain_means)


  W <- mean(apply(chains_mat, 2, stats::var))
  B <- (n / (m - 1)) * sum((chain_means - overall_mean)^2)
  sigma_sq_hat <- ((n - 1) / n) * W + (1 / n) * B
  R_hat <- sqrt(sigma_sq_hat / W)
  # simplified n_eff estimate (not the advanced one in coda
  n_eff <- (m * n * sigma_sq_hat) / B


  results <- data.frame(W = W, B = B, sigma_squared = sigma_sq_hat, R = R_hat, n_eff = n_eff)
  return(results)
}



