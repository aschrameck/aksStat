#' Generate random samples via inverse CDF method
#'
#' Given an inverse-CDF function `f_inv` this returns `n` iid draws
#' from the associated distribution by transforming Uniform(0,1).
#'
#' @param f_inv A function that accepts a numeric vector of uniform(0,1) draws
#' and returns the corresponding inverse CDF as a numeric vector.
#' @param n Integer scalar; desired sample size.
#' @return Numeric vector of length `n` containing the generated draws.
#' @examples
#' inv_exp <- function(u) { -log(1 - u) / 4 }
#' x <- inverse.cdf.rn(inv_exp, 100)
#' @export
inverse.cdf.rn<- function(f_inv, n) {
  if (!is.function(f_inv)) stop("f_inv must be a function")
  if (!is.numeric(n) || length(n) != 1 || n <= 0) stop("n must be a positive integer")
  u <- runif(n)
  inverse_samples <- f_inv(u)
  if (length(inverse_samples) != n) {
    warning("f_inv returned a vector with length != n; returning its result anyway")
  }
  return(inverse_samples)
}



#' Generate random samples using Accept-Reject on a closed domain
#'
#' This function implements the basic Accept/Reject algorithm for a target pdf
#' `f` supported on a closed interval `d = c(lower, upper)`. A rough maximum
#' is estimated by evaluating `f` on a grid; the estimate is slightly inflated
#' to provide a safety buffer.
#'
#' @param f A function that accepts a numeric scalar or vector and returns the
#' (non-normalized allowed) density values on the same scale.
#' @param d Numeric vector of length 2 giving the closed domain c(lower, upper).
#' @param n Integer; number of draws desired.
#' @return Numeric vector of length `n` containing accepted draws.
#' @examples
#' beta_pdf <- function(x) dbeta(x, 2, 5)
#' x <- accept_reject_rn(beta_pdf, c(0,1), 1000)
#' @export
accept_reject_rn <- function(f, d, n) {
  if (!is.function(f)) stop("f must be a function")
  if (!is.numeric(d) || length(d) != 2 || d[1] >= d[2]) stop("d must be numeric c(lower, upper)")
  if (!is.numeric(n) || length(n) != 1 || n <= 0) stop("n must be a positive integer")

  # grid-based max estimate (with small buffer)
  x_grid <- seq(d[1], d[2], length.out = 2000)
  f_vals <- f(x_grid)
  if (any(is.na(f_vals))) stop("f returned NA on the domain grid; check the pdf function")
  m <- max(f_vals, na.rm = TRUE) * 1.1
  if (!is.finite(m) || m <= 0) stop("Estimated envelope height is non-positive or infinite")

  ar_samples <- numeric(0)
  while (length(ar_samples) < n) {
    x <- runif(1, d[1], d[2])
    y <- runif(1, 0, m)

    if (y <= f(x)) {
      ar_samples <- c(ar_samples, x)
    }
  }
  return(ar_samples)
}



#' Simple Metropolis MCMC sampler (1D)
#'
#' A straightforward Metropolis algorithm for a univariate target density `pdf`.
#' The proposal mechanism should be a function that accepts the current state
#' and returns a proposed state (i.e. it may draw randomness internally).
#'
#' @param pdf A function that returns non-negative density values (not necessarily normalized).
#' @param proposal A function of one argument (current state) that returns a proposed next state.
#' @param start Numeric scalar giving the starting location of the chain.
#' @param n Integer; number of iterations (length of returned vector).
#' @return Numeric vector of length `n` containing the Markov chain.
#' @examples
#' target_pdf <- function(x) dnorm(x)
#' prop <- function(x) rnorm(1, mean = x, sd = 0.5)
#' chain <- mcmc_sampling(target_pdf, prop, start = 0, n = 1000)
#' @export
mcmc_sampling <- function(pdf, proposal, start, n) {
  if (!is.function(pdf) || !is.function(proposal)) stop("pdf and proposal must be functions")
  if (!is.numeric(start) || length(start) != 1) stop("start must be a single numeric value")
  if (!is.numeric(n) || length(n) != 1 || n <= 1) stop("n must be an integer > 1")

  mcmc_samples <- numeric(n)
  mcmc_samples[1] <- start

  for (i in 2:n) {
    proposed <- proposal(mcmc_samples[i - 1])

    # compute acceptance ratio; allow for unnormalized pdfs
    p_cur <- pdf(mcmc_samples[i - 1])
    p_prop <- pdf(proposed)

    if (!is.finite(p_cur) || !is.finite(p_prop)) stop("pdf returned non-finite value")

    alpha <- ifelse(p_cur == 0, 1, min(1, p_prop / p_cur))

    if (runif(1) < alpha) {
      mcmc_samples[i] <- proposed
    } else {
      mcmc_samples[i] <- mcmc_samples[i - 1]
    }
  }
  return(mcmc_samples)
}

