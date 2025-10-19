# aksStat

`aksStat` is an R package providing statistical tools for random number generation, MCMC sampling, subset cross-validation, and Gelman diagnostics. It is designed for education, simulations, and research purposes.

------------------------------------------------------------------------

## Installation

``` r
# Install remotes if not already installed
if (!require("remotes")) install.packages("remotes")

# Install aksStat from GitHub
remotes::install_github("aschrameck/aksStat")
```

------------------------------------------------------------------------

## Core Functions

| Function | Description |
|------------------------|------------------------------------------------|
| `accept_reject_rn()` | Generates random numbers using the accept-reject method |
| `inverse_cdf_rn()` | Generates random numbers via inverse CDF sampling |
| `mcmc_sampling()` | Simple Metropolis MCMC sampler |
| `gelman_diag()` | Gelman-Rubin convergence diagnostic for multiple chains |
| `subset.cross.validation()` | Subset-based cross-validation for GLM, LDA, QDA, KNN |
| `tune.k()` | Tunes optimal K for KNN models |

------------------------------------------------------------------------

## Example Usage

``` r
library(aksStat)

# Generate random numbers using accept-reject
beta_pdf <- function(x) dbeta(x, 2, 5)
ar <- accept_reject_rn(beta_pdf, c(0,1), 1000)

# Generate random numbers using inverse CDF
inv_exp <- function(u) { -log(1 - u) / 4 }
cdf <- inverse.cdf.rn(inv_exp, 100)

# Run MCMC sampling
target_pdf <- function(x) dnorm(x)
prop <- function(x) rnorm(1, mean = x, sd = 0.5)
chain <- mcmc_sampling(target_pdf, prop, start = 0, n = 1000)

# Check convergence
ch1 <- rnorm(1000)
ch2 <- rnorm(1000, mean = 0.1)
gelman_diag(list(ch1, ch2))

# Subset cross-validation for GLM
subset.cross.validation(
  iris, y = "Species",
  vars = c("Sepal.Length", "Sepal.Width"),
  trials = 10, split = 0.7, model = "GLM")

# Tune optimal k for KNN
tune.k(
  iris, 
  y = "Species",
  vars = c("Sepal.Length", "Sepal.Width"),
  trials = 20,
  split = 0.7,
  k.grid = 1:10)
```

------------------------------------------------------------------------

## License

`aksStat` is released under the MIT License.
