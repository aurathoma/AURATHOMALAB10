#' Difference in Densities for Beta Distribution HDI
#'
#' Computes the difference in probability density values at the lower and upper
#' boundary quantiles for a Beta distribution given a lower tail probability allocation \code{a1}.
#' This helper function is designed to be solved via numerical root-finding
#' (\code{\link[stats]{uniroot}}) to determine the Highest Density Interval (HDI).
#'
#' @param a1 Numeric. The lower tail probability allocation (\eqn{\alpha_1}).
#' @param alpha Numeric. The total significance level (default is 0.05).
#' @param shape1 Numeric. Non-negative shape parameter \eqn{\alpha} for the Beta distribution (default is 3).
#' @param shape2 Numeric. Non-negative shape parameter \eqn{\beta} for the Beta distribution (default is 10).
#'
#' @return A numeric scalar representing the difference between the probability density at
#'   the lower quantile \code{qbeta(a1)} and the upper quantile \code{qbeta(1 - a2)}.
#'   A value of zero indicates that the two endpoints have equal density (defining the HDI).
#'
#' @importFrom stats qbeta dbeta uniroot
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Set parameters
#' alpha_val <- 0.05
#' a1_grid <- seq(0, alpha_val, length.out = 1000)

#' # 2. Evaluate density differences across grid
#' diff_vals <- sapply(a1_grid, dendiff_beta, alpha = alpha_val, shape1 = 3, shape2 = 10)

#' # 3. Find root where density difference is zero
#' root_info <- uniroot(dendiff_beta, interval = c(0, alpha_val),
#'                      alpha = alpha_val, shape1 = 3, shape2 = 10)
#' a1_root <- root_info$root

#' # 4. Generate the "Find a1 and a2" plot
#' plot(a1_grid, diff_vals, type = "l", col = "black",
#'      xlab = expression(a[1]),
#'      ylab = "difference in densities",
#'      main = "Find a1 and a2 - Aura Lee Thoma")

#' # Add reference lines
#' abline(h = 0, col = "red", lty = 1)
#' abline(v = a1_root, col = "red", lty = 1)
#' }
# Function to find the difference in densities for Beta(3, 10)
dendiff_beta <- function(a1, alpha = 0.05, shape1 = 3, shape2 = 10) {
  a2 <- alpha - a1
  # Quantiles corresponding to a1 and a2
  q1 <- qbeta(a1, shape1 = shape1, shape2 = shape2)
  q2 <- qbeta(1 - a2, shape1 = shape1, shape2 = shape2)

  # Difference in densities at these quantiles
  dbeta(q1, shape1 = shape1, shape2 = shape2) - dbeta(q2, shape1 = shape1, shape2 = shape2)
}

# Find the root using uniroot
root_res <- uniroot(dendiff_beta, interval = c(0, 0.05))
a1_val <- root_res$root
a2_val <- 0.05 - a1_val

cat("a1:", a1_val, "\n")
cat("a2:", a2_val, "\n")
