#' Calculate Highest Density Intervals (HDI) and Equal-Tailed Intervals (ETI)
#'
#' Computes the Highest Density Interval (HDI) and Equal-Tailed Interval (ETI)
#' for any continuous distribution available in R (e.g., normal, beta, gamma)
#' by numerically solving for equal boundary densities.
#'
#' @param stem Character string. The root name of the statistical distribution
#'   (e.g., \code{"norm"} for normal, \code{"beta"} for beta). Default is \code{"norm"}.
#' @param args A named list of distribution-specific parameters passed to the
#'   density and quantile functions (e.g., \code{list(mean = 10, sd = 5)}). Default is \code{list(mean = 10, sd = 5)}.
#' @param alpha Numeric. The total significance level or tail area (default is 0.05).
#'
#' @return A named list containing:
#' \describe{
#'   \item{alpha}{The total significance level (\code{alpha}).}
#'   \item{alpha1}{The optimized lower tail probability for the HDI.}
#'   \item{alpha2}{The optimized upper tail probability for the HDI.}
#'   \item{HDI}{A numeric vector of length 2 providing the lower and upper bounds of the Highest Density Interval.}
#'   \item{RHDI}{The range (width) of the Highest Density Interval.}
#'   \item{ETI}{A numeric vector of length 2 providing the lower and upper bounds of the Equal-Tailed Interval.}
#'   \item{RETI}{The range (width) of the Equal-Tailed Interval.}
#' }
#'
#' @importFrom stats uniroot
#' @export
#'
#' @examples
#' # Calculate HDI and ETI for a Normal distribution
#' hdicalc(stem = "norm", args = list(mean = 10, sd = 5), alpha = 0.05)
#'
#' # Calculate HDI and ETI for a Beta distribution
#' hdicalc(stem = "beta", args = list(shape1 = 3, shape2 = 10), alpha = 0.05)
hdicalc <- function(stem = "norm", args = list(mean = 10, sd = 5), alpha = 0.05){

  # Define function names once outside the root finder for optimal speed
  f <- paste0("d", stem)
  q <- paste0("q", stem)

  # function to find the difference in densities
  dendiff <- function(x, nr = TRUE){
    a2 <- alpha - x

    qa1 <- do.call(q, c(list(p = x), args))
    qa2 <- do.call(q, c(list(p = 1 - a2), args))

    if(nr == TRUE){
      do.call(f, c(list(x = qa1), args)) - do.call(f, c(list(x = qa2), args))
    } else {
      c(qa1, qa2)
    }
  }

  # find the root of the dendiff function
  l <- uniroot(dendiff, interval = c(0, alpha))

  # Not for NR BUT HDI's
  ll <- dendiff(x = l$root, nr = FALSE)

  # Not for NR but ETI's
  ll2 <- dendiff(x = alpha / 2, nr = FALSE)

  list(
    alpha  = alpha,
    alpha1 = l$root,
    alpha2 = alpha - l$root,
    HDI    = ll,
    RHDI   = ll[2] - ll[1],
    ETI    = ll2,
    RETI   = ll2[2] - ll2[1]
  )
}
