#' Non-Symmetric Confidence Interval and Hypothesis Test
#'
#' Computes a non-symmetric confidence interval for the population mean using
#' Student's t-distribution, visualizes the data distribution via a ggplot histogram,
#' and tests a null hypothesis against a specified tolerance ball around a null mean.
#'
#' @param x A numeric vector representing the quantitative data sample.
#' @param alpha Numeric. The total significance level (default is 0.05).
#' @param alpha2 Numeric. The lower tail probability allocation (default is 0.02).
#'   Note that the upper tail allocation is automatically determined as \code{alpha1 = alpha - alpha2}.
#' @param mu0 Numeric. The hypothesized central population mean value (default is 45).
#' @param epsilon Numeric. The half-width of the tolerance ball around \code{mu0} (default is 4).
#' @param bins Integer. The number of bins for the ggplot histogram (default is 25).
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{ci}{A numeric vector of length 2 providing the lower and upper bounds of the non-symmetric confidence interval.}
#'   \item{t}{A numeric vector of length 2 providing the lower and upper t-critical values used in the calculation.}
#'   \item{test}{A character string ("TRUE" or "FALSE") indicating whether the tolerance ball is completely contained within the confidence interval.}
#'   \item{ball}{A numeric vector of length 2 providing the lower and upper bounds of the hypothesized ball (\code{mu0 +/- epsilon}).}
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' ddt <- read.csv("DDT.csv")
#' df <- ddt %>% filter(SPECIES == "CCATFISH")
#'
#' ciNonSym(x = df$LENGTH,
#'          alpha = 0.05,
#'          alpha2 = 0.02,
#'          mu0 = 45,
#'          epsilon = 4,
#'          bins = 25)
#' }
ciNonSym <- function(x, alpha = 0.05, alpha2 = 0.02, mu0 = 45, epsilon = 4, bins = 25) {
  alpha1 <- alpha - alpha2
  n <- length(x)
  ybar <- mean(x)
  s <- sd(x)
  se <- s / sqrt(n)
  df_val <- n - 1

  # Calculate t critical values
  t_low <- qt(alpha2, df_val)
  t_high <- qt(1 - alpha1, df_val)

  # Confidence interval bounds
  ci_low <- ybar + t_low * se
  ci_high <- ybar + t_high * se
  ci <- c(ci_low, ci_high)
  t_vals <- c(t_low, t_high)

  # Hypothesis test against mu0 +/- epsilon (ball)
  ball <- c(mu0 - epsilon, mu0 + epsilon)
  test_res <- if (ball[1] >= ci[1] && ball[2] <= ci[2]) "TRUE" else "FALSE"

  # ggplot histogram
  p <- ggplot(data.frame(x = x), aes(x = x)) +
    geom_histogram(bins = bins, fill = "steelblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = ci, linetype = "dashed", color = "red", linewidth = 1) +
    geom_vline(xintercept = c(mu0, ball), linetype = c("solid", "dotted", "dotted"),
               color = c("blue", "darkgreen", "darkgreen"), linewidth = 1) +
    labs(title = "Non-Symmetric Confidence Interval Test",
         subtitle = paste("mu0 =", mu0, "| epsilon =", epsilon, "| Test:", test_res),
         x = "Variable Value", y = "Count") +
    theme_minimal()

  print(p)

  list(
    ci = ci,
    t = t_vals,
    test = test_res,
    ball = ball
  )
}
