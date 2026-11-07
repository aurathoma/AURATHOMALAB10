#' Calculate Highest Density Intervals (HDI) and Equal-Tailed Intervals (ETI)
#'
#' Computes HDI and ETI intervals for various continuous distributions
#' using root-solving bisection algorithms.
#'
#' @param stem Character string specifying the distribution stem (e.g., "norm", "gamma", "beta").
#' @param args A list of parameters specific to the chosen distribution (e.g., shape parameters, mean, sd).
#' @param alpha Significance level, default is 0.05.
#'
#' @return A list containing the calculated intervals and parameters.
#' @export
#'
#' @examples
#' hdicalc(stem = "beta", args = list(shape1 = 3, shape2 = 10), alpha = 0.05)


library(ggplot2)

# Generate sequence for the curve
x_vals <- seq(0, 0.05, length.out = 500)
y_vals <- sapply(x_vals, dendiff_beta)

df_beta_plot <- data.frame(a1 = x_vals, diff = y_vals)

ggplot(df_beta_plot, aes(x = a1, y = diff)) +
  geom_line(color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = a1_val, color = "red", linetype = "dashed") +
  labs(
    title = "Find a1 and a2 - Aura Thoma",
    subtitle = "Beta(3, 10) Distribution",
    x = "a1",
    y = "Difference in density"
  ) +
  theme_minimal()
