make_prediction_data <- function(n = 30L, seed = 42L) {
  set.seed(seed)
  actual <- seq_len(n) + stats::rnorm(n, sd = 0.2)
  predictions <- cbind(
    model_a = actual + stats::rnorm(n, sd = 0.3),
    model_b = actual + stats::rnorm(n, sd = 0.4),
    model_c = actual + stats::rnorm(n, sd = 1.5),
    model_d = actual + stats::rnorm(n, sd = 1.7)
  )
  list(x = predictions, y = actual)
}

make_regression_data <- function(n = 80L, seed = 42L) {
  set.seed(seed)
  x1 <- stats::rnorm(n)
  x2 <- stats::runif(n)
  data.frame(y = 2 + 3 * x1 - x2 + stats::rnorm(n, sd = 0.2), x1, x2)
}
