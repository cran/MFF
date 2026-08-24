#' Display an MFF Object
#'
#' @description
#' Print a concise description of a fitted or tuned Meta Fuzzy Function object.
#'
#' @param x An object of class \code{mff}.
#' @param ... Additional arguments, currently unused.
#'
#' @return The input object \code{x}, invisibly.
#'
#' @examples
#' set.seed(123)
#' y <- seq_len(20) + stats::rnorm(20, sd = 0.2)
#' predictions <- cbind(
#'   model_a = y + stats::rnorm(20, sd = 0.3),
#'   model_b = y + stats::rnorm(20, sd = 0.4),
#'   model_c = y + stats::rnorm(20, sd = 1.2)
#' )
#' fit <- mff(predictions, y, c = 2, method = "kmeans", nstart = 10)
#' print(fit)
#'
#' @method print mff
#' @export
print.mff <- function(x, ...) {
  algorithm <- if (!is.null(x$algorithm)) x$algorithm else x$method
  tuned <- !is.null(x$best_cluster)

  cat("Meta Fuzzy Function object\n")
  cat("  Algorithm:", algorithm, "\n")
  cat("  Candidate models:", nrow(x$weights), "\n")
  cat("  Meta functions:", ncol(x$weights), "\n")

  if (tuned) {
    cat("  Selection metric:", x$eval.method, "\n")
    cat("  Validation-selected function:", x$best_cluster, "\n")
  }

  invisible(x)
}

#' Summarize an MFF Object
#'
#' @description
#' Construct a structured summary of a fitted or tuned Meta Fuzzy Function
#' object, including its algorithm, dimensions, membership-derived weights,
#' validation scores, and selection information when tuning was performed.
#'
#' @param object An object of class \code{mff}.
#' @param ... Additional arguments, currently unused.
#'
#' @return An object of class \code{summary.mff}.
#'
#' @examples
#' set.seed(123)
#' y <- seq_len(20) + stats::rnorm(20, sd = 0.2)
#' predictions <- cbind(
#'   model_a = y + stats::rnorm(20, sd = 0.3),
#'   model_b = y + stats::rnorm(20, sd = 0.4),
#'   model_c = y + stats::rnorm(20, sd = 1.2)
#' )
#' fit <- mff(predictions, y, c = 2, method = "kmeans", nstart = 10)
#' summary(fit)
#'
#' @method summary mff
#' @export
summary.mff <- function(object, ...) {
  tuned <- !is.null(object$best_cluster)
  scores <- if (tuned) object$best_scores else object$cluster_scores

  out <- list(
    algorithm = if (!is.null(object$algorithm)) object$algorithm else object$method,
    n_models = nrow(object$weights),
    n_functions = ncol(object$weights),
    weights = object$weights,
    scores = scores,
    tuned = tuned
  )

  if (tuned) {
    out$eval.method <- object$eval.method
    out$best_params <- object$best_params
    out$best_cluster <- object$best_cluster
    out$best_weight <- object$best_weight
  }

  structure(out, class = "summary.mff")
}

#' @param x An object of class \code{summary.mff}.
#' @rdname summary.mff
#' @return The input object \code{x}, invisibly.
#' @method print summary.mff
#' @export
print.summary.mff <- function(x, ...) {
  cat("Summary of Meta Fuzzy Function object\n")
  cat("  Algorithm:", x$algorithm, "\n")
  cat("  Candidate models:", x$n_models, "\n")
  cat("  Meta functions:", x$n_functions, "\n")

  if (x$tuned) {
    cat("  Selection metric:", x$eval.method, "\n")
    cat("  Validation-selected function:", x$best_cluster, "\n")
    cat("\nBest parameters:\n")
    print(x$best_params)
  }

  cat("\nMembership-derived weights:\n")
  print(round(x$weights, 4))
  cat("\nValidation scores:\n")
  print(x$scores)

  invisible(x)
}

#' Plot an MFF Object
#'
#' @description
#' Visualize membership-derived candidate-model weights, validation scores,
#' test scores, observed-versus-predicted test values, or observed and predicted
#' values in test-observation order.
#'
#' @param x An object of class \code{mff}.
#' @param type Character string selecting \code{"weights"},
#' \code{"validation_scores"}, \code{"scores"}, \code{"test_scores"},
#' \code{"observed_predicted"}, or \code{"series"}. The value
#' \code{"scores"} is retained as an alias for \code{"validation_scores"}.
#' @param metric Optional score column for validation or test score plots.
#' Defaults to the tuning metric for tuned objects and \code{"RMSE"} otherwise.
#' @param pred_matrix Optional numeric test prediction matrix with observations
#' in rows and candidate models in columns. Required for test-based plots.
#' @param actual Optional numeric vector of observed test responses. Required
#' for test-based plots.
#' @param show_all Logical. If \code{FALSE}, test plots use only the function
#' selected on validation data. If \code{TRUE}, all functions are displayed
#' descriptively; no new function is selected from test performance.
#' @param ... Additional graphical arguments passed to the underlying base R
#' plotting function.
#'
#' @details
#' Test-based plots transfer weights learned from validation data unchanged to
#' the supplied test prediction matrix. By default they require a tuned object
#' and use its validation-selected \code{best_weight}. Setting
#' \code{show_all = TRUE} displays all functions for descriptive comparison but
#' does not alter \code{best_cluster} or perform test-based model selection.
#'
#' @return The input object \code{x}, invisibly.
#'
#' @examples
#' set.seed(123)
#' y <- seq_len(20) + stats::rnorm(20, sd = 0.2)
#' predictions <- cbind(
#'   model_a = y + stats::rnorm(20, sd = 0.3),
#'   model_b = y + stats::rnorm(20, sd = 0.4),
#'   model_c = y + stats::rnorm(20, sd = 1.2)
#' )
#' fit <- mff(predictions, y, c = 2, method = "kmeans", nstart = 10)
#' plot(fit, type = "weights")
#' plot(fit, type = "validation_scores", metric = "RMSE")
#'
#' # Select a function using validation data, then visualize test performance.
#' tuned <- tune.mff(
#'   predictions, y, max_c = 2,
#'   mff.method = "kmeans", nstart = 10,
#'   parallel = FALSE, logging = FALSE
#' )
#' set.seed(456)
#' y_test <- seq_len(10) + stats::rnorm(10, sd = 0.2)
#' pred_test <- cbind(
#'   model_a = y_test + stats::rnorm(10, sd = 0.3),
#'   model_b = y_test + stats::rnorm(10, sd = 0.4),
#'   model_c = y_test + stats::rnorm(10, sd = 1.2)
#' )
#' plot(tuned, type = "test_scores", pred_matrix = pred_test, actual = y_test)
#' plot(tuned, type = "observed_predicted", pred_matrix = pred_test,
#'      actual = y_test)
#' plot(tuned, type = "series", pred_matrix = pred_test, actual = y_test)
#'
#' @method plot mff
#' @export
plot.mff <- function(
    x,
    type = c("weights", "validation_scores", "scores", "test_scores",
             "observed_predicted", "series"),
    metric = NULL,
    pred_matrix = NULL,
    actual = NULL,
    show_all = FALSE,
    ...) {
  type <- match.arg(type)
  if (type == "scores") type <- "validation_scores"

  validate_test_inputs <- function() {
    if (is.null(pred_matrix) || is.null(actual)) {
      stop("'pred_matrix' and 'actual' are required for test-based plots.")
    }
    if (!is.matrix(pred_matrix) || !is.numeric(pred_matrix)) {
      stop("'pred_matrix' must be a numeric matrix.")
    }
    if (!is.numeric(actual)) stop("'actual' must be numeric.")
    if (nrow(pred_matrix) != length(actual)) {
      stop("The number of rows in 'pred_matrix' must equal length(actual).")
    }
    if (ncol(pred_matrix) != nrow(x$weights)) {
      stop("The number of columns in 'pred_matrix' must equal the number of candidate models.")
    }
    if (any(!is.finite(pred_matrix)) || any(!is.finite(actual))) {
      stop("'pred_matrix' and 'actual' must contain only finite values.")
    }

    expected_names <- rownames(x$weights)
    supplied_names <- colnames(pred_matrix)
    if (!is.null(expected_names) && !is.null(supplied_names) &&
        !identical(expected_names, supplied_names)) {
      stop("The candidate-model columns in 'pred_matrix' must match the fitted weight rows in the same order.")
    }
  }

  test_predictions <- function() {
    validate_test_inputs()
    if (isTRUE(show_all)) {
      weights <- x$weights
      labels <- paste0("Function ", seq_len(ncol(weights)))
    } else {
      if (is.null(x$best_weight) || is.null(x$best_cluster)) {
        stop("A validation-selected tuned MFF object is required when 'show_all = FALSE'. Use tune.mff() or set 'show_all = TRUE'.")
      }
      weights <- matrix(x$best_weight, ncol = 1)
      labels <- paste0("Selected function ", x$best_cluster)
    }
    list(predictions = pred_matrix %*% weights, labels = labels)
  }

  if (type == "weights") {
    weights <- x$weights
    model_names <- rownames(weights)
    if (is.null(model_names)) model_names <- paste0("Model ", seq_len(nrow(weights)))
    function_names <- colnames(weights)
    if (is.null(function_names)) {
      function_names <- paste0("Function ", seq_len(ncol(weights)))
    }

    graphics::barplot(
      t(weights), beside = TRUE,
      names.arg = model_names,
      legend.text = function_names,
      ylab = "Membership-derived weight",
      xlab = "Candidate model",
      ...
    )
  } else if (type == "validation_scores") {
    scores <- if (!is.null(x$best_scores)) x$best_scores else x$cluster_scores
    if (is.null(metric)) {
      metric <- if (!is.null(x$eval.method)) x$eval.method else "RMSE"
    }
    if (!(metric %in% colnames(scores))) {
      stop("Unknown metric: ", metric, ". Available metrics are: ",
           paste(colnames(scores), collapse = ", "), ".")
    }

    graphics::barplot(
      scores[, metric],
      names.arg = paste0("Function ", seq_len(nrow(scores))),
      ylab = metric,
      xlab = "Meta fuzzy function",
      ...
    )
  } else if (type == "test_scores") {
    test_result <- test_predictions()
    scores <- evaluate(test_result$predictions, actual)
    if (is.null(metric)) {
      metric <- if (!is.null(x$eval.method)) x$eval.method else "RMSE"
    }
    if (!(metric %in% colnames(scores))) {
      stop("Unknown metric: ", metric, ". Available metrics are: ",
           paste(colnames(scores), collapse = ", "), ".")
    }

    graphics::barplot(
      scores[, metric],
      names.arg = test_result$labels,
      ylab = paste("Test", metric),
      xlab = "Meta fuzzy function",
      ...
    )
  } else if (type == "observed_predicted") {
    test_result <- test_predictions()
    predictions <- test_result$predictions
    colours <- seq_len(ncol(predictions))

    graphics::plot(
      actual, predictions[, 1],
      pch = 19, col = colours[1],
      xlab = "Observed test values",
      ylab = "MFF test predictions",
      ...
    )
    if (ncol(predictions) > 1) {
      for (j in 2:ncol(predictions)) {
        graphics::points(actual, predictions[, j], pch = 19, col = colours[j])
      }
      graphics::legend(
        "topleft", legend = test_result$labels,
        col = colours, pch = 19, bty = "n"
      )
    }
    graphics::abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
  } else if (type == "series") {
    test_result <- test_predictions()
    predictions <- test_result$predictions
    series_values <- cbind(Observed = actual, predictions)
    series_labels <- c("Observed", test_result$labels)
    colours <- seq_len(ncol(series_values))

    graphics::matplot(
      series_values,
      type = "l", lty = 1, lwd = 2, col = colours,
      xlab = "Test observation", ylab = "Response",
      ...
    )
    graphics::legend(
      "topleft", legend = series_labels,
      col = colours, lty = 1, lwd = 2, bty = "n"
    )
  }

  invisible(x)
}
