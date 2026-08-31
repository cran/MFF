.validate_scalar_integer <- function(x, name, lower = 1L) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x != as.integer(x) || x < lower) {
    stop(sprintf("'%s' must be a single integer greater than or equal to %d.",
                 name, lower), call. = FALSE)
  }
  as.integer(x)
}

.validate_prediction_problem <- function(x, y, x_name = "x", y_name = "y") {
  if (!is.matrix(x) || !is.numeric(x)) {
    stop(sprintf("'%s' must be a numeric matrix.", x_name), call. = FALSE)
  }
  if (nrow(x) < 2L) {
    stop(sprintf("'%s' must contain at least two observations.", x_name),
         call. = FALSE)
  }
  if (ncol(x) < 2L) {
    stop(sprintf("'%s' must contain at least two candidate-model columns.",
                 x_name), call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop(sprintf("'%s' must contain only finite values.", x_name),
         call. = FALSE)
  }
  if (!is.numeric(y) || is.matrix(y) || !is.null(dim(y))) {
    stop(sprintf("'%s' must be a numeric vector.", y_name), call. = FALSE)
  }
  if (length(y) != nrow(x)) {
    stop(sprintf("length('%s') must equal nrow('%s').", y_name, x_name),
         call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop(sprintf("'%s' must contain only finite values.", y_name),
         call. = FALSE)
  }

  candidate_names <- colnames(x)
  if (!is.null(candidate_names) &&
      (anyNA(candidate_names) || any(!nzchar(candidate_names)) ||
       anyDuplicated(candidate_names))) {
    stop(sprintf("Column names of '%s' must be non-empty and unique.", x_name),
         call. = FALSE)
  }

  invisible(TRUE)
}

.validate_new_prediction_matrix <- function(pred_matrix, weights) {
  if (!is.matrix(pred_matrix) || !is.numeric(pred_matrix)) {
    stop("'pred_matrix' must be a numeric matrix.", call. = FALSE)
  }
  if (nrow(pred_matrix) < 1L) {
    stop("'pred_matrix' must contain at least one observation.", call. = FALSE)
  }
  if (any(!is.finite(pred_matrix))) {
    stop("'pred_matrix' must contain only finite values.", call. = FALSE)
  }
  if (ncol(pred_matrix) != nrow(weights)) {
    stop(paste0(
      "The number of columns in 'pred_matrix' must equal the number of ",
      "candidate models in the fitted MFF object."
    ), call. = FALSE)
  }

  expected_names <- rownames(weights)
  supplied_names <- colnames(pred_matrix)
  if (!is.null(expected_names)) {
    if (is.null(supplied_names)) {
      stop(paste0(
        "'pred_matrix' must have candidate-model column names matching the ",
        "fitted MFF object."
      ), call. = FALSE)
    }
    if (!identical(expected_names, supplied_names)) {
      stop(paste0(
        "Candidate-model columns in 'pred_matrix' must match the fitted ",
        "MFF weights in the same order."
      ), call. = FALSE)
    }
  }

  invisible(TRUE)
}
