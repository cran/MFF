test_that("print and summary methods describe a fitted MFF object", {
  dat <- make_prediction_data()
  fit <- mff(dat$x, dat$y, c = 2, method = "kmeans", nstart = 10)

  expect_output(returned <- print(fit), "Meta Fuzzy Function object")
  expect_identical(returned, fit)

  summary_fit <- summary(fit)
  expect_s3_class(summary_fit, "summary.mff")
  expect_identical(summary_fit$algorithm, "kmeans")
  expect_equal(summary_fit$n_models, ncol(dat$x))
  expect_equal(summary_fit$n_functions, 2L)
  expect_false(summary_fit$tuned)
  expect_output(print(summary_fit), "Membership-derived weights")
})

test_that("summary reports validation-selected tuning information", {
  dat <- make_prediction_data()
  fit <- tune.mff(
    dat$x, dat$y, max_c = 3,
    mff.method = "kmeans", nstart = 10,
    parallel = FALSE, logging = FALSE
  )

  summary_fit <- summary(fit)
  expect_true(summary_fit$tuned)
  expect_equal(summary_fit$best_cluster, fit$best_cluster)
  expect_equal(summary_fit$best_weight, fit$best_weight)
  expect_output(print(fit), "Validation-selected function")
})

test_that("plot.mff draws weights and scores", {
  dat <- make_prediction_data()
  fit <- mff(dat$x, dat$y, c = 2, method = "kmeans", nstart = 10)

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_identical(plot(fit, type = "weights"), fit)
  expect_identical(plot(fit, type = "weight_heatmap"), fit)
  expect_identical(
    plot(fit, type = "weight_heatmap", heatmap_values = FALSE),
    fit
  )
  expect_identical(plot(fit, type = "scores", metric = "MAE"), fit)
  expect_error(plot(fit, type = "scores", metric = "unknown"), "Unknown metric")
  expect_error(
    plot(fit, type = "weight_heatmap", heatmap_values = NA),
    "must be TRUE or FALSE"
  )
  expect_error(
    plot(fit, type = "weight_heatmap", heatmap_digits = -1),
    "non-negative integer"
  )
})

test_that("plot.mff creates validation-selected test plots", {
  dat <- make_prediction_data()
  fit <- tune.mff(
    dat$x, dat$y, max_c = 3,
    mff.method = "kmeans", nstart = 10,
    parallel = FALSE, logging = FALSE
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_identical(
    plot(fit, type = "test_scores", pred_matrix = dat$x, actual = dat$y),
    fit
  )
  expect_identical(
    plot(fit, type = "observed_predicted", pred_matrix = dat$x, actual = dat$y),
    fit
  )
  expect_identical(
    plot(fit, type = "series", pred_matrix = dat$x, actual = dat$y),
    fit
  )
  expect_identical(
    plot(fit, type = "test_scores", pred_matrix = dat$x, actual = dat$y,
         show_all = TRUE),
    fit
  )
})

test_that("plot.mff validates test inputs and prevents implicit test selection", {
  dat <- make_prediction_data()
  fit <- mff(dat$x, dat$y, c = 2, method = "kmeans", nstart = 10)

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(plot(fit, type = "test_scores"), "required")
  expect_error(
    plot(fit, type = "test_scores", pred_matrix = dat$x, actual = dat$y),
    "validation-selected tuned MFF object"
  )

  misaligned <- dat$x[, rev(seq_len(ncol(dat$x))), drop = FALSE]
  expect_error(
    plot(fit, type = "test_scores", pred_matrix = misaligned,
         actual = dat$y, show_all = TRUE),
    "must match"
  )
})
