test_that("tune.mff selects a cluster using validation scores", {
  dat <- make_prediction_data()

  fit <- tune.mff(
    dat$x, dat$y,
    max_c = 3,
    mff.method = "kmeans",
    nstart = 10,
    seed = 123,
    logging = FALSE
  )

  expect_s3_class(fit, "mff")
  expect_identical(fit$algorithm, "kmeans")
  expect_true(fit$best_cluster %in% seq_len(ncol(fit$weights)))
  expect_equal(fit$best_weight, fit$weights[, fit$best_cluster])
  expect_equal(
    fit$best_cluster,
    unname(which.min(fit$best_scores[, fit$eval.method]))
  )
})

test_that("tune.mff rejects more clusters than candidate models", {
  dat <- make_prediction_data()
  expect_error(
    tune.mff(dat$x, dat$y, max_c = 5, mff.method = "kmeans"),
    "cannot exceed"
  )
})

test_that("tune.mff validates grids and execution arguments", {
  dat <- make_prediction_data()

  expect_error(tune.mff(dat$x, dat$y, max_c = 1), "greater than or equal")
  expect_error(
    tune.mff(dat$x, dat$y, max_c = 2, mff.method = "fcm", m_seq = c(1, 2)),
    "greater than 1"
  )
  expect_error(
    tune.mff(dat$x, dat$y, max_c = 2, mff.method = "pfcm", eta_seq = 0),
    "positive"
  )
  expect_error(tune.mff(dat$x, dat$y, max_c = 2, parallel = NA),
               "TRUE or FALSE")
})

test_that("tune.mff propagates PCA clustering reduction", {
  dat <- make_prediction_data(n = 60L)
  fit <- tune.mff(
    dat$x, dat$y, max_c = 2, m_seq = 2,
    mff.method = "gk", iter.max = 30, nstart = 2,
    clustering.reduction = "pca", logging = FALSE
  )
  expect_identical(fit$clustering_reduction$method, "pca")
  expect_lte(fit$clustering_reduction$retained_dimensions,
             ncol(dat$x) - 1L)
})

test_that("FCM tuning uses the declared nstart argument", {
  dat <- make_prediction_data(n = 40L)
  fit <- tune.mff(
    dat$x, dat$y, max_c = 2, m_seq = 2,
    mff.method = "fcm", iter.max = 30, nstart = 3,
    clustering.reduction = "pca", logging = FALSE
  )
  expect_s3_class(fit, "mff")
  expect_identical(fit$algorithm, "fcm")
})

test_that("tune.mff skips a failed configuration and records diagnostics", {
  dat <- make_prediction_data(n = 30L)
  testthat::local_mocked_bindings(
    mff = function(x, y, c, m, eta, iter.max, nstart, method, stand,
                   clustering.reduction) {
      if (m < 1.5) stop("deliberate grid failure", call. = FALSE)
      weights <- matrix(1 / ncol(x), nrow = ncol(x), ncol = c)
      rownames(weights) <- colnames(x)
      colnames(weights) <- seq_len(c)
      scores <- matrix(rep(c(1, 1, 1, 1, 1, 1), c), nrow = c,
                       byrow = TRUE)
      colnames(scores) <- c("MAE", "RMSE", "MAPE", "SMAPE", "MSE", "MedAE")
      list(
        weights = weights,
        cluster_scores = scores,
        clustering_reduction = list(method = clustering.reduction)
      )
    },
    .package = "MFF"
  )

  fit <- tune.mff(
    dat$x, dat$y, max_c = 2, m_seq = c(1.1, 2),
    mff.method = "fcm", logging = FALSE
  )

  expect_equal(fit$n_successful, 1L)
  expect_equal(fit$n_failed, 1L)
  expect_equal(fit$best_params$m, 2)
  expect_equal(fit$search_diagnostics$success, c(FALSE, TRUE))
  expect_match(fit$search_diagnostics$message[[1L]], "deliberate grid failure")
})
