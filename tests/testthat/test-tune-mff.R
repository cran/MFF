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
