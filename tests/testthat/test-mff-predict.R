test_that("mff returns normalized k-means weights and cluster scores", {
  dat <- make_prediction_data()
  set.seed(123)

  fit <- mff(dat$x, dat$y, c = 2, method = "kmeans", nstart = 10)

  expect_s3_class(fit, "mff")
  expect_identical(fit$method, "kmeans")
  expect_equal(dim(fit$weights), c(ncol(dat$x), 2L))
  expect_equal(colSums(fit$weights), rep(1, 2))
  expect_equal(rownames(fit$weights), colnames(dat$x))
  expect_equal(dim(fit$cluster_scores), c(2L, 6L))
})

test_that("all membership-generation methods return normalized weights", {
  dat <- make_prediction_data()

  for (method in c("fcm", "pfcm", "gk")) {
    set.seed(123)
    fit <- mff(
      dat$x, dat$y,
      c = 2, method = method,
      nstart = 2, iter.max = 30
    )

    expect_s3_class(fit, "mff")
    expect_equal(dim(fit$weights), c(ncol(dat$x), 2L))
    expect_true(all(is.finite(fit$weights)))
    expect_equal(unname(colSums(fit$weights)), rep(1, 2), tolerance = 1e-7)
  }
})

test_that("predict.mff applies all weights or the validation-selected weight", {
  dat <- make_prediction_data()
  fit <- mff(dat$x, dat$y, c = 2, method = "kmeans", nstart = 10)

  all_predictions <- predict(fit, pred_matrix = dat$x, type = "all")
  expect_equal(all_predictions$mff_preds, dat$x %*% fit$weights)

  fit$best_weight <- fit$weights[, 2]
  best_prediction <- predict(fit, pred_matrix = dat$x, type = "best")
  expect_equal(drop(best_prediction$mff_preds), drop(dat$x %*% fit$best_weight))
})

test_that("mff rejects invalid prediction matrices", {
  dat <- make_prediction_data()
  with_na <- dat$x
  with_na[1, 1] <- NA_real_

  expect_error(mff(with_na, dat$y, c = 2), "contains NA")
  expect_error(mff(dat$x, dat$y, c = ncol(dat$x) + 1), "cannot exceed")
})
