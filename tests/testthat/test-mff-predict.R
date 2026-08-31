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

test_that("PCA clustering space preserves candidate-profile distances", {
  dat <- make_prediction_data(n = 80L)
  original <- t(dat$x)
  reduced <- MFF:::.mff_clustering_space(
    original, stand = FALSE, reduction = "pca"
  )

  expect_lte(ncol(reduced$data), nrow(original) - 1L)
  expect_equal(
    as.matrix(stats::dist(reduced$data)),
    as.matrix(stats::dist(original)),
    tolerance = 1e-8
  )

  fit <- mff(
    dat$x, dat$y, c = 2, method = "gk", nstart = 2,
    iter.max = 30, clustering.reduction = "pca"
  )
  expect_identical(fit$clustering_reduction$method, "pca")
  expect_lte(fit$clustering_reduction$retained_dimensions,
             ncol(dat$x) - 1L)
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

  expect_error(mff(with_na, dat$y, c = 2), "finite values")
  expect_error(mff(dat$x, dat$y, c = ncol(dat$x) + 1), "cannot exceed")
  expect_error(mff(as.data.frame(dat$x), dat$y, c = 2), "numeric matrix")
  expect_error(mff(dat$x, dat$y[-1], c = 2), "must equal")
  expect_error(mff(dat$x, dat$y, c = 0), "greater than or equal")
  expect_error(mff(dat$x, dat$y, c = 2, method = "fcm", m = 1),
               "greater than 1")
  expect_error(mff(dat$x, dat$y, c = 2, method = "pfcm", eta = 0),
               "positive")
})

test_that("predict.mff validates candidate identity and best-selection state", {
  dat <- make_prediction_data()
  fit <- mff(dat$x, dat$y, c = 2, method = "kmeans", nstart = 10)

  expect_error(predict(fit, dat$x, type = "best"), "tuned on validation")
  expect_error(predict(fit, dat$x[, -1, drop = FALSE], type = "all"),
               "number of columns")

  reordered <- dat$x[, rev(seq_len(ncol(dat$x))), drop = FALSE]
  expect_error(predict(fit, reordered, type = "all"), "same order")

  unnamed <- unname(dat$x)
  expect_error(predict(fit, unnamed, type = "all"), "must have")
})
