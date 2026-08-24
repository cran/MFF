test_that("evaluate computes metrics for a prediction vector", {
  actual <- c(1, 2, 4)
  predicted <- c(2, 2, 3)

  result <- evaluate(predicted, actual)

  expect_equal(dim(result), c(1L, 6L))
  expect_equal(unname(result[1, "MAE"]), 2 / 3)
  expect_equal(unname(result[1, "MSE"]), 2 / 3)
  expect_equal(unname(result[1, "RMSE"]), sqrt(2 / 3))
  expect_equal(unname(result[1, "MedAE"]), 1)
})

test_that("evaluate treats matrix columns as separate prediction vectors", {
  actual <- c(1, 2, 3)
  predicted <- cbind(perfect = actual, shifted = actual + 1)

  result <- evaluate(predicted, actual)

  expect_equal(dim(result), c(2L, 6L))
  expect_equal(result["perfect", "MAE"], 0)
  expect_equal(result["shifted", "MAE"], 1)
})
