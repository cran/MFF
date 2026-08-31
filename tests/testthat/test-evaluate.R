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

test_that("evaluate rejects malformed or non-finite inputs", {
  expect_error(evaluate(c(1, 2), c(1)), "must equal")
  expect_error(evaluate(c(1, Inf), c(1, 2)), "finite")
  expect_error(evaluate(character(), numeric()), "numeric")
  expect_error(evaluate(numeric(), numeric()), "must not be empty")
})
