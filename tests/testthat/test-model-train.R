test_that("model.train returns compatible prediction matrices", {
  skip_if_not_installed("MASS")

  result <- model.train(
    target = "medv", data = MASS::Boston,
    ntest = 20, nvalid = 20, seed = 123
  )

  expect_equal(dim(result$pred_matrix_valid), c(20L, 7L))
  expect_equal(dim(result$pred_matrix_test), c(20L, 7L))
  expect_identical(colnames(result$pred_matrix_valid), colnames(result$pred_matrix_test))
  expect_length(result$y_valid, 20L)
  expect_length(result$y_test, 20L)
})
