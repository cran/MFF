test_that("boot.train returns aligned validation and test matrices", {
  dat <- make_regression_data()
  result <- boot.train(
    target = "y", data = dat,
    ntest = 12, nvalid = 10, B = 5,
    seed = 123, parallel = FALSE
  )

  expect_equal(dim(result$pred_matrix_valid), c(10L, 5L))
  expect_equal(dim(result$pred_matrix_test), c(12L, 5L))
  expect_length(result$y_valid, 10L)
  expect_length(result$y_test, 12L)
  expect_equal(result$metadata$ntrain, 58L)
  expect_equal(result$metadata$B, 5)
  expect_false(result$metadata$parallel)
})

test_that("boot.train validates essential inputs", {
  dat <- make_regression_data()

  expect_error(
    boot.train("missing", dat, ntest = 10, nvalid = 10, B = 2),
    "not found"
  )
  expect_error(
    boot.train("y", dat, ntest = 40, nvalid = 40, B = 2),
    "exceeds or equals"
  )
  expect_error(
    boot.train("y", dat, ntest = 10, nvalid = 10, B = 0),
    "positive integer"
  )
})

test_that("sequential and parallel bootstrap predictions agree", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 2, "Two cores are required")
  dat <- make_regression_data()

  sequential <- boot.train(
    "y", dat, ntest = 12, nvalid = 10, B = 4,
    seed = 123, parallel = FALSE
  )
  parallel_result <- boot.train(
    "y", dat, ntest = 12, nvalid = 10, B = 4,
    seed = 123, parallel = TRUE, ncores = 2
  )

  expect_equal(parallel_result$pred_matrix_valid, sequential$pred_matrix_valid)
  expect_equal(parallel_result$pred_matrix_test, sequential$pred_matrix_test)
  expect_equal(parallel_result$y_valid, sequential$y_valid)
  expect_equal(parallel_result$y_test, sequential$y_test)
})
