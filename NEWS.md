# MFF 0.2.3

* Added an end-to-end vignette demonstrating the
  `boot.train()`--`tune.mff()`--`predict()`--`evaluate()` workflow and the
  separation of validation-based selection from final test evaluation.
* Added a `testthat` unit-test suite covering the public interface, all four
  membership-generation methods, input validation, prediction-matrix
  dimensions, and sequential/parallel bootstrap reproducibility.
* Reworked examples to use lightweight prediction matrices and disabled
  parallel execution explicitly in examples and the vignette.
* Limited XGBoost and LightGBM inside `model.train()` to one thread to avoid
  oversubscribing CRAN check machines.
* Added `print()`, `summary()`, and `plot()` methods for fitted and tuned MFF
  objects.
* Extended `plot.mff()` with validation-safe test score,
  observed-versus-predicted, and test-series visualizations. Test plots use the
  validation-selected function by default and never reselect a function using
  test performance.
* Made the computationally intensive `model.train()` example interactive-only
  so that CRAN example checks do not initialize several external learner
  libraries merely to demonstrate the convenience function.
