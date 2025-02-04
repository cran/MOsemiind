test_that("Dependence measures return correct values", {
  measures <- computeDependenceMeasures(NULL)  # Use mock input
  expect_named(measures, c("kendall", "spearman"))
  expect_true(all(sapply(measures, is.numeric)))
})
