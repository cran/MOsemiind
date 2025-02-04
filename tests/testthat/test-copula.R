test_that("Copula function works correctly", {
  copula_obj1 <- computeCopula("Clayton", 1.5)
  copula_obj2 <- computeCopula("Gumbel", 2)

  expect_true(inherits(copula_obj1, "copula"))
  expect_true(inherits(copula_obj2, "copula"))
  expect_error(computeCopula("InvalidType", 1), "Invalid copula type")
})
