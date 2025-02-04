test_that("Shiny app directory exists", {
  app_dir <- system.file("app", package = "MOsemiind")
  expect_true(dir.exists(app_dir), info = "Shiny app directory should exist")
})
