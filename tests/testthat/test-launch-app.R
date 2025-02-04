test_that("Shiny app launches and stops without errors", {
  skip_on_cran()  # Skip test on CRAN
  skip_if_not(interactive())  # Skip in non-interactive sessions (like `devtools::check()`)

  app <- launch_MOsemiind_app()

  Sys.sleep(2)  # Give time for the app to start

  expect_silent(app)

  on.exit(stopApp(app), add = TRUE)  # Force-close the app to prevent hanging
})
