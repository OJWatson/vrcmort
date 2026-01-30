test_that("Built-in Stan model parses and includes resolve", {
  testthat::skip_if_not_installed("rstan")

  f <- system.file("stan", "vr_reporting_model.stan", package = "vrcmort")
  expect_true(nzchar(f))
  expect_true(file.exists(f))

  x <- rstan::stanc(
    file = f,
    model_name = "vr_reporting_model_test",
    allow_undefined = TRUE
  )

  expect_true(isTRUE(x$status))
})
