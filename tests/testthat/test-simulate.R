test_that("vrc_simulate returns expected objects", {
  sim <- vrc_simulate(R = 5, T = 24, t0 = 12, seed = 1)
  expect_true(is.list(sim))
  expect_true(all(c("df_full", "df_obs", "truth", "meta") %in% names(sim)))
  expect_true(is.data.frame(sim$df_full))
  expect_true(is.data.frame(sim$df_obs))

  # basic dimensions
  A <- length(sim$meta$age_breaks) - 1
  S <- length(sim$meta$sexes)
  G <- sim$meta$G
  expect_equal(nrow(sim$df_full), 5 * 24 * A * S * G)
  expect_true(nrow(sim$df_obs) <= nrow(sim$df_full))

  # required columns
  req <- c(
    "region",
    "time",
    "age",
    "sex",
    "cause",
    "y",
    "exposure",
    "pop",
    "conflict",
    "facility"
  )
  expect_true(all(req %in% names(sim$df_full)))
})

test_that("vrc_simulate accepts partial missingness specifications", {
  expect_silent(vrc_simulate(
    R = 5,
    T = 24,
    t0 = 12,
    seed = 1,
    missing = list(type = "none")
  ))
  expect_silent(vrc_simulate(
    R = 5,
    T = 24,
    t0 = 12,
    seed = 1,
    missing = list(type = "block", block_intercept = -1.5)
  ))
  expect_silent(vrc_simulate(
    R = 5,
    T = 24,
    t0 = 12,
    seed = 1,
    missing = list(type = "age_selective", age_dropout_strength = 1.0)
  ))
  expect_silent(vrc_simulate(
    R = 5,
    T = 24,
    t0 = 12,
    seed = 1,
    missing = list(type = "mnar", mnar_strength = 1.0)
  ))
  expect_silent(vrc_simulate(
    R = 5,
    T = 24,
    t0 = 12,
    seed = 1,
    missing = list(
      type = "combined",
      block_intercept = -2.0,
      age_dropout_strength = 1.0,
      mnar_strength = 1.0
    )
  ))
})
