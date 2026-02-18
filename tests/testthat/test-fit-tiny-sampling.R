test_that("Tiny end-to-end sampling run succeeds", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rstan")

  sim <- vrc_simulate(R = 1, T = 4, t0 = 2, seed = 123)

  fit <- vrc_fit(
    data = sim$df_obs,
    t0 = sim$meta$t0,
    mortality_covariates = ~1,
    reporting_covariates = ~1,
    chains = 1,
    iter = 60,
    warmup = 30,
    seed = 123,
    refresh = 0,
    cores = 1
  )

  expect_true(inherits(fit, "vrcfit"))
  expect_true(inherits(fit$stanfit, "stanfit"))
})

test_that("Tiny end-to-end sampling run succeeds, single cause", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rstan")

  sim <- vrc_simulate(R = 1, T = 4, t0 = 2, seed = 123)

  df_obs <- sim$df_obs[sim$df_obs$cause == 1, ]

  fit <- vrc_fit(
    data = df_obs,
    t0 = sim$meta$t0,
    mortality_covariates = ~1,
    reporting_covariates = ~1,
    chains = 1,
    iter = 60,
    warmup = 30,
    seed = 123,
    refresh = 0,
    cores = 1
  )

  expect_true(inherits(fit, "vrcfit"))
  expect_true(inherits(fit$stanfit, "stanfit"))
})
