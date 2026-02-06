test_that("vrc_standata handles missing labels correctly", {
  # Simulate with thinned labels
  sim <- vrc_simulate(R = 5, T = 20, t0 = 2, seed = 2, omega = 0.8)

  # Check that we have rows with NA region
  expect_true(any(is.na(sim$df_full$region)))

  sdat <- vrc_standata(
    data = sim$df_full,
    t0 = sim$meta$t0,
    use_mar_labels = TRUE
  )

  sd <- sdat$standata
  expect_gt(sd$N_miss, 0)
  expect_equal(sd$use_mar_labels, 1)
  expect_equal(ncol(sd$exposure_miss), sd$R)
  expect_equal(nrow(sd$exposure_miss), sd$N_miss)

  # Check that y_miss matches the sum of unlabeled in truth
  expect_equal(sum(sd$y_miss), sum(sim$truth$Y_miss_raw))
})

test_that("Tiny sampling run with missing labels succeeds", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rstan")

  # Very small simulation
  sim <- vrc_simulate(R = 2, T = 4, t0 = 2, seed = 123, omega = 0.7)

  fit <- vrc_fit(
    data = sim$df_full,
    t0 = sim$meta$t0,
    use_mar_labels = TRUE,
    chains = 1,
    iter = 60,
    warmup = 30,
    seed = 123,
    refresh = 0,
    cores = 1,
    init = function() list(omega = 0.8)
  )

  expect_true(inherits(fit, "vrcfit"))
  expect_true(inherits(fit$stanfit, "stanfit"))

  # Check that omega was estimated
  pars <- rstan::extract(fit$stanfit)
  expect_true("omega" %in% names(pars))
  expect_true(all(pars$omega >= 0 & pars$omega <= 1))
})

test_that("Fitting works with both under-reporting and missing labels", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rstan")

  # Simulation with strong under-reporting (rho), missing labels (omega),
  # and missing cells (NA)
  sim <- vrc_simulate(
    #check it works
    R = 2,
    T = 4,
    t0 = 2,
    seed = 42,
    omega = 0.95,
    rho0_true = c(0.9, 0.9),
    misclass = list(p_non_to_trauma = 0.00, p_trauma_to_non = 0.00),
    missing = list(type = "combined") #,
    #pop_total_range = c(200, 400)
  )

  fit <- vrc_fit(
    data = sim$df_obs,
    t0 = sim$meta$t0,
    use_mar_labels = TRUE,
    chains = 1,
    iter = 60,
    warmup = 30,
    seed = 42,
    refresh = 0,
    cores = 1
  )

  expect_true(inherits(fit, "vrcfit"))

  # Expected counts should be non-negative
  mu <- fitted(fit)
  expect_true(all(mu >= 0))

  # R-hat should be calculated
  sm <- rstan::summary(fit$stanfit, pars = "omega")$summary
  expect_true(!is.na(sm[1, "Rhat"]))
})
