test_that("vrc_standata handles missing labels correctly", {
  # Simulate with thinned labels
  sim <- vrc_simulate(R = 3, T = 12, t0 = 6, seed = 2, omega = 0.8)
  
  # Check that we have rows with NA region
  expect_true(any(is.na(sim$df_full$region)))
  
  sdat <- vrc_standata(
    data = sim$df_full,
    t0 = 6,
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
    iter = 100,
    warmup = 50,
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
