
test_that("vrc_standata produces a valid list", {
  sim <- vrc_simulate(R = 3, T = 12, t0 = 6, seed = 2)
  sdat <- vrc_standata(
    data = sim$df_obs,
    t0 = 6,
    mortality_covariates = ~ facility,
    reporting_covariates = ~ facility
  )

  expect_true(is.list(sdat))
  expect_true(all(c("standata", "df", "meta", "scaling", "priors", "priors_resolved") %in% names(sdat)))

  sd <- sdat$standata
  expect_true(is.list(sd))
  expect_true(all(c("N", "R", "T", "A", "S", "G") %in% names(sd)))
  expect_equal(sd$R, length(unique(sim$df_obs$region)))
  expect_equal(sd$T, length(unique(sim$df_obs$time)))
  expect_true(sd$K_mort >= 0)
  expect_true(sd$K_rep >= 0)

  # conflict should be numeric
  expect_true(is.numeric(sd$conflict))

  # exposure should be numeric
  expect_true(is.numeric(sd$exposure))

  # post indicator should have length T
  expect_equal(length(sd$post), sd$T)

  # structured flags
  expect_true(all(c("use_beta_conf_re", "use_gamma_conf_re", "use_rw_region_lambda", "use_rw_region_rho") %in% names(sd)))
  expect_equal(sd$use_beta_conf_re, 0)
  expect_equal(sd$use_gamma_conf_re, 0)
  expect_equal(sd$use_rw_region_lambda, 0)
  expect_equal(sd$use_rw_region_rho, 0)

  # priors should be present in standata
  expect_true(all(c(
    "prior_alpha0_loc",
    "prior_beta_conf_loc",
    "prior_kappa0_loc",
    "prior_phi_rate"
  ) %in% names(sd)))
})

test_that("vrc_standata toggles structured flags", {
  sim <- vrc_simulate(R = 3, T = 12, t0 = 6, seed = 2)
  sdat <- vrc_standata(
    data = sim$df_obs,
    t0 = 6,
    mortality_covariates = ~ facility,
    reporting_covariates = ~ facility,
    mortality_conflict = "region",
    reporting_conflict = "region",
    mortality_time = "region",
    reporting_time = "region"
  )
  sd <- sdat$standata
  expect_equal(sd$use_beta_conf_re, 1)
  expect_equal(sd$use_gamma_conf_re, 1)
  expect_equal(sd$use_rw_region_lambda, 1)
  expect_equal(sd$use_rw_region_rho, 1)
})
