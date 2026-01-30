test_that("vrc_standata handles K_mort=0 and K_rep=0", {
  sim <- vrc_simulate(R = 2, T = 6, t0 = 3, seed = 1)

  sdat <- vrc_standata(
    data = sim$df_obs,
    t0 = sim$meta$t0,
    mortality_covariates = ~ 1,
    reporting_covariates = ~ 1
  )

  expect_equal(sdat$standata$K_mort, 0)
  expect_equal(sdat$standata$K_rep, 0)
  expect_true(is.matrix(sdat$standata$X_mort))
  expect_true(is.matrix(sdat$standata$X_rep))
  expect_equal(ncol(sdat$standata$X_mort), 0)
  expect_equal(ncol(sdat$standata$X_rep), 0)
})
