test_that("vrc_fit returns standata when chains=0", {
  sim <- vrc_simulate(R = 2, T = 6, t0 = 3, seed = 1)
  out <- vrc_fit(
    data = sim$df_obs,
    t0 = 3,
    mortality_covariates = ~facility,
    reporting_covariates = ~facility,
    chains = 0
  )

  expect_true(is.list(out))
  expect_true(all(
    c("standata", "df", "meta", "scaling", "priors", "priors_resolved") %in%
      names(out)
  ))
})
