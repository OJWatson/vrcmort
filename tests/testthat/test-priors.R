
test_that("custom priors are propagated into standata", {
  # Minimal deterministic dataset
  df <- expand.grid(
    region = 1:2,
    time = 1:2,
    age = 1:2,
    sex = "all",
    cause = c("trauma", "non"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  df$y <- rep(0:1, length.out = nrow(df))
  df$exposure <- 1000
  df$conflict <- rep(c(0, 1), length.out = nrow(df))
  df$x <- rep(c(0, 2), length.out = nrow(df))

  pr <- vrc_priors(
    beta_conf = normal(0.5, 0.1),
    beta_mort = normal(0, 0.4, autoscale = TRUE),
    phi = exponential(2)
  )

  sdat <- vrc_standata(
    data = df,
    t0 = 2,
    mortality_covariates = ~ x,
    reporting_covariates = NULL,
    standardise = FALSE,
    priors = pr
  )

  sd <- sdat$standata
  G <- sd$G

  expect_equal(as.numeric(sd$prior_beta_conf_loc), rep(0.5, G))
  expect_equal(as.numeric(sd$prior_beta_conf_scale), rep(0.1, G))
  expect_equal(as.numeric(sd$prior_phi_rate), rep(2, G))

  # autoscale for x: two unique values -> range = 2
  expect_equal(as.numeric(sd$prior_beta_mort_scale), 0.4 / 2)
})
