test_that("vrc_validate_data errors when exposure is missing", {
  sim <- vrc_simulate(R = 2, T = 6, t0 = 3, seed = 1)
  df <- sim$df_obs
  df$exposure <- NULL
  df$pop <- NULL

  expect_error(vrc_validate_data(df), "Missing exposure")
})

test_that("vrc_index creates integer indices and maps t0", {
  sim <- vrc_simulate(R = 3, T = 10, t0 = 4, seed = 2)
  idx <- vrc_index(sim$df_obs, t0 = 4, duplicates = "sum")
  expect_true(all(
    c("region_id", "time_id", "age_id", "sex_id", "cause_id") %in%
      names(idx$data)
  ))
  expect_equal(idx$meta$t0, 4)
  expect_equal(idx$meta$R, 3)
  expect_equal(idx$meta$T, 10)
})

test_that("vrc_diagnose_reporting returns tables and plots", {
  sim <- vrc_simulate(R = 2, T = 8, t0 = 4, seed = 3)
  diag <- vrc_diagnose_reporting(sim$df_obs, t0 = 4)
  expect_true(is.list(diag$tables))
  expect_true(is.list(diag$plots))
  expect_true(all(
    c("totals_time", "totals_time_cause") %in% names(diag$tables)
  ))
})
