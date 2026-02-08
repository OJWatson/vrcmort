test_that("vrc_standata imputes missing y_miss and warns", {
  sim <- vrc_simulate(R = 2, T = 4, t0 = 2, seed = 123, omega = 0.95)

  # Identify a cell that has labeled deaths
  labeled_cells <- sim$df_full |>
    dplyr::filter(!is.na(region)) |>
    dplyr::distinct(time, age, sex, cause)

  target_cell <- labeled_cells[1, ]

  # Remove the corresponding missing label row from df_full
  df_incomplete <- sim$df_full |>
    dplyr::filter(
      !(is.na(region) &
        time == target_cell$time &
        age == target_cell$age &
        sex == target_cell$sex &
        cause == target_cell$cause)
    )

  expect_warning(
    sdat <- vrc_standata(data = df_incomplete, t0 = 2, use_mar_labels = TRUE),
    "Some cells with labeled deaths have no corresponding missing-region entry"
  )

  sd <- sdat$standata

  # Map values to indices (vrc_index uses 1..N based on unique values)
  # For simulation, they are already 1..N
  target_time_id <- target_cell$time
  target_age_id <- target_cell$age
  target_sex_id <- target_cell$sex
  target_cause_id <- target_cell$cause

  group_idx <- which(
    sd$time == target_time_id &
      sd$age == target_age_id &
      sd$sex == target_sex_id &
      sd$cause == target_cause_id
  )

  expect_equal(length(group_idx), 1)
  expect_equal(sd$y_miss[group_idx], 0)
})

test_that("vrc_standata completes df for all regions", {
  sim <- vrc_simulate(R = 3, T = 4, t0 = 2, seed = 123)

  # Ensure we have the values right for filtering
  r1 <- 1
  t1 <- 1
  a1 <- 1

  # Remove one region for one group
  df_incomplete <- sim$df_obs |>
    dplyr::filter(!(region == r1 & time == t1 & age == a1))

  sdat <- vrc_standata(data = df_incomplete, t0 = 2)
  sd <- sdat$standata

  # sd$y should be matrix [N, R]
  expect_equal(dim(sd$y), c(sd$N, sd$R))
  expect_equal(sd$R, 3)

  # The missing row should be imputed with y=0, exposure=1e-9
  group_idx <- which(sd$time == t1 & sd$age == a1 & sd$sex == 1 & sd$cause == 1)
  region_idx <- r1

  expect_equal(sd$y[group_idx, region_idx], 0)
  expect_equal(sd$exposure[group_idx, region_idx], 1e-9)
})
