
test_that("vrc_model_spec handles built-in and file paths", {
  spec1 <- vrc_model_spec("vr_reporting_model")
  expect_true(inherits(spec1, "vrc_model_spec"))
  expect_true(file.exists(spec1$file))
  expect_equal(spec1$name, "vr_reporting_model")
  expect_true(is.list(spec1$param_map))
  expect_equal(spec1$param_map$rho, "rho_rep")

  td <- tempfile("vrc_stan_")
  dir.create(td)
  base <- vrc_write_stan_template(td, overwrite = TRUE)
  expect_true(file.exists(base))

  # Should copy include subfolders
  expect_true(file.exists(file.path(td, "functions", "rw1_centered.stan")))
  expect_true(file.exists(file.path(td, "data", "data_vr.stan")))

  spec2 <- vrc_model_spec(base)
  expect_true(inherits(spec2, "vrc_model_spec"))
  expect_true(file.exists(spec2$file))
  expect_equal(normalizePath(base, winslash = "/"), spec2$file)
})
