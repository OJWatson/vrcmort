# Extract cell-level truth from a simulated dataset

Helper for benchmarking. Given an object returned by
[`vrc_simulate()`](https://github.com/OJWatson/vrcmort/reference/vrc_simulate.md),
return a data.frame aligned to `sim$df_full` containing cell-level true
mortality rates, reporting completeness, and true deaths.

## Usage

``` r
vrc_sim_truth(sim)
```

## Arguments

- sim:

  A list returned by
  [`vrc_simulate()`](https://github.com/OJWatson/vrcmort/reference/vrc_simulate.md).

## Value

A data.frame with additional columns:

- `lambda_true` latent mortality rate

- `rho_true` reporting completeness

- `D_true` true deaths for the cause group
