# Define a model specification

Create a small model specification object that can be used to fit either
the built-in Stan model shipped with `vrcmort` or a user-supplied Stan
model file. This keeps the R-side data preparation and posterior
extraction tools working even when users modify Stan code.

The `param_map` component is a named list mapping key quantities to Stan
variable names. The default mapping matches the generated quantities in
the base model.

## Usage

``` r
vrc_model_spec(
  model = "vr_reporting_model",
  stan_file = NULL,
  backend = c("rstan"),
  param_map = NULL
)
```

## Arguments

- model:

  Either the name of a built-in Stan model (see
  [`vrc_model_names()`](https://github.com/OJWatson/vrcmort/reference/vrc_model_names.md))
  or a path to a `.stan` file.

- stan_file:

  Optional explicit path to a `.stan` file. If supplied, it overrides
  `model`.

- backend:

  Backend to use. Currently only `"rstan"` is supported.

- param_map:

  Named list mapping `rho`, `lambda`, `mu`, `y_rep`, and optionally
  `log_lik` to Stan variable names.

## Value

An object of class `vrc_model_spec`.
