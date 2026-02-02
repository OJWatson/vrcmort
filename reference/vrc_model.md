# Load and cache a Stan model

Load and cache a Stan model

## Usage

``` r
vrc_model(name = "vr_reporting_model", backend = c("rstan"), ...)
```

## Arguments

- name:

  Stan model name (see
  [`vrc_model_names()`](https://github.com/OJWatson/vrcmort/reference/vrc_model_names.md))
  or a path to a `.stan` file, or a
  [`vrc_model_spec()`](https://github.com/OJWatson/vrcmort/reference/vrc_model_spec.md)
  object.

- backend:

  Backend to use. Currently only `"rstan"` is supported.

- ...:

  Additional arguments passed to the backend model compiler.

## Value

A compiled Stan model object.
