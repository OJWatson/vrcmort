# Write a custom Stan template to a directory

Copy the modular Stan model shipped with the package (including all
include files) to a user-specified directory. This provides a safe
starting point for custom model development.

Users can then edit the copied Stan files and fit them by passing the
path to the `.stan` file into
[`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md)
via `stan_model`.

## Usage

``` r
vrc_write_stan_template(path, model = "vr_reporting_model", overwrite = FALSE)
```

## Arguments

- path:

  Output directory.

- model:

  Name of the base Stan model file to copy (default:
  `"vr_reporting_model"`).

- overwrite:

  Logical. If `TRUE`, existing files in `path` will be overwritten.

## Value

Invisibly returns the path to the copied base Stan file.
