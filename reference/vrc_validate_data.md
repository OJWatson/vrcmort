# Validate a VR long-format dataset

Validate that a dataset follows the `vrcmort` canonical VR long format.
This function is intentionally opinionated: it aims to catch common
failure modes early (duplicate cells, missing denominators, negative
counts, and pipelines where missing values are silently converted to
zeros).

The function does not modify `data` unless `repair = TRUE`.

## Usage

``` r
vrc_validate_data(
  data,
  id_cols = c("region", "time", "age", "sex", "cause"),
  y_col = "y",
  exposure_col = NULL,
  conflict_col = "conflict",
  allow_na_y = TRUE,
  duplicates = c("error", "warn"),
  check_missing_as_zero = TRUE,
  repair = FALSE
)
```

## Arguments

- data:

  A data.frame.

- id_cols:

  Character vector of identifier column names. Defaults to
  `c("region","time","age","sex","cause")`.

- y_col:

  Name of the observed count column. Defaults to `"y"`.

- exposure_col:

  Name of the exposure column. If `NULL` (default), uses `"exposure"` if
  present; otherwise uses `"pop"` if present.

- conflict_col:

  Name of the conflict intensity column. Defaults to `"conflict"`.

- allow_na_y:

  Logical. If TRUE (default), `y` may contain `NA`.

- duplicates:

  How to handle duplicated identifier rows. One of `"error"` (default)
  or `"warn"`.

- check_missing_as_zero:

  Logical. If TRUE (default), warns when there is evidence that missing
  rows may have been converted to zeros.

- repair:

  Logical. If TRUE, creates an `exposure` column from `pop` when
  `exposure` is absent.

## Value

Invisibly returns the input data.frame. If `repair = TRUE` and
`exposure` is absent but `pop` is present, an `exposure` column is
added.
