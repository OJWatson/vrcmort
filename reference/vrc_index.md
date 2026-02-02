# Index a VR long-format dataset

Convert identifier columns to integer indices (1..R, 1..T, ...) used by
Stan. This also creates a stable ordering of time values and optionally
aggregates duplicate identifier rows.

## Usage

``` r
vrc_index(
  data,
  t0 = NULL,
  id_cols = c("region", "time", "age", "sex", "cause"),
  y_col = "y",
  exposure_col = NULL,
  conflict_col = "conflict",
  duplicates = c("error", "sum"),
  sort_time = TRUE
)
```

## Arguments

- data:

  A data.frame.

- t0:

  Conflict start time. May be an integer index in 1..T or a value
  present in `data$time`.

- id_cols:

  Identifier columns. See
  [`vrc_validate_data()`](https://github.com/OJWatson/vrcmort/reference/vrc_validate_data.md).

- y_col:

  Outcome column name.

- exposure_col:

  Exposure column name. If `NULL`, uses `exposure` if present; otherwise
  uses `pop`.

- conflict_col:

  Conflict column name.

- duplicates:

  How to handle duplicates. One of `"error"` (default) or `"sum"`.

- sort_time:

  Logical. If TRUE (default), time values are sorted.

## Value

A list with components:

- `data`: the processed data.frame (including `*_id` integer columns and
  an `exposure` column)

- `meta`: a list with levels and dimension sizes
