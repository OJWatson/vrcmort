# Diagnose VR reporting artefacts

Quick exploratory summaries and plots to assess whether VR reporting
appears to change over time, particularly around a conflict start.

This is designed to be run before model fitting.

## Usage

``` r
vrc_diagnose_reporting(
  data,
  t0 = NULL,
  id_cols = c("region", "time", "age", "sex", "cause"),
  y_col = "y",
  cause_labels = NULL,
  aggregate_duplicates = TRUE
)
```

## Arguments

- data:

  A VR long-format data.frame.

- t0:

  Conflict start time (optional). May be an index or a time value.

- id_cols:

  Identifier columns.

- y_col:

  Outcome column.

- cause_labels:

  Optional named character vector mapping cause levels to human-readable
  labels.

- aggregate_duplicates:

  If TRUE (default), aggregates duplicate cells by summing `y`.

## Value

A list with components `tables` and `plots`.
