# Canonical VR long format

`vrcmort` works with a canonical "VR long" data layout: one row per cell
defined by region, time, age group, sex, and cause.

The package separates two concepts:

- **exposure**: the person-time at risk for the cell (for example,
  `population * days_in_period`, or `population * coverage_fraction`).

- **count**: the observed VR death count for that cell.

In many settings, time bins are regular (for example, monthly) and
exposure can be set to the population estimate. If time bins are
irregular or only partially covered, exposure should reflect that.

The minimal required columns are:

- `region`, `time`, `age`, `sex`, `cause`: identifiers

- `y`: observed VR counts (non-negative integer; `NA` allowed)

- `exposure`: person-time at risk (positive numeric). If `exposure` is
  not present, `vrcmort` will use `pop` if available.

- `conflict`: conflict intensity proxy (numeric)

Optional but commonly useful columns are:

- `pop`: population size (if different from exposure)

- `quality_flag`: label for known low-quality observations (for example,
  `"system_down"`, `"age_missing_high"`).

- `quality_score`: numeric score between 0-1 capturing data quality.

- `weight`: optional weight used for aggregation or composition models.

## Usage

``` r
vrc_canonical_columns()
```

## Value

A character vector of recommended canonical column names.
