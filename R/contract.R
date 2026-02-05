# Data contract, validation, indexing, and diagnostics

#' Canonical VR long format
#'
#' @description
#' `vrcmort` works with a canonical "VR long" data layout: one row per cell
#' defined by region, time, age group, sex, and cause.
#'
#' The package separates two concepts:
#'
#' * **exposure**: the person-time at risk for the cell (for example,
#'   `population * days_in_period`, or `population * coverage_fraction`).
#' * **count**: the observed VR death count for that cell.
#'
#' In many settings, time bins are regular (for example, monthly) and exposure
#' can be set to the population estimate. If time bins are irregular or only
#' partially covered, exposure should reflect that.
#'
#' The minimal required columns are:
#'
#' * `region`, `time`, `age`, `sex`, `cause`: identifiers
#' * `y`: observed VR counts (non-negative integer; `NA` allowed)
#' * `exposure`: person-time at risk (positive numeric). If `exposure` is not
#'   present, `vrcmort` will use `pop` if available.
#' * `conflict`: conflict intensity proxy (numeric)
#'
#' Optional but commonly useful columns are:
#'
#' * `pop`: population size (if different from exposure)
#' * `quality_flag`: label for known low-quality observations (for example,
#'   `"system_down"`, `"age_missing_high"`).
#' * `quality_score`: numeric score between 0-1 capturing data quality.
#' * `weight`: optional weight used for aggregation or composition models.
#'
#' @return A character vector of recommended canonical column names.
#' @export
vrc_canonical_columns <- function() {
  c(
    "region",
    "time",
    "age",
    "sex",
    "cause",
    "y",
    "exposure",
    "pop",
    "conflict",
    "quality_flag",
    "quality_score",
    "weight"
  )
}

#' Validate a VR long-format dataset
#'
#' @description
#' Validate that a dataset follows the `vrcmort` canonical VR long format.
#' This function is intentionally opinionated: it aims to catch common failure
#' modes early (duplicate cells, missing denominators, negative counts, and
#' pipelines where missing values are silently converted to zeros).
#'
#' The function does not modify `data` unless `repair = TRUE`.
#'
#' @param data A data.frame.
#' @param id_cols Character vector of identifier column names.
#'   Defaults to `c("region","time","age","sex","cause")`.
#' @param y_col Name of the observed count column. Defaults to `"y"`.
#' @param exposure_col Name of the exposure column. If `NULL` (default), uses
#'   `"exposure"` if present; otherwise uses `"pop"` if present.
#' @param conflict_col Name of the conflict intensity column.
#'   Defaults to `"conflict"`.
#' @param allow_na_y Logical. If TRUE (default), `y` may contain `NA`.
#' @param duplicates How to handle duplicated identifier rows.
#'   One of `"error"` (default) or `"warn"`.
#' @param check_missing_as_zero Logical. If TRUE (default), warns when there is
#'   evidence that missing rows may have been converted to zeros.
#' @param repair Logical. If TRUE, creates an `exposure` column from `pop` when
#'   `exposure` is absent.
#'
#' @return Invisibly returns the input data.frame. If `repair = TRUE` and
#'   `exposure` is absent but `pop` is present, an `exposure` column is added.
#' @export
vrc_validate_data <- function(
  data,
  id_cols = c("region", "time", "age", "sex", "cause"),
  y_col = "y",
  exposure_col = NULL,
  conflict_col = "conflict",
  allow_na_y = TRUE,
  duplicates = c("error", "warn"),
  check_missing_as_zero = TRUE,
  repair = FALSE
) {
  stopifnot(is.data.frame(data))
  duplicates <- match.arg(duplicates)

  req <- c(id_cols, y_col, conflict_col)
  stop_if_missing_cols(data, req)

  # Determine exposure column
  if (is.null(exposure_col)) {
    if ("exposure" %in% names(data)) {
      exposure_col <- "exposure"
    } else if ("pop" %in% names(data)) {
      exposure_col <- "pop"
    } else {
      exposure_col <- NA_character_
    }
  }
  if (is.na(exposure_col) || !exposure_col %in% names(data)) {
    stop("Missing exposure column. Provide `exposure` or `pop`.", call. = FALSE)
  }

  # Optionally repair by creating exposure column
  if (
    isTRUE(repair) && exposure_col == "pop" && !("exposure" %in% names(data))
  ) {
    data$exposure <- data$pop
    exposure_col <- "exposure"
  }

  # Basic checks on identifiers
  for (nm in id_cols) {
    if (any(is.na(data[[nm]]))) {
      stop("Identifier column contains NA: ", nm, call. = FALSE)
    }
  }

  # Outcome checks
  y <- data[[y_col]]
  if (!allow_na_y && any(is.na(y))) {
    stop("`", y_col, "` contains NA but allow_na_y = FALSE", call. = FALSE)
  }
  y_non_na <- y[!is.na(y)]
  if (length(y_non_na)) {
    if (any(y_non_na < 0)) {
      stop("`", y_col, "` must be non-negative", call. = FALSE)
    }
    if (any(abs(y_non_na - round(y_non_na)) > 1e-8)) {
      stop("`", y_col, "` must be integer-like", call. = FALSE)
    }
  }

  # Exposure checks
  expo <- as.numeric(data[[exposure_col]])
  if (any(is.na(expo))) {
    stop("`", exposure_col, "` contains NA", call. = FALSE)
  }
  if (any(expo <= 0)) {
    stop("`", exposure_col, "` must be > 0", call. = FALSE)
  }

  # Duplicated cell keys
  key_df <- data[id_cols]
  dup <- duplicated(key_df)
  if (any(dup)) {
    msg <- paste0(
      "Duplicate rows detected for the identifier columns: ",
      paste(id_cols, collapse = ", "),
      ". Aggregate or deduplicate before fitting."
    )
    if (duplicates == "error") {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  # Heuristic: missing values reported as zeros
  if (isTRUE(check_missing_as_zero)) {
    if (any(!is.na(y) & y == 0)) {
      # look for suspicious step increases in zero rates post-conflict-like times
      # (only a warning, not definitive)
      zero_rate <- mean(y_non_na == 0)
      if (!is.na(zero_rate) && zero_rate > 0.50) {
        warning(
          "More than 50% of observed cells have y == 0. ",
          "If your pipeline converts missing values to zero, set them to NA before fitting.",
          call. = FALSE
        )
      }
    }
  }

  invisible(data)
}

#' Index a VR long-format dataset
#'
#' @description
#' Convert identifier columns to integer indices (1..R, 1..T, ...) used by Stan.
#' This also creates a stable ordering of time values and optionally aggregates
#' duplicate identifier rows.
#'
#' @param data A data.frame.
#' @param t0 Conflict start time. May be an integer index in 1..T or a value
#'   present in `data$time`.
#' @param id_cols Identifier columns. See [vrc_validate_data()].
#' @param y_col Outcome column name.
#' @param exposure_col Exposure column name. If `NULL`, uses `exposure` if
#'   present; otherwise uses `pop`.
#' @param conflict_col Conflict column name.
#' @param duplicates How to handle duplicates. One of `"error"` (default) or
#'   `"sum"`.
#' @param sort_time Logical. If TRUE (default), time values are sorted.
#'
#' @return A list with components:
#' * `data`: the processed data.frame (including `*_id` integer columns and an
#'   `exposure` column)
#' * `meta`: a list with levels and dimension sizes
#' @export
vrc_index <- function(
  data,
  t0 = NULL,
  id_cols = c("region", "time", "age", "sex", "cause"),
  y_col = "y",
  exposure_col = NULL,
  conflict_col = "conflict",
  duplicates = c("error", "sum"),
  sort_time = TRUE
) {
  duplicates <- match.arg(duplicates)
  df <- vrc_validate_data(
    data = data,
    id_cols = id_cols,
    y_col = y_col,
    exposure_col = exposure_col,
    conflict_col = conflict_col,
    allow_na_y = TRUE,
    duplicates = if (duplicates == "sum") "warn" else "error",
    repair = TRUE
  )

  # Standardise core column names used throughout the package.
  if (y_col != "y") {
    df$y <- df[[y_col]]
  }
  if (conflict_col != "conflict") {
    df$conflict <- df[[conflict_col]]
  }

  # Normalise exposure column name. If the user only provides `pop`, treat it
  # as exposure by default.
  if (!"exposure" %in% names(df)) {
    if (!is.null(exposure_col) && exposure_col %in% names(df)) {
      df$exposure <- df[[exposure_col]]
    } else if ("pop" %in% names(df)) {
      df$exposure <- df$pop
    }
  }

  # Keep a `pop` column if possible for user convenience. If population is not
  # distinct from exposure, duplicate it.
  if (!"pop" %in% names(df)) {
    df$pop <- df$exposure
  }

  # Aggregate duplicates if requested
  key_df <- df[id_cols]
  if (duplicates == "sum" && any(duplicated(key_df))) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package 'dplyr' is required for duplicates = 'sum'", call. = FALSE)
    }
    if (!requireNamespace("rlang", quietly = TRUE)) {
      stop("Package 'rlang' is required for duplicates = 'sum'", call. = FALSE)
    }

    other_cols <- setdiff(
      names(df),
      c(id_cols, "y", "exposure", "pop", "conflict")
    )
    gsyms <- rlang::syms(id_cols)
    df <- dplyr::group_by(df, !!!gsyms)
    df <- dplyr::summarise(
      df,
      y = sum(.data$y, na.rm = TRUE),
      exposure = dplyr::first(.data$exposure),
      pop = dplyr::first(.data$pop),
      conflict = dplyr::first(.data$conflict),
      dplyr::across(dplyr::all_of(other_cols), ~ dplyr::first(.x)),
      .groups = "drop"
    )
  }

  # Factors for stable indexing
  df$region_f <- factor(df$region)
  df$age_f <- factor(df$age)
  df$sex_f <- factor(df$sex)
  df$cause_f <- factor(df$cause)

  time_vals <- unique(df$time)
  if (isTRUE(sort_time)) {
    time_vals <- sort(time_vals)
  }
  df$time_f <- factor(df$time, levels = time_vals)

  df$region_id <- as.integer(df$region_f)
  df$time_id <- as.integer(df$time_f)
  df$age_id <- as.integer(df$age_f)
  df$sex_id <- as.integer(df$sex_f)
  df$cause_id <- as.integer(df$cause_f)

  # Compute t0 index if provided
  t0_index <- NA_integer_
  if (!is.null(t0)) {
    t0_index <- match(t0, time_vals)
    if (is.na(t0_index)) {
      if (
        is.numeric(t0) && length(t0) == 1 && t0 >= 1 && t0 <= length(time_vals)
      ) {
        t0_index <- as.integer(round(t0))
      } else {
        stop(
          "t0 must be a time value present in data$time, or an integer index in [1, T]",
          call. = FALSE
        )
      }
    }
  }

  meta <- list(
    R = nlevels(df$region_f),
    T = nlevels(df$time_f),
    A = nlevels(df$age_f),
    S = nlevels(df$sex_f),
    G = nlevels(df$cause_f),
    region_levels = levels(df$region_f),
    time_levels = time_vals,
    age_levels = levels(df$age_f),
    sex_levels = levels(df$sex_f),
    cause_levels = levels(df$cause_f),
    t0 = t0_index
  )

  list(data = df, meta = meta)
}

#' Diagnose VR reporting artefacts
#'
#' @description
#' Quick exploratory summaries and plots to assess whether VR reporting appears
#' to change over time, particularly around a conflict start.
#'
#' This is designed to be run before model fitting.
#'
#' @param data A VR long-format data.frame.
#' @param t0 Conflict start time (optional). May be an index or a time value.
#' @param id_cols Identifier columns.
#' @param y_col Outcome column.
#' @param cause_labels Optional named character vector mapping cause levels to
#'   human-readable labels.
#' @param aggregate_duplicates If TRUE (default), aggregates duplicate cells by
#'   summing `y`.
#'
#' @return A list with components `tables` and `plots`.
#' @export
vrc_diagnose_reporting <- function(
  data,
  t0 = NULL,
  id_cols = c("region", "time", "age", "sex", "cause"),
  y_col = "y",
  cause_labels = NULL,
  aggregate_duplicates = TRUE
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required", call. = FALSE)
  }

  idx <- vrc_index(
    data = data,
    t0 = t0,
    id_cols = id_cols,
    y_col = y_col,
    duplicates = if (isTRUE(aggregate_duplicates)) "sum" else "error",
    sort_time = TRUE
  )
  df <- idx$data
  meta <- idx$meta

  # Standardise a cause label for plotting
  df$cause_label <- as.character(df$cause)
  if (!is.null(cause_labels)) {
    if (!is.null(names(cause_labels))) {
      df$cause_label <- unname(cause_labels[df$cause_label])
    }
  }

  # Summaries
  totals_time <- dplyr::summarise(
    dplyr::group_by(df, .data$time_id),
    time = dplyr::first(.data$time),
    total_y = sum(.data[[y_col]], na.rm = TRUE),
    n_cells = dplyr::n(),
    n_missing = sum(is.na(.data[[y_col]])),
    .groups = "drop"
  )

  totals_time_cause <- dplyr::summarise(
    dplyr::group_by(df, .data$time_id, .data$cause_label),
    time = dplyr::first(.data$time),
    total_y = sum(.data[[y_col]], na.rm = TRUE),
    .groups = "drop"
  )

  cause_share <- dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(totals_time_cause, .data$time_id),
      share = .data$total_y / sum(.data$total_y)
    )
  )

  # Pre vs post age composition (all causes)
  if (!is.na(meta$t0)) {
    df$post <- as.integer(df$time_id >= meta$t0)
  } else {
    df$post <- NA_integer_
  }

  age_comp <- dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(
        dplyr::summarise(
          dplyr::group_by(df, .data$post, .data$age),
          total_y = sum(.data[[y_col]], na.rm = TRUE),
          .groups = "drop"
        ),
        .data$post
      ),
      share = .data$total_y / sum(.data$total_y)
    )
  )

  # Plots
  p_total <- ggplot2::ggplot(
    totals_time,
    ggplot2::aes(x = .data$time_id, y = .data$total_y)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = "Total VR deaths") +
    ggplot2::theme_minimal()

  if (!is.na(meta$t0)) {
    p_total <- p_total + ggplot2::geom_vline(xintercept = meta$t0, linetype = 2)
  }

  p_by_cause <- ggplot2::ggplot(
    totals_time_cause,
    ggplot2::aes(
      x = .data$time_id,
      y = .data$total_y,
      colour = .data$cause_label
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = "VR deaths", colour = NULL) +
    ggplot2::theme_minimal()

  if (!is.na(meta$t0)) {
    p_by_cause <- p_by_cause +
      ggplot2::geom_vline(xintercept = meta$t0, linetype = 2)
  }

  p_cause_share <- ggplot2::ggplot(
    cause_share,
    ggplot2::aes(x = .data$time_id, y = .data$share, colour = .data$cause_label)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = "Cause share", colour = NULL) +
    ggplot2::theme_minimal()

  if (!is.na(meta$t0)) {
    p_cause_share <- p_cause_share +
      ggplot2::geom_vline(xintercept = meta$t0, linetype = 2)
  }

  p_age_prepost <- ggplot2::ggplot(
    age_comp,
    ggplot2::aes(x = .data$age, y = .data$share, group = factor(.data$post))
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Age group", y = "Share of deaths", colour = NULL) +
    ggplot2::theme_minimal()

  if (!is.na(meta$t0)) {
    p_age_prepost <- p_age_prepost +
      ggplot2::facet_wrap(
        ~post,
        labeller = ggplot2::labeller(post = c(`0` = "Pre", `1` = "Post"))
      )
  }

  list(
    tables = list(
      totals_time = totals_time,
      totals_time_cause = totals_time_cause,
      cause_share = cause_share,
      age_comp_prepost = age_comp
    ),
    plots = list(
      total = p_total,
      by_cause = p_by_cause,
      cause_share = p_cause_share,
      age_prepost = p_age_prepost
    ),
    meta = meta
  )
}
