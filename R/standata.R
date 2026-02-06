#' Build Stan data for the VR reporting model
#'
#' @description
#' Convert a long-format VR dataset into the list structure expected by the
#' Stan model shipped with the package.
#'
#' The Stan model expects one row per cell:
#' `region` x `time` x `age` x `sex` x `cause`.
#'
#' The minimal required columns are:
#'
#' * `region`, `time`, `age`, `sex`, `cause`: identifiers
#' * `y`: observed VR counts (integer, can be `NA`)
#' * `exposure`: person-time at risk (numeric, > 0). If `exposure` is absent,
#'   `pop` is used as a fallback.
#' * `conflict`: conflict intensity proxy (numeric)
#'
#' Additional covariates can be included via `mortality_covariates` and
#' `reporting_covariates`.
#'
#' @param data A data.frame in VR long format.
#' @param t0 Conflict start time. May be an integer index, or a value that
#'   matches a value in `data$time`.
#' @param mortality_covariates Optional formula specifying additional covariates
#'   for the mortality (log-rate) component, excluding `conflict`.
#' @param reporting_covariates Optional formula specifying additional covariates
#'   for the reporting (logit-completeness) component, excluding `conflict`.
#' @param mortality_conflict How to model the conflict effect in the mortality
#'   component. Use `"fixed"` (default) for one effect shared across regions,
#'   or `"region"` for partial pooling (random slopes) by region.
#' @param reporting_conflict How to model the conflict effect in the reporting
#'   component. Use `"fixed"` (default) for one effect shared across regions,
#'   or `"region"` for partial pooling (random slopes) by region.
#' @param mortality_time How to model time variation in the mortality component.
#'   `"national"` uses a single random walk shared across regions; `"region"`
#'   adds region-specific random walk deviations around the national trend.
#' @param reporting_time How to model time variation in the reporting component.
#'   `"national"` uses a single random walk shared across regions; `"region"`
#'   adds region-specific random walk deviations around the national trend.
#' @param standardise Logical. If TRUE (recommended), standardise `conflict` and
#'   any covariate columns produced by the formulas.
#' @param scale_binary Logical. If TRUE, binary dummy columns in model matrices
#'   are also standardised. Defaults to FALSE.
#' @param drop_na_y Logical. If TRUE (default), rows with missing `y` are removed.
#' @param duplicates How to handle duplicate identifier rows. One of
#'   `"error"` (default) or `"sum"`.
#' @param use_mar_labels Logical. If TRUE, account for missing region labels
#'   using a Missing At Random (MAR) assumption with a labeling probability
#'   `omega`. If FALSE (default), rows with missing region labels are ignored.
#' @param priors Optional prior specification created by [vrc_priors()]. If
#'   `NULL` (default), uses [vrc_priors()] with package defaults.
#' @param prior_PD Logical. If TRUE, Stan ignores the likelihood and samples from
#'   the prior. Useful for prior predictive checks.
#'
#' @return A list with components:
#'
#' * `standata`: list passed to Stan
#' * `df`: processed data used for fitting (subset if `drop_na_y`)
#' * `meta`: dimension metadata and level mappings
#' * `scaling`: scaling information for `conflict`, `X_mort`, and `X_rep`
#'
#' @export
vrc_standata <- function(
  data,
  t0,
  mortality_covariates = NULL,
  reporting_covariates = NULL,
  mortality_conflict = c("fixed", "region"),
  reporting_conflict = c("fixed", "region"),
  mortality_time = c("national", "region"),
  reporting_time = c("national", "region"),
  standardise = TRUE,
  scale_binary = FALSE,
  drop_na_y = TRUE,
  duplicates = c("error", "sum"),
  use_mar_labels = FALSE,
  priors = NULL,
  prior_PD = FALSE
) {
  stopifnot(is.data.frame(data))
  duplicates <- match.arg(duplicates)
  mortality_conflict <- match.arg(mortality_conflict)
  reporting_conflict <- match.arg(reporting_conflict)
  mortality_time <- match.arg(mortality_time)
  reporting_time <- match.arg(reporting_time)

  if (!is.logical(standardise) || length(standardise) != 1) {
    stop("standardise must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(drop_na_y) || length(drop_na_y) != 1) {
    stop("drop_na_y must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(use_mar_labels) || length(use_mar_labels) != 1) {
    stop("use_mar_labels must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(prior_PD) || length(prior_PD) != 1) {
    stop("prior_PD must be TRUE or FALSE", call. = FALSE)
  }

  idx <- vrc_index(
    data = data,
    t0 = t0,
    duplicates = duplicates,
    sort_time = TRUE
  )

  df <- idx$data
  df_miss <- idx$data_miss
  meta <- idx$meta

  # Store modelling options in meta for downstream methods
  meta$model_options <- list(
    mortality_conflict = mortality_conflict,
    reporting_conflict = reporting_conflict,
    mortality_time = mortality_time,
    reporting_time = reporting_time,
    use_mar_labels = use_mar_labels
  )

  if (meta$T < 2) {
    stop("data must contain at least 2 time points", call. = FALSE)
  }
  if (is.na(meta$t0)) {
    stop("t0 could not be mapped to the time column", call. = FALSE)
  }

  # Optionally drop missing y
  if (isTRUE(drop_na_y)) {
    df <- df[!is.na(df$y), , drop = FALSE]
  }

  if (any(is.na(df$y))) {
    stop("y contains NA after drop_na_y=TRUE", call. = FALSE)
  }
  if (any(df$y < 0)) {
    stop("y must be non-negative", call. = FALSE)
  }
  check_positive(df$exposure, "exposure")

  # Standardise conflict
  if (standardise) {
    sc <- standardise_vector(df$conflict)
    df$conflict_z <- sc$x
    conflict_scaling <- sc
  } else {
    df$conflict_z <- as.numeric(df$conflict)
    conflict_scaling <- list(x = NULL, centre = 0, scale = 1)
  }

  # Model matrices
  X_mort <- model_matrix_no_intercept(mortality_covariates, df)
  X_rep <- model_matrix_no_intercept(reporting_covariates, df)

  if (standardise) {
    X_mort <- standardise_matrix(X_mort, scale_binary = scale_binary)
    X_rep <- standardise_matrix(X_rep, scale_binary = scale_binary)
  } else {
    attr(X_mort, "scaling") <- list(
      centre = rep(0, ncol(X_mort)),
      scale = rep(1, ncol(X_mort)),
      colnames = colnames(X_mort)
    )
    attr(X_rep, "scaling") <- list(
      centre = rep(0, ncol(X_rep)),
      scale = rep(1, ncol(X_rep)),
      colnames = colnames(X_rep)
    )
  }

  R <- meta$R
  T <- meta$T
  A <- meta$A
  S <- meta$S
  G <- meta$G

  if (G < 2) {
    stop(
      "The current Stan model assumes at least 2 cause groups (for example, trauma and non-trauma). ",
      call. = FALSE
    )
  }

  if (G > 2) {
    warning(
      "G > 2 detected. The shipped Stan model applies the age-selective reporting penalty only to cause==2. ",
      "Check that your cause coding matches this assumption.",
      call. = FALSE
    )
  }

  post <- as.integer(seq_len(T) >= meta$t0)

  # Prepare missing label data if present AND requested
  N_miss <- nrow(df_miss)
  if (N_miss > 0 && isTRUE(use_mar_labels)) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package 'dplyr' is required for missing labels", call. = FALSE)
    }

    # Identify shared grouping variables
    join_cols <- c("time_id", "age_id", "sex_id", "cause_id")

    # Create a reference for covariates/exposure by region
    # We want one row per (time, age, sex) with columns for each region's exposure/conflict
    # and shared covariates.

    # First, get unique (time, age, sex) cells in df_miss
    miss_cells <- df_miss |>
      dplyr::select(dplyr::all_of(join_cols)) |>
      dplyr::distinct()

    # Join labeled data to these cells to get region-specific values
    # We use a long-to-wide pivot approach conceptually, but simpler to just
    # extract matrices.

    exposure_miss <- matrix(0, N_miss, R)
    conflict_miss <- matrix(0, N_miss, R)

    # Shared covariates (assuming they don't vary by region for the same time/age/sex)
    # Extract from X_mort and X_rep matrices
    X_mort_miss <- matrix(0, N_miss, ncol(X_mort))
    X_rep_miss <- matrix(0, N_miss, ncol(X_rep))

    # To vectorize, we can join df_miss with df and then populate
    # However, since Stan needs matrices of size N_miss x R, we need to match indices.

    for (r_id in seq_len(R)) {
      # Filter labeled data for this region
      df_r <- df |> dplyr::filter(.data$region_id == r_id)

      # Join to df_miss
      matched_r <- df_miss |>
        dplyr::select(-c(conflict, exposure)) |> # avoid duplicate columns
        dplyr::left_join(
          df_r |>
            dplyr::select(dplyr::all_of(join_cols), "exposure", "conflict_z"),
          by = join_cols
        )

      exposure_miss[, r_id] <- matched_r$exposure |> dplyr::coalesce(0)
      conflict_miss[, r_id] <- matched_r$conflict_z |> dplyr::coalesce(0)
    }

    # For X_mort and X_rep, we assume they are the same across regions for a cell.
    # We take the first available match from df for each row in df_miss.
    df_covs <- df |>
      dplyr::select(dplyr::all_of(join_cols)) |>
      dplyr::mutate(.row_orig = dplyr::row_number()) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) |>
      dplyr::slice(1) |>
      dplyr::ungroup()

    matched_covs <- df_miss |>
      dplyr::left_join(df_covs, by = join_cols)

    valid_matches <- !is.na(matched_covs$.row_orig)
    row_indices <- matched_covs$.row_orig[valid_matches]

    if (length(row_indices) > 0) {
      X_mort_miss[valid_matches, ] <- X_mort[row_indices, , drop = FALSE]
      X_rep_miss[valid_matches, ] <- X_rep[row_indices, , drop = FALSE]
    }
  } else {
    exposure_miss <- matrix(0, 0, R)
    conflict_miss <- matrix(0, 0, R)
    X_mort_miss <- matrix(0, 0, ncol(X_mort))
    X_rep_miss <- matrix(0, 0, ncol(X_rep))
    N_miss <- 0
    miss_index <- NULL
  }

  standata <- list(
    N = nrow(df),
    R = R,
    T = T,
    A = A,
    S = S,
    G = G,
    region = as_stan_array_int(df$region_id),
    time = as_stan_array_int(df$time_id),
    age = as_stan_array_int(df$age_id),
    sex = as_stan_array_int(df$sex_id),
    cause = as_stan_array_int(df$cause_id),
    y = as_stan_array_int(df$y),
    exposure = as_stan_array(df$exposure),
    conflict = as_stan_array(df$conflict_z),
    use_beta_conf_re = as.integer(mortality_conflict == "region"),
    use_gamma_conf_re = as.integer(reporting_conflict == "region"),
    use_rw_region_lambda = as.integer(mortality_time == "region"),
    use_rw_region_rho = as.integer(reporting_time == "region"),
    K_mort = ncol(X_mort),
    X_mort = X_mort,
    K_rep = ncol(X_rep),
    X_rep = X_rep,
    post = as_stan_array_int(post),
    t0 = as.integer(meta$t0),
    N_miss = N_miss,
    time_miss = as_stan_array_int(df_miss$time_id[seq_len(N_miss)]),
    age_miss = as_stan_array_int(df_miss$age_id[seq_len(N_miss)]),
    sex_miss = as_stan_array_int(df_miss$sex_id[seq_len(N_miss)]),
    cause_miss = as_stan_array_int(df_miss$cause_id[seq_len(N_miss)]),
    y_miss = as_stan_array_int(df_miss$y[seq_len(N_miss)]),
    exposure_miss = exposure_miss,
    conflict_miss = conflict_miss,
    X_mort_miss = X_mort_miss,
    X_rep_miss = X_rep_miss,
    use_mar_labels = as.integer(use_mar_labels),
    prior_PD = as.integer(prior_PD)
  )

  priors_resolved <- vrc_resolve_priors(
    priors = priors,
    G = G,
    K_mort = ncol(X_mort),
    K_rep = ncol(X_rep),
    X_mort = X_mort,
    X_rep = X_rep
  )

  standata <- c(standata, priors_resolved)

  scaling <- list(
    conflict = conflict_scaling,
    X_mort = attr(X_mort, "scaling"),
    X_rep = attr(X_rep, "scaling")
  )

  list(
    standata = standata,
    df = df,
    meta = meta,
    scaling = scaling,
    priors = if (is.null(priors)) vrc_priors() else priors,
    priors_resolved = priors_resolved
  )
}
