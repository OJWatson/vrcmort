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
  meta <- idx$meta

  # Store modelling options in meta for downstream methods
  meta$model_options <- list(
    mortality_conflict = mortality_conflict,
    reporting_conflict = reporting_conflict,
    mortality_time = mortality_time,
    reporting_time = reporting_time
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

  if (any(is.na(df$y))) stop("y contains NA after drop_na_y=TRUE", call. = FALSE)
  if (any(df$y < 0)) stop("y must be non-negative", call. = FALSE)
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
    attr(X_mort, "scaling") <- list(centre = rep(0, ncol(X_mort)), scale = rep(1, ncol(X_mort)), colnames = colnames(X_mort))
    attr(X_rep, "scaling") <- list(centre = rep(0, ncol(X_rep)), scale = rep(1, ncol(X_rep)), colnames = colnames(X_rep))
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

  standata <- list(
    N = nrow(df),
    R = R,
    T = T,
    A = A,
    S = S,
    G = G,
    region = as.integer(df$region_id),
    time = as.integer(df$time_id),
    age = as.integer(df$age_id),
    sex = as.integer(df$sex_id),
    cause = as.integer(df$cause_id),
    y = as.integer(df$y),
    exposure = as.numeric(df$exposure),
    conflict = as.numeric(df$conflict_z),
    use_beta_conf_re = as.integer(mortality_conflict == "region"),
    use_gamma_conf_re = as.integer(reporting_conflict == "region"),
    use_rw_region_lambda = as.integer(mortality_time == "region"),
    use_rw_region_rho = as.integer(reporting_time == "region"),
    K_mort = ncol(X_mort),
    X_mort = X_mort,
    K_rep = ncol(X_rep),
    X_rep = X_rep,
    post = post,
    t0 = as.integer(meta$t0),
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
