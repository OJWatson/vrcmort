# High-level interface

#' Mortality model component specification
#'
#' @description
#' Create a mortality component specification for [vrcm()]. This is a light
#' wrapper around a formula describing additional mortality covariates (beyond
#' the always-included conflict proxy).
#'
#' The formula should not include `conflict`; conflict is included in the core
#' Stan model as `beta_conf`.
#'
#' @param formula A model formula. The default `~ 1` specifies no additional
#'   covariates.
#' @param conflict How to model the conflict effect in the mortality component.
#'   Use `"fixed"` for one effect shared across regions (per cause), or
#'   `"region"` for partial pooling (random slopes) by region (per cause).
#' @param time How to model time variation in the mortality component.
#'   `"national"` uses a single random walk shared across regions (per cause).
#'   `"region"` adds region-specific random walk deviations around the national
#'   trend.
#'
#' @return An object of class `vrc_mortality`.
#' @export
vrc_mortality <- function(
  formula = ~1,
  conflict = c("fixed", "region"),
  time = c("national", "region")
) {
  if (!inherits(formula, "formula")) {
    stop("formula must be a formula", call. = FALSE)
  }

  conflict <- match.arg(conflict)
  time <- match.arg(time)

  structure(
    list(formula = formula, conflict = conflict, time = time),
    class = "vrc_mortality"
  )
}

#' Reporting model component specification
#'
#' @description
#' Create a reporting component specification for [vrcm()]. This is a light
#' wrapper around a formula describing additional reporting covariates (beyond
#' the always-included conflict proxy).
#'
#' The formula should not include `conflict`; conflict is included in the core
#' Stan model as `gamma_conf`.
#'
#' @param formula A model formula. The default `~ 1` specifies no additional
#'   covariates.
#' @param conflict How to model the conflict effect in the reporting component.
#'   Use `"fixed"` for one effect shared across regions (per cause), or
#'   `"region"` for partial pooling (random slopes) by region (per cause).
#' @param time How to model time variation in the reporting component.
#'   `"national"` uses a single random walk shared across regions (per cause).
#'   `"region"` adds region-specific random walk deviations around the national
#'   trend.
#'
#' @return An object of class `vrc_reporting`.
#' @export
vrc_reporting <- function(
  formula = ~1,
  conflict = c("fixed", "region"),
  time = c("national", "region")
) {
  if (!inherits(formula, "formula")) {
    stop("formula must be a formula", call. = FALSE)
  }

  conflict <- match.arg(conflict)
  time <- match.arg(time)

  structure(
    list(formula = formula, conflict = conflict, time = time),
    class = "vrc_reporting"
  )
}

#' Fit a VR mortality + reporting model
#'
#' @description
#' High-level wrapper around [vrc_fit()] modelled after the user experience of
#' packages like `epidemia` and `rstanarm`. You specify a mortality component and a reporting
#' component, and `vrcm()` handles data preparation and model fitting.
#'
#' The fitted model separates:
#'
#' - a latent mortality process (`lambda`), and
#' - a reporting completeness process (`rho`).
#'
#' Both processes include a conflict proxy by default.
#'
#' @param mortality A [vrc_mortality()] specification, or a formula.
#' @param reporting A [vrc_reporting()] specification, or a formula.
#' @param data A data.frame in canonical VR long format (see [vrc_standata()]).
#' @param t0 Conflict start time (index or a value in `data$time`).
#' @param use_mar_labels Logical. If TRUE, account for missing region labels
#'   using a Missing At Random (MAR) assumption with a labeling probability
#'   `omega`. If FALSE (default), rows with missing region labels are ignored.
#' @param priors Optional prior specification created by [vrc_priors()]. If
#'   `NULL` (default), uses [vrc_priors()] with package defaults.
#' @param ... Passed through to [vrc_fit()].
#'
#' @return A `vrcfit` object, or (if `chains = 0`) a standata bundle returned
#'   by [vrc_standata()].
#' @export
vrcm <- function(
  mortality = vrc_mortality(~1),
  reporting = vrc_reporting(~1),
  data,
  t0,
  use_mar_labels = FALSE,
  priors = NULL,
  ...
) {
  if (inherits(mortality, "formula")) {
    mortality <- vrc_mortality(mortality)
  }
  if (inherits(reporting, "formula")) {
    reporting <- vrc_reporting(reporting)
  }

  if (!inherits(mortality, "vrc_mortality")) {
    stop("mortality must be a vrc_mortality object or a formula", call. = FALSE)
  }
  if (!inherits(reporting, "vrc_reporting")) {
    stop("reporting must be a vrc_reporting object or a formula", call. = FALSE)
  }

  out <- vrc_fit(
    data = data,
    t0 = t0,
    mortality_covariates = mortality$formula,
    reporting_covariates = reporting$formula,
    mortality_conflict = mortality$conflict,
    reporting_conflict = reporting$conflict,
    mortality_time = mortality$time,
    reporting_time = reporting$time,
    use_mar_labels = use_mar_labels,
    priors = priors,
    ...
  )

  # If vrc_fit returned the standata bundle (chains = 0), attach component specs
  out$mortality <- mortality
  out$reporting <- reporting

  out
}

#' Summarise covariate effects
#'
#' @description
#' Convenience helper to extract and summarise covariate effects from a fitted
#' model, including cause-specific conflict effects and any additional covariate
#' effects supplied via formulas.
#'
#' By default this returns effects on the standardised covariate scale used for
#' fitting. If `original_scale = TRUE`, coefficients are rescaled back to the
#' original covariate scale using the centring/scaling information stored in
#' `fit$scaling`.
#'
#' @param x A `vrcfit` object.
#' @param probs Quantiles to include.
#' @param original_scale Logical. If TRUE, rescale coefficients back to the
#'   original covariate scale.
#'
#' @return A data.frame.
#' @export
vrc_coef_summary <- function(
  x,
  probs = c(0.1, 0.5, 0.9),
  original_scale = FALSE
) {
  if (!inherits(x, "vrcfit")) {
    stop("x must be a vrcfit object", call. = FALSE)
  }
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' is required", call. = FALSE)
  }

  K_mort <- x$standata$K_mort
  K_rep <- x$standata$K_rep

  pars <- c("beta_conf", "gamma_conf")
  if (K_mort > 0) {
    pars <- c(pars, "beta_mort")
  }
  if (K_rep > 0) {
    pars <- c(pars, "gamma_rep")
  }

  e <- rstan::extract(x$stanfit, pars = pars, permuted = TRUE)

  cause_levels <- x$meta$cause_levels

  # helper to summarise a draws matrix (ndraw x npar)
  summarise_mat <- function(mat) {
    if (is.null(dim(mat))) {
      mat <- matrix(mat, ncol = 1)
    }
    mean_ <- colMeans(mat)
    sd_ <- apply(mat, 2, stats::sd)
    qs <- t(apply(mat, 2, stats::quantile, probs = probs))
    out <- data.frame(mean = mean_, sd = sd_, qs, check.names = FALSE)
    colnames(out)[-(1:2)] <- paste0(
      "q",
      gsub("\\.", "", format(probs, trim = TRUE))
    )
    out
  }
    rows <- list()

  # beta_conf: array[draw, g]
  if (!is.null(e$beta_conf)) {
    mat <- e$beta_conf
    if (length(dim(mat)) == 1) {
      mat <- matrix(mat, ncol = 1)
    }
    sm <- summarise_mat(mat)
    rows[[length(rows) + 1]] <- data.frame(
      component = "mortality",
      parameter = "beta_conf",
      cause = cause_levels[seq_len(ncol(mat))],
      term = "conflict",
      sm,
      stringsAsFactors = FALSE
    )
  }

  # beta_mort: array[draw, g, k]
  if (K_mort > 0 && !is.null(e$beta_mort)) {
    arr <- e$beta_mort
    terms <- x$scaling$X_mort$colnames
    for (g in seq_len(dim(arr)[2])) {
      mat <- arr[, g, , drop = FALSE]
      mat <- matrix(mat, ncol = K_mort)
      sm <- summarise_mat(mat)
      rows[[length(rows) + 1]] <- data.frame(
        component = "mortality",
        parameter = "beta_mort",
        cause = rep(cause_levels[g], K_mort),
        term = terms,
        sm,
        stringsAsFactors = FALSE
      )
    }
  }

  # gamma_conf: array[draw, g]
  if (!is.null(e$gamma_conf)) {
    mat <- e$gamma_conf
    if (length(dim(mat)) == 1) {
      mat <- matrix(mat, ncol = 1)
    }
    sm <- summarise_mat(mat)
    rows[[length(rows) + 1]] <- data.frame(
      component = "reporting",
      parameter = "gamma_conf",
      cause = cause_levels[seq_len(ncol(mat))],
      term = "conflict",
      sm,
      stringsAsFactors = FALSE
    )
  }

  # gamma_rep: array[draw, g, k]
  if (K_rep > 0 && !is.null(e$gamma_rep)) {
    arr <- e$gamma_rep
    terms <- x$scaling$X_rep$colnames
    for (g in seq_len(dim(arr)[2])) {
      mat <- arr[, g, , drop = FALSE]
      mat <- matrix(mat, ncol = K_rep)
      sm <- summarise_mat(mat)
      rows[[length(rows) + 1]] <- data.frame(
        component = "reporting",
        parameter = "gamma_rep",
        cause = rep(cause_levels[g], K_rep),
        term = terms,
        sm,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)

  if (isTRUE(original_scale)) {
    # Rescale coefficients by the standard deviation used in standardisation.
    # For x_z = (x - centre)/scale, coef_orig = coef_z / scale.
    scale_map <- function(component, term) {
      if (term == "conflict") {
        return(x$scaling$conflict$scale)
      }
      if (component == "mortality") {
        scales <- x$scaling$X_mort$scale
        names(scales) <- x$scaling$X_mort$colnames
        return(unname(scales[term]))
      }
      scales <- x$scaling$X_rep$scale
      names(scales) <- x$scaling$X_rep$colnames
      unname(scales[term])
    }

    out$scale <- mapply(scale_map, out$component, out$term)
    out$mean_orig <- out$mean / out$scale
    out$sd_orig <- out$sd / out$scale

    qcols <- grep("^q", names(out), value = TRUE)
    for (qc in qcols) {
      out[[paste0(qc, "_orig")]] <- out[[qc]] / out$scale
    }
  }

  rownames(out) <- NULL
  out
}

#' @export
plot.vrcfit <- function(x, type = c("reporting", "mortality"), ...) {
  type <- match.arg(type)
  if (type == "reporting") {
    return(plot_reporting(x, ...))
  }
  plot_mortality(x, ...)
}

#' @export
fitted.vrcfit <- function(object, ...) {
  df <- posterior_expected_counts(object, draws = FALSE)
  df$mu_mean
}

#' @export
residuals.vrcfit <- function(object, ...) {
  mu <- stats::fitted(object)
  y <- object$data$y
  y - mu
}

#' @export
coef.vrcfit <- function(object, ...) {
  sm <- vrc_coef_summary(object, probs = 0.5, original_scale = FALSE)
  # Create a compact named vector of posterior means
  nm <- paste0(sm$component, ":", sm$cause, ":", sm$term)
  stats::setNames(sm$mean, nm)
}
