#' Fit the VR reporting model
#'
#' @description
#' Fit the hierarchical VR mortality model with an explicit reporting process.
#' The model is implemented in Stan and is fit using `rstan`.
#'
#' @param data A data.frame in long format (see [vrc_standata()]).
#' @param t0 Conflict start time. See [vrc_standata()].
#' @param mortality_covariates Optional formula for additional mortality covariates, excluding `conflict`.
#' @param reporting_covariates Optional formula for additional reporting covariates, excluding `conflict`.
#' @param mortality_conflict How to model the conflict effect in the mortality component.
#'   Use `"fixed"` (default) for one effect shared across regions, or `"region"`
#'   for partial pooling (random slopes) by region.
#' @param reporting_conflict How to model the conflict effect in the reporting component.
#'   Use `"fixed"` (default) for one effect shared across regions, or `"region"`
#'   for partial pooling (random slopes) by region.
#' @param mortality_time How to model time variation in the mortality component.
#'   `"national"` uses a single random walk shared across regions; `"region"` adds
#'   region-specific random walk deviations around the national trend.
#' @param reporting_time How to model time variation in the reporting component.
#'   `"national"` uses a single random walk shared across regions; `"region"` adds
#'   region-specific random walk deviations around the national trend.
#' @param standardise Logical. If TRUE (recommended), standardise `conflict` and
#'   covariate columns in model matrices.
#' @param scale_binary Logical. If TRUE, also standardise binary dummy columns.
#' @param drop_na_y Logical. If TRUE, rows with missing `y` are removed before
#'   fitting.
#' @param duplicates How to handle duplicate identifier rows. One of
#'   `"error"` (default) or `"sum"`.
#' @param algorithm One of `"sampling"`, `"meanfield"`, or `"fullrank"` (passed to `rstan`).
#' @param priors Optional prior specification created by [vrc_priors()]. If
#'   `NULL` (default), uses [vrc_priors()] with package defaults.
#' @param prior_PD Logical. If TRUE, samples from the prior distribution (no likelihood).
#' @param backend Backend to use. Currently only `"rstan"` is supported.
#' @param stan_model Stan model name. Defaults to `"vr_reporting_model"`.
#' @param ... Additional arguments passed to `rstan::sampling` or `rstan::vb`.
#'
#' @return An object of class `vrcfit`.
#' @export
vrc_fit <- function(
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
  algorithm = c("sampling", "meanfield", "fullrank"),
  priors = NULL,
  prior_PD = FALSE,
  backend = c("rstan"),
  stan_model = "vr_reporting_model",
  ...
) {
  call <- match.call(expand.dots = TRUE)
  mortality_conflict <- match.arg(mortality_conflict)
  reporting_conflict <- match.arg(reporting_conflict)
  mortality_time <- match.arg(mortality_time)
  reporting_time <- match.arg(reporting_time)
  duplicates <- match.arg(duplicates)
  algorithm <- match.arg(algorithm)
  backend <- match.arg(backend)

  sdat_obj <- vrc_standata(
    data = data,
    t0 = t0,
    mortality_covariates = mortality_covariates,
    reporting_covariates = reporting_covariates,
    mortality_conflict = mortality_conflict,
    reporting_conflict = reporting_conflict,
    mortality_time = mortality_time,
    reporting_time = reporting_time,
    standardise = standardise,
    scale_binary = scale_binary,
    drop_na_y = drop_na_y,
    duplicates = duplicates,
    priors = priors,
    prior_PD = prior_PD
  )

  # Allow chains = 0 returns standata
  dots <- list(...)
  if (!is.null(dots$chains) && identical(dots$chains, 0)) {
    return(sdat_obj)
  }

  spec <- vrc_model_spec(model = stan_model, backend = backend)

  sm <- vrc_model(spec)

  if (backend != "rstan") {
    stop("Only backend='rstan' is implemented", call. = FALSE)
  }

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' is required", call. = FALSE)
  }

  # default init_r helps avoid extreme initialisation
  if (is.null(dots$init_r)) dots$init_r <- 1e-6

  args <- c(
    dots,
    list(
      object = sm,
      data = sdat_obj$standata
    )
  )

  fit <- if (algorithm == "sampling") {
    do.call(rstan::sampling, args)
  } else {
    args$algorithm <- algorithm
    do.call(rstan::vb, args)
  }

  out <- list(
    call = call,
    stanfit = fit,
    standata = sdat_obj$standata,
    data = sdat_obj$df,
    meta = sdat_obj$meta,
    scaling = sdat_obj$scaling,
    priors = sdat_obj$priors,
    priors_resolved = sdat_obj$priors_resolved,
    algorithm = algorithm,
    backend = backend,
    stan_model = spec$name,
    stan_file = spec$file,
    model_spec = spec
  )

  class(out) <- "vrcfit"
  out
}

#' @export
print.vrcfit <- function(x, ...) {
  cat("vrcmort model fit\n")
  cat("- Stan model:", x$stan_model, "\n")
  if (!is.null(x$stan_file)) cat("- Stan file:", x$stan_file, "\n")
  cat("- Algorithm:", x$algorithm, "\n")
  cat("- N (observed cells):", x$standata$N, "\n")
  cat("- Dimensions: R=", x$standata$R, ", T=", x$standata$T, ", A=", x$standata$A,
      ", S=", x$standata$S, ", G=", x$standata$G, "\n", sep = "")
  invisible(x)
}

#' @export
summary.vrcfit <- function(object, pars = c("beta_conf", "kappa0", "kappa_post", "gamma_conf", "phi"), ...) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' is required", call. = FALSE)
  }
  s <- rstan::summary(object$stanfit, pars = pars, ...)$summary
  s
}
