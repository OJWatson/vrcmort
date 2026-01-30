# Prior specification utilities

#' Prior specification constructors
#
#' @description
#' `vrcmort` uses a lightweight prior specification system inspired by the
#' `rstanarm` and `epidemia` packages. Priors are represented as simple named
#' lists and are converted into Stan hyperparameters by [vrc_standata()].
#'
#' Only a small set of distributions are currently supported because the
#' shipped Stan model uses conjugate-friendly priors:
#'
#' * `normal(location, scale)`
#' * `exponential(rate)`
#'
#' @name vrc_priors
NULL

#' Normal prior
#'
#' @param location Prior mean.
#' @param scale Prior standard deviation (> 0).
#' @param autoscale Logical. If `TRUE`, the scale is adjusted by the column
#'   scale of the design matrix (similar to rstanarm/epidemia). With the default
#'   `standardise = TRUE`, autoscaling has no practical effect for continuous
#'   predictors.
#'
#' @return A prior specification list.
#' @export
normal <- function(location = 0, scale = 1, autoscale = FALSE) {
  if (!is.numeric(location)) stop("location must be numeric", call. = FALSE)
  if (!is.numeric(scale)) stop("scale must be numeric", call. = FALSE)
  if (any(is.na(scale)) || any(scale <= 0)) stop("scale must be > 0", call. = FALSE)
  if (!is.logical(autoscale) || length(autoscale) != 1) {
    stop("autoscale must be TRUE or FALSE", call. = FALSE)
  }

  structure(
    list(dist = "normal", location = location, scale = scale, autoscale = autoscale),
    class = "vrc_prior"
  )
}

#' Exponential prior
#
#' @param rate Rate parameter (> 0).
#'
#' @return A prior specification list.
#' @export
exponential <- function(rate = 1) {
  if (!is.numeric(rate)) stop("rate must be numeric", call. = FALSE)
  if (any(is.na(rate)) || any(rate <= 0)) stop("rate must be > 0", call. = FALSE)

  structure(list(dist = "exponential", rate = rate), class = "vrc_prior")
}

#' Create a bundled prior specification for vrcmort models
#
#' @description
#' Create a bundled set of priors for use in [vrcm()] / [vrc_fit()]. The
#' returned object is converted into Stan hyperparameters by [vrc_standata()].
#'
#' The defaults are designed to match the hard-coded priors used in earlier
#' versions of `vrcmort`.
#'
#' @param beta_conf Prior for the conflict effect in the mortality component.
#'   This parameter is constrained to be non-negative in the base model.
#' @param beta_mort Prior for additional mortality covariate effects.
#' @param gamma_conf Prior for the conflict effect in the reporting component.
#' @param gamma_rep Prior for additional reporting covariate effects.
#' @param kappa0 Optional prior for baseline reporting on the logit scale.
#'   If `NULL` (default), an anchored prior is used: cause 1 is centred on
#'   logit(0.70), cause 2 on logit(0.90), and remaining causes on logit(0.85).
#' @param kappa_post Prior for the post-conflict reporting shift.
#' @param alpha0 Prior for the mortality intercept.
#' @param alpha_age Prior scale for age effects in the mortality component.
#' @param alpha_sex Prior scale for sex effects in the mortality component.
#' @param sigma_u_lambda Prior scale for region random effect scales (mortality).
#' @param sigma_v_lambda Prior scale for national time RW scales (mortality).
#' @param sigma_beta_conf Prior scale for region-varying conflict slope scales (mortality).
#' @param sigma_v_lambda_region Prior scale for region-specific time RW scales (mortality).
#' @param sigma_u_rho Prior scale for region random effect scales (reporting).
#' @param sigma_v_rho Prior scale for national time RW scales (reporting).
#' @param sigma_gamma_conf Prior scale for region-varying conflict slope scales (reporting).
#' @param sigma_v_rho_region Prior scale for region-specific time RW scales (reporting).
#' @param delta_age_incr Prior scale for increments in the monotone age penalty.
#' @param delta_age_scale Prior scale for the overall monotone age penalty scale.
#' @param phi Prior for the NB2 dispersion parameter.
#'
#' @return A list of class `vrc_priors`.
#' @export
vrc_priors <- function(
  beta_conf = normal(0.2, 0.3, autoscale = FALSE),
  beta_mort = normal(0, 0.3, autoscale = TRUE),
  gamma_conf = normal(0, 0.5, autoscale = FALSE),
  gamma_rep = normal(0, 0.5, autoscale = TRUE),
  kappa0 = NULL,
  kappa_post = normal(0, 0.7, autoscale = FALSE),
  alpha0 = normal(-9, 2, autoscale = FALSE),
  alpha_age = normal(0, 1, autoscale = FALSE),
  alpha_sex = normal(0, 0.5, autoscale = FALSE),
  sigma_u_lambda = normal(0, 0.5, autoscale = FALSE),
  sigma_v_lambda = normal(0, 0.2, autoscale = FALSE),
  sigma_beta_conf = normal(0, 0.3, autoscale = FALSE),
  sigma_v_lambda_region = normal(0, 0.2, autoscale = FALSE),
  sigma_u_rho = normal(0, 0.5, autoscale = FALSE),
  sigma_v_rho = normal(0, 0.2, autoscale = FALSE),
  sigma_gamma_conf = normal(0, 0.5, autoscale = FALSE),
  sigma_v_rho_region = normal(0, 0.2, autoscale = FALSE),
  delta_age_incr = normal(0, 0.5, autoscale = FALSE),
  delta_age_scale = normal(0, 1, autoscale = FALSE),
  phi = exponential(1)
) {
  # basic checks
  .check_prior_or_null <- function(p, nm) {
    if (is.null(p)) return(invisible(TRUE))
    if (!is.list(p) || is.null(p$dist)) {
      stop(nm, " must be a prior created by normal() or exponential()", call. = FALSE)
    }
    invisible(TRUE)
  }

  .check_prior_or_null(beta_conf, "beta_conf")
  .check_prior_or_null(beta_mort, "beta_mort")
  .check_prior_or_null(gamma_conf, "gamma_conf")
  .check_prior_or_null(gamma_rep, "gamma_rep")
  .check_prior_or_null(kappa0, "kappa0")
  .check_prior_or_null(kappa_post, "kappa_post")
  .check_prior_or_null(alpha0, "alpha0")
  .check_prior_or_null(alpha_age, "alpha_age")
  .check_prior_or_null(alpha_sex, "alpha_sex")
  .check_prior_or_null(sigma_u_lambda, "sigma_u_lambda")
  .check_prior_or_null(sigma_v_lambda, "sigma_v_lambda")
  .check_prior_or_null(sigma_beta_conf, "sigma_beta_conf")
  .check_prior_or_null(sigma_v_lambda_region, "sigma_v_lambda_region")
  .check_prior_or_null(sigma_u_rho, "sigma_u_rho")
  .check_prior_or_null(sigma_v_rho, "sigma_v_rho")
  .check_prior_or_null(sigma_gamma_conf, "sigma_gamma_conf")
  .check_prior_or_null(sigma_v_rho_region, "sigma_v_rho_region")
  .check_prior_or_null(delta_age_incr, "delta_age_incr")
  .check_prior_or_null(delta_age_scale, "delta_age_scale")
  .check_prior_or_null(phi, "phi")

  structure(
    list(
      beta_conf = beta_conf,
      beta_mort = beta_mort,
      gamma_conf = gamma_conf,
      gamma_rep = gamma_rep,
      kappa0 = kappa0,
      kappa_post = kappa_post,
      alpha0 = alpha0,
      alpha_age = alpha_age,
      alpha_sex = alpha_sex,
      sigma_u_lambda = sigma_u_lambda,
      sigma_v_lambda = sigma_v_lambda,
      sigma_beta_conf = sigma_beta_conf,
      sigma_v_lambda_region = sigma_v_lambda_region,
      sigma_u_rho = sigma_u_rho,
      sigma_v_rho = sigma_v_rho,
      sigma_gamma_conf = sigma_gamma_conf,
      sigma_v_rho_region = sigma_v_rho_region,
      delta_age_incr = delta_age_incr,
      delta_age_scale = delta_age_scale,
      phi = phi
    ),
    class = "vrc_priors"
  )
}

# Internal helpers ---------------------------------------------------------

#' @keywords internal
vrc_is_prior <- function(x) {
  is.list(x) && !is.null(x$dist)
}

#' @keywords internal
vrc_broadcast <- function(x, n, name = "value") {
  if (length(x) == n) return(x)
  if (length(x) == 1) return(rep(x, n))
  stop(name, " must have length 1 or ", n, call. = FALSE)
}

#' @keywords internal
vrc_col_scale_for_prior <- function(X) {
  X <- as.matrix(X)
  if (ncol(X) == 0) return(numeric())
  out <- numeric(ncol(X))
  for (j in seq_len(ncol(X))) {
    x <- X[, j]
    ux <- unique(as.numeric(x))
    if (length(ux) == 2) {
      out[j] <- diff(range(x))
    } else {
      out[j] <- stats::sd(x)
    }
    if (is.na(out[j]) || out[j] == 0) out[j] <- 1
  }
  pmax(1e-12, out)
}

#' Resolve priors into Stan hyperparameters
#'
#' @keywords internal
vrc_resolve_priors <- function(priors, G, K_mort, K_rep, X_mort, X_rep) {
  if (is.null(priors)) priors <- vrc_priors()
  if (!inherits(priors, "vrc_priors")) {
    stop("priors must be created by vrc_priors()", call. = FALSE)
  }

  # helpers
  get_loc_scale <- function(p, n, default_loc = 0, default_scale = 1, autoscale = FALSE, x_scale = NULL) {
    if (is.null(p)) {
      loc <- rep(default_loc, n)
      sc <- rep(default_scale, n)
      return(list(loc = loc, scale = sc))
    }
    if (!vrc_is_prior(p) || p$dist != "normal") {
      stop("Only normal() priors are supported for this parameter", call. = FALSE)
    }
    loc <- vrc_broadcast(as.numeric(p$location), n, "prior location")
    sc <- vrc_broadcast(as.numeric(p$scale), n, "prior scale")
    if (isTRUE(p$autoscale) && !is.null(x_scale)) {
      sc <- sc / x_scale
    }
    list(loc = loc, scale = sc)
  }

  get_rate <- function(p, n, default_rate = 1) {
    if (is.null(p)) return(vrc_broadcast(default_rate, n, "prior rate"))
    if (!vrc_is_prior(p) || p$dist != "exponential") {
      stop("Only exponential() priors are supported for this parameter", call. = FALSE)
    }
    vrc_broadcast(as.numeric(p$rate), n, "prior rate")
  }

  # Covariate prior autoscaling (uses the matrix passed to Stan)
  x_mort_scale <- vrc_col_scale_for_prior(X_mort)
  x_rep_scale <- vrc_col_scale_for_prior(X_rep)

  # alpha0 (mortality intercept)
  a0 <- get_loc_scale(priors$alpha0, G, default_loc = -9, default_scale = 2)

  # conflict effects
  bc <- get_loc_scale(priors$beta_conf, G, default_loc = 0.2, default_scale = 0.3)
  gc <- get_loc_scale(priors$gamma_conf, G, default_loc = 0, default_scale = 0.5)

  # additional covariate effects
  bm <- get_loc_scale(priors$beta_mort, K_mort, default_loc = 0, default_scale = 0.3, x_scale = x_mort_scale)
  gr <- get_loc_scale(priors$gamma_rep, K_rep, default_loc = 0, default_scale = 0.5, x_scale = x_rep_scale)

  # reporting anchors
  if (is.null(priors$kappa0)) {
    # mimic earlier hard-coded defaults
    k_loc <- rep(stats::qlogis(0.85), G)
    k_sc <- rep(0.6, G)
    if (G >= 1) { k_loc[1] <- stats::qlogis(0.70); k_sc[1] <- 0.7 }
    if (G >= 2) { k_loc[2] <- stats::qlogis(0.90); k_sc[2] <- 0.4 }
    k0 <- list(loc = k_loc, scale = k_sc)
  } else {
    k0 <- get_loc_scale(priors$kappa0, G)
  }
  kp <- get_loc_scale(priors$kappa_post, G, default_loc = 0, default_scale = 0.7)

  # fixed-effect scales for age/sex (mortality)
  aa <- get_loc_scale(priors$alpha_age, 1, default_loc = 0, default_scale = 1)
  asx <- get_loc_scale(priors$alpha_sex, 1, default_loc = 0, default_scale = 0.5)

  # half-normal scales for hierarchical SDs
  suL <- get_loc_scale(priors$sigma_u_lambda, G, default_loc = 0, default_scale = 0.5)
  svL <- get_loc_scale(priors$sigma_v_lambda, G, default_loc = 0, default_scale = 0.2)
  sbc <- get_loc_scale(priors$sigma_beta_conf, G, default_loc = 0, default_scale = 0.3)
  svLr <- get_loc_scale(priors$sigma_v_lambda_region, G, default_loc = 0, default_scale = 0.2)

  suR <- get_loc_scale(priors$sigma_u_rho, G, default_loc = 0, default_scale = 0.5)
  svR <- get_loc_scale(priors$sigma_v_rho, G, default_loc = 0, default_scale = 0.2)
  sgc <- get_loc_scale(priors$sigma_gamma_conf, G, default_loc = 0, default_scale = 0.5)
  svRr <- get_loc_scale(priors$sigma_v_rho_region, G, default_loc = 0, default_scale = 0.2)

  da_incr <- get_loc_scale(priors$delta_age_incr, 1, default_loc = 0, default_scale = 0.5)
  da_scale <- get_loc_scale(priors$delta_age_scale, 1, default_loc = 0, default_scale = 1)

  phi_rate <- get_rate(priors$phi, G, default_rate = 1)

  list(
    prior_alpha0_loc = a0$loc,
    prior_alpha0_scale = a0$scale,
    prior_alpha_age_scale = aa$scale[1],
    prior_alpha_sex_scale = asx$scale[1],
    prior_beta_conf_loc = bc$loc,
    prior_beta_conf_scale = bc$scale,
    prior_beta_mort_loc = bm$loc,
    prior_beta_mort_scale = bm$scale,
    prior_sigma_u_lambda_scale = suL$scale,
    prior_sigma_v_lambda_scale = svL$scale,
    prior_sigma_beta_conf_scale = sbc$scale,
    prior_sigma_v_lambda_region_scale = svLr$scale,
    prior_kappa0_loc = k0$loc,
    prior_kappa0_scale = k0$scale,
    prior_kappa_post_loc = kp$loc,
    prior_kappa_post_scale = kp$scale,
    prior_gamma_conf_loc = gc$loc,
    prior_gamma_conf_scale = gc$scale,
    prior_gamma_rep_loc = gr$loc,
    prior_gamma_rep_scale = gr$scale,
    prior_sigma_u_rho_scale = suR$scale,
    prior_sigma_v_rho_scale = svR$scale,
    prior_sigma_gamma_conf_scale = sgc$scale,
    prior_sigma_v_rho_region_scale = svRr$scale,
    prior_delta_age_incr_scale = da_incr$scale[1],
    prior_delta_age_scale_scale = da_scale$scale[1],
    prior_phi_rate = phi_rate
  )
}

#' Summarise priors used by a fitted model
#'
#' @param x A `vrcfit` object (recommended) or a resolved prior list as returned
#'   by the internal `vrc_resolve_priors()`.
#'
#' @return A data.frame with one row per prior hyperparameter group.
#' @export
vrc_prior_summary <- function(x) {
  pri <- NULL
  if (inherits(x, "vrcfit")) {
    pri <- x$priors_resolved
  } else if (is.list(x) && !is.null(x$prior_alpha0_loc)) {
    pri <- x
  }
  if (is.null(pri)) {
    stop("x must be a vrcfit object or a resolved prior list", call. = FALSE)
  }

  mk <- function(name, value) {
    data.frame(name = name, value = I(list(value)), stringsAsFactors = FALSE)
  }

  out <- rbind(
    mk("alpha0_loc", pri$prior_alpha0_loc),
    mk("alpha0_scale", pri$prior_alpha0_scale),
    mk("alpha_age_scale", pri$prior_alpha_age_scale),
    mk("alpha_sex_scale", pri$prior_alpha_sex_scale),
    mk("beta_conf_loc", pri$prior_beta_conf_loc),
    mk("beta_conf_scale", pri$prior_beta_conf_scale),
    mk("beta_mort_loc", pri$prior_beta_mort_loc),
    mk("beta_mort_scale", pri$prior_beta_mort_scale),
    mk("sigma_u_lambda_scale", pri$prior_sigma_u_lambda_scale),
    mk("sigma_v_lambda_scale", pri$prior_sigma_v_lambda_scale),
    mk("sigma_beta_conf_scale", pri$prior_sigma_beta_conf_scale),
    mk("sigma_v_lambda_region_scale", pri$prior_sigma_v_lambda_region_scale),
    mk("kappa0_loc", pri$prior_kappa0_loc),
    mk("kappa0_scale", pri$prior_kappa0_scale),
    mk("kappa_post_loc", pri$prior_kappa_post_loc),
    mk("kappa_post_scale", pri$prior_kappa_post_scale),
    mk("gamma_conf_loc", pri$prior_gamma_conf_loc),
    mk("gamma_conf_scale", pri$prior_gamma_conf_scale),
    mk("gamma_rep_loc", pri$prior_gamma_rep_loc),
    mk("gamma_rep_scale", pri$prior_gamma_rep_scale),
    mk("sigma_u_rho_scale", pri$prior_sigma_u_rho_scale),
    mk("sigma_v_rho_scale", pri$prior_sigma_v_rho_scale),
    mk("sigma_gamma_conf_scale", pri$prior_sigma_gamma_conf_scale),
    mk("sigma_v_rho_region_scale", pri$prior_sigma_v_rho_region_scale),
    mk("delta_age_incr_scale", pri$prior_delta_age_incr_scale),
    mk("delta_age_scale_scale", pri$prior_delta_age_scale_scale),
    mk("phi_rate", pri$prior_phi_rate)
  )

  out
}
