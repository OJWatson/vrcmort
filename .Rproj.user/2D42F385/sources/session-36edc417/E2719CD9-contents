# Posterior extraction helpers

#' @keywords internal
.vrc_param <- function(x, key, default) {
  if (inherits(x, "vrcfit")) {
    pm <- NULL
    if (!is.null(x$model_spec) && is.list(x$model_spec$param_map)) pm <- x$model_spec$param_map
    if (is.null(pm) && !is.null(x$param_map) && is.list(x$param_map)) pm <- x$param_map
    if (!is.null(pm) && !is.null(pm[[key]])) return(as.character(pm[[key]])[1])
  }
  default
}

#' Posterior summaries for reporting completeness
#'
#' @description
#' Extract posterior draws (or summaries) for the cell-level reporting
#' completeness `rho` returned from Stan as a generated quantity (`rho_rep`).
#'
#' @param x A `vrcfit` object.
#' @param draws Logical. If TRUE, returns a matrix of posterior draws with one
#'   column per observed cell.
#' @param probs Numeric vector of quantiles to compute.
#' @param ... Reserved for future use.
#'
#' @return If `draws = TRUE`, a matrix with dimensions `n_draws x N`.
#' Otherwise a data.frame combining `x$data` with summary columns for `rho`.
#'
#' @export
posterior_reporting <- function(x, draws = FALSE, probs = c(0.1, 0.5, 0.9), ...) {
  .posterior_cell_summary(x, var = .vrc_param(x, "rho", "rho_rep"), draws = draws, probs = probs, prefix = "rho")
}

#' Posterior summaries for latent mortality rates
#'
#' @description
#' Extract posterior draws/summaries for the cell-level latent mortality rates
#' `lambda` returned from Stan as a generated quantity (`lambda_rep`).
#'
#' @inheritParams posterior_reporting
#' @export
posterior_mortality <- function(x, draws = FALSE, probs = c(0.1, 0.5, 0.9), ...) {
  .posterior_cell_summary(x, var = .vrc_param(x, "lambda", "lambda_rep"), draws = draws, probs = probs, prefix = "lambda")
}

#' Posterior summaries for expected observed counts
#'
#' @description
#' Extract posterior draws/summaries for the expected observed counts `mu`
#' returned from Stan as a generated quantity (`mu_rep`).
#'
#' @inheritParams posterior_reporting
#' @export
posterior_expected_counts <- function(x, draws = FALSE, probs = c(0.1, 0.5, 0.9), ...) {
  .posterior_cell_summary(x, var = .vrc_param(x, "mu", "mu_rep"), draws = draws, probs = probs, prefix = "mu")
}

#' Posterior predictive distribution for observed counts
#'
#' @description
#' Extract posterior predictive draws `y_rep` from the Stan generated quantities.
#'
#' @inheritParams posterior_reporting
#' @export
posterior_predict <- function(x, draws = FALSE, probs = c(0.1, 0.5, 0.9), ...) {
  .posterior_cell_summary(x, var = .vrc_param(x, "y_rep", "y_rep"), draws = draws, probs = probs, prefix = "yrep")
}

#' @keywords internal
.posterior_cell_summary <- function(x, var, draws, probs, prefix) {
  if (!inherits(x, "vrcfit")) stop("x must be a vrcfit object", call. = FALSE)
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' is required", call. = FALSE)
  }

  e <- rstan::extract(x$stanfit, pars = var, permuted = TRUE)
  if (!var %in% names(e)) stop("Parameter not found in stanfit: ", var, call. = FALSE)

  mat <- e[[var]]
  # rstan returns vector for scalar; ensure 2D matrix
  if (is.null(dim(mat))) {
    mat <- matrix(mat, ncol = 1)
  }

  if (isTRUE(draws)) {
    return(mat)
  }

  # Summarise by column (cell)
  mean_ <- apply(mat, 2, mean)
  sd_ <- apply(mat, 2, stats::sd)
  qs <- t(apply(mat, 2, stats::quantile, probs = probs))
  colnames(qs) <- paste0(prefix, "_q", gsub("\\.", "", format(probs, trim = TRUE)))

  out <- x$data
  out[[paste0(prefix, "_mean")]] <- as.numeric(mean_)
  out[[paste0(prefix, "_sd")]] <- as.numeric(sd_)

  for (j in seq_len(ncol(qs))) {
    out[[colnames(qs)[j]]] <- qs[, j]
  }

  out
}
