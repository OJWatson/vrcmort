#' Extract cell-level truth from a simulated dataset
#'
#' @description
#' Helper for benchmarking. Given an object returned by [vrc_simulate()], return
#' a data.frame aligned to `sim$df_full` containing cell-level true mortality
#' rates, reporting completeness, and true deaths.
#'
#' @param sim A list returned by [vrc_simulate()].
#'
#' @return A data.frame with additional columns:
#'   * `lambda_true` latent mortality rate
#'   * `rho_true` reporting completeness
#'   * `D_true` true deaths for the cause group
#'
#' @export
vrc_truth <- function(sim) {
  if (!is.list(sim) || !all(c("df_full", "truth") %in% names(sim))) {
    stop("sim must be an object returned by vrc_simulate()", call. = FALSE)
  }

  df <- sim$df_full
  tr <- sim$truth

  idx <- cbind(df$region, df$time, df$age, df$sex, df$cause)

  df$lambda_true <- tr$lambda_true[idx]
  df$rho_true <- tr$rho_true[idx]
  df$D_true <- tr$D_true[idx]

  df
}
