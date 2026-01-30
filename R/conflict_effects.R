#' Region-specific conflict effects
#'
#' @description
#' Extract posterior summaries of the conflict effect by region. This is most
#' useful when you fit a model with partial pooling of conflict effects (random
#' slopes by region), by setting `conflict = "region"` in [vrc_mortality()] and/or
#' [vrc_reporting()].
#'
#' The underlying Stan model always defines region-specific conflict effects
#' (`beta_conf_rg` for mortality and `gamma_conf_rg` for reporting). When the
#' corresponding feature is disabled (`conflict = "fixed"`), these region-specific
#' effects are equal to the global effect for all regions.
#'
#' @param x A `vrcfit` object.
#' @param component Which model component to summarise: mortality (`beta_conf_rg`)
#'   or reporting (`gamma_conf_rg`).
#' @param probs Quantiles to report.
#' @param draws Logical. If `FALSE` (default), returns posterior summaries.
#'   If `TRUE`, returns a long data.frame of posterior draws.
#'
#' @return A data.frame. If `draws = FALSE`, columns include `mean`, `sd` and the
#'   requested quantiles. If `draws = TRUE`, columns include `draw` and `value`.
#' @export
vrc_conflict_effects <- function(
  x,
  component = c("mortality", "reporting"),
  probs = c(0.1, 0.5, 0.9),
  draws = FALSE
) {
  if (!inherits(x, "vrcfit")) stop("x must be a vrcfit object", call. = FALSE)
  component <- match.arg(component)

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' is required", call. = FALSE)
  }

  par <- if (component == "mortality") "beta_conf_rg" else "gamma_conf_rg"
  e <- rstan::extract(x$stanfit, pars = par, permuted = TRUE)
  arr <- e[[par]]
  if (is.null(arr)) {
    stop("Parameter not found in stanfit: ", par, call. = FALSE)
  }

  # arr: iterations x R x G
  R <- dim(arr)[2]
  G <- dim(arr)[3]

  region_levels <- x$meta$region_levels
  cause_levels <- x$meta$cause_levels

  if (length(region_levels) < R) region_levels <- as.character(seq_len(R))
  if (length(cause_levels) < G) cause_levels <- paste0("cause", seq_len(G))

  if (isTRUE(draws)) {
    nd <- dim(arr)[1]
    out <- expand.grid(
      draw = seq_len(nd),
      region = region_levels[seq_len(R)],
      cause = cause_levels[seq_len(G)],
      stringsAsFactors = FALSE
    )
    out$value <- as.numeric(arr)
    out$component <- component
    return(out)
  }

  # Summaries
  summarise_vec <- function(v) {
    c(
      mean = mean(v),
      sd = stats::sd(v),
      stats::quantile(v, probs = probs, names = TRUE)
    )
  }

  rows <- list()
  for (r in seq_len(R)) {
    for (g in seq_len(G)) {
      s <- summarise_vec(arr[, r, g])
      df <- as.data.frame(as.list(s), check.names = FALSE)
      # rename quantile columns to q10,q50,q90 style
      qn <- names(df)
      qcols <- grep("^\\d+%$", qn, value = TRUE)
      if (length(qcols)) {
        new <- paste0("q", gsub("%", "", qcols))
        names(df)[match(qcols, names(df))] <- new
      }
      df$component <- component
      df$region <- region_levels[r]
      df$cause <- cause_levels[g]
      rows[[length(rows) + 1]] <- df
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
