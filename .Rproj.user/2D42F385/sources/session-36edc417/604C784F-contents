#' Plot estimated reporting completeness over time
#'
#' @description
#' Convenience plotting helper for visualising estimated reporting completeness
#' over time, aggregated over age and sex.
#'
#' @param x A `vrcfit` object.
#' @param group_vars Character vector of columns to group by in addition to
#'   `time`. Defaults to `c("region", "cause")`.
#' @param weight_var Column name used as weights when aggregating completeness across age/sex.
#'   Defaults to `"exposure"`.
#' @param probs Quantiles to plot (must match those computed by [posterior_reporting()]).
#' @param ribbon Logical. If TRUE, draws a ribbon between the lowest and highest prob quantiles.
#' @param ... Additional args passed to [ggplot2::theme()].
#'
#' @return A ggplot object.
#' @export
plot_reporting <- function(
  x,
  group_vars = c("region", "cause"),
  weight_var = "exposure",
  probs = c(0.1, 0.9),
  ribbon = TRUE,
  ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required", call. = FALSE)
  }

  df <- posterior_reporting(x, draws = FALSE, probs = c(min(probs), 0.5, max(probs)))

  qlo <- paste0("rho_q", gsub("\\.", "", format(min(probs), trim = TRUE)))
  qhi <- paste0("rho_q", gsub("\\.", "", format(max(probs), trim = TRUE)))

  if (!all(c(qlo, qhi) %in% names(df))) {
    stop("Quantile columns not found. Try calling with probs that match posterior_reporting().", call. = FALSE)
  }

  gvars <- unique(c("time", group_vars))

  df_ag <- dplyr::group_by(df, !!!rlang::syms(gvars))
  df_ag <- dplyr::summarise(
    df_ag,
    w = sum(.data[[weight_var]], na.rm = TRUE),
    rho_mean = stats::weighted.mean(.data$rho_mean, w = .data[[weight_var]], na.rm = TRUE),
    rho_lo = stats::weighted.mean(.data[[qlo]], w = .data[[weight_var]], na.rm = TRUE),
    rho_hi = stats::weighted.mean(.data[[qhi]], w = .data[[weight_var]], na.rm = TRUE),
    .groups = "drop"
  )

  p <- ggplot2::ggplot(df_ag, ggplot2::aes(x = .data$time, y = .data$rho_mean))

  if (length(group_vars) >= 1) {
    p <- p + ggplot2::aes(colour = interaction(!!!rlang::syms(group_vars), sep = " / "))
  }

  if (isTRUE(ribbon)) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$rho_lo, ymax = .data$rho_hi, fill = interaction(!!!rlang::syms(group_vars), sep = " / ")),
      alpha = 0.2,
      colour = NA
    )
  }

  p <- p + ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = "Reporting completeness (rho)", colour = NULL, fill = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(...)

  p
}

#' Plot estimated mortality over time
#'
#' @description
#' Convenience plotting helper for visualising estimated latent mortality.
#' By default this plots expected true deaths (population x lambda), aggregated
#' over age and sex.
#'
#' @param x A `vrcfit` object.
#' @param group_vars Character vector of columns to group by in addition to `time`.
#'   Defaults to `c("region", "cause")`.
#' @param value One of `"rate"` or `"true_deaths"`.
#' @param ... Additional arguments passed to [ggplot2::theme()].
#'
#' @return A ggplot object.
#' @export
plot_mortality <- function(
  x,
  group_vars = c("region", "cause"),
  value = c("true_deaths", "rate"),
  ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required", call. = FALSE)
  }

  value <- match.arg(value)
  df <- posterior_mortality(x, draws = FALSE, probs = c(0.1, 0.5, 0.9))

  if (!"exposure" %in% names(df) && "pop" %in% names(df)) {
    df$exposure <- df$pop
  }
  df <- dplyr::mutate(df, true_deaths_mean = .data$exposure * .data$lambda_mean)

  gvars <- unique(c("time", group_vars))

  if (value == "true_deaths") {
    df_ag <- dplyr::group_by(df, !!!rlang::syms(gvars))
    df_ag <- dplyr::summarise(
      df_ag,
      value = sum(.data$true_deaths_mean, na.rm = TRUE),
      .groups = "drop"
    )
    ylab <- "Expected true deaths"
  } else {
    # exposure-weighted mean rate
    df_ag <- dplyr::group_by(df, !!!rlang::syms(gvars))
    df_ag <- dplyr::summarise(
      df_ag,
      value = stats::weighted.mean(.data$lambda_mean, w = .data$exposure, na.rm = TRUE),
      .groups = "drop"
    )
    ylab <- "Latent mortality rate (lambda)"
  }

  p <- ggplot2::ggplot(df_ag, ggplot2::aes(x = .data$time, y = .data$value))
  if (length(group_vars) >= 1) {
    p <- p + ggplot2::aes(colour = interaction(!!!rlang::syms(group_vars), sep = " / "))
  }

  p <- p + ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = ylab, colour = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(...)

  p
}
