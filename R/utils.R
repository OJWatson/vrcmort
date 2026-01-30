# Internal helper utilities

#' @keywords internal
nlist <- function(...) {
  dots <- list(...)
  nm <- as.character(match.call(expand.dots = TRUE))[-1L]
  if (is.null(names(dots))) names(dots) <- nm
  dots
}

#' @keywords internal
stop_if_missing_cols <- function(data, cols) {
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop(
      "Missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @keywords internal
as_int_index <- function(x, name = deparse(substitute(x))) {
  if (is.factor(x)) return(as.integer(x))
  if (is.character(x)) return(as.integer(factor(x)))
  if (is.integer(x)) return(x)
  if (is.numeric(x)) {
    if (any(is.na(x))) stop("`", name, "` contains NA", call. = FALSE)
    if (any(abs(x - round(x)) > 1e-8)) {
      stop("`", name, "` must be integer-like", call. = FALSE)
    }
    return(as.integer(round(x)))
  }
  stop("Unsupported type for `", name, "`", call. = FALSE)
}

#' @keywords internal
check_positive <- function(x, name = deparse(substitute(x))) {
  if (any(is.na(x))) stop("`", name, "` contains NA", call. = FALSE)
  if (any(x <= 0)) stop("`", name, "` must be > 0", call. = FALSE)
  invisible(TRUE)
}

#' @keywords internal
standardise_vector <- function(x, centre = TRUE, scale = TRUE) {
  x <- as.numeric(x)
  if (!centre && !scale) return(list(x = x, centre = 0, scale = 1))
  mu <- if (centre) mean(x) else 0
  sig <- if (scale) stats::sd(x) else 1
  if (is.na(sig) || sig == 0) sig <- 1
  list(x = (x - mu) / sig, centre = mu, scale = sig)
}

#' @keywords internal
is_binary_column <- function(x, tol = 1e-12) {
  ux <- sort(unique(as.numeric(x)))
  length(ux) <= 2 && all(abs(ux - c(0, 1)[seq_along(ux)]) < tol)
}

#' @keywords internal
standardise_matrix <- function(mm, centre = TRUE, scale = TRUE, scale_binary = FALSE) {
  mm <- as.matrix(mm)
  if (ncol(mm) == 0) {
    attr(mm, "scaling") <- list(centre = numeric(), scale = numeric())
    return(mm)
  }

  centres <- rep(0, ncol(mm))
  scales <- rep(1, ncol(mm))

  for (j in seq_len(ncol(mm))) {
    col <- mm[, j]
    if (!scale_binary && is_binary_column(col)) {
      centres[j] <- 0
      scales[j] <- 1
      next
    }

    mu <- if (centre) mean(col) else 0
    sig <- if (scale) stats::sd(col) else 1
    if (is.na(sig) || sig == 0) sig <- 1

    mm[, j] <- (col - mu) / sig
    centres[j] <- mu
    scales[j] <- sig
  }

  attr(mm, "scaling") <- list(centre = centres, scale = scales, colnames = colnames(mm))
  mm
}

#' @keywords internal
model_matrix_no_intercept <- function(formula, data) {
  if (is.null(formula)) {
    return(matrix(0, nrow(data), 0))
  }
  mm <- stats::model.matrix(formula, data = data)
  if ("(Intercept)" %in% colnames(mm)) {
    mm <- mm[, setdiff(colnames(mm), "(Intercept)"), drop = FALSE]
  }
  mm
}
