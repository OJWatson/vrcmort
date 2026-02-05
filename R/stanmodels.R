# Stan model access and caching

#' List Stan models shipped with the package
#'
#' @return Character vector of model names.
#' @export
vrc_model_names <- function() {
  c("vr_reporting_model")
}

#' Default mapping for core generated quantities
#'
#' @keywords internal
vrc_default_param_map <- function() {
  list(
    rho = "rho_rep",
    lambda = "lambda_rep",
    mu = "mu_rep",
    y_rep = "y_rep",
    log_lik = "log_lik"
  )
}

#' Define a model specification
#'
#' @description
#' Create a small model specification object that can be used to fit either the
#' built-in Stan model shipped with `vrcmort` or a user-supplied Stan model
#' file. This keeps the R-side data preparation and posterior extraction tools
#' working even when users modify Stan code.
#'
#' The `param_map` component is a named list mapping key quantities to Stan
#' variable names. The default mapping matches the generated quantities in the
#' base model.
#'
#' @param model Either the name of a built-in Stan model (see
#'   [vrc_model_names()]) or a path to a `.stan` file.
#' @param stan_file Optional explicit path to a `.stan` file. If supplied, it
#'   overrides `model`.
#' @param backend Backend to use. Currently only `"rstan"` is supported.
#' @param param_map Named list mapping `rho`, `lambda`, `mu`, `y_rep`, and
#'   optionally `log_lik` to Stan variable names.
#'
#' @return An object of class `vrc_model_spec`.
#' @export
vrc_model_spec <- function(
  model = "vr_reporting_model",
  stan_file = NULL,
  backend = c("rstan"),
  param_map = NULL
) {
  backend <- match.arg(backend)

  if (inherits(model, "vrc_model_spec") && is.null(stan_file)) {
    return(model)
  }

  file <- stan_file
  name <- NULL

  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1) {
      stop("stan_file must be a single file path", call. = FALSE)
    }
    if (!file.exists(file)) {
      stop("stan_file does not exist: ", file, call. = FALSE)
    }
    name <- sub("\\.stan$", "", basename(file))
  } else {
    if (!is.character(model) || length(model) != 1) {
      stop("model must be a single string", call. = FALSE)
    }

    if (file.exists(model)) {
      file <- model
      name <- sub("\\.stan$", "", basename(file))
    } else {
      # built-in model
      if (!model %in% vrc_model_names()) {
        stop("Unknown Stan model: ", model, call. = FALSE)
      }
      name <- model
      file <- system.file("stan", paste0(model, ".stan"), package = "vrcmort")
      if (file == "") {
        # development fallback
        dev_file <- file.path("inst", "stan", paste0(model, ".stan"))
        if (!file.exists(dev_file)) {
          stop(
            "Could not find Stan file for model '",
            model,
            "'",
            call. = FALSE
          )
        }
        file <- dev_file
      }
    }
  }

  file <- normalizePath(file, winslash = "/", mustWork = TRUE)

  if (is.null(param_map)) {
    param_map <- vrc_default_param_map()
  }
  if (!is.list(param_map) || is.null(names(param_map))) {
    stop("param_map must be a named list", call. = FALSE)
  }

  structure(
    list(
      name = name,
      file = file,
      backend = backend,
      param_map = param_map
    ),
    class = "vrc_model_spec"
  )
}

#' Write a custom Stan template to a directory
#'
#' @description
#' Copy the modular Stan model shipped with the package (including all include
#' files) to a user-specified directory. This provides a safe starting point
#' for custom model development.
#'
#' Users can then edit the copied Stan files and fit them by passing the path
#' to the `.stan` file into [vrc_fit()] via `stan_model`.
#'
#' @param path Output directory.
#' @param model Name of the base Stan model file to copy (default:
#'   `"vr_reporting_model"`).
#' @param overwrite Logical. If `TRUE`, existing files in `path` will be
#'   overwritten.
#'
#' @return Invisibly returns the path to the copied base Stan file.
#' @export
vrc_write_stan_template <- function(
  path,
  model = "vr_reporting_model",
  overwrite = FALSE
) {
  if (!is.character(path) || length(path) != 1) {
    stop("path must be a single directory", call. = FALSE)
  }
  if (!is.character(model) || length(model) != 1) {
    stop("model must be a single string", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("overwrite must be TRUE or FALSE", call. = FALSE)
  }

  src <- system.file("stan", package = "vrcmort")
  if (src == "") {
    stop("Could not find installed Stan templates", call. = FALSE)
  }

  if (dir.exists(path)) {
    existing <- list.files(path, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0 && !isTRUE(overwrite)) {
      stop("Directory already exists and is not empty: ", path, call. = FALSE)
    }
  } else {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  src_files <- list.files(src, recursive = TRUE, full.names = TRUE)
  for (f in src_files) {
    rel <- substring(f, nchar(src) + 2)
    dest <- file.path(path, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(f, dest, overwrite = TRUE)
    if (!isTRUE(ok)) {
      stop("Failed to copy Stan template file: ", rel, call. = FALSE)
    }
  }

  # Add a small README for custom model development
  readme <- file.path(path, "README_CUSTOM_STAN.md")
  if (!file.exists(readme) || isTRUE(overwrite)) {
    writeLines(
      c(
        "# Custom Stan model template for vrcmort",
        "",
        "This directory contains a copy of the modular Stan model shipped with the vrcmort package.",
        "",
        "## How to use",
        "",
        "1. Edit the Stan files (for example, change priors, add a misclassification matrix, or change the reporting submodel).",
        "2. Fit the model by passing the path to the base .stan file into vrc_fit() or vrcm().",
        "",
        "Example:",
        "",
        "```r",
        "fit <- vrc_fit(data = df, t0 = 25, stan_model = 'path/to/vr_reporting_model.stan', chains = 4, iter = 1000)",
        "```",
        "",
        "If you change generated quantity names, pass a param_map using vrc_model_spec().",
        ""
      ),
      con = readme
    )
  }

  base_file <- file.path(path, paste0(model, ".stan"))
  if (!file.exists(base_file)) {
    stop("Base Stan file not found after copy: ", base_file, call. = FALSE)
  }

  invisible(normalizePath(base_file, winslash = "/", mustWork = TRUE))
}

#' Load and cache a Stan model
#'
#' @param name Stan model name (see [vrc_model_names()]) or a path to a `.stan`
#'   file, or a [vrc_model_spec()] object.
#' @param backend Backend to use. Currently only `"rstan"` is supported.
#' @param ... Additional arguments passed to the backend model compiler.
#' @return A compiled Stan model object.
#' @export
vrc_model <- function(name = "vr_reporting_model", backend = c("rstan"), ...) {
  backend <- match.arg(backend)
  spec <- vrc_model_spec(model = name, backend = backend)

  if (spec$backend != "rstan") {
    stop("Only backend='rstan' is implemented", call. = FALSE)
  }

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' is required for backend='rstan'", call. = FALSE)
  }

  finfo <- file.info(spec$file)
  mtime <- if (!is.na(finfo$mtime)) as.integer(finfo$mtime) else 0L
  hash <- sum(utf8ToInt(spec$file))

  key <- paste0(
    "stanmodel_",
    spec$backend,
    "_",
    spec$name,
    "_",
    hash,
    "_",
    mtime
  )
  if (exists(key, envir = .vrcmort_env, inherits = FALSE)) {
    return(get(key, envir = .vrcmort_env, inherits = FALSE))
  }

  sm <- rstan::stan_model(
    file = spec$file,
    model_name = spec$name,
    allow_undefined = TRUE,
    ...
  )
  assign(key, sm, envir = .vrcmort_env)
  sm
}
