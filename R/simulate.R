#' Simulate VR mortality with conflict-driven reporting collapse
#'
#' @description
#' Simulate a long-format vital registration (VR) dataset with a latent
#' mortality process and an explicit observation (reporting) process that can
#' collapse after a conflict start time.
#'
#' The simulator generates:
#'
#' * latent cause-specific mortality rates (by region, time, age, sex),
#' * true deaths given population denominators,
#' * observed VR counts given cause- and age-specific reporting completeness,
#' * optional misclassification between trauma and non-trauma,
#' * optional additional missingness mechanisms to stress test inference.
#'
#' @param R Number of regions.
#' @param T Number of time points (for example, months).
#' @param age_breaks Numeric vector of age breakpoints. Defaults to 8 age groups.
#' @param sexes Character vector of sex labels.
#' @param t0 Integer time index at which conflict begins.
#' @param seed Random seed.
#' @param missing A list controlling additional missingness mechanisms.
#'   See Details.
#' @param pop_total_range Length-2 numeric vector giving the range of total
#'   region populations to sample from.
#' @param pop_error_sd Numeric. Lognormal measurement error (standard deviation
#'   on the log scale) applied to the population denominator used for inference.
#' @param displacement_post_sd Numeric. Random-walk volatility controlling
#'   region population scaling after conflict (approximates displacement).
#' @param conflict_post_mean Numeric. Mean of the conflict covariate after
#'   conflict start.
#' @param conflict_post_sd Numeric. Standard deviation of the conflict covariate
#'   after conflict start.
#' @param conflict_spike_prob Numeric in (0,1). Probability of an additional
#'   conflict spike at a given time point after conflict start.
#' @param conflict_spike_mean Numeric. Mean spike size (gamma-distributed).
#' @param facility_pre_mean Numeric. Mean facility functioning score pre-conflict
#'   (bounded to 0-1).
#' @param facility_post_drop Numeric. Average drop in facility functioning at
#'   conflict start.
#' @param facility_noise_sd Numeric. Noise SD used when generating facility
#'   functioning.
#' @param beta_conf_true Length-2 numeric vector. True effect of the conflict
#'   covariate on log mortality rates for trauma and non-trauma.
#' @param beta_conf_region_sd Length-2 numeric vector. Standard deviation of
#'   region-level deviations for the conflict effect in the mortality model.
#' @param beta_fac_mort_true Length-2 numeric vector. True effect of facility
#'   functioning on log mortality rates.
#' @param sigma_u_lambda_true Length-2 numeric vector. True SD of region random
#'   intercepts in the mortality model.
#' @param sigma_v_lambda_true Length-2 numeric vector. True SD of national RW1
#'   time effects in the mortality model.
#' @param sigma_v_lambda_region_true Length-2 numeric vector. True SD of
#'   region-specific RW1 deviations around the national time effect.
#' @param phi_true Length-2 numeric vector. NB2 size (overdispersion)
#'   parameters for trauma and non-trauma deaths.
#' @param rho0_true Length-2 numeric vector. Baseline (pre-conflict) reporting
#'   completeness for trauma and non-trauma.
#' @param kappa_post_true Length-2 numeric vector. Post-conflict shift on the
#'   logit reporting scale for trauma and non-trauma.
#' @param gamma_conf_true Length-2 numeric vector. True effect of conflict on
#'   reporting completeness (logit scale).
#' @param gamma_conf_region_sd Length-2 numeric vector. Standard deviation of
#'   region-level deviations for the conflict effect in the reporting model.
#' @param gamma_fac_true Length-2 numeric vector. True effect of facility
#'   functioning on reporting completeness (logit scale).
#' @param sigma_u_rho_true Length-2 numeric vector. True SD of region random
#'   intercepts in the reporting model.
#' @param sigma_v_rho_true Length-2 numeric vector. True SD of national RW1 time
#'   effects in the reporting model.
#' @param sigma_v_rho_region_true Length-2 numeric vector. True SD of
#'   region-specific RW1 deviations around the national reporting time effect.
#' @param age_penalty_non Optional numeric vector of length equal to the number
#'   of age groups. If provided, gives the monotone age-selective reporting
#'   penalty for non-trauma post-conflict.
#' @param misclass A list with elements `p_non_to_trauma` and `p_trauma_to_non`
#'   giving misclassification probabilities among recorded deaths.
#'
#' @details
#' The `missing` argument allows additional missingness beyond the reporting
#' process. This is useful for stress testing identifiability.
#'
#' Supported patterns:
#'
#' * `type = "none"`: no additional missingness.
#' * `type = "block"`: region-time block dropout.
#' * `type = "age_selective"`: older-age dropout (typically for non-trauma).
#' * `type = "mnar"`: missing-not-at-random dropout driven by true death burden.
#' * `type = "combined"`: apply all three.
#'
#' If `missing$missing_to_zero = TRUE`, missing cells are converted to zeros
#' (tests robustness to bad pipelines). Otherwise missing values become `NA`.
#'
#' @return A list with elements:
#'   * `df_full`: full long-format dataset (may contain `NA` in `y`).
#'   * `df_obs`: subset of rows with observed `y`.
#'   * `truth`: latent arrays and parameters used for simulation.
#'   * `meta`: dimension metadata.
#'
#' @export
vrc_simulate <- function(
  R = 5,
  T = 120,
  age_breaks = c(0, 5, 15, 25, 35, 45, 55, 65, Inf),
  sexes = c("F", "M"),
  t0 = floor(T / 2),
  seed = 1,
  missing = list(
    type = "none",
    block_intercept = -2.5,
    block_conflict_coef = 0.8,
    block_facility_coef = -1.0,
    age_dropout_strength = 0.0,
    age_dropout_old_from = 6,
    age_dropout_post_only = TRUE,
    mnar_strength = 0.0,
    missing_to_zero = FALSE
  ),
  # Population
  pop_total_range = c(3e5, 1.2e6),
  pop_error_sd = 0.05,
  displacement_post_sd = 0.02,
  # Covariate dynamics
  conflict_post_mean = 1.0,
  conflict_post_sd = 0.6,
  conflict_spike_prob = 0.08,
  conflict_spike_mean = 2.0,
  facility_pre_mean = 0.9,
  facility_post_drop = 0.35,
  facility_noise_sd = 0.08,
  # True mortality parameters (roughly monthly rates)
  beta_conf_true = c(1.0, 0.20),
  beta_conf_region_sd = c(0.0, 0.0),
  beta_fac_mort_true = c(0.0, -0.10),
  sigma_u_lambda_true = c(0.35, 0.25),
  sigma_v_lambda_true = c(0.08, 0.05),
  sigma_v_lambda_region_true = c(0.0, 0.0),
  phi_true = c(30, 60),
  # Reporting parameters
  rho0_true = c(0.70, 0.90),
  kappa_post_true = c(0.00, -1.00),
  gamma_conf_true = c(-0.10, -0.50),
  gamma_conf_region_sd = c(0.0, 0.0),
  gamma_fac_true = c(0.60, 0.80),
  sigma_u_rho_true = c(0.40, 0.35),
  sigma_v_rho_true = c(0.12, 0.08),
  sigma_v_rho_region_true = c(0.0, 0.0),
  age_penalty_non = NULL,
  # Misclassification between observed trauma and non-trauma (only among recorded deaths)
  misclass = list(p_non_to_trauma = 0.04, p_trauma_to_non = 0.01)
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # ------------------------------------------------------------------
  # Complete the missingness specification.
  #
  # Users often supply a partial list such as:
  #   list(type = "block", block_intercept = -1.5)
  #
  # Without completing defaults, downstream arithmetic on missing$...
  # values can error when elements are NULL.
  # ------------------------------------------------------------------
  missing_defaults <- list(
    type = "none",
    block_intercept = -2.5,
    block_conflict_coef = 0.8,
    block_facility_coef = -1.0,
    age_dropout_strength = 0.0,
    age_dropout_old_from = 6,
    age_dropout_post_only = TRUE,
    mnar_strength = 0.0,
    missing_to_zero = FALSE
  )

  if (is.null(missing)) {
    missing <- missing_defaults
  }
  if (!is.list(missing)) {
    stop("missing must be a list (or NULL)", call. = FALSE)
  }
  missing <- utils::modifyList(missing_defaults, missing)
  missing$type <- tolower(as.character(missing$type)[1])
  allowed_types <- c("none", "block", "age_selective", "mnar", "combined")
  if (!missing$type %in% allowed_types) {
    stop(
      sprintf(
        "missing$type must be one of: %s",
        paste(allowed_types, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  A <- length(age_breaks) - 1
  S <- length(sexes)
  G <- 2L

  if (t0 < 1 || t0 > T) {
    stop("t0 must be between 1 and T", call. = FALSE)
  }

  logit <- function(p) stats::qlogis(p)
  inv_logit <- function(x) stats::plogis(x)

  rw1 <- function(T, sigma) {
    x <- cumsum(stats::rnorm(T, 0, sigma))
    x - mean(x)
  }

  # Age midpoints for baseline shapes
  age_low <- age_breaks[-length(age_breaks)]
  age_high <- age_breaks[-1]
  age_mid <- (age_low + pmin(age_high, age_low + 20)) / 2
  age_mid[is.infinite(age_high)] <- age_low[is.infinite(age_high)] + 10

  if (is.null(age_penalty_non)) {
    age_penalty_non <- seq(0, 1.5, length.out = A)
    age_penalty_non[1] <- 0
  } else {
    if (length(age_penalty_non) != A) {
      stop("age_penalty_non must have length A", call. = FALSE)
    }
  }

  # ----------------------------
  # Population (true + estimated)
  # ----------------------------
  total_pop_r <- round(stats::runif(R, pop_total_range[1], pop_total_range[2]))

  # region-specific age weights
  age_shape <- c(8, 10, 9, 8, 7, 6, 4, 3)[seq_len(A)]
  age_w <- matrix(NA_real_, R, A)
  for (r in seq_len(R)) {
    w <- stats::rgamma(A, shape = age_shape, rate = 1)
    age_w[r, ] <- w / sum(w)
  }

  sex_w <- c(0.50, 0.50)

  base_pop <- array(0, dim = c(R, A, S))
  for (r in seq_len(R)) {
    for (a in seq_len(A)) {
      for (s in seq_len(S)) {
        base_pop[r, a, s] <- total_pop_r[r] * age_w[r, a] * sex_w[s]
      }
    }
  }

  pop_scale <- matrix(1, R, T)
  for (r in seq_len(R)) {
    for (t in 2:T) {
      sd_t <- if (t >= t0) displacement_post_sd else displacement_post_sd / 4
      pop_scale[r, t] <- pop_scale[r, t - 1] * exp(stats::rnorm(1, 0, sd_t))
    }
  }

  N_true <- array(0, dim = c(R, T, A, S))
  for (r in seq_len(R)) {
    for (t in seq_len(T)) {
      N_true[r, t, , ] <- base_pop[r, , ] * pop_scale[r, t]
    }
  }

  N_est <- N_true * exp(stats::rnorm(length(N_true), 0, pop_error_sd))
  dim(N_est) <- dim(N_true)

  # ----------------------------
  # Covariates: conflict + facility (region-time)
  # ----------------------------
  conflict_rt <- matrix(0, R, T)
  for (r in seq_len(R)) {
    for (t in seq_len(T)) {
      if (t < t0) {
        conflict_rt[r, t] <- stats::rnorm(1, 0, 0.05)
      } else {
        base <- stats::rnorm(1, conflict_post_mean, conflict_post_sd)
        ar <- if (t == t0) base else 0.75 * conflict_rt[r, t - 1] + 0.25 * base
        spike <- if (stats::runif(1) < conflict_spike_prob) {
          stats::rgamma(1, shape = 2, scale = conflict_spike_mean / 2)
        } else {
          0
        }
        conflict_rt[r, t] <- max(0, ar + spike)
      }
    }
  }

  facility_rt <- matrix(NA_real_, R, T)
  for (r in seq_len(R)) {
    for (t in seq_len(T)) {
      if (t < t0) {
        facility_rt[r, t] <- pmin(
          1,
          pmax(0, stats::rnorm(1, facility_pre_mean, facility_noise_sd))
        )
      } else {
        base <- facility_pre_mean -
          facility_post_drop +
          stats::rnorm(1, 0, facility_noise_sd)
        harm <- 0.08 * conflict_rt[r, t]
        facility_rt[r, t] <- pmin(1, pmax(0, base - harm))
      }
    }
  }

  conflict_z <- as.numeric(base::scale(as.vector(conflict_rt)))
  facility_z <- as.numeric(base::scale(as.vector(facility_rt)))
  dim(conflict_z) <- c(R, T)
  dim(facility_z) <- c(R, T)

  post_t <- as.integer(seq_len(T) >= t0)

  # ----------------------------
  # Optional: region-varying conflict effects (random slopes)
  # ----------------------------
  beta_conf_region_true <- matrix(
    rep(beta_conf_true, each = R),
    nrow = R,
    ncol = G
  )
  if (any(beta_conf_region_sd != 0)) {
    for (g in seq_len(G)) {
      z <- stats::rnorm(R, 0, 1)
      z <- z - mean(z)
      s <- beta_conf_region_sd[g]
      beta_conf_region_true[, g] <- beta_conf_true[g] * exp(s * z - 0.5 * s^2)
    }
  }

  gamma_conf_region_true <- matrix(
    rep(gamma_conf_true, each = R),
    nrow = R,
    ncol = G
  )
  if (any(gamma_conf_region_sd != 0)) {
    for (g in seq_len(G)) {
      z <- stats::rnorm(R, 0, 1)
      z <- z - mean(z)
      gamma_conf_region_true[, g] <- gamma_conf_true[g] +
        gamma_conf_region_sd[g] * z
    }
  }

  # ----------------------------
  # Optional: region-specific time random walk deviations
  # ----------------------------
  v_lambda_region_true <- array(0, dim = c(T, R, G))
  for (g in seq_len(G)) {
    if (sigma_v_lambda_region_true[g] > 0) {
      for (r in seq_len(R)) {
        v_lambda_region_true[, r, g] <- rw1(T, sigma_v_lambda_region_true[g])
      }
    }
  }

  v_rho_region_true <- array(0, dim = c(T, R, G))
  for (g in seq_len(G)) {
    if (sigma_v_rho_region_true[g] > 0) {
      for (r in seq_len(R)) {
        v_rho_region_true[, r, g] <- rw1(T, sigma_v_rho_region_true[g])
      }
    }
  }

  # ----------------------------
  # True mortality: log lambda
  # ----------------------------
  non_age_shape <- 0.0010 * (age_mid - 30)^2
  non_age_shape <- non_age_shape - mean(non_age_shape)

  bump <- exp(-(age_mid - 25)^2 / (2 * 15^2))
  trauma_age_shape <- 1.2 * (bump - mean(bump))

  sex_eff_trauma <- c(0.0, 0.35) # F, M
  sex_eff_non <- c(0.0, 0.10)

  alpha0_true <- c(-10.5, -7.0) # trauma, non

  u_lambda_true <- cbind(
    stats::rnorm(R, 0, sigma_u_lambda_true[1]),
    stats::rnorm(R, 0, sigma_u_lambda_true[2])
  )

  v_lambda_true <- cbind(
    rw1(T, sigma_v_lambda_true[1]),
    rw1(T, sigma_v_lambda_true[2])
  )

  lambda_true <- array(NA_real_, dim = c(R, T, A, S, G))

  for (r in seq_len(R)) {
    for (t in seq_len(T)) {
      for (a in seq_len(A)) {
        for (s in seq_len(S)) {
          loglam1 <- alpha0_true[1] +
            trauma_age_shape[a] +
            sex_eff_trauma[s] +
            u_lambda_true[r, 1] +
            v_lambda_true[t, 1] +
            v_lambda_region_true[t, r, 1] +
            beta_conf_region_true[r, 1] * conflict_z[r, t] +
            beta_fac_mort_true[1] * facility_z[r, t]

          loglam2 <- alpha0_true[2] +
            non_age_shape[a] +
            sex_eff_non[s] +
            u_lambda_true[r, 2] +
            v_lambda_true[t, 2] +
            v_lambda_region_true[t, r, 2] +
            beta_conf_region_true[r, 2] * conflict_z[r, t] +
            beta_fac_mort_true[2] * facility_z[r, t]

          lambda_true[r, t, a, s, 1] <- exp(loglam1)
          lambda_true[r, t, a, s, 2] <- exp(loglam2)
        }
      }
    }
  }

  # ----------------------------
  # Reporting: logit rho
  # ----------------------------
  kappa0_true <- logit(rho0_true)

  u_rho_true <- cbind(
    stats::rnorm(R, 0, sigma_u_rho_true[1]),
    stats::rnorm(R, 0, sigma_u_rho_true[2])
  )

  v_rho_true <- cbind(
    rw1(T, sigma_v_rho_true[1]),
    rw1(T, sigma_v_rho_true[2])
  )

  rho_true <- array(NA_real_, dim = c(R, T, A, S, G))

  for (r in seq_len(R)) {
    for (t in seq_len(T)) {
      for (a in seq_len(A)) {
        for (s in seq_len(S)) {
          logrho1 <- kappa0_true[1] +
            kappa_post_true[1] * post_t[t] +
            u_rho_true[r, 1] +
            v_rho_true[t, 1] +
            v_rho_region_true[t, r, 1] +
            gamma_conf_region_true[r, 1] * conflict_z[r, t] +
            gamma_fac_true[1] * facility_z[r, t]

          age_pen <- -age_penalty_non[a] * post_t[t]
          logrho2 <- kappa0_true[2] +
            kappa_post_true[2] * post_t[t] +
            u_rho_true[r, 2] +
            v_rho_true[t, 2] +
            v_rho_region_true[t, r, 2] +
            gamma_conf_region_true[r, 2] * conflict_z[r, t] +
            gamma_fac_true[2] * facility_z[r, t] +
            age_pen

          rho_true[r, t, a, s, 1] <- inv_logit(logrho1)
          rho_true[r, t, a, s, 2] <- inv_logit(logrho2)
        }
      }
    }
  }

  # ----------------------------
  # Misclassification (among recorded deaths)
  # ----------------------------
  p_n2t <- misclass$p_non_to_trauma
  p_t2n <- misclass$p_trauma_to_non
  M <- matrix(
    c(
      1 - p_t2n,
      p_t2n,
      p_n2t,
      1 - p_n2t
    ),
    nrow = 2,
    byrow = TRUE
  )

  # ----------------------------
  # Generate true and observed deaths
  # ----------------------------
  D_true <- array(0L, dim = c(R, T, A, S, G))
  Y_obs <- array(0L, dim = c(R, T, A, S, G))

  for (r in seq_len(R)) {
    for (t in seq_len(T)) {
      for (a in seq_len(A)) {
        for (s in seq_len(S)) {
          for (g_true in 1:G) {
            mu_true <- N_true[r, t, a, s] * lambda_true[r, t, a, s, g_true]
            D <- stats::rnbinom(1, size = phi_true[g_true], mu = mu_true)
            D_true[r, t, a, s, g_true] <- D

            rho <- rho_true[r, t, a, s, g_true]
            p_obs1 <- rho * M[g_true, 1]
            p_obs2 <- rho * M[g_true, 2]
            p_unobs <- max(0, 1 - rho)

            alloc <- as.integer(stats::rmultinom(
              1,
              size = D,
              prob = c(p_obs1, p_obs2, p_unobs)
            ))
            Y_obs[r, t, a, s, 1] <- Y_obs[r, t, a, s, 1] + alloc[1]
            Y_obs[r, t, a, s, 2] <- Y_obs[r, t, a, s, 2] + alloc[2]
          }
        }
      }
    }
  }

  # ----------------------------
  # Build long-format data frame
  # ----------------------------
  df <- expand.grid(
    region = seq_len(R),
    time = seq_len(T),
    age = seq_len(A),
    sex = seq_len(S),
    cause = seq_len(G)
  )

  idx <- with(df, cbind(region, time, age, sex, cause))
  df$y <- Y_obs[idx]
  df$pop <- N_est[cbind(df$region, df$time, df$age, df$sex)]
  # By default, treat exposure as population for the period.
  # Users can override this (for example, pop * days_covered) before fitting.
  df$exposure <- df$pop
  df$conflict <- conflict_z[cbind(df$region, df$time)]
  df$facility <- facility_z[cbind(df$region, df$time)]
  df$post <- post_t[df$time]

  df$y_true_total <- rowSums(cbind(
    D_true[cbind(df$region, df$time, df$age, df$sex, 1)],
    D_true[cbind(df$region, df$time, df$age, df$sex, 2)]
  ))

  # ----------------------------
  # Apply additional missingness patterns
  # ----------------------------
  df$missing_flag <- FALSE

  if (!identical(missing$type, "none")) {
    if (missing$type %in% c("block", "combined")) {
      block_drop <- matrix(FALSE, R, T)
      for (r in seq_len(R)) {
        for (t in seq_len(T)) {
          lin <- missing$block_intercept +
            missing$block_conflict_coef * conflict_z[r, t] +
            missing$block_facility_coef * facility_z[r, t] +
            0.3 * stats::rnorm(1)
          p <- inv_logit(lin)
          block_drop[r, t] <- (stats::runif(1) < p)
        }
      }
      df$missing_flag <- df$missing_flag | block_drop[cbind(df$region, df$time)]
    }

    if (
      missing$type %in%
        c("age_selective", "combined") &&
        missing$age_dropout_strength > 0
    ) {
      is_old <- df$age >= missing$age_dropout_old_from
      is_post <- if (isTRUE(missing$age_dropout_post_only)) {
        df$post == 1
      } else {
        TRUE
      }
      lin <- logit(0.02) +
        missing$age_dropout_strength * df$conflict +
        0.6 * is_old +
        0.3 * is_post
      p <- inv_logit(lin)
      df$missing_flag <- df$missing_flag |
        (stats::runif(nrow(df)) < p & is_old & is_post & df$cause == 2)
    }

    if (missing$type %in% c("mnar", "combined") && missing$mnar_strength > 0) {
      overload <- as.numeric(base::scale(df$y_true_total))
      lin <- logit(0.01) + missing$mnar_strength * overload
      p <- inv_logit(lin)
      df$missing_flag <- df$missing_flag | (stats::runif(nrow(df)) < p)
    }

    if (isTRUE(missing$missing_to_zero)) {
      df$y[df$missing_flag] <- 0L
    } else {
      df$y[df$missing_flag] <- NA_integer_
    }
  }

  df_obs <- df[!is.na(df$y), , drop = FALSE]

  truth <- list(
    N_true = N_true,
    N_est = N_est,
    conflict_rt = conflict_rt,
    facility_rt = facility_rt,
    conflict_z = conflict_z,
    facility_z = facility_z,
    beta_conf_region_true = beta_conf_region_true,
    gamma_conf_region_true = gamma_conf_region_true,
    v_lambda_region_true = v_lambda_region_true,
    v_rho_region_true = v_rho_region_true,
    lambda_true = lambda_true,
    rho_true = rho_true,
    D_true = D_true,
    Y_obs = Y_obs,
    params = list(
      alpha0_true = alpha0_true,
      beta_conf_true = beta_conf_true,
      beta_conf_region_sd = beta_conf_region_sd,
      beta_fac_mort_true = beta_fac_mort_true,
      sigma_u_lambda_true = sigma_u_lambda_true,
      sigma_v_lambda_true = sigma_v_lambda_true,
      sigma_v_lambda_region_true = sigma_v_lambda_region_true,
      phi_true = phi_true,
      rho0_true = rho0_true,
      kappa_post_true = kappa_post_true,
      gamma_conf_true = gamma_conf_true,
      gamma_conf_region_sd = gamma_conf_region_sd,
      gamma_fac_true = gamma_fac_true,
      sigma_u_rho_true = sigma_u_rho_true,
      sigma_v_rho_true = sigma_v_rho_true,
      sigma_v_rho_region_true = sigma_v_rho_region_true,
      age_penalty_non = age_penalty_non,
      misclass = misclass,
      M = M
    )
  )

  meta <- list(
    R = R,
    T = T,
    A = A,
    S = S,
    G = G,
    t0 = t0,
    age_breaks = age_breaks,
    sexes = sexes,
    post_t = post_t
  )

  list(df_full = df, df_obs = df_obs, truth = truth, meta = meta)
}


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
vrc_sim_truth <- function(sim) {
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
