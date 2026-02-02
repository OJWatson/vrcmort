# Tutorial: exploring covariates and pooling

## Goal

This tutorial shows a pragmatic workflow for:

1.  fitting a simple base model,
2.  adding covariates to either the mortality or reporting submodel,
3.  turning on partial pooling for conflict effects by region,
4.  comparing fitted models.

We use simulated data for illustration, but the same code works with
your region-month VR long table.

## Simulate a data set (optional)

``` r
library(vrcmort)

sim <- vrc_simulate(R = 5, T = 48, t0 = 20, seed = 1)
vr_long <- sim$df_obs

t0 <- sim$meta$t0
```

## Step 1: fit a base model

Start with no additional covariates beyond the built-in conflict proxy.

``` r
fit0 <- vrcm(
  mortality = vrc_mortality(~ 1),
  reporting = vrc_reporting(~ 1),
  data = vr_long,
  t0 = t0,
  chains = 4,
  iter = 1000,
  seed = 1
)

plot(fit0, type = "reporting")
plot(fit0, type = "mortality", value = "true_deaths")
```

## Step 2: add covariates

A rule of thumb is:

- covariates that affect the true burden (for example food insecurity)
  go in the mortality model,
- covariates that affect what is recorded (for example facility
  functioning) go in the reporting model.

``` r
fit1 <- vrcm(
  mortality = vrc_mortality(~ facility),
  reporting = vrc_reporting(~ facility),
  data = vr_long,
  t0 = t0,
  chains = 4,
  iter = 1000,
  seed = 1
)

vrc_coef_summary(fit1)
```

## Step 3: allow region-varying conflict effects (partial pooling)

If conflict affects regions differently, or if reporting collapses
differently, allow the conflict slope to vary by region.

``` r
fit2 <- vrcm(
  mortality = vrc_mortality(~ facility, conflict = "region"),
  reporting = vrc_reporting(~ facility, conflict = "region"),
  data = vr_long,
  t0 = t0,
  chains = 4,
  iter = 1000,
  seed = 1
)

# Region-specific conflict effects
vrc_conflict_effects(fit2, component = "mortality")
vrc_conflict_effects(fit2, component = "reporting")
```

## Step 4: add region-specific time trends

If regions have local temporal shocks (outages, displacement waves,
localised violence) you can add region-specific random walk deviations
around the national trend.

``` r
fit3 <- vrcm(
  mortality = vrc_mortality(~ facility, conflict = "region", time = "region"),
  reporting = vrc_reporting(~ facility, conflict = "region", time = "region"),
  data = vr_long,
  t0 = t0,
  chains = 4,
  iter = 1000,
  seed = 1
)
```

## Step 5: compare models

There are three complementary ways to compare models in this setting.

### 1. Substantive plausibility

Check whether the fitted model produces sensible behaviour:

- trauma mortality increases with conflict,
- non-trauma mortality does not implausibly decrease,
- inferred completeness drops post-conflict, especially for older
  non-trauma deaths.

### 2. Posterior predictive checks

``` r
pp <- posterior_predict(fit2)
# Compare distributions of y_rep to observed y
```

### 3. Predictive model comparison with LOO

The Stan model includes pointwise `log_lik`, so you can use `loo` if you
want.

``` r
library(loo)

loglik0 <- rstan::extract(fit0$stanfit, pars = "log_lik")$log_lik
loglik1 <- rstan::extract(fit1$stanfit, pars = "log_lik")$log_lik
loglik2 <- rstan::extract(fit2$stanfit, pars = "log_lik")$log_lik

loo0 <- loo(loglik0)
loo1 <- loo(loglik1)
loo2 <- loo(loglik2)

loo_compare(loo0, loo1, loo2)
```

LOO is not a substitute for scientific judgement, but it is useful for
checking whether added complexity is actually improving predictive fit.

## Suggested workflow for real analyses

1.  Fit a base model.
2.  Add the covariates you believe are essential.
3.  Turn on partial pooling for conflict effects if you see clear
    regional heterogeneity.
4.  Use simulation (and the `missingness` vignette) to test whether the
    model recovers truth under plausible failure modes.
