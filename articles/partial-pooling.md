# Partial Pooling

## Why partial pooling matters

In many conflict mortality applications you have a small number of
regions (for example five), and the data quality may vary sharply across
them.

A fully pooled model (one coefficient shared across regions) can be too
rigid: it forces conflict to have the same effect everywhere.

A fully separate model (fit each region independently) can be too noisy:
sparse regions can produce extreme estimates driven by reporting
artefacts.

**Partial pooling** is the compromise: each region gets its own effect,
but regions share information through a hierarchical prior.

`vrcmort` uses partial pooling in several places by default (for example
region random intercepts), and also allows partial pooling for conflict
effects.

## Region random intercepts

Both the mortality and reporting submodels include region-level random
intercepts by cause group.

For mortality:

$$u_{r,g}^{(\lambda)} \sim \mathcal{N}\left( 0,\sigma_{u,g}^{(\lambda)} \right).$$

For reporting:

$$u_{r,g}^{(\rho)} \sim \mathcal{N}\left( 0,\sigma_{u,g}^{(\rho)} \right).$$

These terms capture persistent differences between regions (baseline
death rates and baseline completeness) that are not explained by
covariates.

## Partial pooling for conflict effects (random slopes)

A core extension in `vrcmort` is allowing the conflict effect to vary by
region.

### Mortality side

Instead of a single conflict effect $\beta_{conf,g}$, we allow:

$$\beta_{conf,r,g} = \beta_{conf,g} + b_{r,g}^{(\beta)},$$

where

$$b_{r,g}^{(\beta)} \sim \mathcal{N}\left( 0,\sigma_{\beta,g} \right).$$

This lets conflict have a stronger association with true mortality in
some regions than others, while shrinking weakly informed regions back
towards the global mean.

In the Stan program the region-specific effects are parameterised as a
non-centred random effect for better sampling.

### Reporting side

Similarly, the effect of conflict on reporting completeness can vary by
region:

$$\gamma_{conf,r,g} = \gamma_{conf,g} + b_{r,g}^{(\gamma)},\quad b_{r,g}^{(\gamma)} \sim \mathcal{N}\left( 0,\sigma_{\gamma,g} \right).$$

This is useful when some regions experience a much sharper collapse in
registration after conflict starts.

## Region-specific time trends

The base model includes national random walks in time for both mortality
and reporting.

In many conflict settings, you also want region-specific departures from
the national trend. `vrcmort` supports this by adding a region-specific
random walk deviation around the national time effect.

Conceptually:

$$v_{t,g}^{(\lambda)} + v_{r,t,g}^{(\lambda,reg)}$$

and

$$v_{t,g}^{(\rho)} + v_{r,t,g}^{(\rho,reg)}.$$

These terms can absorb local shocks (for example temporary outages or
spikes) without forcing all regions to move together.

## How to specify pooling in vrcmort

Pooling options are controlled through the component specifications
passed to
[`vrcm()`](https://github.com/OJWatson/vrcmort/reference/vrcm.md).

``` r
library(vrcmort)

# Region-varying conflict effects and region-specific time trends
mort_pp <- vrc_mortality(~ facility, conflict = "region", time = "region")
rep_pp  <- vrc_reporting(~ facility, conflict = "region", time = "region")

fit_pp <- vrcm(
  mortality = mort_pp,
  reporting = rep_pp,
  data = vr_long,
  t0 = conflict_start_month,
  chains = 4,
  iter = 1000
)

# Extract region-specific conflict effects
vrc_conflict_effects(fit_pp, component = "mortality")
vrc_conflict_effects(fit_pp, component = "reporting")
```

## Practical advice

- If you have only a few regions, partial pooling is usually safer than
  fitting separate models.
- If reporting collapse clearly differs by region, allowing
  region-varying conflict effects in the reporting model is often
  important.
- Region-specific time trends can be valuable, but they add parameters.
  If sampling becomes difficult, start with national trends only and add
  region-specific trends later.
