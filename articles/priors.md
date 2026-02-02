# Priors

## Why priors are central in this model

The likelihood for VR counts depends on the product of the latent
mortality rate and the reporting completeness:

$$\mu = E \cdot \lambda \cdot \rho.$$

Without additional information, many combinations of $\lambda$ and
$\rho$ can produce the same $\mu$. This means priors are not a minor
detail; they are part of what makes inference possible.

This vignette explains:

- which priors exist in the base model,
- what they mean, and
- how to change them using \[vrc_priors()\].

## The prior interface

`vrcmort` uses a simple prior specification system inspired by
`rstanarm` and `epidemia`.

You create a prior bundle with
[`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md)
and pass it into
[`vrcm()`](https://github.com/OJWatson/vrcmort/reference/vrcm.md) or
[`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md).

``` r
pri <- vrc_priors(
  beta_conf = normal(0.3, 0.2),
  beta_mort = normal(0, 0.5, autoscale = TRUE),
  gamma_rep = normal(0, 0.7, autoscale = TRUE),
  phi = exponential(1)
)

fit <- vrcm(
  mortality = vrc_mortality(~ facility),
  reporting = vrc_reporting(~ facility),
  data = vr_long,
  t0 = conflict_start_month,
  priors = pri,
  chains = 4,
  iter = 1000
)
```

You can inspect the resolved priors (after autoscaling) with:

``` r
vrc_prior_summary(fit)
```

## Baseline completeness priors

The most influential priors are the baseline completeness terms
$\kappa_{0,g}$ on the logit scale.

In the base model:

$$\text{logit}\left( \rho_{r,t,a,s,g} \right) = \kappa_{0,g} + \ldots$$

A practical way to think about this is that $\kappa_{0,g}$ sets the
*typical completeness before conflict starts*.

If you have strong prior knowledge (for example from capture-recapture
work, demographic methods, or historical audits), you should use it
here.

In `vrcmort` the default priors are intentionally different for trauma
and non-trauma (for $G = 2$):

- trauma completeness is often lower (facility-based capture),
- non-trauma completeness can be higher pre-conflict.

To change these priors, provide a vector on the logit scale:

``` r
# Suppose you believe pre-conflict completeness is around:
# trauma ~ 0.6, non-trauma ~ 0.9
pri <- vrc_priors(
  kappa0 = normal(qlogis(c(0.6, 0.9)), c(0.6, 0.4))
)
```

## Conflict effect priors

### Mortality conflict effect (beta_conf)

The mortality submodel includes a conflict effect $\beta_{conf,g}$ on
the log rate.

The base model constrains this parameter to be non-negative:

$$\beta_{conf,g} \geq 0.$$

This encodes a weak but important assumption: conflict should not reduce
true mortality.

A typical weakly informative prior is a half-normal (implemented as
[`normal()`](https://github.com/OJWatson/vrcmort/reference/normal.md) on
a constrained parameter in Stan):

``` r
pri <- vrc_priors(
  beta_conf = normal(0.2, 0.3)
)
```

### Reporting conflict effect (gamma_conf)

The reporting submodel includes $\gamma_{conf,g}$ on the logit
completeness.

This can be either positive or negative. For non-trauma, it is often
negative (conflict reduces completeness). For trauma, it can be closer
to zero or even positive if conflict-related trauma is preferentially
recorded.

``` r
pri <- vrc_priors(
  gamma_conf = normal(0, 0.5)
)
```

## Random effect scale priors

Random effect standard deviations control how much regions and time
periods are allowed to differ.

Examples:

- $\sigma_{u,g}^{(\lambda)}$: region variation in mortality
- $\sigma_{u,g}^{(\rho)}$: region variation in completeness
- $\sigma_{v,g}^{(\lambda)}$: smoothness of the national time random
  walk

The default priors are weakly regularising half-normals. You can tighten
them if you find the model is over-fitting noise.

## Dispersion priors

The observation model uses a negative binomial with dispersion parameter
$\phi_{g}$ per cause.

In `vrcmort` we use an exponential prior on $\phi_{g}$:

$$\phi_{g} \sim \text{Exponential}\left( \text{rate} \right).$$

Smaller $\phi$ implies more over-dispersion. Larger $\phi$ approaches a
Poisson model.

``` r
# More over-dispersion
pri <- vrc_priors(phi = exponential(2))

# Less over-dispersion (more Poisson-like)
pri <- vrc_priors(phi = exponential(0.5))
```

## Age-selective reporting priors

The age-selective reporting penalty is implemented as a monotone
sequence $\delta_{a}$ that is multiplied by `post` and applied to
non-trauma completeness.

The priors on the increments and overall scale control how strongly the
model is allowed to down-weight older ages post-conflict.

If your data clearly show older deaths disappearing from VR, loosening
these priors can help the model explain the age shift through reporting
rather than through implausible mortality changes.

## Prior recipes

Here are three common prior bundles as starting points.

### 1. Conservative completeness collapse

Assume VR remains fairly complete post-conflict.

``` r
pri <- vrc_priors(
  kappa_post = normal(0, 0.3),
  delta_age_scale = normal(0, 0.5)
)
```

### 2. Strong reporting collapse for non-trauma

Assume non-trauma completeness drops sharply after conflict starts.

``` r
pri <- vrc_priors(
  kappa_post = normal(c(0, -1.5), c(0.5, 0.5)),
  gamma_conf = normal(c(0, -0.5), c(0.5, 0.5))
)
```

### 3. Strong prior that conflict increases trauma mortality

``` r
pri <- vrc_priors(
  beta_conf = normal(c(0.6, 0.1), c(0.3, 0.2))
)
```

These are not universally correct; the goal is to make your assumptions
explicit and test sensitivity.
