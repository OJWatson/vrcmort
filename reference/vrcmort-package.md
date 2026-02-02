# vrcmort: Vital registration mortality models with reporting processes

The `vrcmort` package provides tools for fitting hierarchical Bayesian
models to cause-specific vital registration (VR) mortality counts when
the VR reporting mechanism changes over time (for example, during armed
conflict). The core model separates a latent mortality process from an
observation (reporting) process, allowing conflict to increase true
mortality while observed non-trauma VR counts can decline because
reporting collapses.

The package also includes a simulator with multiple missingness
mechanisms for stress testing inference.
