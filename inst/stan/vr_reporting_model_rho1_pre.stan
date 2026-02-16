// VR mortality + reporting model variant with pre-conflict reporting fixed to 1
//
// This model is identical to vr_reporting_model except that pre-conflict
// observations are assumed fully observed (rho = 1), and the reporting
// intercept is defined only for post-conflict time points.

#include "include/license.stan"

functions {
#include "functions/rw1_centered.stan"
}

data {
#include "data/data_vr.stan"
}

parameters {
#include "parameters/parameters_vr_rho1_pre.stan"
}

transformed parameters {
#include "tparameters/tparameters_vr.stan"
}

model {
#include "model/priors_vr_rho1_pre.stan"
#include "model/likelihood_vr_nb2_rho1_pre.stan"
}

generated quantities {
#include "generated_quantities/gq_vr_rho1_pre.stan"
}
