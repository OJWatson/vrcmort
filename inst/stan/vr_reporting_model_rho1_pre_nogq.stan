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
