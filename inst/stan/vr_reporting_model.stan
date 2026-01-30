// Base VR mortality + reporting model (assembled from include files)
//
// This file mirrors the modular Stan layout used by the epidemia R package.
// Individual pieces live under inst/stan/{functions,data,parameters,tparameters,model,generated_quantities}.

#include "include/license.stan"

functions {
#include "functions/rw1_centered.stan"
}

data {
#include "data/data_vr.stan"
}

parameters {
#include "parameters/parameters_vr.stan"
}

transformed parameters {
#include "tparameters/tparameters_vr.stan"
}

model {
#include "model/priors_vr.stan"
#include "model/likelihood_vr_nb2.stan"
}

generated quantities {
#include "generated_quantities/gq_vr.stan"
}
