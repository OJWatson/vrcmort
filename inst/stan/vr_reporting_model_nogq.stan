#include "include/license.stan"

functions {
#include "functions/rw1_centered.stan"
}

data {
#include "data/data_vr.stan"
}

transformed data {
#include "data/tdata_vr.stan"
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
