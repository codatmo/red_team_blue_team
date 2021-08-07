data {
  int<lower = 0> N;           // number of data elements
  vector[N] x_calls_111;      // predictor vector
  vector[N] y_cvd_hosp;      // outcomes vector
  int<lower=0, upper=1> compute_likelihood; 
  int<lower=0, upper=1> compute_prediction;
}

transformed data {
  int P = compute_prediction ? N : 0; //controls whether predictions are accumulated, see generated quantites{}
}

parameters {
  real beta_coef_111_call;
  real alpha_intercept;
  real<lower = 0> sigma_sd; 
}

model {
  alpha_intercept ~ normal(0,1000);
  beta_coef_111_call ~ normal(0, 2);
  sigma_sd ~ normal(500, 300);
  if (compute_likelihood == 1) {
    for (n in 1:N) {
      y_cvd_hosp[n] ~ normal(alpha_intercept + beta_coef_111_call * x_calls_111[n], 
                                sigma_sd); // likelihood
    }
  }
}

generated quantities {
  vector[P] y_cvd_hosp_pred; // if P==0 then variable is not accumulated in posterior draws
    if (compute_prediction == 1) {
      for (p in 1:P) {
        y_cvd_hosp_pred[p] = normal_rng(alpha_intercept + beta_coef_111_call*x_calls_111[p],
                                        sigma_sd);
      }
    }
}
