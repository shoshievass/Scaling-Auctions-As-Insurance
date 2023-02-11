data {
  int N; // number of observations
  int P; // number of covariates in X
  vector[N] q_at;
  vector[N] q_ot;
  matrix[N, P] X;
  int<lower = 1, upper = 3> state[N]; // 1 for q_at == 0, 2 for q_at==q_ot
}
parameters {
  vector[P+1] gamma;
  vector[P+1] beta;
}
transformed parameters {
//
}
model {
  // priors
  gamma[1] ~ normal(-5, 1);
  gamma[2:] ~ normal(0, .1);
  beta[1] ~ normal(1.1, .1);
  beta[2:] ~ normal(0, 1);

  // likelihood
  for(n in 1:N) {
      target += normal_lpdf(q_at[n]/q_ot[n] |
                            beta[1] + X[n] * beta[2:],
                            exp(gamma[1] + X[n]*gamma[2:]));
  }
}
generated quantities {
  vector[N] q_at_model;
  vector[N] sigma_t;

  for(n in 1:N){
    q_at_model[n] = beta[1] +  X[n] * beta[2:];
    sigma_t[n] = exp(gamma[1] + X[n]*gamma[2:]);
  }
}